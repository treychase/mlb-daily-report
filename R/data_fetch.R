# ── Data Fetching Functions ────────────────────────────────────────────────

fetch_schedule <- function() {
  # Attempt 1: newer baseballr (start_date / end_date params)
  s <- tryCatch(
    suppressWarnings(
      mlb_schedule(start_date = TODAY_STR, end_date = TODAY_STR, sport_id = 1)
    ),
    error = function(e) NULL
  )
  if (!is.null(s) && nrow(s) > 0) return(s)

  # Attempt 2: single date= param (some baseballr builds)
  s <- tryCatch(
    mlb_schedule(date = TODAY_STR, sport_id = 1),
    error = function(e) NULL
  )
  if (!is.null(s) && nrow(s) > 0) return(s)

  # Attempt 3: date= without sport_id
  s <- tryCatch(
    mlb_schedule(date = TODAY_STR),
    error = function(e) NULL
  )
  if (!is.null(s) && nrow(s) > 0) return(s)

  # Attempt 4: direct MLB Stats API JSON -- always works
  message("mlb_schedule() failed; falling back to direct MLB Stats API...")
  safe({
    url  <- paste0(
      "https://statsapi.mlb.com/api/v1/schedule",
      "?sportId=1&gameType=R&date=", TODAY_STR,
      "&hydrate=team,linescore,venue,probablePitcher"
    )
    resp <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    dates <- resp$dates
    if (length(dates) == 0) return(data.frame())
    games <- dates[[1]]$games
    bind_rows(lapply(games, function(g) {
      data.frame(
        game_pk                    = g$gamePk,
        game_type                  = g$gameType %||% "R",
        game_datetime              = g$gameDate %||% NA_character_,
        status_detailed_state      = g$status$detailedState    %||% "Unknown",
        status_abstract_game_state = g$status$abstractGameState %||% "Unknown",
        teams_away_team_name       = g$teams$away$team$name    %||% "Away",
        teams_home_team_name       = g$teams$home$team$name    %||% "Home",
        teams_away_team_id  = { tmp <- tryCatch(g$teams$away$team$id, error = function(e) NULL)
                                if (is.null(tmp) || length(tmp) == 0) NA_integer_ else as.integer(tmp) },
        teams_home_team_id  = { tmp <- tryCatch(g$teams$home$team$id, error = function(e) NULL)
                                if (is.null(tmp) || length(tmp) == 0) NA_integer_ else as.integer(tmp) },
        teams_away_score    = { tmp <- tryCatch(g$teams$away$score, error = function(e) NULL)
                                if (is.null(tmp) || length(tmp) == 0) NA_integer_ else as.integer(tmp) },
        teams_home_score    = { tmp <- tryCatch(g$teams$home$score, error = function(e) NULL)
                                if (is.null(tmp) || length(tmp) == 0) NA_integer_ else as.integer(tmp) },
        venue_name          = { tmp <- tryCatch(g$venue$name, error = function(e) NULL)
                                if (is.null(tmp) || length(tmp) == 0) NA_character_ else as.character(tmp) },
        teams_away_probable = { tmp <- tryCatch(g$teams$away$probablePitcher$fullName, error = function(e) NULL)
                                if (is.null(tmp) || length(tmp) == 0) NA_character_ else as.character(tmp) },
        teams_home_probable = { tmp <- tryCatch(g$teams$home$probablePitcher$fullName, error = function(e) NULL)
                                if (is.null(tmp) || length(tmp) == 0) NA_character_ else as.character(tmp) },
        stringsAsFactors    = FALSE
      )
    }))
  })
}

# --- Boxscore ---------------------------------------------------------------
# mlb_boxscore() is absent in many baseballr builds; call the API directly.
# The response structure matches what parse_batting/parse_pitching expect.

fetch_boxscore <- function(game_pk) {
  safe({
    url <- paste0("https://statsapi.mlb.com/api/v1/game/",
                  as.integer(game_pk), "/boxscore")
    jsonlite::fromJSON(url, simplifyVector = FALSE)
  })
}

# --- Linescore ---------------------------------------------------------------
fetch_linescore <- function(game_pk) {
  safe({
    url <- paste0("https://statsapi.mlb.com/api/v1/game/",
                  as.integer(game_pk), "/linescore")
    jsonlite::fromJSON(url, simplifyVector = FALSE)
  })
}

# --- Batting ----------------------------------------------------------------

fetch_lb_batters_raw <- function() {
  message("Fetching hitter leaderboard from Baseball Reference...")
  df <- safe({
    yr <- format(TODAY, "%Y")
    baseballr::bref_daily_batter(
      t1 = paste0(yr, "-03-15"),
      t2 = format(TODAY, "%Y-%m-%d")
    )
  })
  if (!is.null(df)) .war_cache$batters <- df   # populate WAR cache
  df
}
fetch_lb_batters <- memoise::memoise(fetch_lb_batters_raw)

fetch_lb_pitchers_raw <- function() {
  message("Fetching pitcher leaderboard from Baseball Reference...")
  df <- safe({
    yr <- format(TODAY, "%Y")
    baseballr::bref_daily_pitcher(
      t1 = paste0(yr, "-03-15"),
      t2 = format(TODAY, "%Y-%m-%d")
    )
  })
  if (!is.null(df)) .war_cache$pitchers <- df  # populate WAR cache
  df
}
fetch_lb_pitchers <- memoise::memoise(fetch_lb_pitchers_raw)

# --- Statcast movement data (pitcher, grouped by pitch type) ------------------

fetch_pitcher_movement_raw <- function(person_id) {
  safe({
    season  <- format(TODAY, "%Y")
    # type=details returns raw pitches with consistent column names (pfx_x, pfx_z,
    # release_speed, release_spin_rate).  We aggregate to per-pitch-type in R.
    mov_url <- paste0(
      "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
      "&player_id=", as.integer(person_id),
      "&player_type=pitcher",
      "&hfSea=", season, "%7C",
      "&type=details"
    )
    df <- suppressWarnings(read.csv(mov_url, check.names=FALSE, stringsAsFactors=FALSE))
    if (nrow(df) == 0 || !"pitch_type" %in% names(df)) return(NULL)
    if (!all(c("pfx_x","pfx_z") %in% names(df))) return(NULL)

    pname_col <- intersect(c("pitch_type_description","pitch_name"), names(df))[1]

    agg <- df %>%
      dplyr::filter(!is.na(pitch_type), nchar(as.character(pitch_type)) > 0) %>%
      dplyr::mutate(
        pfx_x_n = suppressWarnings(as.numeric(.data[["pfx_x"]])),
        pfx_z_n = suppressWarnings(as.numeric(.data[["pfx_z"]])),
        spd_n   = suppressWarnings(as.numeric(.data[["release_speed"]])),
        spin_n  = suppressWarnings(as.numeric(.data[["release_spin_rate"]]))
      ) %>%
      dplyr::group_by(pitch_type) %>%
      dplyr::summarise(
        pitch_name        = if (!is.na(pname_col))
                              dplyr::first(.data[[pname_col]])
                            else dplyr::first(pitch_type),
        pfx_x             = mean(pfx_x_n, na.rm = TRUE),
        pfx_z             = mean(pfx_z_n, na.rm = TRUE),
        release_speed     = mean(spd_n,   na.rm = TRUE),
        release_spin_rate = mean(spin_n,  na.rm = TRUE),
        plate_x           = mean(suppressWarnings(as.numeric(.data[["plate_x"]])), na.rm=TRUE),
        plate_z           = mean(suppressWarnings(as.numeric(.data[["plate_z"]])), na.rm=TRUE),
        n_pitches         = dplyr::n(),
        .groups           = "drop"
      ) %>%
      dplyr::filter(!is.na(pfx_x), !is.na(pfx_z))

    if (nrow(agg) == 0) return(NULL)

    tot              <- sum(agg$n_pitches, na.rm = TRUE)
    agg$pitch_percent <- agg$n_pitches / max(tot, 1)
    agg$h_break_in    <- agg$pfx_x * 12
    agg$v_break_in    <- agg$pfx_z * 12
    agg$velo           <- agg$release_speed
    agg$spin_rate      <- agg$release_spin_rate

    tibble::as_tibble(agg)
  })
}
# Memoised so repeated clicks on the same pitcher are instant
fetch_pitcher_movement <- memoise::memoise(fetch_pitcher_movement_raw)
fetch_pitcher_movement <- memoise::memoise(fetch_pitcher_movement_raw)

# Cache for WAR values — populated when leaderboard is fetched,
# consulted when player modal opens (avoids triggering a slow bref fetch).
.war_cache <- new.env(parent = emptyenv())
.war_cache$batters  <- NULL
.war_cache$pitchers <- NULL


# --- Monthly Statcast -------------------------------------------------------
# statcast_search() has a hardcoded 92-name vector; Savant now returns 118 cols.
# Read the CSV directly so column names come straight from the actual header.
# Wrapped in memoise() so the 16 MB download only happens once per session.

fetch_monthly_sc_raw <- function() {
  message("Fetching monthly Statcast (", MONTH_START, " -> ", TODAY_STR, ")...")
  safe({
    sc_url <- paste0(
      "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
      "&hfGT=R%7CPO%7CS%7C",
      "&hfSea=", format(TODAY, "%Y"), "%7C",
      "&player_type=batter",
      "&game_date_gt=", MONTH_START,
      "&game_date_lt=", TODAY_STR,
      "&min_pitches=0&min_results=0&group_by=name",
      "&sort_col=pitches&player_event_sort=h_launch_speed",
      "&sort_order=desc&min_abs=0&type=details"
    )
    df <- read.csv(sc_url, check.names = FALSE, stringsAsFactors = FALSE)
    tibble::as_tibble(df)
  })
}
fetch_monthly_sc <- memoise::memoise(fetch_monthly_sc_raw)

# --- Player season-stats (for click-through modal) --------------------------

fetch_player_season_stats <- function(person_id) {
  safe({
    season  <- format(TODAY, "%Y")
    bio_url <- paste0("https://statsapi.mlb.com/api/v1/people/", person_id,
                      "?hydrate=currentTeam")
    hit_url <- paste0("https://statsapi.mlb.com/api/v1/people/", person_id,
                      "/stats?stats=season&group=hitting&season=",  season, "&sportId=1")
    pit_url <- paste0("https://statsapi.mlb.com/api/v1/people/", person_id,
                      "/stats?stats=season&group=pitching&season=", season, "&sportId=1")

    bio_data   <- jsonlite::fromJSON(bio_url, simplifyVector = FALSE)$people[[1]]
    is_pitcher <- tryCatch(
      !is.null(bio_data$primaryPosition) &&
        bio_data$primaryPosition$code %in% c("1"),
      error = function(e) FALSE
    )
    hit_data <- jsonlite::fromJSON(hit_url, simplifyVector = FALSE)$stats
    pit_data <- jsonlite::fromJSON(pit_url, simplifyVector = FALSE)$stats
    team_id  <- tryCatch(as.integer(bio_data$currentTeam$id), error = function(e) NA_integer_)

    # Compute wRC+ from hitting stats (2024 linear weights & league constants)
    wrc_plus <- if (length(hit_data) > 0 && length(hit_data[[1]]$splits) > 0) {
      s <- hit_data[[1]]$splits[[1]]$stat
      tryCatch({
        bb  <- as.numeric(s$baseOnBalls %||% 0)
        hbp <- as.numeric(s$hitByPitch  %||% 0)
        sf  <- as.numeric(s$sacFlies    %||% 0)
        h   <- as.numeric(s$hits        %||% 0)
        d   <- as.numeric(s$doubles     %||% 0)
        tri <- as.numeric(s$triples     %||% 0)
        hr  <- as.numeric(s$homeRuns    %||% 0)
        ab  <- as.numeric(s$atBats      %||% 0)
        pa  <- ab + bb + hbp + sf
        if (pa < 10) return(NA_real_)
        single <- h - d - tri - hr
        woba <- (0.690*bb + 0.722*hbp + 0.888*single +
                 1.271*d  + 1.616*tri  + 2.101*hr) /
                max(1, ab + bb + hbp + sf)
        round(100 * ((woba - 0.317) / 1.21 + 0.127) / 0.127)
      }, error = function(e) NA_real_)
    } else NA_real_

    # Pitch arsenal from Baseball Savant (pitchers only)
    pitch_arsenal <- if (is_pitcher) {
      safe({
        ars_url <- paste0(
          "https://baseballsavant.mlb.com/api/vr/pitch-arsenal-stats",
          "?min=10&year=", season,
          "&team=&breakdown=&position=&type=pitcher&id=", person_id
        )
        df <- suppressWarnings(jsonlite::fromJSON(ars_url, simplifyDataFrame = TRUE))
        if (is.data.frame(df) && nrow(df) > 0) {
          # Compute usage % from count / total pitches
          if (!"pitch_percent" %in% names(df) && "count" %in% names(df)) {
            total <- sum(as.numeric(df$count), na.rm = TRUE)
            df$pitch_percent <- if (total > 0) as.numeric(df$count) / total else NA_real_
          }
          df[order(-df$pitch_percent), ]
        } else NULL
      })
    } else NULL

    pct_type <- if (is_pitcher) "pitcher" else "batter"
    pct_url  <- paste0("https://baseballsavant.mlb.com/api/vr/percentile-rankings?",
                       "type=", pct_type, "&playerId=", person_id, "&season=", season)
    pct_data <- safe(suppressWarnings(jsonlite::fromJSON(pct_url, simplifyVector = TRUE)))

    # L/R splits for hitters (vs Left-Handed Pitching / vs Right-Handed Pitching)
    splits_data <- if (!is_pitcher) {
      safe({
        sp_url <- paste0("https://statsapi.mlb.com/api/v1/people/", person_id,
                         "/stats?stats=statSplits&group=hitting&season=", season,
                         "&sportId=1&sitCodes=vl,vr")
        sp_raw <- jsonlite::fromJSON(sp_url, simplifyVector = FALSE)$stats
        if (is.null(sp_raw) || length(sp_raw) == 0) return(list(vl=NULL, vr=NULL))
        vl <- NULL; vr <- NULL
        for (sp in sp_raw[[1]]$splits) {
          code <- tryCatch(sp$split$code, error = function(e) "")
          if (identical(code, "vl")) vl <- sp$stat
          if (identical(code, "vr")) vr <- sp$stat
        }
        list(vl = vl, vr = vr)
      })
    } else NULL

    # Pitcher L/R splits (vs Left-Handed Batters / vs Right-Handed Batters)
    pit_splits_data <- if (is_pitcher) {
      safe({
        sp_url <- paste0("https://statsapi.mlb.com/api/v1/people/", person_id,
                         "/stats?stats=statSplits&group=pitching&season=", season,
                         "&sportId=1&sitCodes=vl,vr")
        sp_raw <- jsonlite::fromJSON(sp_url, simplifyVector = FALSE)$stats
        if (is.null(sp_raw) || length(sp_raw) == 0) return(list(vl=NULL, vr=NULL))
        vl <- NULL; vr <- NULL
        for (sp in sp_raw[[1]]$splits) {
          code <- tryCatch(sp$split$code, error = function(e) "")
          if (identical(code, "vl")) vl <- sp$stat   # vs left-handed batters
          if (identical(code, "vr")) vr <- sp$stat   # vs right-handed batters
        }
        list(vl = vl, vr = vr)
      })
    } else NULL

    # Statcast movement + velocity data for pitchers
    movement_data <- if (is_pitcher) fetch_pitcher_movement(person_id) else NULL

    # bWAR — look up from WAR cache only (populated when Leaderboard tab loads)
    # Never triggers a slow bref fetch; shows NA until leaderboard has been visited.
    war_val <- safe({
      nm <- tolower(as.character(bio_data$fullName %||% ""))
      df <- if (!is_pitcher) .war_cache$batters else .war_cache$pitchers
      if (!is.null(df) && "Name" %in% names(df) && "WAR" %in% names(df)) {
        rows <- df[tolower(as.character(df$Name)) == nm, ]
        if (nrow(rows) > 0) suppressWarnings(as.numeric(rows$WAR[1])) else NA_real_
      } else NA_real_
    })

    list(
      bio           = bio_data,
      hitting       = hit_data,
      pitching      = pit_data,
      wrc_plus      = wrc_plus,
      percentiles   = pct_data,
      is_pitcher    = is_pitcher,
      person_id     = as.integer(person_id),
      team_id       = team_id,
      pitch_arsenal = pitch_arsenal,
      splits_vl     = if (!is.null(splits_data))     splits_data$vl     else NULL,
      splits_vr     = if (!is.null(splits_data))     splits_data$vr     else NULL,
      pit_splits_vl = if (!is.null(pit_splits_data)) pit_splits_data$vl else NULL,
      pit_splits_vr = if (!is.null(pit_splits_data)) pit_splits_data$vr else NULL,
      movement_data = movement_data,
      war_val       = war_val
    )
  })
}

