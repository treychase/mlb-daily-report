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

# ── try_bref_war() ────────────────────────────────────────────────────────
# Pulls bref_war_bat() / bref_war_pitch() and filters to the current season.
# Returns a small df with (mlb_id, Name, WAR) or NULL on any failure.
.try_bref_war <- function(is_pitcher = FALSE) {
  tryCatch({
    w <- if (is_pitcher) baseballr::bref_war_pitch() else baseballr::bref_war_bat()
    if (is.null(w) || nrow(w) == 0) {
      message("  bref_war_*() returned empty.")
      return(NULL)
    }
    message("  bref_war_*() returned ", nrow(w), " rows.")
    
    yr_col <- intersect(c("year_ID","year","Year","season"), names(w))[1]
    if (is.na(yr_col)) {
      message("  \u2717 No year column in bref_war_*().")
      return(NULL)
    }
    yr_i <- as.integer(format(TODAY, "%Y"))
    w <- w[suppressWarnings(as.integer(w[[yr_col]])) == yr_i, ]
    message("  Filtered to ", nrow(w), " rows for year ", yr_i, ".")
    if (nrow(w) == 0) {
      message("  \u2717 No BBRef WAR for ", yr_i, " yet \u2014 will try FanGraphs.")
      return(NULL)
    }
    
    nm_col <- intersect(c("name_common","Name","player_name","player"), names(w))[1]
    id_col <- intersect(c("mlb_ID","mlbID","mlb_id","player_ID"),       names(w))[1]
    w_col  <- names(w)[tolower(names(w)) %in% c("war","war_total")][1]
    if (is.na(w_col)) return(NULL)
    
    out <- data.frame(
      mlb_id = if (!is.na(id_col)) suppressWarnings(as.integer(w[[id_col]])) else NA_integer_,
      Name   = if (!is.na(nm_col)) as.character(w[[nm_col]])                  else NA_character_,
      WAR    = suppressWarnings(as.numeric(w[[w_col]])),
      stringsAsFactors = FALSE
    )
    out <- out[!is.na(out$WAR), ]
    if (nrow(out) == 0) return(NULL)
    
    if (any(!is.na(out$mlb_id))) {
      out %>% dplyr::group_by(mlb_id, Name) %>%
        dplyr::summarise(WAR = sum(WAR, na.rm=TRUE), .groups="drop")
    } else {
      out %>% dplyr::group_by(Name) %>%
        dplyr::summarise(WAR = sum(WAR, na.rm=TRUE), .groups="drop")
    }
  }, error = function(e) {
    message("  \u2717 bref_war_*() threw: ", conditionMessage(e))
    NULL
  })
}

# ── try_fg_war() ──────────────────────────────────────────────────────────
# FanGraphs leaderboard fallback (fWAR).  baseballr renamed the functions
# across versions, so we attempt both signatures.
.try_fg_war <- function(is_pitcher = FALSE) {
  yr_i <- as.integer(format(TODAY, "%Y"))
  message("  Attempting FanGraphs leaderboard (", yr_i, ")...")
  
  fg <- NULL
  # Modern baseballr (v1.6+): fg_*_leaders(startseason, endseason, ...)
  if (is.null(fg)) fg <- tryCatch({
    if (is_pitcher) baseballr::fg_pitcher_leaders(startseason = yr_i, endseason = yr_i, qual = 0)
    else            baseballr::fg_batter_leaders (startseason = yr_i, endseason = yr_i, qual = 0)
  }, error = function(e) { message("  fg_*_leaders(startseason=): ", conditionMessage(e)); NULL })
  # Older baseballr: fg_*_leaders(x, y, qual)
  if (is.null(fg)) fg <- tryCatch({
    if (is_pitcher) baseballr::fg_pitch_leaders(x = yr_i, y = yr_i, qual = 0)
    else            baseballr::fg_bat_leaders  (x = yr_i, y = yr_i, qual = 0)
  }, error = function(e) NULL)
  
  if (is.null(fg) || nrow(fg) == 0) {
    message("  \u2717 FanGraphs returned no data.")
    return(NULL)
  }
  message("  FanGraphs returned ", nrow(fg), " rows.")
  message("  FG sample columns: ", paste(head(names(fg), 18), collapse=", "))
  
  nm_col <- intersect(c("PlayerName","Name","playerName","player_name","Player"), names(fg))[1]
  id_col <- intersect(c("xMLBAMID","mlbamid","MLBAMID","mlb_id","mlbid"),         names(fg))[1]
  w_col  <- names(fg)[tolower(names(fg)) %in% c("war","fwar","war_fg")][1]
  # FanGraphs sometimes calls it "Pos" / "position" / "primary_position"
  pos_col <- names(fg)[tolower(names(fg)) %in%
                         c("pos","position","primary_position","primarypos")][1]
  
  message("  Using name='", nm_col %||% "NONE",
          "', mlb_id='", id_col %||% "NONE",
          "', war='",    w_col  %||% "NONE",
          "', pos='",    pos_col %||% "NONE", "'.")
  if (is.na(w_col)) return(NULL)
  
  out <- data.frame(
    mlb_id = if (!is.na(id_col)) suppressWarnings(as.integer(fg[[id_col]])) else NA_integer_,
    Name   = if (!is.na(nm_col)) as.character(fg[[nm_col]])                  else NA_character_,
    WAR    = suppressWarnings(as.numeric(fg[[w_col]])),
    Pos    = if (!is.na(pos_col)) as.character(fg[[pos_col]])               else NA_character_,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$WAR), ]
  if (nrow(out) == 0) return(NULL)
  
  # For traded players FG may produce two rows; keep WAR sum + last Pos
  if (any(!is.na(out$mlb_id))) {
    out %>% dplyr::group_by(mlb_id, Name) %>%
      dplyr::summarise(WAR = sum(WAR, na.rm=TRUE),
                       Pos = dplyr::last(Pos[!is.na(Pos) & nchar(Pos) > 0]) %||% NA_character_,
                       .groups="drop")
  } else {
    out %>% dplyr::group_by(Name) %>%
      dplyr::summarise(WAR = sum(WAR, na.rm=TRUE),
                       Pos = dplyr::last(Pos[!is.na(Pos) & nchar(Pos) > 0]) %||% NA_character_,
                       .groups="drop")
  }
}

# ── compute_war_estimate() ────────────────────────────────────────────────
# From-scratch WAR approximation when neither BBRef nor FanGraphs has 2026
# data yet (very common in April–May).  Uses only the season stats we
# already fetched from the MLB Stats API.  Output is labeled "WAR (est.)"
# in the UI so users know it's our calculation, not an external source.
#
# Hitters: wRAA (from wOBA, 2024 weights) + baserunning + positional adj
#          + replacement level, all divided by ~10 runs/win.
# Pitchers: FIP-based runs above average + replacement, ~10 runs/win.
.compute_war_estimate <- function(stats, position = "OF", is_pitcher = FALSE) {
  if (is.null(stats)) return(NA_real_)
  num <- function(x) suppressWarnings(as.numeric(x %||% 0))
  
  if (!is_pitcher) {
    # ── HITTER WAR estimate ─────────────────────────────────────────────
    ab  <- num(stats$atBats);     h   <- num(stats$hits)
    d   <- num(stats$doubles);    tri <- num(stats$triples)
    hr  <- num(stats$homeRuns);   bb  <- num(stats$baseOnBalls)
    hbp <- num(stats$hitByPitch); sf  <- num(stats$sacFlies)
    sb  <- num(stats$stolenBases); cs <- num(stats$caughtStealing)
    pa  <- ab + bb + hbp + sf
    if (pa < 25) return(NA_real_)   # too few PAs to be meaningful
    
    single <- max(0, h - d - tri - hr)
    # 2024 linear weights & league constants
    woba <- (0.690*bb + 0.722*hbp + 0.888*single +
               1.271*d  + 1.616*tri  + 2.101*hr) / max(1, pa)
    wraa <- ((woba - 0.317) / 1.21) * pa            # wRAA: runs vs avg
    bsr  <- (sb * 0.20) - (cs * 0.43)                # baserunning runs
    
    # Positional adjustment (FanGraphs scale, prorated by PA / 600)
    pos_adj_per_600 <- switch(toupper(position),
                              "C"  =  9, "SS" =  7, "CF" =  3, "2B" =  3, "3B" =  2,
                              "LF" = -7, "RF" = -7, "OF" = -3, "1B" = -9, "DH" = -15,
                              0)
    pos_adj <- (pa / 600) * pos_adj_per_600
    
    # Replacement level: ~20 runs per 600 PA
    rep_lvl <- (pa / 600) * 20
    
    # Defensive runs unavailable here — treat as 0
    total_runs <- wraa + bsr + pos_adj + rep_lvl
    round(total_runs / 10, 1)                        # ~10 runs per win
    
  } else {
    # ── PITCHER WAR estimate ────────────────────────────────────────────
    ip_str <- as.character(stats$inningsPitched %||% "0.0")
    # MLB IP format "53.2" means 53 and 2/3 innings, not 53.2 decimal.
    ip <- tryCatch({
      parts  <- strsplit(ip_str, "\\.")[[1]]
      whole  <- suppressWarnings(as.numeric(parts[1])) %||% 0
      thirds <- if (length(parts) > 1) suppressWarnings(as.numeric(parts[2])) %||% 0 else 0
      whole + thirds / 3
    }, error = function(e) suppressWarnings(as.numeric(ip_str)) %||% 0)
    if (is.null(ip) || is.na(ip) || ip < 5) return(NA_real_)
    
    bb  <- num(stats$baseOnBalls);  so <- num(stats$strikeOuts)
    hr  <- num(stats$homeRuns);     hbp <- num(stats$hitByPitch)
    
    # FIP, with constant chosen to roughly align with league ERA
    fip   <- ((13*hr) + (3*(bb + hbp)) - (2*so)) / ip + 3.10
    lg_fip <- 4.20
    
    # Runs above average (negative FIP-lg = better than average)
    raa <- (lg_fip - fip) * (ip / 9)
    # Replacement level: ~0.5 runs/IP above replacement → 0.5 * 9 = 4.5 per 9 IP
    rep_lvl <- 0.5 * ip
    total_runs <- raa + rep_lvl
    round(total_runs / 10, 1)
  }
}

# ── augment_with_war() ────────────────────────────────────────────────────
# Multi-source WAR augmentation:
#   1.  daily df already has a WAR column                       (BBRef)
#   2.  bref_war_bat() / bref_war_pitch() filtered to current   (BBRef)
#   3.  fg_*_leaders() filtered to current season               (FanGraphs)
# Tags the returned df with attr(., "war_source") so the UI can label
# "bWAR" vs "fWAR" appropriately.
.augment_with_war <- function(df, is_pitcher = FALSE) {
  if (is.null(df) || nrow(df) == 0) return(df)
  
  # ── Strategy 1: WAR is already in the daily table ───────────────────────
  existing <- names(df)[tolower(names(df)) %in% c("war","war_total","bwar")][1]
  src      <- NA_character_
  if (!is.na(existing)) {
    message("  \u2713 WAR column already present in daily data: '", existing, "'")
    if (existing != "WAR") df[["WAR"]] <- df[[existing]]
    src <- "bref"
  } else {
    # ── Strategy 2: BBRef war_daily_bat / war_daily_pitch ────────────────
    message("  No WAR in daily table. Attempting Baseball Reference...")
    war_df <- .try_bref_war(is_pitcher)
    src    <- "bref"
    # ── Strategy 3: FanGraphs leaderboard (also brings Pos + mlb_id) ────
    if (is.null(war_df) || nrow(war_df) == 0) {
      war_df <- .try_fg_war(is_pitcher)
      src    <- "fangraphs"
    }
    if (is.null(war_df) || nrow(war_df) == 0) {
      message("  \u2717 No WAR source available; WAR column will be missing.")
      attr(df, "war_source") <- "none"
    } else {
      df_name_col <- intersect(c("Name","player_name","bbref_id"), names(df))[1]
      if (!is.na(df_name_col) && df_name_col != "Name") df[["Name"]] <- df[[df_name_col]]
      if ("Name" %in% names(df)) {
        # Keep mlb_id + Pos in the join (we need both for click handler + filter)
        df <- dplyr::left_join(df, war_df, by = "Name")
        n_matched <- sum(!is.na(df$WAR))
        message("  \u2713 Joined WAR for ", n_matched, "/", nrow(df),
                " players (source: ", src, ").")
      } else {
        message("  \u2717 df has no Name column to join on.")
      }
    }
  }
  
  # ── Always try to enrich with FG position info (for hitters), even when
  # bref daily already provided WAR.  bref daily doesn't expose Pos.
  has_pos <- "Pos" %in% names(df) && any(!is.na(df$Pos) & nchar(as.character(df$Pos)) > 0)
  has_pid <- "mlb_id" %in% names(df) && any(!is.na(df$mlb_id))
  
  if (!is_pitcher && (!has_pos || !has_pid)) {
    message("  Enriching with FG position/mlb_id (has_pos=", has_pos, ", has_pid=", has_pid, ")...")
    fg_extra <- .try_fg_war(is_pitcher = FALSE)
    if (!is.null(fg_extra) && nrow(fg_extra) > 0 && "Name" %in% names(df)) {
      keep_cols <- intersect(c("Name","mlb_id","Pos"), names(fg_extra))
      # Drop any columns we already have to avoid duplicates from left_join
      fg_extra <- fg_extra[, keep_cols, drop = FALSE]
      # Rename to .fg suffix so we can coalesce manually
      names(fg_extra)[names(fg_extra) == "mlb_id"] <- "mlb_id_fg"
      names(fg_extra)[names(fg_extra) == "Pos"]    <- "Pos_fg"
      df <- dplyr::left_join(df, fg_extra, by = "Name")
      if ("mlb_id_fg" %in% names(df)) {
        if (!"mlb_id" %in% names(df)) df$mlb_id <- df$mlb_id_fg
        else df$mlb_id <- ifelse(is.na(df$mlb_id), df$mlb_id_fg, df$mlb_id)
        df$mlb_id_fg <- NULL
      }
      if ("Pos_fg" %in% names(df)) {
        if (!"Pos" %in% names(df)) df$Pos <- df$Pos_fg
        else df$Pos <- ifelse(is.na(df$Pos) | nchar(as.character(df$Pos)) == 0,
                              df$Pos_fg, df$Pos)
        df$Pos_fg <- NULL
      }
      message("  \u2713 FG enrichment: Pos populated for ",
              sum(!is.na(df$Pos) & nchar(as.character(df$Pos)) > 0), "/", nrow(df))
    } else {
      message("  \u2717 FG enrichment unavailable.")
    }
  }
  
  attr(df, "war_source") <- src
  df
}

fetch_lb_batters_raw <- function() {
  message("=== Fetching hitter leaderboard ===")
  df <- safe({
    yr <- format(TODAY, "%Y")
    baseballr::bref_daily_batter(
      t1 = paste0(yr, "-03-15"),
      t2 = format(TODAY, "%Y-%m-%d")
    )
  })
  if (is.null(df) || nrow(df) == 0) {
    message("  ✗ bref_daily_batter() returned no data.")
    return(NULL)
  }
  message("  bref_daily_batter() returned ", nrow(df), " rows.")
  message("  Columns: ", paste(names(df), collapse=", "))
  
  df <- .augment_with_war(df, is_pitcher = FALSE)
  
  .war_cache$batters        <- df
  .war_cache$batters_source <- attr(df, "war_source") %||% "none"
  has_war <- "WAR" %in% names(df) && any(!is.na(df$WAR))
  message("  → Cached ", nrow(df), " hitter rows.  WAR populated: ", has_war,
          "  (source: ", .war_cache$batters_source, ")")
  df
}
fetch_lb_batters <- memoise::memoise(fetch_lb_batters_raw)

fetch_lb_pitchers_raw <- function() {
  message("=== Fetching pitcher leaderboard ===")
  df <- safe({
    yr <- format(TODAY, "%Y")
    baseballr::bref_daily_pitcher(
      t1 = paste0(yr, "-03-15"),
      t2 = format(TODAY, "%Y-%m-%d")
    )
  })
  if (is.null(df) || nrow(df) == 0) {
    message("  ✗ bref_daily_pitcher() returned no data.")
    return(NULL)
  }
  message("  bref_daily_pitcher() returned ", nrow(df), " rows.")
  message("  Columns: ", paste(names(df), collapse=", "))
  
  df <- .augment_with_war(df, is_pitcher = TRUE)
  
  .war_cache$pitchers        <- df
  .war_cache$pitchers_source <- attr(df, "war_source") %||% "none"
  has_war <- "WAR" %in% names(df) && any(!is.na(df$WAR))
  message("  → Cached ", nrow(df), " pitcher rows.  WAR populated: ", has_war,
          "  (source: ", .war_cache$pitchers_source, ")")
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
      dplyr::filter(!is.na(pfx_x), !is.na(pfx_z), n_pitches >= 10)
    
    if (nrow(agg) == 0) return(NULL)
    
    tot              <- sum(agg$n_pitches, na.rm = TRUE)
    agg$pitch_percent <- agg$n_pitches / max(tot, 1)
    agg$h_break_in    <- agg$pfx_x * 12
    agg$v_break_in    <- agg$pfx_z * 12
    agg$velo           <- agg$release_speed
    agg$spin_rate      <- agg$release_spin_rate
    
    # ── Raw individual pitches for the per-pitch-type strike-zone heatmap ──
    raw_pitches <- df %>%
      dplyr::filter(!is.na(pitch_type), nchar(as.character(pitch_type)) > 0) %>%
      dplyr::transmute(
        pitch_type = as.character(pitch_type),
        plate_x    = suppressWarnings(as.numeric(.data[["plate_x"]])),
        plate_z    = suppressWarnings(as.numeric(.data[["plate_z"]]))
      ) %>%
      dplyr::filter(!is.na(plate_x), !is.na(plate_z))
    
    list(
      summary     = tibble::as_tibble(agg),
      raw_pitches = tibble::as_tibble(raw_pitches)
    )
  })
}
# Memoised so repeated clicks on the same pitcher are instant
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

fetch_player_season_stats_raw <- function(person_id) {
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
    # NOTE: avoid return() inside tryCatch({}) — it exits the parent function
    # (same trap as the WAR block above).  Use if/else expressions instead.
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
        if (pa < 10) {
          NA_real_
        } else {
          single <- h - d - tri - hr
          woba <- (0.690*bb + 0.722*hbp + 0.888*single +
                     1.271*d  + 1.616*tri  + 2.101*hr) /
            max(1, ab + bb + hbp + sf)
          round(100 * ((woba - 0.317) / 1.21 + 0.127) / 0.127)
        }
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
        if (is.null(sp_raw) || length(sp_raw) == 0) {
          list(vl=NULL, vr=NULL)
        } else {
          vl <- NULL; vr <- NULL
          for (sp in sp_raw[[1]]$splits) {
            code <- tryCatch(sp$split$code, error = function(e) "")
            if (identical(code, "vl")) vl <- sp$stat
            if (identical(code, "vr")) vr <- sp$stat
          }
          list(vl = vl, vr = vr)
        }
      })
    } else NULL
    
    # Pitcher L/R splits (vs Left-Handed Batters / vs Right-Handed Batters)
    pit_splits_data <- if (is_pitcher) {
      safe({
        sp_url <- paste0("https://statsapi.mlb.com/api/v1/people/", person_id,
                         "/stats?stats=statSplits&group=pitching&season=", season,
                         "&sportId=1&sitCodes=vl,vr")
        sp_raw <- jsonlite::fromJSON(sp_url, simplifyVector = FALSE)$stats
        if (is.null(sp_raw) || length(sp_raw) == 0) {
          list(vl=NULL, vr=NULL)
        } else {
          vl <- NULL; vr <- NULL
          for (sp in sp_raw[[1]]$splits) {
            code <- tryCatch(sp$split$code, error = function(e) "")
            if (identical(code, "vl")) vl <- sp$stat   # vs left-handed batters
            if (identical(code, "vr")) vr <- sp$stat   # vs right-handed batters
          }
          list(vl = vl, vr = vr)
        }
      })
    } else NULL
    
    # Statcast movement + velocity data for pitchers
    mov_full <- if (is_pitcher) fetch_pitcher_movement(person_id) else NULL
    movement_data <- if (!is.null(mov_full)) mov_full$summary     else NULL
    movement_raw  <- if (!is.null(mov_full)) mov_full$raw_pitches else NULL
    
    # bWAR lookup — try multiple matching strategies for robustness.
    # NOTE: must NOT use return() inside safe({}); return() exits the parent
    # function (fetch_player_season_stats_raw), making it return NA_real_
    # instead of the list of season stats.  Use a result variable instead.
    war_val <- safe({
      if (!is_pitcher && is.null(.war_cache$batters))  fetch_lb_batters()
      if (is_pitcher  && is.null(.war_cache$pitchers)) fetch_lb_pitchers()
      df <- if (!is_pitcher) .war_cache$batters else .war_cache$pitchers
      
      result <- NA_real_
      if (!is.null(df) && "WAR" %in% names(df)) {
        pid_i <- as.integer(person_id)
        nm    <- tolower(trimws(as.character(bio_data$fullName %||% "")))
        
        # Strategy 1: match by mlb_id (most reliable)
        if ("mlb_id" %in% names(df) && !is.na(pid_i)) {
          rows <- df[!is.na(df$mlb_id) & df$mlb_id == pid_i, ]
          if (nrow(rows) > 0)
            result <- suppressWarnings(as.numeric(rows$WAR[1]))
        }
        # Strategy 2: exact case-insensitive name match
        if (is.na(result) && "Name" %in% names(df) && nchar(nm) > 0) {
          rows <- df[tolower(trimws(as.character(df$Name))) == nm, ]
          if (nrow(rows) > 0)
            result <- suppressWarnings(as.numeric(rows$WAR[1]))
        }
        # Strategy 3: bbref_id column (some daily tables use this)
        if (is.na(result) && "bbref_id" %in% names(df) && nchar(nm) > 0) {
          rows <- df[tolower(trimws(as.character(df$bbref_id))) == nm, ]
          if (nrow(rows) > 0)
            result <- suppressWarnings(as.numeric(rows$WAR[1]))
        }
      }
      result
    })
    
    # Determine which source produced the WAR value (for the label)
    war_source <- if (is_pitcher) (.war_cache$pitchers_source %||% "none")
    else            (.war_cache$batters_source  %||% "none")
    
    # ── Strategy 4: COMPUTE our own WAR estimate ──────────────────────────
    # Falls back to a from-scratch calculation if no external source had data.
    # Uses MLB Stats API season stats we already fetched (no extra calls).
    # Marked as "estimate" so the UI labels it "WAR (est.)".
    if (is.null(war_val) || is.na(war_val)) {
      computed <- safe({
        pos_code <- tryCatch(as.character(bio_data$primaryPosition$abbreviation %||% "OF"),
                             error = function(e) "OF")
        if (!is_pitcher && length(hit_data) > 0 && length(hit_data[[1]]$splits) > 0) {
          .compute_war_estimate(hit_data[[1]]$splits[[1]]$stat, pos_code, is_pitcher = FALSE)
        } else if (is_pitcher && length(pit_data) > 0 && length(pit_data[[1]]$splits) > 0) {
          .compute_war_estimate(pit_data[[1]]$splits[[1]]$stat, pos_code, is_pitcher = TRUE)
        } else NA_real_
      })
      if (!is.null(computed) && !is.na(computed) && is.finite(computed)) {
        war_val    <- computed
        war_source <- "estimate"
      }
    }
    
    # Full team history this season (e.g. "OAK,BOS" for traded players)
    team_history <- safe({
      if (!is.null(df_war_lookup <- if (!is_pitcher) .war_cache$batters else .war_cache$pitchers)) {
        nm   <- tolower(as.character(bio_data$fullName %||% ""))
        rows <- df_war_lookup[tolower(as.character(df_war_lookup$Name)) == nm, ]
        if (nrow(rows) > 0) {
          tcol <- intersect(c("Team","Tm","team","tm"), names(rows))[1]
          if (!is.na(tcol)) as.character(rows[[tcol]][1]) else NULL
        }
      }
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
      movement_raw  = movement_raw,
      war_val       = war_val,
      war_source    = war_source,
      team_history  = team_history
    )
  })
}

# Memoise so repeat clicks on the same player are instant (deduplicates the
# observer + renderUI double-fetch and avoids 6+ duplicate API calls).
# Wrapping in a fresh memoise() on each source() ensures any stale cached
# values from a previous (buggy) version don't survive an app restart.
fetch_player_season_stats <- memoise::memoise(fetch_player_season_stats_raw)