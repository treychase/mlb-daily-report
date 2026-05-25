# ═══════════════════════════════════════════════════════════════════════════
#  MLB Daily Report  |  Shiny Dashboard
#  Uses baseballr (MLB Stats API + Baseball Savant Statcast)
#  Packages: shiny, bslib, baseballr, dplyr, DT, lubridate, purrr
# ═══════════════════════════════════════════════════════════════════════════

library(shiny)
library(bslib)
library(baseballr)
library(dplyr)
library(DT)
library(lubridate)
library(purrr)
library(jsonlite)
library(memoise)

# ── Globals ────────────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

TODAY        <- Sys.Date() - 1
TODAY_STR    <- format(TODAY, "%Y-%m-%d")
MONTH_START  <- format(floor_date(TODAY, "month"), "%Y-%m-%d")
MONTH_LABEL  <- format(TODAY, "%B %Y")
MIN_AB       <- 15L   # Monthly leaderboard minimum AB

# ── Data helpers ───────────────────────────────────────────────────────────

safe <- function(expr, fallback = NULL) {
  tryCatch(expr, error = function(e) { message("⚠ ", conditionMessage(e)); fallback })
}

int0 <- function(x) as.integer(x %||% 0L)

# Statcast stores teams as abbreviations ("PIT"); the MLB schedule API uses
# full names ("Pittsburgh Pirates").  TEAM_ABB bridges the two.
TEAM_ABB <- c(
  "Arizona Diamondbacks"  = "ARI", "Atlanta Braves"        = "ATL",
  "Baltimore Orioles"     = "BAL", "Boston Red Sox"        = "BOS",
  "Chicago Cubs"          = "CHC", "Chicago White Sox"     = "CWS",
  "Cincinnati Reds"       = "CIN", "Cleveland Guardians"   = "CLE",
  "Colorado Rockies"      = "COL", "Detroit Tigers"        = "DET",
  "Houston Astros"        = "HOU", "Kansas City Royals"    = "KC",
  "Los Angeles Angels"    = "LAA", "Los Angeles Dodgers"   = "LAD",
  "Miami Marlins"         = "MIA", "Milwaukee Brewers"     = "MIL",
  "Minnesota Twins"       = "MIN", "New York Mets"         = "NYM",
  "New York Yankees"      = "NYY", "Oakland Athletics"     = "OAK",
  "Philadelphia Phillies" = "PHI", "Pittsburgh Pirates"    = "PIT",
  "San Diego Padres"      = "SD",  "San Francisco Giants"  = "SF",
  "Seattle Mariners"      = "SEA", "St. Louis Cardinals"   = "STL",
  "Tampa Bay Rays"        = "TB",  "Texas Rangers"         = "TEX",
  "Toronto Blue Jays"     = "TOR", "Washington Nationals"  = "WSH",
  "Athletics"             = "OAK", "Cleveland Indians"     = "CLE"
)
name_to_abb <- function(x) { a <- TEAM_ABB[x]; ifelse(is.na(a), x, a) }

# Format "Hunter Greene" -> "H. Greene" for display
fmt_prob <- function(name) {
  if (is.null(name) || length(name) == 0 || is.na(name)) return(NA_character_)
  parts <- strsplit(trimws(as.character(name)), "\\s+")[[1]]
  if (length(parts) < 2) return(as.character(name))
  paste0(substr(parts[1], 1, 1), ". ", paste(parts[-1], collapse = " "))
}

# --- Schedule ---------------------------------------------------------------
# Tries multiple approaches for compatibility across baseballr versions
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

# --- Batting ----------------------------------------------------------------
parse_batting <- function(bs, side) {
  safe({
    td  <- bs$teams[[side]]
    ids <- td$batters
    pl  <- td$players
    if (is.null(ids) || is.null(pl)) return(empty_batting())

    rows <- compact(lapply(ids, function(pid) {
      p   <- pl[[paste0("ID", pid)]]
      bat <- p$stats$batting
      if (is.null(bat) || is.null(bat$atBats)) return(NULL)

      tibble(
        .ord      = int0(p$battingOrder),
        Name      = as.character(p$person$fullName %||% "—"),
        person_id = as.integer(pid),
        Pos   = as.character(p$position$abbreviation %||% "—"),
        AB    = int0(bat$atBats),
        R     = int0(bat$runs),
        H     = int0(bat$hits),
        `2B`  = int0(bat$doubles),
        `3B`  = int0(bat$triples),
        HR    = int0(bat$homeRuns),
        RBI   = int0(bat$rbi),
        BB    = int0(bat$baseOnBalls),
        SO    = int0(bat$strikeOuts),
        AVG   = as.character(bat$avg %||% ".---"),
        OBP   = as.character(bat$obp %||% ".---"),
        SLG   = as.character(bat$slg %||% ".---"),
        OPS   = as.character(bat$ops %||% ".---")
      )
    }))

    if (!length(rows)) return(empty_batting())
    bind_rows(rows) %>%
      arrange(.ord) %>%
      select(-.ord) %>%
      filter(AB > 0 | BB > 0 | R > 0)
  }, fallback = empty_batting())
}

empty_batting <- function() {
  tibble(Name=character(), Pos=character(), AB=integer(), R=integer(),
         H=integer(), `2B`=integer(), `3B`=integer(), HR=integer(),
         RBI=integer(), BB=integer(), SO=integer(),
         AVG=character(), OBP=character(), SLG=character(), OPS=character(),
         person_id=integer())
}

# --- Pitching ---------------------------------------------------------------
parse_pitching <- function(bs, side) {
  safe({
    td  <- bs$teams[[side]]
    ids <- td$pitchers
    pl  <- td$players
    if (is.null(ids) || is.null(pl)) return(empty_pitching())

    rows <- compact(lapply(ids, function(pid) {
      p   <- pl[[paste0("ID", pid)]]
      pit <- p$stats$pitching
      if (is.null(pit)) return(NULL)

      tibble(
        Name      = as.character(p$person$fullName %||% "—"),
        person_id = as.integer(pid),
        IP      = as.character(pit$inningsPitched  %||% "0.0"),
        H       = int0(pit$hits),
        R       = int0(pit$runs),
        ER      = int0(pit$earnedRuns),
        BB      = int0(pit$baseOnBalls),
        SO      = int0(pit$strikeOuts),
        HR      = int0(pit$homeRuns),
        ERA     = as.character(pit$era %||% "—"),
        Pitches = int0(pit$numberOfPitches),
        Strikes = int0(pit$strikes)
      )
    }))

    if (!length(rows)) return(empty_pitching())
    bind_rows(rows) %>% filter(Pitches > 0)
  }, fallback = empty_pitching())
}

empty_pitching <- function() {
  tibble(Name=character(), IP=character(), H=integer(), R=integer(),
         ER=integer(), BB=integer(), SO=integer(), HR=integer(),
         ERA=character(), Pitches=integer(), Strikes=integer(),
         person_id=integer())
}

# --- Top 2 hitters by today's OPS -------------------------------------------
top2_by_ops <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  df %>%
    mutate(.o = suppressWarnings(as.numeric(OPS))) %>%
    filter(!is.na(.o)) %>%
    slice_max(.o, n = 2, with_ties = FALSE) %>%
    pull(Name)
}

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
    bio_data <- jsonlite::fromJSON(bio_url, simplifyVector = FALSE)$people[[1]]
    is_pitcher <- tryCatch(
      !is.null(bio_data$primaryPosition) &&
        bio_data$primaryPosition$code %in% c("1"),
      error = function(e) FALSE
    )

    pct_type   <- if (is_pitcher) "pitcher" else "batter"
    pct_url    <- paste0("https://baseballsavant.mlb.com/api/vr/percentile-rankings?",
                         "type=", pct_type, "&playerId=", person_id,
                         "&season=", season)
    pct_data   <- safe(jsonlite::fromJSON(pct_url, simplifyVector = TRUE))

    list(
      bio        = bio_data,
      hitting    = jsonlite::fromJSON(hit_url, simplifyVector = FALSE)$stats,
      pitching   = jsonlite::fromJSON(pit_url, simplifyVector = FALSE)$stats,
      percentiles = pct_data,
      is_pitcher  = is_pitcher
    )
  })
}

build_player_modal_ui <- function(data) {
  if (is.null(data))
    return(div(style = "color:#8b949e;padding:20px;", "Season stats unavailable."))

  bio  <- data$bio
  team <- tryCatch(bio$currentTeam$name       %||% "\u2014", error = function(e) "\u2014")
  pos  <- tryCatch(bio$primaryPosition$name   %||% "\u2014", error = function(e) "\u2014")
  age  <- tryCatch(as.character(bio$currentAge %||% "\u2014"), error = function(e) "\u2014")
  bats <- tryCatch(bio$batSide$description    %||% "\u2014", error = function(e) "\u2014")
  thro <- tryCatch(bio$pitchHand$description  %||% "\u2014", error = function(e) "\u2014")

  sb <- function(val, lbl)
    div(class = "ms-stat",
      div(class = "ms-val", as.character(val %||% "\u2014")),
      div(class = "ms-lbl", lbl))

  fmt3 <- function(x) tryCatch(sprintf("%.3f", as.numeric(x)), error = function(e) "\u2014")
  fmt0 <- function(x) tryCatch(as.character(as.integer(x)),   error = function(e) "\u2014")

  hit_sec <- {
    h <- data$hitting
    if (length(h) > 0 && length(h[[1]]$splits) > 0) {
      s <- h[[1]]$splits[[1]]$stat
      tagList(
        div(class = "ms-section-hdr", "Season Hitting"),
        div(class = "ms-stat-grid",
          sb(fmt0(s$gamesPlayed),  "G"),    sb(fmt0(s$atBats),      "AB"),
          sb(fmt0(s$hits),         "H"),    sb(fmt0(s$doubles),     "2B"),
          sb(fmt0(s$triples),      "3B"),   sb(fmt0(s$homeRuns),    "HR"),
          sb(fmt0(s$rbi),          "RBI"),  sb(fmt0(s$baseOnBalls), "BB"),
          sb(fmt0(s$strikeOuts),   "K"),    sb(fmt0(s$stolenBases), "SB"),
          sb(fmt3(s$avg),          "AVG"),  sb(fmt3(s$obp),         "OBP"),
          sb(fmt3(s$slg),          "SLG"),  sb(fmt3(s$ops),         "OPS"),
          sb(fmt3(s$babip),        "BABIP"),sb(fmt0(s$runs),        "R")
        )
      )
    }
  }
  pit_sec <- {
    p <- data$pitching
    if (length(p) > 0 && length(p[[1]]$splits) > 0) {
      s <- p[[1]]$splits[[1]]$stat
      tagList(
        div(class = "ms-section-hdr", "Season Pitching"),
        div(class = "ms-stat-grid",
          sb(fmt0(s$gamesPlayed),   "G"),   sb(fmt0(s$gamesStarted), "GS"),
          sb(paste0(fmt0(s$wins), "-", fmt0(s$losses)), "W-L"),
          sb(as.character(s$era              %||% "\u2014"), "ERA"),
          sb(as.character(s$inningsPitched   %||% "\u2014"), "IP"),
          sb(fmt0(s$strikeOuts),    "K"),   sb(fmt0(s$baseOnBalls),  "BB"),
          sb(fmt0(s$hits),          "H"),   sb(fmt0(s$homeRuns),     "HR"),
          sb(as.character(s$whip             %||% "\u2014"), "WHIP"),
          sb(as.character(s$strikeoutsPer9Inn %||% "\u2014"), "K/9"),
          sb(as.character(s$walksPer9Inn      %||% "\u2014"), "BB/9"),
          sb(fmt3(s$avg),           "BAA"), sb(fmt0(s$saves),        "SV")
        )
      )
    }
  }
  # Percentile badge helper (Savant-style colour wheel)
  pct_badge <- function(pct, label) {
    if (is.null(pct) || length(pct) == 0 || is.na(pct)) return(NULL)
    v   <- as.integer(round(as.numeric(pct)))
    col <- dplyr::case_when(
      v >= 90 ~ "#ef4444", v >= 67 ~ "#f97316",
      v >= 34 ~ "#8b949e", v >= 11 ~ "#3b82f6",
      TRUE    ~ "#1d4ed8"
    )
    div(class = "pct-item",
      div(class = "pct-badge", style = paste0("background:", col, ";"), v),
      div(class = "pct-label", label)
    )
  }

  pct_sec <- {
    pd <- data$percentiles
    if (!is.null(pd) && (is.data.frame(pd) || is.list(pd)) && length(pd) > 0) {
      row <- if (is.data.frame(pd)) pd[1, ] else pd
      g   <- function(nm) tryCatch(row[[nm]], error = function(e) NULL)
      if (!isTRUE(data$is_pitcher)) {
        tagList(
          div(class = "ms-section-hdr", "Statcast Percentiles"),
          div(class = "pct-grid",
            pct_badge(g("exit_velocity_avg"),   "Exit Velo"),
            pct_badge(g("hard_hit_percent"),     "Hard Hit%"),
            pct_badge(g("barrel_batted_rate"),   "Barrel%"),
            pct_badge(g("xba"),                  "xBA"),
            pct_badge(g("xslg"),                 "xSLG"),
            pct_badge(g("xwoba"),                "xwOBA"),
            pct_badge(g("xobp"),                 "xOBP"),
            pct_badge(g("sprint_speed"),         "Speed")
          )
        )
      } else {
        tagList(
          div(class = "ms-section-hdr", "Statcast Percentiles"),
          div(class = "pct-grid",
            pct_badge(g("fastball_avg_speed"),   "FB Velo"),
            pct_badge(g("fastball_avg_spin"),     "FB Spin"),
            pct_badge(g("xera"),                  "xERA"),
            pct_badge(g("xba"),                   "xBA"),
            pct_badge(g("xwoba"),                 "xwOBA"),
            pct_badge(g("whiff_percent"),         "Whiff%"),
            pct_badge(g("k_percent"),             "K%"),
            pct_badge(g("bb_percent"),            "BB%")
          )
        )
      }
    }
  }

  div(style = "padding:4px;",
    div(class = "ms-bio",
      span(class = "ms-team", team),
      span(class = "ms-sep",  "\u00b7"),
      span(class = "ms-pos",  pos),
      span(class = "ms-sep",  "\u00b7"),
      span(style = "color:#6e7681;", paste0("Age ", age)),
      span(class = "ms-sep",  "\u00b7"),
      span(style = "color:#6e7681;", paste0("Bats: ", bats)),
      span(class = "ms-sep",  "\u00b7"),
      span(style = "color:#6e7681;", paste0("Throws: ", thro))
    ),
    pct_sec,
    hit_sec,
    pit_sec,
    if (is.null(hit_sec) && is.null(pit_sec))
      p(style = "color:#8b949e;margin-top:16px;",
        "No season stats recorded yet.")
  )
}

compute_monthly <- function(sc) {
  if (is.null(sc) || nrow(sc) == 0) return(NULL)

  # Guard: statcast CSV columns can shift between versions; ensure required cols exist
  needed <- c("events","inning_topbot","away_team","home_team","game_pk","player_name")
  missing_cols <- setdiff(needed, names(sc))
  if (length(missing_cols) > 0) {
    message("Statcast missing columns: ", paste(missing_cols, collapse=", "))
    return(NULL)
  }

  pa_evts <- c(
    "single","double","triple","home_run",
    "strikeout","field_out","grounded_into_double_play","force_out",
    "fielders_choice","fielders_choice_out","double_play","triple_play","other_out",
    "walk","intent_walk","hit_by_pitch","sac_fly","sac_bunt"
  )
  ab_evts <- c(
    "single","double","triple","home_run",
    "strikeout","field_out","grounded_into_double_play","force_out",
    "fielders_choice","fielders_choice_out","double_play","triple_play","other_out"
  )

  sc %>%
    filter(events %in% pa_evts) %>%
    mutate(batter_team = if_else(inning_topbot == "Top", away_team, home_team)) %>%
    group_by(player_name, batter_team) %>%
    summarise(
      G   = n_distinct(game_pk),
      AB  = sum(events %in% ab_evts, na.rm = TRUE),
      H   = sum(events %in% c("single","double","triple","home_run"), na.rm = TRUE),
      D   = sum(events == "double",    na.rm = TRUE),
      Tri = sum(events == "triple",    na.rm = TRUE),
      HR  = sum(events == "home_run",  na.rm = TRUE),
      BB  = sum(events %in% c("walk","intent_walk"), na.rm = TRUE),
      HBP = sum(events == "hit_by_pitch", na.rm = TRUE),
      SF  = sum(events == "sac_fly",   na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # TB = 1B + 2×2B + 3×3B + 4×HR  =  H + 2B + 2×3B + 3×HR
      TB  = H + D + 2 * Tri + 3 * HR,
      AVG = if_else(AB > 0, round(H / AB, 3), NA_real_),
      OBP = if_else((AB + BB + HBP + SF) > 0,
                    round((H + BB + HBP) / (AB + BB + HBP + SF), 3), NA_real_),
      SLG = if_else(AB > 0, round(TB / AB, 3), NA_real_),
      OPS = round(OBP + SLG, 3)
    ) %>%
    filter(AB >= MIN_AB) %>%
    arrange(desc(OPS))
}

# ── UI helpers ─────────────────────────────────────────────────────────────

status_html <- function(state) {
  if (is.na(state) || is.null(state)) state <- "Scheduled"
  cls <- dplyr::case_when(
    grepl("Final|Completed|Game Over", state, TRUE) ~ "badge-final",
    grepl("Progress|Live",             state, TRUE) ~ "badge-live",
    TRUE                                             ~ "badge-sched"
  )
  lbl <- if (grepl("Progress|Live", state, TRUE)) "🔴 LIVE" else state
  sprintf('<span class="status-badge %s">%s</span>', cls, htmltools::htmlEscape(lbl))
}

score_display <- function(away, home, as_, hs_, state, gtime, venue,
                          away_prob = NA_character_, home_prob = NA_character_) {
  as_str <- tryCatch(as.character(as.integer(as_)), error = function(e) "–")
  hs_str <- tryCatch(as.character(as.integer(hs_)), error = function(e) "–")
  if (is.na(as_str)) as_str <- "–"
  if (is.na(hs_str)) hs_str <- "–"

  div(class = "game-hdr",
    div(class = "score-row",
      div(class = "score-team away-team",
        div(class = "team-city", strsplit(away, " ")[[1]] |> head(-1) |> paste(collapse = " ")),
        div(class = "team-nickname", strsplit(away, " ")[[1]] |> tail(1)),
        div(class = "score-num", as_str)
      ),
      div(class = "score-mid",
        HTML(status_html(state)),
        if (!is.null(gtime) && !is.na(gtime) && nchar(gtime) > 0)
          div(class = "game-time", gtime),
        div(class = "vs-sep", "vs"),
        if (!is.null(venue) && !is.na(venue) && nchar(venue) > 0)
          div(class = "game-venue", venue),
        if (!is.na(away_prob) && !is.na(home_prob))
          div(class = "probable-pitchers",
            div(class = "pp-label", "Probable"),
            div(class = "pp-names",
              HTML(paste0(away_prob,
                          '<span class="pp-sep">vs</span>',
                          home_prob))
            )
          )
      ),
      div(class = "score-team home-team",
        div(class = "team-city", strsplit(home, " ")[[1]] |> head(-1) |> paste(collapse = " ")),
        div(class = "team-nickname", strsplit(home, " ")[[1]] |> tail(1)),
        div(class = "score-num", hs_str)
      )
    )
  )
}

sec_hdr <- function(icon_txt, label) {
  div(class = "sec-hdr", span(class = "sec-icon", icon_txt), label)
}

make_dt <- function(df, top2 = NULL) {
  if (is.null(df) || nrow(df) == 0) {
    return(
      DT::datatable(
        data.frame(`  ` = "No data available \u2014 game may not have started yet.",
                   check.names = FALSE),
        rownames = FALSE, options = list(dom = "t"),
        class = "table-dark compact"
      )
    )
  }

  has_pid <- "person_id" %in% names(df)
  df      <- df %>% mutate(.h = as.integer(Name %in% (top2 %||% character(0))))
  # Move person_id to just before .h so JS data[data.length-2] always finds it
  if (has_pid) df <- df %>% dplyr::relocate(person_id, .before = .h)

  # Hidden columns (0-based): .h is always last; person_id is second-to-last
  n_cols  <- ncol(df)
  h_idx   <- n_cols - 1L
  pid_idx <- if (has_pid) n_cols - 2L else integer(0)
  hidden  <- c(h_idx, pid_idx)

  # Row click: send player id + name to Shiny for the season-stats modal
  row_cb <- if (has_pid) DT::JS(
    "function(row, data) {
       $(row).css('cursor','pointer').on('click', function() {
         Shiny.setInputValue('player_click',
           { id: data[data.length - 2], name: data[0] },
           { priority: 'event' });
       });
     }"
  ) else NULL

  opts <- list(
    dom        = "t",
    pageLength = 30,
    scrollX    = TRUE,
    ordering   = FALSE,
    columnDefs = list(list(visible = FALSE, targets = hidden))
  )
  if (!is.null(row_cb)) opts$rowCallback <- row_cb

  DT::datatable(df,
    rownames = FALSE, escape = FALSE,
    class    = "table-dark compact cell-border",
    options  = opts
  ) %>%
    DT::formatStyle(
      ".h",
      target          = "row",
      backgroundColor = DT::styleEqual(1L, "rgba(255,210,0,0.09)"),
      borderLeft      = DT::styleEqual(1L, "3px solid #FFD700")
    )
}

player_stat_card <- function(rank, name, g, ab, avg, obp, slg, ops) {
  fmt3 <- function(x) if (!is.na(x) && is.finite(x)) sprintf("%.3f", x) else ".---"
  ops_color <- dplyr::case_when(
    !is.na(ops) & ops >= 0.900 ~ "#4ade80",
    !is.na(ops) & ops >= 0.800 ~ "#86efac",
    !is.na(ops) & ops >= 0.700 ~ "#fbbf24",
    TRUE                        ~ "#c9d1d9"
  )
  div(class = "player-card",
    div(class = "pc-left",
      div(class = "pc-rank", rank),
      div(class = "pc-info",
        div(class = "pc-name", name),
        div(class = "pc-meta", sprintf("%d G | %d AB", g, ab))
      )
    ),
    div(class = "pc-stats",
      div(class = "pc-stat",
        div(class = "ps-val", style = paste0("color:", ops_color), fmt3(ops)),
        div(class = "ps-lbl", "OPS")
      ),
      div(class = "pc-stat",
        div(class = "ps-val", fmt3(slg)),
        div(class = "ps-lbl", "SLG")
      ),
      div(class = "pc-stat",
        div(class = "ps-val", fmt3(avg)),
        div(class = "ps-lbl", "AVG")
      ),
      div(class = "pc-stat",
        div(class = "ps-val", fmt3(obp)),
        div(class = "ps-lbl", "OBP")
      )
    )
  )
}

# ── CSS ────────────────────────────────────────────────────────────────────

APP_CSS <- "
/* === Reset & Base === */
@import url('https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=DM+Sans:ital,opsz,wght@0,9..40,300;0,9..40,500;0,9..40,700;0,9..40,900;1,9..40,300&display=swap');

body {
  background: #080c10 !important;
  color: #c9d1d9;
  font-family: 'DM Sans', sans-serif;
}
* { box-sizing: border-box; }
.container-fluid { max-width: 1600px; margin: 0 auto; }

/* === Page Header === */
.pg-hdr {
  background: linear-gradient(160deg, #080c10 0%, #0d1117 60%, #111823 100%);
  padding: 24px 32px 20px;
  border-bottom: 1px solid #1d2633;
  margin-bottom: 28px;
  position: relative;
  overflow: hidden;
}
.pg-hdr::before {
  content: '⚾';
  position: absolute; right: 32px; top: 50%; transform: translateY(-50%);
  font-size: 6rem; opacity: 0.04; pointer-events: none;
}
.pg-title {
  color: #e6edf3; font-size: 1.75rem; font-weight: 900;
  letter-spacing: -0.04em; margin: 0; line-height: 1;
  font-family: 'DM Sans', sans-serif;
}
.pg-title span { color: #3b82f6; }
.pg-meta { color: #8b949e; font-size: 0.82rem; margin: 6px 0 0; font-family: 'Space Mono', monospace; }
.pg-section-title {
  color: #e6edf3; font-weight: 800; font-size: 1.05rem; margin: 0 0 18px;
  display: flex; align-items: center; gap: 10px;
}
.pg-section-title::after {
  content: ''; flex: 1; height: 1px; background: linear-gradient(90deg,#1d2633,transparent);
}

/* === Game Header === */
.game-hdr {
  background: linear-gradient(135deg, #0d1117 0%, #111823 100%);
  border: 1px solid #1d2633; border-radius: 12px;
  padding: 20px 24px; margin-bottom: 20px;
}
.score-row {
  display: flex; align-items: stretch; gap: 12px;
}
.score-team {
  flex: 1; text-align: center; padding: 4px 0;
}
.team-city {
  color: #6e7681; font-size: 0.72rem; text-transform: uppercase;
  letter-spacing: .12em; font-family: 'Space Mono', monospace;
}
.team-nickname {
  color: #8b949e; font-size: 0.9rem; font-weight: 700; margin: 1px 0 6px;
}
.score-num {
  font-size: 3rem; font-weight: 900; color: #e6edf3; line-height: 1;
  font-family: 'Space Mono', monospace; letter-spacing: -0.04em;
}
.score-mid {
  display: flex; flex-direction: column; align-items: center;
  justify-content: center; gap: 5px; min-width: 140px; padding: 0 8px;
  border-left: 1px solid #1d2633; border-right: 1px solid #1d2633;
}
.vs-sep   { color: #30363d; font-size: 0.72rem; text-transform: uppercase; letter-spacing: .12em; }
.game-time { color: #8b949e; font-size: 0.78rem; font-family: 'Space Mono', monospace; }
.game-venue { color: #6e7681; font-size: 0.7rem; text-align: center; }

/* === Status Badges === */
.status-badge {
  font-size: 0.65rem; padding: 3px 10px; border-radius: 20px;
  font-weight: 700; letter-spacing: .08em; font-family: 'Space Mono', monospace;
}
.badge-final  { background: #1f4e8c; color: #79b8ff; border: 1px solid #2d6cbe44; }
.badge-live   { background: #3d0a0a; color: #ff7b72; border: 1px solid #da363344;
                animation: liveblink 1.6s ease-in-out infinite; }
.badge-sched  { background: transparent; color: #8b949e; border: 1px solid #30363d; }
@keyframes liveblink { 0%,100%{opacity:1;box-shadow:0 0 0 0 #da363340} 50%{opacity:.7;box-shadow:0 0 0 4px #da363310} }

/* === Section Headers === */
.sec-hdr {
  display: flex; align-items: center; gap: 8px;
  color: #8b949e; font-size: 0.72rem; text-transform: uppercase;
  letter-spacing: .1em; font-family: 'Space Mono', monospace;
  border-bottom: 1px solid #1d2633; padding-bottom: 8px; margin: 0 0 10px;
}
.sec-icon { font-size: 0.9rem; }

/* === Sub-tab Nav === */
.nav-tabs { border-bottom: 1px solid #1d2633; margin-bottom: 0; }
.nav-tabs .nav-link {
  color: #6e7681; border: none; padding: 7px 18px; font-size: 0.82rem;
  font-weight: 500; border-radius: 0; transition: all .15s;
}
.nav-tabs .nav-link:hover  { color: #c9d1d9; background: #111823; border-radius: 6px 6px 0 0; }
.nav-tabs .nav-link.active {
  color: #e6edf3 !important; background: transparent !important;
  border-bottom: 2px solid #3b82f6 !important; border-radius: 0 !important;
  font-weight: 700;
}
.tab-content { padding-top: 16px; }

/* === Top-level Game Tabs === */
#game_tabs.nav-tabs .nav-link {
  font-size: 0.8rem; padding: 8px 14px; color: #8b949e;
  font-family: 'Space Mono', monospace;
}
#game_tabs.nav-tabs .nav-link.active {
  color: #3b82f6 !important; border-bottom: 2px solid #3b82f6 !important;
}

/* === DataTables === */
table.dataTable {
  border-collapse: collapse !important;
}
table.dataTable thead th {
  background: #0d1117 !important; color: #6e7681 !important;
  font-size: 0.68rem; text-transform: uppercase; letter-spacing: .1em;
  border-bottom: 1px solid #1d2633 !important; border-top: none !important;
  padding: 8px 10px; font-family: 'Space Mono', monospace; font-weight: 400;
}
table.dataTable tbody td {
  color: #c9d1d9; border-color: #111823 !important;
  font-size: 0.8rem; padding: 6px 10px;
  font-family: 'Space Mono', monospace;
}
table.dataTable tbody tr:hover td { background: #111823 !important; }
table.dataTable.cell-border tbody td { border-right: 1px solid #111823 !important; }
.dataTables_wrapper { color: #6e7681; font-size: 0.75rem; }
.dataTables_info     { color: #6e7681; font-size: 0.7rem; padding-top: 10px; }

/* === Top-2 Note === */
.top2-legend {
  display: flex; align-items: center; gap: 8px;
  font-size: 0.72rem; color: #8b949e; margin: 0 0 12px;
  font-family: 'Space Mono', monospace;
}
.top2-legend-dot {
  width: 16px; height: 3px; background: #FFD700; border-radius: 2px; flex-shrink: 0;
}

/* === Game Container === */
.game-container {
  background: #0d1117; border: 1px solid #1d2633; border-radius: 12px;
  padding: 24px; margin-bottom: 0;
}

/* === Monthly Cards === */
.monthly-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(360px, 1fr));
  gap: 16px;
}
.monthly-card {
  background: #0d1117; border: 1px solid #1d2633; border-radius: 12px; padding: 18px;
}
.monthly-card-hdr {
  font-size: 0.85rem; font-weight: 800; color: #3b82f6;
  margin-bottom: 14px; padding-bottom: 10px; border-bottom: 1px solid #1d2633;
  display: flex; align-items: center; gap: 8px;
}
.monthly-card-hdr::before {
  content: '';  width: 4px; height: 14px; background: #3b82f6; border-radius: 2px;
}

.player-card {
  display: flex; align-items: center; justify-content: space-between;
  padding: 10px 0; border-bottom: 1px solid #111823;
}
.player-card:last-child { border-bottom: none; padding-bottom: 0; }

.pc-left  { display: flex; align-items: center; gap: 10px; flex: 1; min-width: 0; }
.pc-rank  {
  background: linear-gradient(135deg, #1d3a6e, #1d4e89);
  color: #79b8ff; border-radius: 50%; width: 22px; height: 22px;
  display: flex; align-items: center; justify-content: center;
  font-size: 0.65rem; font-weight: 800; flex-shrink: 0;
  font-family: 'Space Mono', monospace;
}
.pc-info  { min-width: 0; }
.pc-name  { color: #e6edf3; font-size: 0.85rem; font-weight: 700;
             white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }
.pc-meta  { color: #6e7681; font-size: 0.68rem; font-family: 'Space Mono', monospace; margin-top: 1px; }

.pc-stats { display: flex; gap: 12px; flex-shrink: 0; }
.pc-stat  { text-align: center; min-width: 40px; }
.ps-val   { color: #c9d1d9; font-size: 0.88rem; font-weight: 700;
             font-family: 'Space Mono', monospace; }
.ps-lbl   { color: #6e7681; font-size: 0.6rem; text-transform: uppercase;
             letter-spacing: .08em; margin-top: 1px; }

/* === Alerts === */
.alert-info    { background: #111d2e; border: 1px solid #1d3a6e; color: #79b8ff; border-radius: 8px; }
.alert-warning { background: #1c1609; border: 1px solid #3d2e00; color: #e3b341; border-radius: 8px; }

/* === Refresh button === */
.btn-refresh {
  background: transparent; border: 1px solid #30363d; color: #8b949e;
  font-size: 0.78rem; padding: 6px 14px; border-radius: 6px;
  font-family: 'Space Mono', monospace; transition: all .15s;
}
.btn-refresh:hover { border-color: #3b82f6; color: #3b82f6; }

/* === Spinner override === */
.shiny-spinner-output-container { min-height: 80px; }

/* === Probable Pitchers === */
.probable-pitchers { margin-top: 5px; text-align: center; }
.pp-label { color: #6e7681; font-size: 0.58rem; text-transform: uppercase; letter-spacing: .12em; font-family: 'Space Mono', monospace; }
.pp-names { color: #8b949e; font-size: 0.7rem; font-family: 'Space Mono', monospace; margin-top: 1px; }
.pp-sep   { color: #30363d; margin: 0 3px; }

/* === Player Stats Modal === */
.modal-content { background: #0d1117 !important; border: 1px solid #1d2633 !important; border-radius: 12px !important; }
.modal-header  { border-bottom: 1px solid #1d2633 !important; padding: 16px 20px; }
.modal-footer  { border-top:    1px solid #1d2633 !important; }
.modal-title   { color: #e6edf3 !important; font-family: 'DM Sans', sans-serif !important; font-weight: 800 !important; }
.btn-default, .btn-default:focus { background: transparent !important; border: 1px solid #30363d !important; color: #8b949e !important; }
.btn-default:hover { border-color: #3b82f6 !important; color: #3b82f6 !important; }
.ms-bio { display: flex; align-items: center; gap: 8px; flex-wrap: wrap; padding: 8px 0 14px; border-bottom: 1px solid #1d2633; margin-bottom: 14px; font-size: 0.82rem; }
.ms-team { color: #3b82f6; font-weight: 700; }
.ms-pos  { color: #8b949e; }
.ms-sep  { color: #30363d; }
.ms-section-hdr { color: #6e7681; font-size: 0.65rem; text-transform: uppercase; letter-spacing: .12em; font-family: 'Space Mono', monospace; border-bottom: 1px solid #1d2633; padding-bottom: 6px; margin: 14px 0 10px; }
.ms-stat-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(70px, 1fr)); gap: 8px; }
.ms-stat { text-align: center; padding: 8px 4px; background: #111823; border: 1px solid #1d2633; border-radius: 6px; }
.ms-val  { color: #e6edf3; font-size: 0.9rem; font-weight: 700; font-family: 'Space Mono', monospace; }
.ms-lbl  { color: #6e7681; font-size: 0.58rem; text-transform: uppercase; letter-spacing: .08em; margin-top: 2px; }

/* === Statcast Percentile Badges === */
.pct-grid  { display: flex; flex-wrap: wrap; gap: 10px; margin: 4px 0 6px; }
.pct-item  { display: flex; flex-direction: column; align-items: center; gap: 4px; min-width: 56px; }
.pct-badge { width: 42px; height: 42px; border-radius: 50%; display: flex; align-items: center; justify-content: center; font-size: 0.78rem; font-weight: 800; color: #fff; font-family: 'Space Mono', monospace; box-shadow: 0 0 0 2px #1d2633; }
.pct-label { color: #6e7681; font-size: 0.58rem; text-transform: uppercase; letter-spacing: .06em; text-align: center; line-height: 1.2; }
"

# ── UI ─────────────────────────────────────────────────────────────────────

ui <- page_fluid(
  theme = bs_theme(
    bootswatch       = "darkly",
    primary          = "#3b82f6",
    `font-size-base` = "0.875rem"
  ),
  tags$head(tags$style(HTML(APP_CSS))),

  # ── Page Header ──────────────────────────────────────────────────────────
  div(class = "pg-hdr",
    fluidRow(
      column(9,
        div(style = "display:flex;align-items:center;gap:14px;",
          tags$img(
            src    = "https://www.mlbstatic.com/team-logos/league-on-dark/1.svg",
            height = "44px",
            style  = "filter: drop-shadow(0 0 6px rgba(59,130,246,0.3));"
          ),
          h1(HTML("MLB <span>Daily Report</span>"), class = "pg-title")
        ),
        p(
          paste0(format(TODAY, "%A, %B %d, %Y"),
                 "  ·  Data via MLB Stats API & Baseball Savant"),
          class = "pg-meta"
        )
      ),
      column(3, style = "text-align:right;display:flex;align-items:center;justify-content:flex-end;",
        actionButton("refresh", "↻  Refresh Data", class = "btn btn-refresh")
      )
    )
  ),

  # ── Main content ─────────────────────────────────────────────────────────
  div(style = "padding: 0 28px 48px;",

    # Games section
    div(class = "pg-section-title", "Today's Games"),
    uiOutput("games_ui"),

    # Monthly leaders
    div(style = "margin-top: 44px;"),
    div(class = "pg-section-title",
        paste0("📊  ", MONTH_LABEL, " Monthly Leaders")),
    p("Top 2 hitters per team participating in today's games · Minimum ",
      MIN_AB, " AB · Ranked by OPS",
      style = "color:#6e7681;font-size:0.78rem;font-family:'Space Mono',monospace;margin: -12px 0 20px;"),
    uiOutput("monthly_ui")
  )
)

# ── Server ─────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # ── Schedule ─────────────────────────────────────────────────────────────
  sched_r <- reactive({
    input$refresh  # manual trigger
    withProgress(message = "Fetching today's schedule…", value = 0.3, {
      s <- fetch_schedule()
      incProgress(0.7)
      # Auto-refresh every 60 s while any game is live
      if (!is.null(s) && nrow(s) > 0 &&
          any(grepl("Progress|Live|In Progress",
                    s$status_detailed_state, ignore.case = TRUE), na.rm = TRUE))
        invalidateLater(60000)
      s
    })
  })

  # Parsed games list
  games_r <- reactive({
    s <- sched_r()
    req(s, nrow(s) > 0)
    s %>%
      filter(game_type == "R") %>%       # regular season only
      distinct(game_pk, .keep_all = TRUE) %>%
      arrange(game_datetime)
  })

  # ── Boxscores per game ────────────────────────────────────────────────────
  boxscores_r <- reactive({
    gms <- games_r()
    n   <- nrow(gms)
    withProgress(message = "Loading box scores…", value = 0, {
      lapply(seq_len(n), function(i) {
        incProgress(1/n, detail = sprintf("Game %d of %d", i, n))
        fetch_boxscore(gms$game_pk[i])
      })
    })
  })

  # ── Monthly Statcast ──────────────────────────────────────────────────────
  monthly_r <- reactive({
    input$refresh  # manual trigger; fetch_monthly_sc is memoised so re-runs are instant
    withProgress(message = paste("Loading", MONTH_LABEL, "Statcast data…"), value = 0.1, {
      sc <- fetch_monthly_sc()
      incProgress(0.9)
      compute_monthly(sc)
    })
  })

  # ── Dynamic DT outputs ────────────────────────────────────────────────────
  observe({
    gms <- games_r()
    bxs <- boxscores_r()

    lapply(seq_len(nrow(gms)), function(i) {
      local({
        ii <- i
        bs <- bxs[[ii]]
        g  <- gms[ii, ]

        away <- g$teams_away_team_name %||% "Away"
        home <- g$teams_home_team_name %||% "Home"

        away_bat <- parse_batting(bs, "away")
        home_bat <- parse_batting(bs, "home")
        away_pit <- parse_pitching(bs, "away")
        home_pit <- parse_pitching(bs, "home")

        away_t2 <- top2_by_ops(away_bat)
        home_t2 <- top2_by_ops(home_bat)

        output[[paste0("away_bat_", ii)]] <- DT::renderDT(make_dt(away_bat, away_t2), server = FALSE)
        output[[paste0("home_bat_", ii)]] <- DT::renderDT(make_dt(home_bat, home_t2), server = FALSE)
        output[[paste0("away_pit_", ii)]] <- DT::renderDT(make_dt(away_pit), server = FALSE)
        output[[paste0("home_pit_", ii)]] <- DT::renderDT(make_dt(home_pit), server = FALSE)
      })
    })
  })

  # ── Games UI ─────────────────────────────────────────────────────────────
  output$games_ui <- renderUI({
    s <- sched_r()

    if (is.null(s)) {
      return(div(class = "alert alert-warning",
        "⚠ Could not load today's schedule. Check your internet connection."))
    }

    reg <- tryCatch(
      s %>% filter(game_type == "R") %>% distinct(game_pk, .keep_all = TRUE),
      error = function(e) NULL
    )

    if (is.null(reg) || nrow(reg) == 0) {
      return(div(class = "alert alert-info",
        "ℹ No regular-season MLB games scheduled today."))
    }

    gms <- games_r()
    bxs <- boxscores_r()

    panels <- lapply(seq_len(nrow(gms)), function(i) {
      g    <- gms[i, ]
      away <- g$teams_away_team_name %||% "Away"
      home <- g$teams_home_team_name %||% "Home"

      # Scores
      as_  <- tryCatch(g$teams_away_score, error = function(e) NA)
      hs_  <- tryCatch(g$teams_home_score, error = function(e) NA)
      state <- tryCatch(as.character(g$status_detailed_state), error = function(e) "Scheduled") %||% "Scheduled"
      venue <- tryCatch(as.character(g$venue_name), error = function(e) NA) %||% NA_character_

      # Probable pitchers
      away_prob <- tryCatch({ v <- g[["teams_away_probable"]]; if (is.null(v) || is.na(v)) NA_character_ else fmt_prob(v) }, error = function(e) NA_character_)
      home_prob <- tryCatch({ v <- g[["teams_home_probable"]]; if (is.null(v) || is.na(v)) NA_character_ else fmt_prob(v) }, error = function(e) NA_character_)

      # Game time (ET)
      gtime <- tryCatch({
        dt <- g$game_datetime
        if (is.na(dt)) return("")
        format(lubridate::with_tz(as.POSIXct(dt, tz = "UTC"), "America/New_York"), "%I:%M %p ET")
      }, error = function(e) "")

      # Tab label
      tab_lbl <- sprintf("%s @ %s", away, home)

      tabPanel(
        tab_lbl,
        value = paste0("gtab_", i),

        div(class = "game-container",

          # Score header
          score_display(away, home, as_, hs_, state, gtime, venue, away_prob, home_prob),

          # Top-2 legend
          div(class = "top2-legend",
            div(class = "top2-legend-dot"),
            "Gold rows = Top 2 hitters by today's OPS"
          ),

          # Batting / Pitching sub-tabs
          tabsetPanel(
            type = "tabs",

            # ── Batting ─────────────────────────────────────────────────
            tabPanel("🏏  Batting",
              fluidRow(
                column(6,
                  sec_hdr("▶", paste(away, "Batters")),
                  DT::DTOutput(paste0("away_bat_", i))
                ),
                column(6,
                  sec_hdr("▶", paste(home, "Batters")),
                  DT::DTOutput(paste0("home_bat_", i))
                )
              )
            ),

            # ── Pitching ─────────────────────────────────────────────────
            tabPanel("⚾  Pitching",
              fluidRow(
                column(6,
                  sec_hdr("▶", paste(away, "Pitchers")),
                  DT::DTOutput(paste0("away_pit_", i))
                ),
                column(6,
                  sec_hdr("▶", paste(home, "Pitchers")),
                  DT::DTOutput(paste0("home_pit_", i))
                )
              )
            )
          )  # /tabsetPanel
        )  # /game-container
      )  # /tabPanel
    })  # /lapply

    do.call(tabsetPanel,
      c(list(type = "tabs", id = "game_tabs"), panels)
    )
  })

  # ── Monthly Leaders UI ───────────────────────────────────────────────────
  output$monthly_ui <- renderUI({
    mo <- monthly_r()
    s  <- sched_r()

    if (is.null(s) || nrow(s) == 0) {
      return(div(class = "alert alert-info",
        "ℹ Load today's schedule first."))
    }
    if (is.null(mo)) {
      return(div(class = "alert alert-warning",
        "⚠ Monthly Statcast data unavailable — Baseball Savant may be rate-limiting. Try again in a moment."))
    }

    # Teams in today's games — convert full names to Statcast abbreviations
    teams_full <- unique(c(
      s$teams_away_team_name,
      s$teams_home_team_name
    ))
    teams_full <- sort(teams_full[!is.na(teams_full)])
    teams_abb  <- name_to_abb(teams_full)

    cards <- compact(lapply(seq_along(teams_full), function(ti) {
      team    <- teams_full[ti]
      team_sc <- teams_abb[ti]
      top2 <- mo %>%
        filter(batter_team == team_sc) %>%
        slice_max(OPS, n = 2, with_ties = FALSE)

      if (nrow(top2) == 0) return(NULL)

      pcards <- lapply(seq_len(nrow(top2)), function(j) {
        r <- top2[j, ]
        player_stat_card(j, r$player_name, r$G, r$AB, r$AVG, r$OBP, r$SLG, r$OPS)
      })

      div(class = "monthly-card",
        div(class = "monthly-card-hdr", team),
        tagList(pcards)
      )
    }))

    if (length(cards) == 0) {
      return(div(class = "alert alert-warning",
        "⚠ No qualified hitters found (min ", MIN_AB, " AB). Try later in the month."))
    }

    div(class = "monthly-grid", tagList(cards))
  })

  # ── Player season-stats modal ───────────────────────────────────────────
  output$player_modal_content <- renderUI({
    req(input$player_click)
    data <- fetch_player_season_stats(as.integer(input$player_click$id))
    build_player_modal_ui(data)
  })

  observeEvent(input$player_click, {
    req(input$player_click)
    showModal(modalDialog(
      title = div(
        style = paste0("color:#e6edf3;font-family:'DM Sans',sans-serif;",
                       "font-weight:800;font-size:1.1rem;"),
        input$player_click$name
      ),
      size      = "l",
      easyClose = TRUE,
      footer    = modalButton("Close"),
      uiOutput("player_modal_content")
    ))
  })

}

# ── Run ────────────────────────────────────────────────────────────────────
shinyApp(ui, server)
