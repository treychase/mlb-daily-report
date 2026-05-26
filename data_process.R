# ── Data Processing Functions ─────────────────────────────────────────────

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

      # Game counting stats
      ab_  <- int0(bat$atBats);    h_  <- int0(bat$hits)
      d_   <- int0(bat$doubles);   t_  <- int0(bat$triples)
      hr_  <- int0(bat$homeRuns);  bb_ <- int0(bat$baseOnBalls)
      hbp_ <- int0(bat$hitByPitch %||% 0)
      sf_  <- int0(bat$sacFlies   %||% 0)
      tb_  <- h_ + d_ + 2*t_ + 3*hr_
      den_ <- ab_ + bb_ + hbp_ + sf_
      obp_ <- if (den_ > 0) (h_ + bb_ + hbp_) / den_ else NA_real_
      slg_ <- if (ab_  > 0) tb_ / ab_             else NA_real_

      # Season slash line: use boxscore seasonStats (zero extra API calls)
      # Falls back to today-only computation if seasonStats is unavailable.
      seas <- tryCatch(p$seasonStats$batting, error = function(e) NULL)
      sl <- function(field, fallback_val) {
        v <- tryCatch(as.character(seas[[field]]), error = function(e) "")
        if (length(v) > 0 && nchar(v) > 0 && !v %in% c("---", ".---", "-.--"))
          v
        else
          fmt_rate(fallback_val)
      }

      tibble(
        .ord      = int0(p$battingOrder),
        Name      = as.character(p$person$fullName %||% "\u2014"),
        person_id = as.integer(pid),
        Pos   = as.character(p$position$abbreviation %||% "\u2014"),
        AB    = ab_,
        R     = int0(bat$runs),
        H     = h_,
        `2B`  = d_,
        `3B`  = t_,
        HR    = hr_,
        RBI   = int0(bat$rbi),
        BB    = bb_,
        SO    = int0(bat$strikeOuts),
        AVG   = sl("avg", if (ab_ > 0) h_/ab_ else NA_real_),
        OBP   = sl("obp", obp_),
        SLG   = sl("slg", slg_),
        OPS   = sl("ops", if (!is.na(obp_) && !is.na(slg_)) obp_+slg_ else NA_real_)
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

# --- Baseball Reference leaderboards (memoise to fetch once per session) ------

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

