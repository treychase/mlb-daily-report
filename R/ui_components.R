# ── UI Component Functions ─────────────────────────────────────────────────

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
                          away_prob = NA_character_, home_prob = NA_character_,
                          away_id = NA_integer_,    home_id = NA_integer_) {
  as_str <- tryCatch(as.character(as.integer(as_)), error = function(e) "–")
  hs_str <- tryCatch(as.character(as.integer(hs_)), error = function(e) "–")
  if (is.na(as_str)) as_str <- "–"
  if (is.na(hs_str)) hs_str <- "–"
  
  div(class = "game-hdr",
      div(class = "score-row",
          div(class = "score-team away-team",
              if (!is.null(away_id) && !is.na(away_id))
                tags$img(class = "score-team-logo",
                         src   = paste0("https://www.mlbstatic.com/team-logos/", away_id, ".svg")),
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
              if (!is.null(home_id) && !is.na(home_id))
                tags$img(class = "score-team-logo",
                         src   = paste0("https://www.mlbstatic.com/team-logos/", home_id, ".svg")),
              div(class = "team-city", strsplit(home, " ")[[1]] |> head(-1) |> paste(collapse = " ")),
              div(class = "team-nickname", strsplit(home, " ")[[1]] |> tail(1)),
              div(class = "score-num", hs_str)
          )
      )
  )
}


linescore_display <- function(ls, away_name, home_name) {
  if (is.null(ls) || is.null(ls$innings) || length(ls$innings) == 0) return(NULL)
  
  short <- function(nm)
    tail(strsplit(trimws(as.character(nm)), "\\s+")[[1]], 1)
  
  safe_r <- function(x) {
    r <- tryCatch(x$runs, error = function(e) NULL)
    if (is.null(r) || length(r) == 0 || is.na(r)) "" else as.character(as.integer(r))
  }
  tot <- function(side, stat) {
    v <- tryCatch(ls$teams[[side]][[stat]], error = function(e) NULL)
    if (is.null(v) || length(v) == 0 || is.na(v)) "" else as.character(as.integer(v))
  }
  
  inn_hdrs   <- lapply(ls$innings, function(inn)
    tags$th(class="ls-inn", as.character(inn$num %||% "")))
  away_cells <- lapply(ls$innings, function(inn)
    tags$td(class="ls-cell", safe_r(inn$away)))
  home_cells <- lapply(ls$innings, function(inn)
    tags$td(class="ls-cell", safe_r(inn$home)))
  
  div(class = "linescore-wrap",
      tags$table(class = "linescore-tbl",
                 tags$thead(tags$tr(
                   tags$th(class = "ls-team-col", ""),
                   tagList(inn_hdrs),
                   tags$th(class = "ls-div", ""),
                   tags$th(class = "ls-tot-hdr", "R"),
                   tags$th(class = "ls-tot-hdr", "H"),
                   tags$th(class = "ls-tot-hdr", "E")
                 )),
                 tags$tbody(
                   tags$tr(
                     tags$td(class = "ls-team-name", short(away_name)),
                     tagList(away_cells),
                     tags$td(class = "ls-div", ""),
                     tags$td(class = "ls-total", tot("away","runs")),
                     tags$td(class = "ls-total", tot("away","hits")),
                     tags$td(class = "ls-total", tot("away","errors"))
                   ),
                   tags$tr(
                     tags$td(class = "ls-team-name", short(home_name)),
                     tagList(home_cells),
                     tags$td(class = "ls-div", ""),
                     tags$td(class = "ls-total", tot("home","runs")),
                     tags$td(class = "ls-total", tot("home","hits")),
                     tags$td(class = "ls-total", tot("home","errors"))
                   )
                 )
      )
  )
}


sec_hdr <- function(icon_txt, label) {
  div(class = "sec-hdr", span(class = "sec-icon", icon_txt), label)
}


make_dt <- function(df, top2 = NULL, kind = c("batting","pitching")) {
  kind <- match.arg(kind)
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
  if (has_pid) df <- df %>% dplyr::relocate(person_id, .before = .h)
  
  n_cols  <- ncol(df)
  h_idx   <- n_cols - 1L
  pid_idx <- if (has_pid) n_cols - 2L else integer(0)
  hidden  <- c(h_idx, pid_idx)
  
  click_target <- if (identical(kind, "pitching")) "pitcher_profile_click" else "player_click"
  row_cb <- if (has_pid) DT::JS(sprintf(
    "function(row, data) {
       $(row).css('cursor','pointer').on('click', function() {
         Shiny.setInputValue('%s',
           { id: data[data.length - 2], name: data[0] },
           { priority: 'event' });
       });
     }", click_target
  )) else NULL
  
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


player_stat_card <- function(rank, name, g, ab, avg, obp, slg, ops, war = NA) {
  fmt3 <- function(x) if (!is.na(x) && is.finite(x)) sprintf("%.3f", x) else ".---"
  ops_color <- dplyr::case_when(
    !is.na(ops) & ops >= 0.900 ~ "#4ade80",
    !is.na(ops) & ops >= 0.800 ~ "#86efac",
    !is.na(ops) & ops >= 0.700 ~ "#fbbf24",
    TRUE                        ~ "#c9d1d9"
  )
  war_v <- suppressWarnings(as.numeric(war))
  war_box <- if (!is.null(war_v) && !is.na(war_v) && is.finite(war_v)) {
    war_col <- dplyr::case_when(
      war_v >= 5  ~ "#4ade80", war_v >= 2  ~ "#86efac",
      war_v >= 0  ~ "#fbbf24", TRUE         ~ "#f87171"
    )
    div(class = "pc-stat",
        div(class = "ps-val", style = paste0("color:", war_col),
            sprintf("%.1f", war_v)),
        div(class = "ps-lbl", "bWAR")
    )
  } else NULL
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
          ),
          war_box
      )
  )
}


build_player_modal_ui <- function(data) {
  if (is.null(data) || !is.list(data))
    return(div(style = "color:#8b949e;padding:20px;", "Season stats unavailable."))
  
  bio  <- data$bio
  team <- tryCatch(bio$currentTeam$name       %||% "\u2014", error = function(e) "\u2014")
  th <- data$team_history
  if (!is.null(th) && grepl("[,/|]", as.character(th))) {
    teams_all <- trimws(strsplit(as.character(th), "[,/|]")[[1]])
    teams_all <- teams_all[nchar(teams_all) > 0]
    if (length(teams_all) > 1)
      team <- paste0(team, "  (", paste(teams_all, collapse=" \u2192 "), ")")
  }
  pos  <- tryCatch(bio$primaryPosition$name   %||% "\u2014", error = function(e) "\u2014")
  age  <- tryCatch(as.character(bio$currentAge %||% "\u2014"), error = function(e) "\u2014")
  bats <- tryCatch(bio$batSide$description    %||% "\u2014", error = function(e) "\u2014")
  thro <- tryCatch(bio$pitchHand$description  %||% "\u2014", error = function(e) "\u2014")
  
  pid     <- data$person_id
  team_id <- data$team_id
  
  headshot_url  <- paste0(
    "https://img.mlbstatic.com/mlb-photos/image/upload/",
    "d_people:generic:headshot:67:current.png/w_180,q_auto:best",
    "/v1/people/", pid, "/headshot/67/current"
  )
  team_logo_url <- if (!is.null(team_id) && !is.na(team_id))
    paste0("https://www.mlbstatic.com/team-logos/", team_id, ".svg")
  else NULL
  
  fmt3 <- function(x) tryCatch(sprintf("%.3f", as.numeric(x)), error = function(e) "\u2014")
  fmt0 <- function(x) tryCatch(as.character(as.integer(x)),    error = function(e) "\u2014")
  
  pct_badge <- function(pct, label) {
    if (is.null(pct) || length(pct) == 0 || is.na(pct)) return(NULL)
    v <- as.integer(round(as.numeric(pct)))
    div(class = "pct-item",
        div(class = "pct-badge", style = paste0("background:", pct_col(v), ";"), v),
        div(class = "pct-label", label))
  }
  slash_cell <- function(val, lbl, pct = NULL) {
    dot <- if (!is.null(pct) && !is.na(pct)) {
      v <- as.integer(round(as.numeric(pct)))
      div(class = "slash-pct", style = paste0("background:", pct_col(v), ";"), v)
    }
    div(class = "slash-cell", div(class="slash-val", as.character(val %||% "\u2014")),
        div(class="slash-lbl", lbl), dot)
  }
  ops_col <- function(v) dplyr::case_when(
    v >= 160 ~ "#FFD700", v >= 130 ~ "#4ade80",
    v >= 110 ~ "#86efac", v >= 90  ~ "#c9d1d9", TRUE ~ "#6e7681"
  )
  # col_override: pass "#e6edf3" for WAR so it renders white.
  # ops_col() treats WAR values (typically 0–8) as dim grey; override forces white.
  adj_stat <- function(val, lbl, col_override = NULL) {
    v   <- suppressWarnings(as.numeric(val))
    col <- if (!is.null(col_override)) col_override
    else if (!is.null(val) && !is.na(v)) ops_col(v) else "#8b949e"
    div(class = "adj-stat",
        div(class = "adj-val", style = paste0("color:", col, ";"),
            as.character(val %||% "\u2014")),
        div(class = "adj-lbl", lbl))
  }
  sb <- function(val, lbl)
    div(class = "ms-stat",
        div(class = "ms-val", as.character(val %||% "\u2014")),
        div(class = "ms-lbl", lbl))
  
  pitch_col_local <- function(pt) {
    switch(toupper(as.character(pt %||% "")),
           "FF" = "#ef4444", "SI" = "#f97316", "FC" = "#f59e0b",
           "SL" = "#3b82f6", "SW" = "#60a5fa", "ST" = "#93c5fd",
           "CH" = "#22c55e", "FS" = "#14b8a6", "FO" = "#10b981",
           "CU" = "#a855f7", "KC" = "#9333ea", "CS" = "#7c3aed",
           "KN" = "#8b949e", "#6e7681"
    )
  }
  
  player_hdr <- div(class = "player-modal-hdr",
                    tags$img(class = "player-headshot", src = headshot_url,
                             onerror = paste0("this.onerror=null;this.src='",
                                              "https://img.mlbstatic.com/mlb-photos/image/upload/",
                                              "d_people:generic:headshot:67:current.png/v1/people/1/headshot/67/current';")),
                    div(style = "flex:1; min-width:0;",
                        div(class = "ms-bio", style = "border:none; padding:0; margin:0;",
                            span(class = "ms-team", team), span(class = "ms-sep", "\u00b7"),
                            span(class = "ms-pos",  pos),  span(class = "ms-sep", "\u00b7"),
                            span(style = "color:#6e7681;", paste0("Age ", age)),
                            span(class = "ms-sep",  "\u00b7"),
                            span(style = "color:#6e7681;", paste0("Bats: ", bats)),
                            span(class = "ms-sep",  "\u00b7"),
                            span(style = "color:#6e7681;", paste0("Throws: ", thro))
                        )
                    ),
                    if (!is.null(team_logo_url))
                      tags$img(class = "team-logo-hdr", src = team_logo_url)
  )
  
  sc_pct_row <- {
    pd <- data$percentiles
    if (!is.null(pd) && (is.data.frame(pd) || is.list(pd)) && length(pd) > 0) {
      row <- if (is.data.frame(pd)) pd[1, ] else pd
      g   <- function(nm) tryCatch(row[[nm]], error = function(e) NULL)
      if (!isTRUE(data$is_pitcher)) {
        tagList(
          div(class = "ms-section-hdr", "Statcast Percentiles"),
          div(class = "pct-grid",
              pct_badge(g("exit_velocity_avg"),  "Exit Velo"),
              pct_badge(g("hard_hit_percent"),   "Hard Hit%"),
              pct_badge(g("barrel_batted_rate"), "Barrel%"),
              pct_badge(g("xba"),               "xBA"),
              pct_badge(g("xslg"),              "xSLG"),
              pct_badge(g("xwoba"),             "xwOBA"),
              pct_badge(g("xobp"),              "xOBP"),
              pct_badge(g("sprint_speed"),      "Speed")
          )
        )
      } else {
        tagList(
          div(class = "ms-section-hdr", "Statcast Percentiles"),
          div(class = "pct-grid",
              pct_badge(g("fastball_avg_speed"), "FB Velo"),
              pct_badge(g("fastball_avg_spin"),  "FB Spin"),
              pct_badge(g("xera"),              "xERA"),
              pct_badge(g("xba"),               "xBA"),
              pct_badge(g("xwoba"),             "xwOBA"),
              pct_badge(g("whiff_percent"),     "Whiff%"),
              pct_badge(g("k_percent"),         "K%"),
              pct_badge(g("bb_percent"),        "BB%")
          )
        )
      }
    }
  }
  
  # ════════════════════════════════════════════════════════════════════════
  # HITTER LAYOUT
  # ════════════════════════════════════════════════════════════════════════
  if (!isTRUE(data$is_pitcher)) {
    h <- data$hitting
    if (length(h) == 0 || length(h[[1]]$splits) == 0)
      return(div(style="padding:4px;", player_hdr,
                 p(style="color:#8b949e;margin-top:16px;","No season hitting stats recorded yet.")))
    
    s <- h[[1]]$splits[[1]]$stat
    avg_v <- fmt3(s$avg); obp_v <- fmt3(s$obp)
    slg_v <- fmt3(s$slg); ops_v <- fmt3(s$ops)
    
    slash <- div(class = "slash-line",
                 slash_cell(avg_v,"AVG",stat_pct(as.numeric(avg_v),0.185,0.345)),
                 div(class="slash-sep","/"),
                 slash_cell(obp_v,"OBP",stat_pct(as.numeric(obp_v),0.265,0.435)),
                 div(class="slash-sep","/"),
                 slash_cell(slg_v,"SLG",stat_pct(as.numeric(slg_v),0.295,0.660)),
                 div(class="slash-sep","/"),
                 slash_cell(ops_v,"OPS",stat_pct(as.numeric(ops_v),0.545,1.060))
    )
    ops_plus <- tryCatch(round(as.numeric(s$ops)/0.720*100), error=function(e) NA)
    wrc_plus <- data$wrc_plus
    war_h    <- data$war_val
    war_lbl  <- switch(as.character(data$war_source %||% "bref"),
                       "fangraphs" = "fWAR", "estimate" = "WAR (est.)",
                       "none" = "WAR", "bWAR")
    adj <- div(class="adj-stat-line",
               adj_stat(if(!is.null(ops_plus)&&!is.na(ops_plus)) ops_plus else "\u2014","OPS+"),
               adj_stat(if(!is.null(wrc_plus)&&!is.na(wrc_plus)) round(wrc_plus) else "\u2014","wRC+"),
               if(!is.null(war_h)&&!is.na(war_h))
                 adj_stat(sprintf("%.1f", as.numeric(war_h)), war_lbl, col_override = "#e6edf3")
    )
    stats_grid <- tagList(
      div(class="ms-section-hdr","Season Hitting"),
      div(class="ms-stat-grid",
          sb(fmt0(s$gamesPlayed),"G"),    sb(fmt0(s$atBats),     "AB"),
          sb(fmt0(s$hits),      "H"),    sb(fmt0(s$doubles),    "2B"),
          sb(fmt0(s$triples),   "3B"),   sb(fmt0(s$homeRuns),   "HR"),
          sb(fmt0(s$rbi),       "RBI"),  sb(fmt0(s$baseOnBalls),"BB"),
          sb(fmt0(s$strikeOuts),"K"),    sb(fmt0(s$stolenBases),"SB"),
          sb(fmt3(s$babip),    "BABIP"), sb(fmt0(s$runs),       "R")
      )
    )
    splits_sec <- {
      vl <- data$splits_vl;  vr <- data$splits_vr
      if (!is.null(vl) || !is.null(vr)) {
        g0 <- function(sp, field) tryCatch(as.character(sp[[field]]), error=function(e)"—")
        sp_row <- function(label, sp) {
          if (is.null(sp)) return(NULL)
          div(class="split-row",
              div(class="split-label", label),
              div(class="split-stat rate", g0(sp,"avg")),
              div(class="split-stat rate", g0(sp,"obp")),
              div(class="split-stat rate", g0(sp,"slg")),
              div(class="split-stat rate", g0(sp,"ops")),
              div(class="split-stat cnt",  g0(sp,"atBats")),
              div(class="split-stat cnt",  g0(sp,"homeRuns")),
              div(class="split-stat cnt",  g0(sp,"baseOnBalls"))
          )
        }
        tagList(
          div(class="ms-section-hdr", "L/R Splits"),
          div(class="splits-tbl",
              div(class="split-row split-hdr-row",
                  div(class="split-label", ""),
                  div(class="split-stat rate", "AVG"),
                  div(class="split-stat rate", "OBP"),
                  div(class="split-stat rate", "SLG"),
                  div(class="split-stat rate", "OPS"),
                  div(class="split-stat cnt",  "AB"),
                  div(class="split-stat cnt",  "HR"),
                  div(class="split-stat cnt",  "BB")
              ),
              sp_row("vs LHP", vl),
              sp_row("vs RHP", vr)
          )
        )
      }
    }
    
    div(style="padding:4px;", player_hdr, slash, adj, splits_sec, sc_pct_row, stats_grid)
    
  } else {
    # ════════════════════════════════════════════════════════════════════════
    # PITCHER LAYOUT
    # ════════════════════════════════════════════════════════════════════════
    p_data <- data$pitching
    if (length(p_data) == 0 || length(p_data[[1]]$splits) == 0)
      return(div(style="padding:4px;", player_hdr,
                 p(style="color:#8b949e;margin-top:16px;","No season pitching stats recorded yet.")))
    
    s <- p_data[[1]]$splits[[1]]$stat
    era_v  <- as.character(s$era               %||% "\u2014")
    whip_v <- as.character(s$whip              %||% "\u2014")
    k9_v   <- as.character(s$strikeoutsPer9Inn  %||% "\u2014")
    bb9_v  <- as.character(s$walksPer9Inn       %||% "\u2014")
    baa_v  <- fmt3(s$avg)
    
    key_line <- div(class="slash-line",
                    slash_cell(era_v, "ERA",  stat_pct(as.numeric(era_v), 7.50,1.50)),
                    div(class="slash-sep","/"),
                    slash_cell(whip_v,"WHIP", stat_pct(as.numeric(whip_v),2.00,0.70)),
                    div(class="slash-sep","/"),
                    slash_cell(k9_v,  "K/9",  stat_pct(as.numeric(k9_v), 3.00,14.0)),
                    div(class="slash-sep","/"),
                    slash_cell(bb9_v, "BB/9", stat_pct(as.numeric(bb9_v),6.50,0.80)),
                    div(class="slash-sep","/"),
                    slash_cell(baa_v, "BAA",  stat_pct(as.numeric(baa_v),0.320,0.155))
    )
    
    arsenal_sec <- {
      ars <- data$pitch_arsenal
      if (!is.null(ars) && is.data.frame(ars) && nrow(ars) > 0) {
        cards <- lapply(seq_len(nrow(ars)), function(i) {
          row    <- ars[i, ]
          ptype  <- tryCatch(as.character(row$pitch_type), error=function(e)"??")
          pname  <- tryCatch(as.character(row$pitch_name  %||% ptype), error=function(e) ptype)
          usage  <- tryCatch(sprintf("%.1f%%", as.numeric(row$pitch_percent)*100), error=function(e)"?%")
          velo   <- tryCatch(sprintf("%.1f mph", as.numeric(row$avg_speed)), error=function(e) NULL)
          spin   <- tryCatch(paste0(format(round(as.numeric(row$avg_spin)), big.mark=","), " rpm"), error=function(e) NULL)
          whiff  <- tryCatch(sprintf("%.1f%% whiff", as.numeric(row$whiff_percent)), error=function(e) NULL)
          col    <- pitch_col_local(ptype)
          div(class="pitch-card", style=paste0("border-top:3px solid ",col,";"),
              div(class="pitch-type-badge", style=paste0("background:",col,";"), ptype),
              div(class="pitch-name", pname),
              div(class="pitch-usage", usage),
              div(class="pitch-stats-row",
                  if(!is.null(velo))  div(class="pitch-stat-item", velo),
                  if(!is.null(spin))  div(class="pitch-stat-item", spin),
                  if(!is.null(whiff)) div(class="pitch-stat-item", whiff)
              )
          )
        })
        tagList(div(class="ms-section-hdr","Pitch Arsenal"), div(class="pitch-grid", tagList(cards)))
      }
    }
    
    trad_pct <- tagList(
      div(class="ms-section-hdr","Traditional Percentiles"),
      div(class="pct-grid",
          pct_badge(stat_pct(as.numeric(era_v), 7.50,1.50), "ERA"),
          pct_badge(stat_pct(as.numeric(k9_v),  3.00,14.0), "K/9"),
          pct_badge(stat_pct(as.numeric(bb9_v), 6.50,0.80), "BB/9"),
          pct_badge(stat_pct(as.numeric(whip_v),2.00,0.70), "WHIP"),
          pct_badge(stat_pct(as.numeric(baa_v), 0.320,0.155),"BAA")
      )
    )
    stats_grid <- tagList(
      div(class="ms-section-hdr","Season Pitching"),
      div(class="ms-stat-grid",
          sb(fmt0(s$gamesPlayed),"G"),  sb(fmt0(s$gamesStarted),"GS"),
          sb(paste0(fmt0(s$wins),"-",fmt0(s$losses)),"W-L"),
          sb(era_v,"ERA"), sb(as.character(s$inningsPitched %||% "\u2014"),"IP"),
          sb(fmt0(s$strikeOuts),"K"),   sb(fmt0(s$baseOnBalls),"BB"),
          sb(fmt0(s$hits),"H"),         sb(fmt0(s$homeRuns),"HR"),
          sb(whip_v,"WHIP"), sb(k9_v,"K/9"), sb(bb9_v,"BB/9"), sb(baa_v,"BAA")
      )
    )
    war_p    <- data$war_val
    war_lblp <- switch(as.character(data$war_source %||% "bref"),
                       "fangraphs" = "fWAR", "estimate" = "WAR (est.)", "none" = "WAR", "bWAR")
    war_line <- if (!is.null(war_p) && !is.na(war_p)) {
      div(class="adj-stat-line",
          adj_stat(sprintf("%.1f", as.numeric(war_p)), war_lblp, col_override = "#e6edf3"))
    } else NULL
    
    div(style="padding:4px;", player_hdr, key_line, war_line,
        arsenal_sec, sc_pct_row, trad_pct, stats_grid)
  }
}

# ── Pitcher profile (movement + RV/100 + splits) ─────────────────────────────
# movement_data is now NULL on first open (loaded lazily by renderPlot).
# When NULL, falls back to pitch_arsenal for the characteristic cards.
# avg_break_x / avg_break_z_induced are Savant arsenal columns (inches);
# they are added to the fallback chain so break data still shows.

build_pitcher_profile_ui <- function(data) {
  if (is.null(data) || !is.list(data))
    return(div(style="color:#8b949e;padding:10px;", "No pitch data."))
  
  mov <- data$movement_data   # NULL on first open; populated after CSV loads
  ars <- data$pitch_arsenal   # fast JSON endpoint — always available
  primary <- if (!is.null(mov) && is.data.frame(mov) && nrow(mov) > 0) mov else ars
  
  if (is.null(primary) || !is.data.frame(primary) || nrow(primary) == 0)
    return(div(style="color:#8b949e;padding:10px;", "No pitch data available."))
  
  velo_cards <- lapply(seq_len(nrow(primary)), function(i) {
    row   <- primary[i, ]
    ptype <- toupper(tryCatch(as.character(row[["pitch_type"]] %||% "??"), error=function(e)"??"))
    if (ptype %in% NON_PITCH_TYPES) return(NULL)
    n_p <- suppressWarnings(as.numeric(row[["n_pitches"]] %||% row[["count"]] %||% 0))
    if (!is.na(n_p) && n_p < 10) return(NULL)
    pname <- pitch_full_name(ptype)
    usage <- tryCatch({
      if ("pitch_percent" %in% names(primary))
        sprintf("%.1f%%", as.numeric(row[["pitch_percent"]]) * 100)
      else if ("count" %in% names(primary)) {
        tot <- sum(suppressWarnings(as.numeric(primary[["count"]])), na.rm=TRUE)
        if (tot > 0) sprintf("%.1f%%", as.numeric(row[["count"]]) / tot * 100) else "?%"
      } else "?%"
    }, error=function(e) "?%")
    
    velo <- tryCatch(sprintf("%.1f mph",
                             as.numeric(row[["velo"]] %||% row[["release_speed"]] %||% row[["avg_speed"]])),
                     error=function(e) "\u2014")
    
    # Break: Statcast CSV uses h_break_in / pfx_x; arsenal JSON uses avg_break_x
    # avg_break_z_induced is induced vertical break in inches (same as pfx_z*12)
    hb <- tryCatch(sprintf("%+.1f\"",
                           as.numeric(row[["h_break_in"]] %||%
                                        { v <- suppressWarnings(as.numeric(row[["pfx_x"]]));
                                        if (!is.na(v)) v * 12 else NULL } %||%
                                        row[["pitcher_break_x"]] %||%
                                        row[["avg_break_x"]])),
                   error=function(e) "\u2014")
    vb <- tryCatch(sprintf("%+.1f\"",
                           as.numeric(row[["v_break_in"]] %||%
                                        { v <- suppressWarnings(as.numeric(row[["pfx_z"]]));
                                        if (!is.na(v)) v * 12 else NULL } %||%
                                        row[["pitcher_break_z"]] %||%
                                        row[["avg_break_z_induced"]] %||%
                                        row[["avg_break_z"]])),
                   error=function(e) "\u2014")
    
    spin <- tryCatch(
      paste0(format(round(as.numeric(
        row[["spin_rate"]] %||% row[["release_spin_rate"]] %||% row[["avg_spin"]]
      )), big.mark=","), " rpm"),
      error=function(e) "\u2014")
    
    col <- pitch_col(ptype)
    div(class="pitch-char-card", style=paste0("border-left:3px solid ",col,";"),
        div(class="pch-top",
            div(class="pitch-type-badge", style=paste0("background:",col,";"), ptype),
            div(class="pch-name-usage",
                div(class="pch-name", pname), div(class="pch-usage", usage))
        ),
        div(class="pch-metrics",
            div(class="pch-metric", div(class="pch-val",velo), div(class="pch-lbl","Velocity")),
            div(class="pch-metric", div(class="pch-val",spin), div(class="pch-lbl","Spin")),
            div(class="pch-metric", div(class="pch-val",hb),   div(class="pch-lbl","H-Break")),
            div(class="pch-metric", div(class="pch-val",vb),   div(class="pch-lbl","V-Break"))
        )
    )
  })
  
  rv_sec <- NULL
  if (!is.null(ars) && is.data.frame(ars) && nrow(ars) > 0) {
    rv100_pct <- function(rv) {
      v <- suppressWarnings(as.numeric(rv))
      if (is.na(v)) return(NA_integer_)
      as.integer(max(1L, min(99L, round(50 - v * 16))))
    }
    rv_bdg <- lapply(seq_len(nrow(ars)), function(i) {
      row   <- ars[i, ]
      pt    <- toupper(as.character(row$pitch_type %||% "??"))
      rv100 <- tryCatch({
        v <- row[["run_value_per_100"]]
        if (is.null(v) || is.na(suppressWarnings(as.numeric(v)))) {
          rv <- suppressWarnings(as.numeric(row[["run_value"]]))
          ct <- suppressWarnings(as.numeric(row[["count"]]))
          if (!is.na(rv) && !is.na(ct) && ct > 0) rv/ct*100 else NA_real_
        } else as.numeric(v)
      }, error=function(e) NA_real_)
      pct    <- rv100_pct(rv100)
      rv_str <- tryCatch(sprintf("%+.2f", as.numeric(rv100)), error=function(e)"\u2014")
      col    <- pct_col(pct)
      div(class="pct-item",
          div(class="pct-badge",style=paste0("background:",col,";"),
              if (!is.null(pct)&&!is.na(pct)) as.integer(pct) else "\u2014"),
          div(class="pct-label",
              div(pt), div(style="color:#8b949e;font-size:0.53rem;margin-top:1px;", rv_str)))
    })
    rv_sec <- tagList(
      div(class="ms-section-hdr","Run Value Per 100 Pitches"),
      div(class="pct-grid", tagList(rv_bdg)))
  }
  
  splits_sec <- NULL
  vl <- data$pit_splits_vl; vr <- data$pit_splits_vr
  if (!is.null(vl) || !is.null(vr)) {
    g0 <- function(sp,f) tryCatch(as.character(sp[[f]]), error=function(e)"\u2014")
    parse_ip <- function(sp) {
      ip_str <- as.character(sp[["inningsPitched"]] %||% "")
      if (!nchar(ip_str) || is.na(ip_str)) return(NA_real_)
      parts  <- strsplit(ip_str, "\\.")[[1]]
      whole  <- suppressWarnings(as.numeric(parts[1])) %||% 0
      thirds <- if (length(parts) > 1) suppressWarnings(as.numeric(parts[2])) %||% 0 else 0
      ip <- whole + thirds / 3
      if (is.na(ip) || ip <= 0) NA_real_ else ip
    }
    compute_era <- function(sp) {
      tryCatch({
        v <- sp[["era"]] %||% sp[["earnedRunAverage"]]
        if (!is.null(v) && length(v) > 0) {
          n <- suppressWarnings(as.numeric(v))
          if (!is.na(n)) return(sprintf("%.2f", n))
        }
        ip <- parse_ip(sp); if (is.na(ip)) return("\u2014")
        er <- suppressWarnings(as.numeric(sp[["earnedRuns"]] %||% NA))
        if (is.na(er)) return("\u2014")
        sprintf("%.2f", er * 9 / ip)
      }, error = function(e) "\u2014")
    }
    compute_whip <- function(sp) {
      tryCatch({
        v <- sp[["whip"]]
        if (!is.null(v) && length(v) > 0) {
          n <- suppressWarnings(as.numeric(v))
          if (!is.na(n)) return(sprintf("%.2f", n))
        }
        ip <- parse_ip(sp); if (is.na(ip)) return("\u2014")
        bb <- suppressWarnings(as.numeric(sp[["baseOnBalls"]] %||% 0))
        h  <- suppressWarnings(as.numeric(sp[["hits"]] %||% 0))
        if (is.na(bb) || is.na(h)) return("\u2014")
        sprintf("%.2f", (bb + h) / ip)
      }, error = function(e) "\u2014")
    }
    sp_row <- function(lbl, sp) {
      if (is.null(sp)) return(NULL)
      div(class="split-row",
          div(class="split-label", lbl),
          div(class="split-stat rate", g0(sp,"avg")),
          div(class="split-stat rate", g0(sp,"obp")),
          div(class="split-stat rate", g0(sp,"slg")),
          div(class="split-stat rate", compute_era(sp)),
          div(class="split-stat rate", compute_whip(sp)),
          div(class="split-stat cnt",  g0(sp,"strikeOuts")),
          div(class="split-stat cnt",  g0(sp,"baseOnBalls")),
          div(class="split-stat cnt",  g0(sp,"atBats"))
      )
    }
    splits_sec <- tagList(
      div(class="ms-section-hdr", "L/R Splits (vs Batters)"),
      div(class="splits-tbl",
          div(class="split-row split-hdr-row",
              div(class="split-label",""),
              div(class="split-stat rate","BAA"), div(class="split-stat rate","OBP"),
              div(class="split-stat rate","SLG"), div(class="split-stat rate","ERA"),
              div(class="split-stat rate","WHIP"),div(class="split-stat cnt", "K"),
              div(class="split-stat cnt", "BB"),  div(class="split-stat cnt", "AB")
          ),
          sp_row("vs LHB", vl),
          sp_row("vs RHB", vr)
      )
    )
  }
  
  tagList(
    div(class="ms-section-hdr","Pitch Characteristics"),
    div(class="pitch-chars-grid", tagList(velo_cards)),
    rv_sec,
    splits_sec
  )
}