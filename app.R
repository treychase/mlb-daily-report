# ═══════════════════════════════════════════════════════════════════════════════
# MLB Daily Report  |  Main Entry Point
# ═══════════════════════════════════════════════════════════════════════════════

source("R/globals.R")
source("R/data_fetch.R")
source("R/data_process.R")
source("R/ui_components.R")
source("R/plots.R")

APP_CSS <- "
@import url('https://fonts.googleapis.com/css2?family=Space+Mono:wght@400;700&family=DM+Sans:ital,opsz,wght@0,9..40,300;0,9..40,500;0,9..40,700;0,9..40,900;1,9..40,300&display=swap');

body { background:#080c10!important; color:#c9d1d9; font-family:'DM Sans',sans-serif; }
* { box-sizing:border-box; }
.container-fluid { max-width:1600px; margin:0 auto; }

.pg-hdr {
  background:linear-gradient(160deg,#080c10 0%,#0d1117 60%,#111823 100%);
  padding:24px 32px 20px; border-bottom:1px solid #1d2633; margin-bottom:28px;
  position:relative; overflow:hidden;
}
.pg-hdr::before { content:'⚾'; position:absolute; right:32px; top:50%; transform:translateY(-50%);
  font-size:6rem; opacity:.04; pointer-events:none; }
.pg-title { color:#e6edf3; font-size:1.75rem; font-weight:900; letter-spacing:-.04em;
  margin:0; line-height:1; font-family:'DM Sans',sans-serif; }
.pg-title span { color:#3b82f6; }
.pg-meta { color:#8b949e; font-size:.82rem; margin:6px 0 0; font-family:'Space Mono',monospace; }
.pg-section-title { color:#e6edf3; font-weight:800; font-size:1.05rem; margin:0 0 18px;
  display:flex; align-items:center; gap:10px; }
.pg-section-title::after { content:''; flex:1; height:1px;
  background:linear-gradient(90deg,#1d2633,transparent); }

.game-hdr { background:linear-gradient(135deg,#0d1117 0%,#111823 100%);
  border:1px solid #1d2633; border-radius:12px; padding:20px 24px; margin-bottom:20px; }
.score-row { display:flex; align-items:stretch; gap:12px; }
.score-team { flex:1; text-align:center; padding:4px 0; }
.score-team-logo { width:52px; height:52px; object-fit:contain; margin-bottom:6px; display:block; margin-left:auto; margin-right:auto; }
.team-city { color:#6e7681; font-size:.72rem; text-transform:uppercase; letter-spacing:.12em; font-family:'Space Mono',monospace; }
.team-nickname { color:#8b949e; font-size:.9rem; font-weight:700; margin:1px 0 6px; }
.score-num { font-size:3rem; font-weight:900; color:#e6edf3; line-height:1;
  font-family:'Space Mono',monospace; letter-spacing:-.04em; }
.score-mid { display:flex; flex-direction:column; align-items:center; justify-content:center;
  gap:5px; min-width:140px; padding:0 8px; border-left:1px solid #1d2633; border-right:1px solid #1d2633; }
.vs-sep   { color:#30363d; font-size:.72rem; text-transform:uppercase; letter-spacing:.12em; }
.game-time  { color:#8b949e; font-size:.78rem; font-family:'Space Mono',monospace; }
.game-venue { color:#6e7681; font-size:.7rem; text-align:center; }

.status-badge { font-size:.65rem; padding:3px 10px; border-radius:20px;
  font-weight:700; letter-spacing:.08em; font-family:'Space Mono',monospace; }
.badge-final { background:#1f4e8c; color:#79b8ff; border:1px solid #2d6cbe44; }
.badge-live  { background:#3d0a0a; color:#ff7b72; border:1px solid #da363344;
  animation:liveblink 1.6s ease-in-out infinite; }
.badge-sched { background:transparent; color:#8b949e; border:1px solid #30363d; }
@keyframes liveblink { 0%,100%{opacity:1;box-shadow:0 0 0 0 #da363340} 50%{opacity:.7;box-shadow:0 0 0 4px #da363310} }

.probable-pitchers { margin-top:5px; text-align:center; }
.pp-label { color:#6e7681; font-size:.58rem; text-transform:uppercase; letter-spacing:.12em; font-family:'Space Mono',monospace; }
.pp-names { color:#8b949e; font-size:.7rem; font-family:'Space Mono',monospace; margin-top:1px; }
.pp-sep   { color:#30363d; margin:0 3px; }

.nav-tabs { border-bottom:1px solid #1d2633; margin-bottom:0; }
.nav-tabs .nav-link { color:#6e7681; border:none; padding:7px 18px;
  font-size:.82rem; font-weight:500; border-radius:0; transition:all .15s; }
.nav-tabs .nav-link:hover  { color:#c9d1d9; background:#111823; border-radius:6px 6px 0 0; }
.nav-tabs .nav-link.active { color:#e6edf3!important; background:transparent!important;
  border-bottom:2px solid #3b82f6!important; border-radius:0!important; font-weight:700; }
.tab-content { padding-top:16px; }
#game_tabs.nav-tabs .nav-link { font-size:.8rem; padding:8px 14px; color:#8b949e; font-family:'Space Mono',monospace; }
#game_tabs.nav-tabs .nav-link.active { color:#3b82f6!important; border-bottom:2px solid #3b82f6!important; }

table.dataTable { border-collapse:collapse!important; }
table.dataTable thead th { background:#0d1117!important; color:#6e7681!important;
  font-size:.68rem; text-transform:uppercase; letter-spacing:.1em;
  border-bottom:1px solid #1d2633!important; border-top:none!important;
  padding:8px 10px; font-family:'Space Mono',monospace; font-weight:400; }
table.dataTable tbody td { color:#c9d1d9; border-color:#111823!important;
  font-size:.8rem; padding:6px 10px; font-family:'Space Mono',monospace; }
table.dataTable tbody tr:hover td { background:#111823!important; }
table.dataTable.cell-border tbody td { border-right:1px solid #111823!important; }
.dataTables_wrapper { color:#6e7681; font-size:.75rem; }
.dataTables_info { color:#6e7681; font-size:.7rem; padding-top:10px; }
[data-theme='light'] table.dataTable tbody td,
[data-theme='light'] table.dataTable thead th { --bs-table-color:#c9d1d9; --bs-table-bg:#0d1117; }

.sec-hdr { display:flex; align-items:center; gap:8px; color:#8b949e; font-size:.72rem;
  text-transform:uppercase; letter-spacing:.1em; font-family:'Space Mono',monospace;
  border-bottom:1px solid #1d2633; padding-bottom:8px; margin:0 0 10px; }
.sec-icon { font-size:.9rem; }

.top2-legend { display:flex; align-items:center; gap:8px; font-size:.72rem; color:#8b949e;
  margin:0 0 12px; font-family:'Space Mono',monospace; }
.top2-legend-dot { width:16px; height:3px; background:#FFD700; border-radius:2px; flex-shrink:0; }

.game-container { background:#0d1117; border:1px solid #1d2633; border-radius:12px; padding:24px; }

.monthly-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(360px,1fr)); gap:16px; }
.monthly-card { background:#0d1117; border:1px solid #1d2633; border-radius:12px; padding:18px; }
.monthly-card-hdr { font-size:.85rem; font-weight:800; color:#3b82f6; margin-bottom:14px;
  padding-bottom:10px; border-bottom:1px solid #1d2633; display:flex; align-items:center; gap:8px; }
.monthly-card-hdr::before { content:''; width:4px; height:14px; background:#3b82f6; border-radius:2px; }
.player-card { display:flex; align-items:center; justify-content:space-between;
  padding:10px 0; border-bottom:1px solid #111823; }
.player-card:last-child { border-bottom:none; padding-bottom:0; }
.pc-left  { display:flex; align-items:center; gap:10px; flex:1; min-width:0; }
.pc-rank  { background:linear-gradient(135deg,#1d3a6e,#1d4e89); color:#79b8ff; border-radius:50%;
  width:22px; height:22px; display:flex; align-items:center; justify-content:center;
  font-size:.65rem; font-weight:800; flex-shrink:0; font-family:'Space Mono',monospace; }
.pc-info  { min-width:0; }
.pc-name  { color:#e6edf3; font-size:.85rem; font-weight:700; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.pc-meta  { color:#6e7681; font-size:.68rem; font-family:'Space Mono',monospace; margin-top:1px; }
.pc-stats { display:flex; gap:12px; flex-shrink:0; }
.pc-stat  { text-align:center; min-width:40px; }
.ps-val   { color:#c9d1d9; font-size:.88rem; font-weight:700; font-family:'Space Mono',monospace; }
.ps-lbl   { color:#6e7681; font-size:.6rem; text-transform:uppercase; letter-spacing:.08em; margin-top:1px; }

.btn-refresh { background:transparent; border:1px solid #30363d; color:#8b949e;
  font-size:.78rem; padding:6px 14px; border-radius:6px; font-family:'Space Mono',monospace; transition:all .15s; }
.btn-refresh:hover { border-color:#3b82f6; color:#3b82f6; }

.alert-info    { background:#111d2e; border:1px solid #1d3a6e; color:#79b8ff; border-radius:8px; }
.alert-warning { background:#1c1609; border:1px solid #3d2e00; color:#e3b341; border-radius:8px; }

.modal-content { background:#0d1117!important; border:1px solid #1d2633!important; border-radius:12px!important; }
.modal-header  { border-bottom:1px solid #1d2633!important; padding:16px 20px; }
.modal-footer  { border-top:1px solid #1d2633!important; }
.modal-title   { color:#e6edf3!important; font-family:'DM Sans',sans-serif!important; font-weight:800!important; }
.btn-default,.btn-default:focus { background:transparent!important; border:1px solid #30363d!important; color:#8b949e!important; }
.btn-default:hover { border-color:#3b82f6!important; color:#3b82f6!important; }

.player-modal-hdr { display:flex; align-items:center; gap:16px; padding-bottom:14px;
  border-bottom:1px solid #1d2633; margin-bottom:14px; }
.player-headshot { width:72px; height:72px; border-radius:50%; object-fit:cover;
  border:2px solid #1d2633; flex-shrink:0; background:#111823; }
.team-logo-hdr { width:46px; height:46px; object-fit:contain; flex-shrink:0; opacity:.9; }

.ms-bio   { display:flex; align-items:center; gap:8px; flex-wrap:wrap; padding:0;
  border:none; margin:0; font-size:.82rem; }
.ms-team  { color:#3b82f6; font-weight:700; }
.ms-pos   { color:#8b949e; }
.ms-sep   { color:#30363d; }

.ms-section-hdr { color:#6e7681; font-size:.65rem; text-transform:uppercase; letter-spacing:.12em;
  font-family:'Space Mono',monospace; border-bottom:1px solid #1d2633; padding-bottom:6px; margin:14px 0 10px; }

.ms-stat-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(70px,1fr)); gap:8px; }
.ms-stat { text-align:center; padding:8px 4px; background:#111823; border:1px solid #1d2633; border-radius:6px; }
.ms-val  { color:#e6edf3; font-size:.9rem; font-weight:700; font-family:'Space Mono',monospace; }
.ms-lbl  { color:#6e7681; font-size:.58rem; text-transform:uppercase; letter-spacing:.08em; margin-top:2px; }

.slash-line { display:flex; align-items:stretch; gap:4px; margin:10px 0; background:#111823;
  border:1px solid #1d2633; border-radius:10px; padding:12px 16px; flex-wrap:wrap; }
.slash-cell { text-align:center; flex:1; min-width:56px; position:relative; }
.slash-val  { color:#e6edf3; font-size:1.3rem; font-weight:900; font-family:'Space Mono',monospace; line-height:1; }
.slash-lbl  { color:#6e7681; font-size:.58rem; text-transform:uppercase; letter-spacing:.1em; margin-top:3px; }
.slash-sep  { color:#30363d; display:flex; align-items:center; font-size:1.4rem; padding:0 2px; }
.slash-pct  { position:absolute; top:-6px; right:-4px; width:18px; height:18px; border-radius:50%;
  display:flex; align-items:center; justify-content:center; font-size:.55rem; font-weight:800;
  color:#fff; font-family:'Space Mono',monospace; border:1px solid #0d1117; }

.adj-stat-line { display:flex; gap:12px; margin:6px 0 14px; }
.adj-stat { text-align:center; background:#111823; border:1px solid #1d2633; border-radius:8px;
  padding:8px 14px; min-width:64px; }
.adj-val  { font-size:1.15rem; font-weight:900; font-family:'Space Mono',monospace; line-height:1; }
.adj-lbl  { color:#6e7681; font-size:.58rem; text-transform:uppercase; letter-spacing:.1em; margin-top:3px; }

/* ── Percentile badges: 50px circles, blue→grey→orange gradient ── */
.pct-grid  { display:flex; flex-wrap:wrap; gap:10px; margin:4px 0 6px; }
.pct-item  { display:flex; flex-direction:column; align-items:center; gap:4px; min-width:56px; }
.pct-badge { width:50px; height:50px; border-radius:50%; display:flex; align-items:center;
  justify-content:center; font-size:.85rem; font-weight:800; color:#fff;
  font-family:'Space Mono',monospace; box-shadow:0 0 0 2px #1d2633; }
.pct-label { color:#6e7681; font-size:.58rem; text-transform:uppercase; letter-spacing:.06em;
  text-align:center; line-height:1.2; }

.splits-tbl   { border:1px solid #1d2633; border-radius:8px; overflow:hidden; }
.split-row    { display:flex; border-bottom:1px solid #111823; }
.split-row:last-child { border-bottom:none; }
.split-hdr-row .split-label,.split-hdr-row .split-stat { color:#6e7681!important;
  font-size:.62rem; text-transform:uppercase; letter-spacing:.08em; background:#111823;
  font-family:'Space Mono',monospace; padding:6px 8px; }
.split-label  { color:#c9d1d9; font-size:.78rem; font-weight:700; min-width:80px;
  padding:8px 10px; font-family:'Space Mono',monospace; background:#111823; }
.split-stat   { font-size:.78rem; font-family:'Space Mono',monospace; padding:8px 8px;
  text-align:center; flex:1; color:#c9d1d9; }
.split-stat.rate { min-width:52px; }
.split-stat.cnt  { min-width:38px; color:#6e7681; }

.pitch-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(140px,1fr)); gap:10px; margin-bottom:6px; }
.pitch-card { background:#111823; border:1px solid #1d2633; border-radius:8px; padding:10px; }
.pitch-type-badge { display:inline-flex; align-items:center; justify-content:center;
  border-radius:4px; padding:2px 7px; font-size:.7rem; font-weight:800; color:#fff;
  font-family:'Space Mono',monospace; margin-bottom:4px; }
.pitch-name  { color:#e6edf3; font-size:.78rem; font-weight:700; margin-bottom:2px; }
.pitch-usage { color:#8b949e; font-size:.7rem; font-family:'Space Mono',monospace; margin-bottom:6px; }
.pitch-stats-row  { display:flex; flex-direction:column; gap:2px; }
.pitch-stat-item  { color:#6e7681; font-size:.68rem; font-family:'Space Mono',monospace; }

.pitch-chars-grid { display:grid; grid-template-columns:repeat(auto-fill,minmax(200px,1fr)); gap:10px; margin-bottom:6px; }
.pitch-char-card  { background:#111823; border:1px solid #1d2633; border-radius:8px; padding:10px 12px; }
.pch-top   { display:flex; align-items:center; gap:10px; margin-bottom:8px; }
.pch-name-usage { flex:1; min-width:0; }
.pch-name  { color:#e6edf3; font-size:.82rem; font-weight:700; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.pch-usage { color:#8b949e; font-size:.7rem; font-family:'Space Mono',monospace; margin-top:1px; }
.pch-metrics { display:grid; grid-template-columns:1fr 1fr; gap:6px; }
.pch-metric  { text-align:center; background:#0d1117; border-radius:6px; padding:5px 4px; }
.pch-val  { color:#e6edf3; font-size:.82rem; font-weight:700; font-family:'Space Mono',monospace; line-height:1; }
.pch-lbl  { color:#6e7681; font-size:.58rem; text-transform:uppercase; letter-spacing:.06em; margin-top:2px; }

.plot-wrapper { background:#0d1117; border:1px solid #1d2633; border-radius:8px; overflow:hidden; }

.lb-filters { display:flex; gap:12px; flex-wrap:wrap; margin-bottom:16px; align-items:flex-end; }
.lb-filter-group { display:flex; flex-direction:column; gap:4px; }
.lb-filter-group label { color:#6e7681; font-size:.65rem; text-transform:uppercase; letter-spacing:.1em; font-family:'Space Mono',monospace; }
.lb-filter-group select,.lb-filter-group input[type=number] {
  background:#111823; border:1px solid #1d2633; color:#c9d1d9; border-radius:6px;
  padding:5px 10px; font-size:.8rem; font-family:'Space Mono',monospace; min-width:130px; }
"

ui <- page_fluid(
  theme = bs_theme(bootswatch="darkly", primary="#3b82f6", `font-size-base`="0.875rem"),
  tags$head(tags$style(HTML(APP_CSS))),
  
  div(class="pg-hdr",
      fluidRow(
        column(9,
               div(style="display:flex;align-items:center;gap:14px;",
                   tags$img(src="https://www.mlbstatic.com/team-logos/league-on-dark/1.svg",
                            height="44px",
                            style="filter:drop-shadow(0 0 6px rgba(59,130,246,0.3));"),
                   h1(HTML("MLB <span>Daily Report</span>"), class="pg-title")
               ),
               p(paste0(format(TODAY, "%A, %B %d, %Y"),
                        "  ·  Data via MLB Stats API & Baseball Savant"), class="pg-meta")
        ),
        column(3, style="text-align:right;display:flex;align-items:center;justify-content:flex-end;",
               actionButton("refresh", "↻  Refresh", class="btn btn-refresh")
        )
      )
  ),
  
  div(style="padding:0 28px 48px;",
      navset_tab(id="app_tabs",
                 
                 nav_panel("Today", value="today",
                           div(style="padding-top:20px;",
                               div(class="pg-section-title", "Today's Games"),
                               uiOutput("games_ui"),
                               div(style="margin-top:44px;"),
                               div(class="pg-section-title",
                                   paste0("📊  ", MONTH_LABEL, " Monthly Leaders")),
                               p(paste0("Top 2 hitters per team in today's games · min ", MIN_AB,
                                        " AB · ranked by OPS"),
                                 style="color:#6e7681;font-size:.78rem;font-family:'Space Mono',monospace;margin:-12px 0 20px;"),
                               uiOutput("monthly_ui")
                           )
                 ),
                 
                 nav_panel("Leaderboard", value="leaderboard",
                           div(style="padding-top:20px;",
                               tabsetPanel(type="tabs",
                                           tabPanel("🏏  Hitters",
                                                    div(style="margin-top:16px;"),
                                                    div(class="lb-filters",
                                                        div(class="lb-filter-group",
                                                            tags$label("Team"),
                                                            selectInput("lb_hit_team","", choices=c("All Teams"="ALL"), width="140px")),
                                                        div(class="lb-filter-group",
                                                            tags$label("Position"),
                                                            selectInput("lb_hit_pos","", choices=c("All Positions"="ALL"), width="140px")),
                                                        div(class="lb-filter-group",
                                                            tags$label("Min PA"),
                                                            numericInput("lb_hit_pa","", value=50, min=0, step=10, width="90px"))
                                                    ),
                                                    DT::DTOutput("hitter_lb_dt")
                                           ),
                                           tabPanel("⚾  Pitchers",
                                                    div(style="margin-top:16px;"),
                                                    div(class="lb-filters",
                                                        div(class="lb-filter-group",
                                                            tags$label("Team"),
                                                            selectInput("lb_pit_team","", choices=c("All Teams"="ALL"), width="140px")),
                                                        div(class="lb-filter-group",
                                                            tags$label("Role"),
                                                            selectInput("lb_pit_role","",
                                                                        choices=c("All"="ALL","Starter"="SP","Reliever"="RP"), width="120px")),
                                                        div(class="lb-filter-group",
                                                            tags$label("Min IP"),
                                                            numericInput("lb_pit_ip","", value=10, min=0, step=5, width="90px"))
                                                    ),
                                                    DT::DTOutput("pitcher_lb_dt")
                                           )
                               )
                           )
                 )
      )
  )
)

server <- function(input, output, session) {
  
  later::later(function() {
    if (is.null(.war_cache$batters)) {
      message("Pre-warming hitter WAR cache...")
      fetch_lb_batters()
    }
    if (is.null(.war_cache$pitchers)) {
      message("Pre-warming pitcher WAR cache...")
      fetch_lb_pitchers()
    }
  }, delay = 0.1)
  
  sched_r <- reactive({
    input$refresh
    withProgress(message="Fetching today's schedule…", value=0.3, {
      s <- fetch_schedule()
      incProgress(0.7)
      if (!is.null(s) && nrow(s) > 0 &&
          any(grepl("Progress|Live", s$status_detailed_state, ignore.case=TRUE), na.rm=TRUE))
        invalidateLater(60000)
      s
    })
  })
  
  games_r <- reactive({
    s <- sched_r(); req(s, nrow(s) > 0)
    s %>% filter(game_type == "R") %>% distinct(game_pk, .keep_all=TRUE) %>% arrange(game_datetime)
  })
  
  boxscores_r <- reactive({
    gms <- games_r(); n <- nrow(gms)
    withProgress(message="Loading box scores…", value=0, {
      lapply(seq_len(n), function(i) {
        incProgress(1/n, detail=sprintf("Game %d of %d", i, n))
        fetch_boxscore(gms$game_pk[i])
      })
    })
  })
  
  monthly_r <- reactive({
    input$refresh
    withProgress(message=paste("Loading", MONTH_LABEL, "Statcast…"), value=0.1, {
      sc <- fetch_monthly_sc(); incProgress(0.9); compute_monthly(sc)
    })
  })
  
  lb_bat_r <- reactive({ fetch_lb_batters() })
  lb_pit_r <- reactive({ fetch_lb_pitchers() })
  
  bref_col <- function(df, ...) {
    col <- intersect(c(...), names(df))[1]
    if (is.na(col)) return(character(0))
    v <- trimws(as.character(df[[col]]))
    sort(unique(v[!is.na(v) & nchar(v) > 0 & v != "NA"]))
  }
  
  observeEvent(input$app_tabs, {
    req(input$app_tabs == "leaderboard")
    df <- lb_bat_r()
    if (!is.null(df) && is.data.frame(df)) {
      team_col <- intersect(c("Team","Tm","team","tm"), names(df))[1]
      teams <- if (!is.na(team_col)) {
        all_t <- as.character(df[[team_col]])
        parts <- unlist(strsplit(paste(all_t, collapse=","), "[,/|]"))
        sort(unique(trimws(parts[!is.na(parts) & nchar(trimws(parts))>0])))
      } else character(0)
      pos_col <- intersect(c("Pos","pos","position","pos_","Pos.","Pos Summary","POS","Position"), names(df))[1]
      pos_v <- if (!is.na(pos_col)) {
        raw   <- as.character(df[[pos_col]])
        parts <- unlist(strsplit(paste(raw, collapse=","), "[,/|]"))
        parts <- trimws(parts)
        parts <- parts[nchar(parts) > 0 & parts != "NA" & parts != "P"]
        sort(unique(parts))
      } else character(0)
      updateSelectInput(session,"lb_hit_team", choices=c("All Teams"="ALL", setNames(teams,teams)))
      updateSelectInput(session,"lb_hit_pos",  choices=c("All Positions"="ALL", setNames(pos_v,pos_v)))
    }
    df2 <- lb_pit_r()
    if (!is.null(df2) && is.data.frame(df2)) {
      pt_col <- intersect(c("Team","Tm","team","tm"), names(df2))[1]
      teams2 <- if (!is.na(pt_col)) {
        all_t <- as.character(df2[[pt_col]])
        parts <- unlist(strsplit(paste(all_t, collapse=","), "[,/|]"))
        sort(unique(trimws(parts[!is.na(parts) & nchar(trimws(parts))>0])))
      } else character(0)
      updateSelectInput(session,"lb_pit_team", choices=c("All Teams"="ALL", setNames(teams2,teams2)))
    }
  }, ignoreInit=TRUE)
  
  output$hitter_lb_dt <- DT::renderDT({
    req(input$app_tabs == "leaderboard")
    df <- lb_bat_r(); req(!is.null(df), is.data.frame(df))
    
    tf <- input$lb_hit_team %||% "ALL"
    pf <- input$lb_hit_pos  %||% "ALL"
    mp <- max(0, input$lb_hit_pa %||% 50)
    
    team_col <- intersect(c("Team","Tm","team","tm"), names(df))[1]
    if (!is.na(team_col) && tf != "ALL") {
      keep_rows <- vapply(as.character(df[[team_col]]), function(x) {
        parts <- trimws(strsplit(x %||% "","[,/|]")[[1]]); tf %in% parts
      }, logical(1))
      df <- df[keep_rows, ]
    }
    pos_col <- intersect(c("pos","Pos","position","pos_","Pos.","Pos Summary","POS","Position"), names(df))[1]
    if (!is.na(pos_col) && pf != "ALL")
      df <- df[grepl(pf, as.character(df[[pos_col]] %||% ""), fixed=TRUE), ]
    pa_col <- intersect(c("PA","pa"), names(df))[1]
    if (!is.na(pa_col))
      df <- df[suppressWarnings(as.numeric(df[[pa_col]])) >= mp, ]
    
    if (!is.na(team_col)) {
      df[[team_col]] <- vapply(as.character(df[[team_col]]), function(x) {
        parts <- trimws(strsplit(x %||% "","[,/|]")[[1]]); parts <- parts[nchar(parts)>0]
        if (length(parts)==0) NA_character_ else parts[length(parts)]
      }, character(1))
    }
    
    keep <- intersect(c("Name","Team","Tm","pos","Pos","G","PA","AB","HR","RBI","SB",
                        "BA","AVG","OBP","SLG","OPS","OPS_plus","OPS+","WAR","bWAR"), names(df))
    if (length(keep)==0) keep <- names(df)[seq_len(min(12,ncol(df)))]
    has_pid <- "mlb_id" %in% names(df)
    if (has_pid) keep <- c(keep, "mlb_id")
    df <- df[, keep, drop=FALSE]
    names(df) <- sub("OPS_plus","OPS+", sub("^pos$","Pos", sub("^Tm$","Team", names(df))))
    
    war_idx <- which(names(df)=="WAR") - 1L
    ops_idx <- which(names(df)=="OPS+") - 1L
    pid_idx <- if (has_pid) which(names(df)=="mlb_id") - 1L else integer(0)
    
    row_cb <- if (has_pid) DT::JS(sprintf(
      "function(row, data) {
        var pid = data[%d];
        if (pid && pid !== 'NA' && pid !== '' && pid !== 'null') {
          $(row).css('cursor','pointer').on('click', function() {
            Shiny.setInputValue('player_click',{id:parseInt(pid,10),name:data[0]},{priority:'event'});
          });
        }
      }", pid_idx)) else NULL
    
    dt_opts <- list(dom="t", pageLength=100, scrollX=TRUE,
                    order=if(length(war_idx)>0) list(list(war_idx,"desc")) else list())
    if (length(pid_idx)>0) dt_opts$columnDefs <- list(list(visible=FALSE, targets=pid_idx))
    if (!is.null(row_cb)) dt_opts$rowCallback <- row_cb
    
    dt <- DT::datatable(df, rownames=FALSE, class="table-dark compact cell-border", options=dt_opts)
    if (length(ops_idx)>0) dt <- dt %>% DT::formatStyle("OPS+", color="#60a5fa", fontWeight="bold")
    if (length(war_idx)>0) {
      dt <- dt %>%
        DT::formatRound("WAR", digits=2) %>%
        DT::formatStyle("WAR", color=DT::styleInterval(0,c("#f87171","#4ade80")), fontWeight="bold")
    }
    dt
  }, server=FALSE)
  
  output$pitcher_lb_dt <- DT::renderDT({
    req(input$app_tabs == "leaderboard")
    df <- lb_pit_r(); req(!is.null(df), is.data.frame(df))
    
    tf  <- input$lb_pit_team %||% "ALL"
    mip <- max(0, input$lb_pit_ip %||% 10)
    
    team_col <- intersect(c("Team","Tm","team","tm"), names(df))[1]
    if (!is.na(team_col) && tf != "ALL") {
      keep_rows <- vapply(as.character(df[[team_col]]), function(x) {
        parts <- trimws(strsplit(x %||% "","[,/|]")[[1]]); tf %in% parts
      }, logical(1))
      df <- df[keep_rows, ]
    }
    ip_col <- intersect(c("IP","ip"), names(df))[1]
    if (!is.na(ip_col))
      df <- df[suppressWarnings(as.numeric(df[[ip_col]])) >= mip, ]
    
    if (!is.na(team_col)) {
      df[[team_col]] <- vapply(as.character(df[[team_col]]), function(x) {
        parts <- trimws(strsplit(x %||% "","[,/|]")[[1]]); parts <- parts[nchar(parts)>0]
        if (length(parts)==0) NA_character_ else parts[length(parts)]
      }, character(1))
    }
    
    keep <- intersect(c("Name","Team","Tm","G","GS","W","L","SV","IP",
                        "ERA","WHIP","SO","BB","HR","WAR","bWAR"), names(df))
    if (length(keep)==0) keep <- names(df)[seq_len(min(12,ncol(df)))]
    has_pid <- "mlb_id" %in% names(df)
    if (has_pid) keep <- c(keep, "mlb_id")
    df <- df[, keep, drop=FALSE]
    names(df) <- sub("^Tm$","Team", names(df))
    
    war_idx <- which(names(df)=="WAR") - 1L
    pid_idx <- if (has_pid) which(names(df)=="mlb_id") - 1L else integer(0)
    
    row_cb <- if (has_pid) DT::JS(sprintf(
      "function(row, data) {
        var pid = data[%d];
        if (pid && pid !== 'NA' && pid !== '' && pid !== 'null') {
          $(row).css('cursor','pointer').on('click', function() {
            Shiny.setInputValue('pitcher_profile_click',{id:parseInt(pid,10),name:data[0]},{priority:'event'});
          });
        }
      }", pid_idx)) else NULL
    
    dt_opts <- list(dom="t", pageLength=100, scrollX=TRUE,
                    order=if(length(war_idx)>0) list(list(war_idx,"desc")) else list())
    if (length(pid_idx)>0) dt_opts$columnDefs <- list(list(visible=FALSE, targets=pid_idx))
    if (!is.null(row_cb)) dt_opts$rowCallback <- row_cb
    
    dt <- DT::datatable(df, rownames=FALSE, class="table-dark compact cell-border", options=dt_opts)
    if (length(war_idx)>0) {
      dt <- dt %>%
        DT::formatRound("WAR", digits=2) %>%
        DT::formatStyle("WAR", color=DT::styleInterval(0,c("#f87171","#4ade80")), fontWeight="bold")
    }
    dt
  }, server=FALSE)
  
  observe({
    gms <- games_r(); bxs <- boxscores_r()
    lapply(seq_len(nrow(gms)), function(i) {
      local({
        ii <- i; bs <- bxs[[ii]]; g <- gms[ii,]
        away <- g$teams_away_team_name %||% "Away"
        home <- g$teams_home_team_name %||% "Home"
        away_bat <- parse_batting(bs,"away");  home_bat <- parse_batting(bs,"home")
        away_pit <- parse_pitching(bs,"away"); home_pit <- parse_pitching(bs,"home")
        away_t2  <- top2_by_ops(away_bat);    home_t2  <- top2_by_ops(home_bat)
        output[[paste0("away_bat_",ii)]] <- DT::renderDT(make_dt(away_bat,away_t2), server=FALSE)
        output[[paste0("home_bat_",ii)]] <- DT::renderDT(make_dt(home_bat,home_t2), server=FALSE)
        output[[paste0("away_pit_",ii)]] <- DT::renderDT(make_dt(away_pit,kind="pitching"), server=FALSE)
        output[[paste0("home_pit_",ii)]] <- DT::renderDT(make_dt(home_pit,kind="pitching"), server=FALSE)
      })
    })
  })
  
  output$games_ui <- renderUI({
    s <- sched_r()
    if (is.null(s)) return(div(class="alert alert-warning","⚠ Could not load today's schedule."))
    reg <- tryCatch(s %>% filter(game_type=="R") %>% distinct(game_pk,.keep_all=TRUE), error=function(e) NULL)
    if (is.null(reg) || nrow(reg)==0) return(div(class="alert alert-info","ℹ No regular-season games today."))
    
    gms <- games_r(); bxs <- boxscores_r()
    panels <- lapply(seq_len(nrow(gms)), function(i) {
      g     <- gms[i,]
      away  <- g$teams_away_team_name %||% "Away"
      home  <- g$teams_home_team_name %||% "Home"
      as_   <- tryCatch(g$teams_away_score, error=function(e) NA)
      hs_   <- tryCatch(g$teams_home_score, error=function(e) NA)
      state <- tryCatch(as.character(g$status_detailed_state), error=function(e) "Scheduled") %||% "Scheduled"
      venue <- tryCatch(as.character(g$venue_name), error=function(e) NA) %||% NA_character_
      away_id <- tryCatch(as.integer(g$teams_away_team_id), error=function(e) NA_integer_)
      home_id <- tryCatch(as.integer(g$teams_home_team_id), error=function(e) NA_integer_)
      away_prob <- tryCatch({v<-g[["teams_away_probable"]]; if(is.null(v)||is.na(v)) NA_character_ else fmt_prob(v)}, error=function(e) NA_character_)
      home_prob <- tryCatch({v<-g[["teams_home_probable"]]; if(is.null(v)||is.na(v)) NA_character_ else fmt_prob(v)}, error=function(e) NA_character_)
      gtime <- tryCatch({
        dt <- g$game_datetime; if(is.na(dt)) return("")
        format(lubridate::with_tz(lubridate::ymd_hms(dt),"America/New_York"),"%I:%M %p ET")
      }, error=function(e) "")
      tabPanel(sprintf("%s @ %s", away, home), value=paste0("gtab_",i),
               div(class="game-container",
                   score_display(away,home,as_,hs_,state,gtime,venue,away_prob,home_prob,away_id,home_id),
                   div(class="top2-legend", div(class="top2-legend-dot"), "Gold rows = Top 2 hitters by today's OPS"),
                   tabsetPanel(type="tabs",
                               tabPanel("🏏  Batting",
                                        fluidRow(
                                          column(6,sec_hdr("▶",paste(away,"Batters")),DT::DTOutput(paste0("away_bat_",i))),
                                          column(6,sec_hdr("▶",paste(home,"Batters")),DT::DTOutput(paste0("home_bat_",i)))
                                        )),
                               tabPanel("⚾  Pitching",
                                        fluidRow(
                                          column(6,sec_hdr("▶",paste(away,"Pitchers")),DT::DTOutput(paste0("away_pit_",i))),
                                          column(6,sec_hdr("▶",paste(home,"Pitchers")),DT::DTOutput(paste0("home_pit_",i)))
                                        ))
                   )
               )
      )
    })
    do.call(tabsetPanel, c(list(type="tabs", id="game_tabs"), panels))
  })
  
  output$monthly_ui <- renderUI({
    mo <- monthly_r(); s <- sched_r()
    if (is.null(s)||nrow(s)==0) return(div(class="alert alert-info","ℹ Load today's schedule first."))
    if (is.null(mo)) return(div(class="alert alert-warning","⚠ Statcast data unavailable — try again shortly."))
    
    teams_full <- sort(unique(c(s$teams_away_team_name, s$teams_home_team_name)))
    teams_full <- teams_full[!is.na(teams_full)]
    teams_abb  <- name_to_abb(teams_full)
    
    cards <- compact(lapply(seq_along(teams_full), function(ti) {
      top2 <- mo %>% filter(batter_team==teams_abb[ti]) %>% slice_max(OPS,n=2,with_ties=FALSE)
      if (nrow(top2)==0) return(NULL)
      pcards <- lapply(seq_len(nrow(top2)), function(j) {
        r <- top2[j,]; war_v <- NA_real_
        wc <- .war_cache$batters
        if (!is.null(wc)&&is.data.frame(wc)&&"Name"%in%names(wc)&&"WAR"%in%names(wc)) {
          nm <- tolower(as.character(r$player_name))
          wrow <- wc[tolower(as.character(wc$Name))==nm,]
          if (nrow(wrow)>0) war_v <- suppressWarnings(as.numeric(wrow$WAR[1]))
        }
        player_stat_card(j,r$player_name,r$G,r$AB,r$AVG,r$OBP,r$SLG,r$OPS,war_v)
      })
      div(class="monthly-card", div(class="monthly-card-hdr",teams_full[ti]), tagList(pcards))
    }))
    
    if (length(cards)==0) return(div(class="alert alert-warning",
                                     paste0("⚠ No qualified hitters (min ",MIN_AB," AB). Try later in the month.")))
    div(class="monthly-grid", tagList(cards))
  })
  
  # ── Hitter/pitcher stats modal ────────────────────────────────────────────
  output$player_modal_content <- renderUI({
    req(input$player_click)
    data <- fetch_player_season_stats(as.integer(input$player_click$id))
    build_player_modal_ui(data)
  })
  
  observeEvent(input$player_click, {
    req(input$player_click)
    showModal(modalDialog(
      title=div(style="color:#e6edf3;font-family:'DM Sans',sans-serif;font-weight:800;font-size:1.1rem;",
                input$player_click$name),
      size="l", easyClose=TRUE, footer=modalButton("Close"),
      uiOutput("player_modal_content")
    ))
  })
  
  # ── Pitcher profile modal ─────────────────────────────────────────────────
  # ── Movement scatter: calls fetch_pitcher_movement directly (memoised CSV) ─
  output$pitcher_movement_plot <- renderPlot({
    req(input$pitcher_profile_click)
    mov_full <- fetch_pitcher_movement(as.integer(input$pitcher_profile_click$id))
    create_movement_plot(if (is.null(mov_full)) NULL else mov_full$summary)
  }, bg="#0d1117")
  
  # ── Heatmap: also calls fetch_pitcher_movement directly ──────────────────
  # movement_raw was removed from fetch_player_season_stats to avoid blocking
  # the modal open; we call fetch_pitcher_movement here instead.  Since both
  # this output and pitcher_movement_plot call the same memoised function,
  # only one network request is ever made regardless of which renders first.
  output$pitcher_heatmap <- renderPlot({
    req(input$pitcher_profile_click, input$hm_tab)
    pid      <- as.integer(input$pitcher_profile_click$id)
    mov_full <- fetch_pitcher_movement(pid)
    create_strike_zone_heatmap(
      if (!is.null(mov_full)) mov_full$raw_pitches else NULL,
      input$hm_tab
    )
  }, bg="#0d1117")
  
  # ── Modal shell opens instantly; content streams in via renderUI ──────────
  observeEvent(input$pitcher_profile_click, {
    req(input$pitcher_profile_click)
    name <- input$pitcher_profile_click$name %||% "Pitcher"
    showModal(modalDialog(
      title=div(style="color:#e6edf3;font-family:'DM Sans',sans-serif;font-weight:800;font-size:1.1rem;",
                HTML(paste0("\u26be\ufe0f\u2002", name))),
      size="l", easyClose=TRUE, footer=modalButton("Close"),
      uiOutput("pitcher_modal_full_ui")
    ))
  })
  
  output$pitcher_modal_full_ui <- renderUI({
    req(input$pitcher_profile_click)
    pid  <- as.integer(input$pitcher_profile_click$id)
    data <- fetch_player_season_stats(pid)
    
    # ── Heatmap tabs: use pitch_arsenal pitch types (fast JSON, no CSV needed)
    # Previously used data$movement_data which was NULL until the CSV loaded.
    ars <- data$pitch_arsenal
    pts <- if (!is.null(ars) && is.data.frame(ars) && nrow(ars) > 0)
      as.character(ars$pitch_type) else character(0)
    pts <- pts[!is.na(pts) & !pts %in% NON_PITCH_TYPES & nchar(pts) > 0]
    # Order by usage (most-thrown first)
    if (length(pts) > 0 && "pitch_percent" %in% names(ars)) {
      ord <- order(-suppressWarnings(as.numeric(ars$pitch_percent[match(pts, ars$pitch_type)])))
      pts <- pts[ord]
    }
    
    heatmap_section <- if (length(pts) > 0) {
      tab_panels <- lapply(pts, function(pt) tabPanel(title=pitch_full_name(pt), value=pt))
      tabset <- do.call(tabsetPanel, c(list(id="hm_tab", type="tabs", selected=pts[1]), tab_panels))
      tagList(
        div(class="ms-section-hdr","Pitch Location Heatmap"),
        tabset,
        div(class="plot-wrapper", style="margin-top:8px;",
            plotOutput("pitcher_heatmap", height="340px"))
      )
    } else NULL
    
    tagList(
      build_player_modal_ui(data),
      div(style="margin:14px 0 6px;",
          div(class="ms-section-hdr","Pitch Movement"),
          div(class="plot-wrapper", plotOutput("pitcher_movement_plot", height="320px"))
      ),
      heatmap_section,
      build_pitcher_profile_ui(data)
    )
  })
}

shinyApp(ui, server)