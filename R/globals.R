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

# Format a rate stat the baseball way: .300, 1.050 (no leading zero when < 1)
fmt_rate <- function(v) {
  if (is.null(v) || length(v) == 0 || is.na(v) || !is.finite(v)) return(".---")
  s <- sprintf("%.3f", v)
  if (v < 1) sub("^0", "", s) else s
}

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


# Pitch-type accent colours (global so movement plot + profile modal can share it)
pitch_col <- function(pt) {
  switch(toupper(as.character(pt %||% "")),
    "FF" = "#ef4444", "SI" = "#f97316", "FC" = "#f59e0b",
    "SL" = "#3b82f6", "SW" = "#60a5fa", "ST" = "#93c5fd",
    "CH" = "#22c55e", "FS" = "#14b8a6", "FO" = "#10b981",
    "CU" = "#a855f7", "KC" = "#9333ea", "CS" = "#7c3aed",
    "KN" = "#8b949e", "#6e7681"
  )
}

# Map a stat value to a 1-99 percentile using two anchor points.
# worst -> 1st pct, best -> 99th pct (works for both directions).
stat_pct <- function(val, worst, best) {
  v <- suppressWarnings(as.numeric(val))
  if (is.null(val) || length(val) == 0 || is.na(v)) return(NA_integer_)
  as.integer(max(1L, min(99L, round((v - worst) / (best - worst) * 98 + 1))))
}

# --- Schedule ---------------------------------------------------------------
# Tries multiple approaches for compatibility across baseballr versions

# Full pitch name lookup (abbreviation -> readable name)
PITCH_FULL_NAMES <- c(
  "FF" = "Four-Seam Fastball", "SI" = "Sinker",           "FC" = "Cutter",
  "SL" = "Slider",             "SW" = "Sweeper",           "ST" = "Sweeper",
  "CH" = "Changeup",           "FS" = "Split-Finger",      "FO" = "Forkball",
  "CU" = "Curveball",          "KC" = "Knuckle Curve",     "CS" = "Slow Curve",
  "KN" = "Knuckleball",        "FA" = "Fastball",          "EP" = "Eephus"
)
pitch_full_name <- function(pt) {
  n <- PITCH_FULL_NAMES[toupper(as.character(pt %||% ""))]
  if (is.na(n)) as.character(pt) else as.character(n)
}

# Pitch types that are not real pitches (exclude from profile)
NON_PITCH_TYPES <- c("PO","AB","IN","FA","EP","UN","","NA")
