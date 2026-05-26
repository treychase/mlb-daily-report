# ── Unit Tests ─────────────────────────────────────────────────────────────
# Run with: testthat::test_file("tests/test_functions.R")
# Or:       source("R/globals.R"); testthat::test_file("tests/test_functions.R")
library(testthat)

# ── Globals / helpers ────────────────────────────────────────────────────────

test_that("%||% returns left when not null/empty", {
  expect_equal("a" %||% "b",  "a")
  expect_equal(1L  %||% 0L,   1L)
  expect_equal(NULL %||% "b", "b")
  expect_equal(NA   %||% "b", "b")
  expect_equal(character(0) %||% "b", "b")
})

test_that("safe() catches errors and returns NULL", {
  expect_null(safe(stop("boom")))
  expect_equal(safe(1 + 1), 2)
})

test_that("int0() converts NULL to 0L", {
  expect_equal(int0(NULL), 0L)
  expect_equal(int0(NA),   0L)
  expect_equal(int0(3),    3L)
})

test_that("fmt_rate() formats rates correctly", {
  expect_equal(fmt_rate(0.300), ".300")
  expect_equal(fmt_rate(0.000), ".000")
  expect_equal(fmt_rate(1.050), "1.050")
  expect_equal(fmt_rate(NA),    ".---")
  expect_equal(fmt_rate(NULL),  ".---")
  expect_equal(fmt_rate(NaN),   ".---")
  expect_equal(fmt_rate(Inf),   ".---")
})

test_that("stat_pct() maps correctly to 1-99 range", {
  # ERA: worst=7.50, best=1.50 (lower is better)
  expect_equal(stat_pct(7.50, 7.50, 1.50),  1L)   # worst
  expect_equal(stat_pct(1.50, 7.50, 1.50), 99L)   # best
  expect_equal(stat_pct(4.50, 7.50, 1.50), 50L)   # midpoint
  # OPS: worst=0.545, best=1.060 (higher is better)
  expect_equal(stat_pct(0.545, 0.545, 1.060),  1L)
  expect_equal(stat_pct(1.060, 0.545, 1.060), 99L)
  # Missing values
  expect_true(is.na(stat_pct(NA, 0, 1)))
  # Clamping
  expect_equal(stat_pct(-99, 7.50, 1.50), 99L)  # better than best → 99
  expect_equal(stat_pct(999, 7.50, 1.50),  1L)  # worse than worst → 1
})

test_that("pitch_col() returns a hex color string", {
  expect_match(pitch_col("FF"), "^#[0-9a-fA-F]{6}$")
  expect_match(pitch_col("SL"), "^#[0-9a-fA-F]{6}$")
  expect_match(pitch_col("XX"), "^#[0-9a-fA-F]{6}$")  # unknown → fallback
  expect_match(pitch_col(NULL), "^#[0-9a-fA-F]{6}$")
})

test_that("pitch_full_name() returns readable names", {
  expect_equal(pitch_full_name("FF"), "Four-Seam Fastball")
  expect_equal(pitch_full_name("SL"), "Slider")
  expect_equal(pitch_full_name("CU"), "Curveball")
  expect_equal(pitch_full_name("CH"), "Changeup")
  # Unknown code falls back to the code itself
  expect_equal(pitch_full_name("ZZ"), "ZZ")
  expect_equal(pitch_full_name(NULL), "")
})

test_that("PITCH_FULL_NAMES covers all pitch_col entries", {
  pitch_col_types <- c("FF","SI","FC","SL","SW","ST","CH","FS","FO","CU","KC","CS","KN")
  missing <- setdiff(pitch_col_types, names(PITCH_FULL_NAMES))
  expect_length(missing, 0)
})

test_that("name_to_abb() maps known team names", {
  expect_equal(unname(name_to_abb("New York Yankees")),   "NYY")
  expect_equal(unname(name_to_abb("Los Angeles Dodgers")), "LAD")
  expect_equal(unname(name_to_abb("Toronto Blue Jays")),  "TOR")
  # Unknown team falls back to the input
  expect_equal(unname(name_to_abb("Fake Team")), "Fake Team")
})

# ── Data processing ───────────────────────────────────────────────────────────

test_that("empty_batting() returns correct zero-row tibble", {
  df <- empty_batting()
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
  expect_true("Name" %in% names(df))
  expect_true("OPS"  %in% names(df))
})

test_that("empty_pitching() returns correct zero-row tibble", {
  df <- empty_pitching()
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 0)
  expect_true("ERA"    %in% names(df))
  expect_true("Pitches" %in% names(df))
})

test_that("compute_monthly() returns NULL for empty input", {
  expect_null(compute_monthly(NULL))
  expect_null(compute_monthly(data.frame()))
})

test_that("compute_monthly() computes slash line from valid Statcast rows", {
  mock <- data.frame(
    player_name   = c("Trey Chase","Trey Chase","Trey Chase"),
    events        = c("single","home_run","strikeout"),
    inning_topbot = c("Top","Top","Top"),
    away_team     = c("NYY","NYY","NYY"),
    home_team     = c("BOS","BOS","BOS"),
    game_pk       = c(1L, 1L, 1L),
    at_bat_number = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )
  result <- compute_monthly(mock)
  # Should return a summarised tibble with one row per player-game
  expect_s3_class(result, "data.frame")
  expect_true("player_name" %in% names(result))
})
