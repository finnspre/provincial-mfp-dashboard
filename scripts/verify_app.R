# One-off headless verification script -- exercises server-side reactive
# logic via shiny::testServer so chart/table rendering errors surface
# without needing a browser. Not part of the app itself.
#
# Uses a small synthetic mfp_data fixture already in the FINAL post-pipeline
# shape (Year/Variable/Industry/IndustryLevel/Value/UOM), since load_mfp_data()
# is now a thin loader -- not a real StatCan pull, so this runs hermetically
# with no network/cansim dependency.
#
# Four free-standing sections below, each with its own fixture and its own
# fresh sys.source()'d copy of app.R (module-private reactives -- active_pairs(),
# filtered_data(), ranking_data(), etc. -- aren't reachable from outside their
# moduleServer(), so every check here drives fully id-namespaced
# session$setInputs() keys and reads only output[["<id>-<name>"]] or
# top-level helpers, the same way a real browser session could): the main
# interactive-flow checks, the UX-state (loading/empty/error communication)
# checks, the data-contract checks (validate_data_contract()/MFP_DATA_CONTRACT
# -- see data_contract.R), and the safe_load_mfp_data() recovery check.

suppressMessages({
  library(shiny)
})

close_enough <- function(a, b, tol = 1e-3) abs(a - b) < tol

# Shared by every "does this render, or does it show a friendly message
# instead of quietly going blank" check below -- validate()'s condition
# class, confirmed against shiny's own source, is exactly these two.
expect_validation_error <- function(res, label) {
  stopifnot(inherits(res, "shiny.silent.error"), inherits(res, "validation"))
  cat(label, ": caught a catchable validation condition OK, message: ", conditionMessage(res), "\n", sep = "")
}

# A single valid mfp_data row-shape, reused by every data-contract check
# below as the known-good baseline that gets deliberately broken one way at
# a time. Geography = "Ontario" -- matches app.R's DEFAULT_GEOGRAPHY, so
# every existing numeric expectation elsewhere in this file (all seeded
# against Ontario-valued fixture rows) keeps working unchanged now that
# table 36-10-0211-01 carries a real province dimension (see data_pipeline.R).
good_row <- function() {
  data.frame(
    Year = 2021:2023, Geography = "Ontario", Variable = "Multifactor productivity",
    Industry = "Business sector industries", IndustryLevel = "Aggregate", Value = c(100, 101, 102),
    UOM = "Index, 2017=100", stringsAsFactors = FALSE
  )
}

fixture_path <- tempfile(fileext = ".RData")

# Business sector industries (Aggregate): 100/101/102 over 2021-2023 -- the
# default geography/variable/industry combination.
# Manufacturing (2-digit): 95/96/97 over 2021-2023 -- a second, comparable
# industry so "compare multiple industries" has a second full-history series.
# Retail trade (2-digit): 60/62/64 over 2021-2023 -- a third same-level
# industry so the Aggregate vs. 2-digit cumulative-level test has something
# to add.
# Construction (2-digit): 50/52 over 2022-2023 only -- exercises "series
# starts later than the rest of the panel" for the rebase/ranking edge cases.
# geography defaults to "Ontario" (matching DEFAULT_GEOGRAPHY) so every
# existing call site below -- none of which pass a geography of their own --
# keeps building the same fixture data it always has.
series_block <- function(industry, level, years, values, geography = "Ontario") {
  data.frame(
    Year = years, Geography = geography, Variable = "Multifactor productivity",
    Industry = industry, IndustryLevel = level, Value = values,
    UOM = "Index, 2017=100", stringsAsFactors = FALSE
  )
}

make_fixture <- function(sector_value = 100.0, manufacturing_value = 95.0,
                          retail_value = 60.0, extra_year = FALSE) {
  mfp_data <- rbind(
    series_block("Business sector industries", "Aggregate", 2021:2023, sector_value + 0:2),
    series_block("Manufacturing", "2-digit", 2021:2023, manufacturing_value + 0:2),
    series_block("Retail trade", "2-digit", 2021:2023, retail_value + c(0, 2, 4)),
    series_block("Construction", "2-digit", 2022:2023, c(50.0, 52.0)),
    # A second province, deliberately different values throughout, purely so
    # the Geography-filter check below (search "geography filter") has real
    # fixture data to prove switching input$geography actually changes what
    # a tab shows/exports, not just that the picker exists.
    series_block("Business sector industries", "Aggregate", 2021:2023, c(200, 202, 204), geography = "Quebec"),
    series_block("Manufacturing", "2-digit", 2021:2023, c(150, 153, 156), geography = "Quebec")
  )
  if (extra_year) {
    mfp_data <- rbind(mfp_data, series_block("Business sector industries", "Aggregate", 2024L, sector_value + 3))
  }
  save(mfp_data, file = fixture_path)
}

make_fixture()

# MFP_DATA_FILE and RAW_DATA_POLL_MS are read once, at the moment app.R is
# sourced (RAW_DATA_READER is created then) -- so both must be set first.
Sys.setenv(MFP_DATA_FILE = fixture_path)
Sys.setenv(RAW_DATA_POLL_MS = "200")

# Run from the project root: Rscript scripts/verify_app.R
env <- new.env()
sys.source("app.R", envir = env)

cat("== Default industry is a valid, selectable choice at the default level ==\n")
loaded <- env$load_mfp_data(fixture_path)
aggregate_choices <- env$series_choices(
  loaded[loaded$IndustryLevel %in% env$industry_levels_upto(env$DEFAULT_INDUSTRY_LEVEL), ],
  "Industry", env$DEFAULT_INDUSTRY
)
stopifnot(env$DEFAULT_INDUSTRY %in% aggregate_choices)
cat("DEFAULT_INDUSTRY is present among Aggregate-level industry choices\n")

# == Interactive-flow checks (Trends/Rankings/Compare/Data) =================
# Drives every tab through shiny::testServer against one shared session, the
# same way a user would navigate between tabs in one browser session (each
# module keeps fully independent state via its own namespace, so there's no
# cross-tab leakage to worry about). Reads results back via each tab's own
# CSV export (output[["<id>-download_csv"]] resolves, in testServer, to the
# path of the file its content() function actually wrote -- confirmed
# empirically) rather than any module-private reactive, since the latter
# aren't reachable from outside moduleServer() at all.
shiny::testServer(env$server, {
  cat("== Rendering with the default variable/industry (Trends) ==\n")
  # Trends has no ui() here to supply its selected= defaults (testServer
  # exercises server() alone) -- seed the same values a real page load
  # would have sent.
  session$setInputs(
    `trend-geography` = "Ontario",
    `trend-variable` = "Multifactor productivity", `trend-industry` = env$DEFAULT_INDUSTRY,
    `trend-year_range` = c(2021, 2023)
  )
  session$flushReact()
  stopifnot(!is.null(output[["trend-chart"]]))
  stopifnot(nrow(read.csv(output[["trend-download_csv"]])) > 0)
  cat("Trends renders + exports with the default single-series selection OK\n")

  cat("== Trends: its own (separate from Compare's) rebase/growth implementation ==\n")
  session$setInputs(`trend-industry` = "Manufacturing")
  session$flushReact()
  trend_export <- read.csv(output[["trend-download_csv"]], check.names = FALSE)
  stopifnot("GrowthPct" %in% names(trend_export))
  session$setInputs(`trend-rebase_toggle` = TRUE, `trend-base_year` = 2022)
  session$flushReact()
  trend_rebased <- read.csv(output[["trend-download_csv"]], check.names = FALSE)
  stopifnot(close_enough(trend_rebased$RebasedValue[trend_rebased$Year == 2022], 100))
  stopifnot(close_enough(trend_rebased$RebasedValue[trend_rebased$Year == 2021], 95 / 96 * 100))
  session$setInputs(`trend-rebase_toggle` = FALSE)
  session$flushReact()
  cat("Trends' own rebase math matches Compare's shared formula OK\n")

  cat("== Compare/Data start with the default series already active -- no Add-series click needed ==\n")
  # active_pairs() seeds itself from default_pair_row() unconditionally (see
  # tab_module_server()) -- unlike Trends' bare inputs above, this doesn't
  # depend on ui() ever having run.
  session$setInputs(
    `bar-geography` = "Ontario",
    `bar-variable` = "Multifactor productivity", `bar-year_range` = c(2021, 2023)
  )
  session$flushReact()
  stopifnot(!is.null(output[["bar-chart"]]))
  default_csv <- read.csv(output[["bar-download_csv"]])
  stopifnot(setequal(unique(default_csv$Industry), env$DEFAULT_INDUSTRY))
  cat("Compare renders + exports only the default series (Business sector industries) OK\n")

  cat("== Industry detail is cumulative, not exact-tier ==\n")
  # A pure check of industry_levels_upto()/series_choices() against the
  # shared raw_data() -- doesn't need any module input (only Rankings has an
  # industry_level toggle at all; it's exercised with its own namespaced
  # input further down). Filtered to Ontario first -- raw_data() now spans
  # 2 provinces (see make_fixture()), and Quebec's fixture rows only cover
  # Business sector industries/Manufacturing, not Retail trade/Construction,
  # so an unfiltered industry_levels_upto() check would still pass here but
  # would no longer be testing the same thing this check is meant to (the
  # Aggregate/2-digit cumulative relationship for one consistent province).
  ontario_raw <- raw_data()[raw_data()$Geography == "Ontario", ]
  aggregate_choices2 <- env$series_choices(
    ontario_raw[ontario_raw$IndustryLevel %in% env$industry_levels_upto("Aggregate"), ], "Industry"
  )
  stopifnot(identical(aggregate_choices2, "Business sector industries"))
  two_digit_choices <- env$series_choices(
    ontario_raw[ontario_raw$IndustryLevel %in% env$industry_levels_upto("2-digit"), ], "Industry"
  )
  stopifnot(all(c("Business sector industries", "Manufacturing", "Retail trade", "Construction") %in% two_digit_choices))
  cat("2-digit adds to Aggregate OK\n")

  cat("== Adding a duplicate series is a no-op; remove_pair/clear_pairs implement removal ==\n")
  pair_click_count <- 0
  add_bar_pair <- function(industry) {
    pair_click_count <<- pair_click_count + 1
    session$setInputs(`bar-pair_industry` = industry)
    session$flushReact()
    session$setInputs(`bar-add_pair` = pair_click_count)
    session$flushReact()
  }
  clear_click_count <- 0
  clear_bar_pairs <- function() {
    clear_click_count <<- clear_click_count + 1
    session$setInputs(`bar-clear_pairs` = clear_click_count)
    session$flushReact()
  }
  # Distinct Industries currently active, read off the export rather than
  # any module-private reactive -- only ever called after at least one
  # add_bar_pair(), never right after a bare clear_bar_pairs() (with
  # nothing added yet the export legitimately has nothing to build, which
  # req() -- not validate() -- guards; see history_with_growth()).
  bar_industries <- function() unique(read.csv(output[["bar-download_csv"]])$Industry)

  clear_bar_pairs()
  add_bar_pair("Manufacturing")
  add_bar_pair("Manufacturing") # duplicate -- should not add a second row
  stopifnot(length(bar_industries()) == 1)
  add_bar_pair("Retail trade")
  stopifnot(length(bar_industries()) == 2)
  session$setInputs(`bar-remove_pair` = "Manufacturing")
  session$flushReact()
  remaining <- bar_industries()
  stopifnot(length(remaining) == 1, remaining == "Retail trade")
  cat("duplicate series are de-duplicated; remove_pair removes a specific series OK\n")

  cat("== Comparing multiple industries ==\n")
  clear_bar_pairs()
  add_bar_pair("Manufacturing")
  add_bar_pair("Retail trade")
  stopifnot(!is.null(output[["bar-chart"]]))
  stopifnot(setequal(bar_industries(), c("Manufacturing", "Retail trade")))
  cat("multiple industries render onto one chart OK\n")

  cat("== Narrowing the time frame slider (and a single-period selection) ==\n")
  session$setInputs(`bar-year_range` = c(2022, 2023))
  session$flushReact()
  narrowed <- read.csv(output[["bar-download_csv"]])
  stopifnot(all(narrowed$Year >= 2022), !any(narrowed$Year == 2021))
  stopifnot(!is.null(output[["bar-chart"]]))
  # A single-period window (min == max) -- exactly one point per series to
  # plot. Must render, not error: unlike Rankings' CAGR, nothing here needs
  # a 2-year span to be well-defined.
  session$setInputs(`bar-year_range` = c(2022, 2022), `trend-year_range` = c(2022, 2022))
  session$flushReact()
  stopifnot(!is.null(output[["bar-chart"]]), !is.null(output[["trend-chart"]]))
  session$setInputs(`bar-year_range` = c(2021, 2023), `trend-year_range` = c(2021, 2023))
  session$flushReact()
  cat("narrowing the slider excludes years outside the range; a single-period window still renders OK\n")

  cat("== YoY growth is computed against the year before the visible window ==\n")
  clear_bar_pairs()
  add_bar_pair("Manufacturing")
  session$setInputs(`bar-view_mode` = "growth", `bar-year_range` = c(2022, 2023))
  session$flushReact()
  grown <- read.csv(output[["bar-download_csv"]])
  stopifnot(!any(grown$Year == 2021))
  growth_2022 <- grown$GrowthPct[grown$Industry == "Manufacturing" & grown$Year == 2022]
  stopifnot(close_enough(growth_2022, 1 / 95 * 100))
  stopifnot(!is.null(output[["bar-chart"]]))
  cat("growth mode reaches back to the prior year outside the zoomed window OK\n")

  cat("== Rebasing to a chosen base year ==\n")
  session$setInputs(`bar-view_mode` = "level", `bar-rebase_toggle` = TRUE, `bar-base_year` = 2022, `bar-year_range` = c(2021, 2023))
  session$flushReact()
  rebased <- read.csv(output[["bar-download_csv"]])
  stopifnot("RebasedValue" %in% names(rebased))
  man <- rebased[rebased$Industry == "Manufacturing", ]
  stopifnot(close_enough(man$RebasedValue[man$Year == 2022], 100))
  stopifnot(close_enough(man$RebasedValue[man$Year == 2021], 95 / 96 * 100))
  stopifnot(close_enough(man$RebasedValue[man$Year == 2023], 97 / 96 * 100))
  cat("rebased values match Value/BaseValue*100 OK\n")

  cat("== Missing base-year data excludes a series from rebasing, not the app ==\n")
  clear_bar_pairs()
  add_bar_pair("Manufacturing")
  add_bar_pair("Retail trade")
  add_bar_pair("Construction") # only has 2022-2023 -- no 2021 row
  session$setInputs(`bar-rebase_toggle` = TRUE, `bar-base_year` = 2021, `bar-year_range` = c(2021, 2023))
  session$flushReact()
  dd <- read.csv(output[["bar-download_csv"]])
  construction_rows <- dd[dd$Industry == "Construction", ]
  stopifnot(nrow(construction_rows) > 0, all(is.na(construction_rows$RebasedValue)))
  stopifnot(!is.null(output[["bar-chart"]])) # the other 2 series still render
  cat("Construction excluded from the rebased view but the app keeps rendering OK\n")

  cat("== CSV export shape follows the current transformation mode ==\n")
  session$setInputs(`bar-rebase_toggle` = FALSE)
  session$flushReact()
  export_off <- read.csv(output[["bar-download_csv"]])
  stopifnot(!("RebasedValue" %in% names(export_off)))
  stopifnot(all(c("Industry", "Variable", "UOM", "GrowthPct") %in% names(export_off)))
  session$setInputs(`bar-rebase_toggle` = TRUE, `bar-base_year` = 2021)
  session$flushReact()
  export_on <- read.csv(output[["bar-download_csv"]])
  stopifnot("RebasedValue" %in% names(export_on))
  cat("the export includes RebasedValue only when rebasing is on OK\n")
  session$setInputs(`bar-rebase_toggle` = FALSE)
  session$flushReact()

  cat("== Growth Ranking CAGR ==\n")
  session$setInputs(
    `ranking-geography` = "Ontario", `ranking-variable` = "Multifactor productivity",
    `ranking-industry_level` = "2-digit", `ranking-year_range` = c(2021, 2023)
  )
  session$flushReact()
  stopifnot(!is.null(output[["ranking-chart"]]))
  ranked <- read.csv(output[["ranking-download_csv"]], check.names = FALSE)
  man_cagr <- ranked[["CAGR (%)"]][ranked$Industry == "Manufacturing"] / 100
  stopifnot(close_enough(man_cagr, sqrt(97 / 95) - 1))
  cat("CAGR matches (End/Start)^(1/years)-1 OK\n")

  cat("== Growth Ranking chart type toggle ==\n")
  session$setInputs(`ranking-chart_type` = "scatter")
  session$flushReact()
  stopifnot(!is.null(output[["ranking-chart"]]))
  session$setInputs(`ranking-chart_type` = "bar")
  session$flushReact()
  stopifnot(!is.null(output[["ranking-chart"]]))
  cat("ranking chart renders in both bar and scatter mode OK\n")

  cat("== Growth Ranking guards a single-year window ==\n")
  session$setInputs(`ranking-year_range` = c(2022, 2022))
  session$flushReact()
  expect_validation_error(
    tryCatch(output[["ranking-chart"]], error = function(e) e),
    "Rankings (single-year window)"
  )
  session$setInputs(`ranking-year_range` = c(2021, 2023))
  session$flushReact()

  cat("== Growth Ranking excludes a series missing a start/end-year value ==\n")
  # Construction only has 2022-2023 in the fixture -- at 2-digit level with a
  # 2021-2023 window it has no row at the start year, so inner_join() drops
  # it from ranking_data() entirely (not merely an NA CAGR).
  ranked2 <- read.csv(output[["ranking-download_csv"]], check.names = FALSE)
  stopifnot(!("Construction" %in% ranked2$Industry))
  cat("Construction (missing the start-year value) is excluded from the ranking OK\n")

  cat("== New Trends: growth rate + rank, computed per Geography ==\n")
  # No `newtrends-geography` input at all -- unlike every other tab, this one
  # has no Geography picker (Geography is what it varies across, not a
  # single scope to narrow to first; see newtrends_tab_ui()'s own comment).
  session$setInputs(
    `newtrends-variable` = "Multifactor productivity", `newtrends-industry` = env$DEFAULT_INDUSTRY,
    `newtrends-year_range` = c(2021, 2023)
  )
  session$flushReact()
  stopifnot(!is.null(output[["newtrends-map_chart"]]), !is.null(output[["newtrends-legend_table"]]))
  nt <- read.csv(output[["newtrends-download_csv"]], check.names = FALSE)
  stopifnot(setequal(nt$Geography, c("Ontario", "Quebec")))
  ontario_cagr <- nt[["CAGR (%)"]][nt$Geography == "Ontario"] / 100
  quebec_cagr <- nt[["CAGR (%)"]][nt$Geography == "Quebec"] / 100
  stopifnot(close_enough(ontario_cagr, sqrt(102 / 100) - 1))
  stopifnot(close_enough(quebec_cagr, sqrt(204 / 200) - 1))
  # Business sector industries grows at exactly the same rate (+2% over the
  # window) in both provinces at make_fixture()'s defaults -- a genuine tie,
  # so ranked_data()'s ties.method = "min" should land both at #1 rather
  # than an arbitrary win/lose.
  stopifnot(all(nt$Rank == 1))
  cat("New Trends CAGR matches (End/Start)^(1/years)-1 per Geography, tied growth -> tied (#1) rank OK\n")

  cat("== New Trends: rank puts strictly-higher growth ahead of strictly-lower growth ==\n")
  # Manufacturing's Ontario (95/96/97) and Quebec (150/153/156) values grow
  # at genuinely different rates (see the Geography-filter check below), so
  # unlike the tied default series above, this pins rank's actual ordering:
  # the higher-CAGR province at #1, the lower-CAGR one at #2.
  session$setInputs(`newtrends-industry` = "Manufacturing")
  session$flushReact()
  nt2 <- read.csv(output[["newtrends-download_csv"]], check.names = FALSE)
  stopifnot(nt2$Rank[nt2$Geography == "Quebec"] == 1)
  stopifnot(nt2$Rank[nt2$Geography == "Ontario"] == 2)
  cat("higher-CAGR Quebec ranks #1, lower-CAGR Ontario ranks #2 OK\n")

  cat("== New Trends guards a single-year window ==\n")
  session$setInputs(`newtrends-year_range` = c(2022, 2022))
  session$flushReact()
  expect_validation_error(
    tryCatch(output[["newtrends-map_chart"]], error = function(e) e),
    "New Trends map (single-year window)"
  )
  expect_validation_error(
    tryCatch(output[["newtrends-legend_table"]], error = function(e) e),
    "New Trends legend table (single-year window)"
  )
  session$setInputs(`newtrends-industry` = env$DEFAULT_INDUSTRY, `newtrends-year_range` = c(2021, 2023))
  session$flushReact()

  cat("== Geography filter: switching province re-scopes the data, not just the picker ==\n")
  # Manufacturing's Ontario values (95/96/97, set by make_fixture()'s
  # defaults) and Quebec values (150/153/156, see make_fixture()'s own
  # comment) are deliberately different, so this proves input$geography is
  # actually wired into scoped_raw()'s filter() -- not merely present and
  # inert -- for both the Compare (bar) and Trends tabs.
  clear_bar_pairs()
  add_bar_pair("Manufacturing")
  session$setInputs(`bar-geography` = "Ontario")
  session$flushReact()
  bar_on_ontario <- read.csv(output[["bar-download_csv"]])
  stopifnot(setequal(unique(bar_on_ontario$Geography), "Ontario"))
  stopifnot(close_enough(bar_on_ontario$Value[bar_on_ontario$Year == 2021], 95))

  session$setInputs(`bar-geography` = "Quebec")
  session$flushReact()
  bar_on_quebec <- read.csv(output[["bar-download_csv"]])
  stopifnot(setequal(unique(bar_on_quebec$Geography), "Quebec"))
  stopifnot(close_enough(bar_on_quebec$Value[bar_on_quebec$Year == 2021], 150))

  session$setInputs(`trend-geography` = "Quebec", `trend-industry` = "Manufacturing")
  session$flushReact()
  trend_on_quebec <- read.csv(output[["trend-download_csv"]])
  stopifnot(close_enough(trend_on_quebec$Value[trend_on_quebec$Year == 2021], 150))
  # Restore both tabs to Ontario -- nothing after this point should have to
  # know this check ever ran.
  session$setInputs(`bar-geography` = "Ontario", `trend-geography` = "Ontario", `trend-industry` = env$DEFAULT_INDUSTRY)
  session$flushReact()
  cat("switching Geography changes the underlying data on both Compare and Trends OK\n")
})

# The RAW_DATA_READER file-watcher itself (does a changed mfp_data_provincial.RData
# actually flow through to a running app without a restart?) is NOT
# exercised above. A bare-bones repro -- a single testServer session that
# does nothing but read raw_data(), rewrite the fixture, then poll
# later::run_now()+session$flushReact() -- reliably picks up the change
# (3/3 runs). But once the full server()'s other reactives (the resync
# observer, the industry_level observer, etc.) are also exercised first in
# the same mock session, testServer's isolated flush cycle stops
# faithfully driving the session = NULL reactivePoll's invalidation. This
# looks like a fidelity gap in how testServer flushes a reader that isn't
# tied to any one session, not a bug in app.R, but it means the bare-bones
# repro only demonstrates the mechanism in isolation -- it is not proof
# the behavior holds in a real, fully-loaded running app. Confirm that by
# hand: launch the app, edit mfp_data_provincial.RData while it's running, and watch
# the charts update within the poll interval with no restart.

unlink(fixture_path)

# == UX-state checks (loading/empty/error communication) ==================
# A separate, self-contained section/fixture -- its own sys.source()'d copy
# of app.R (ux_env), so nothing here depends on the interactive-flow
# section's env/fixture above having run first.
cat("== UX-state: all-NA-DisplayValue/CAGR cases hit a message, not a silent blank chart ==\n")
ux_fixture_path <- tempfile(fileext = ".RData")
mfp_data <- data.frame(
  # Value starts at 0 (not just "some other base year") so the *same*
  # fixture also serves the Rankings check below: compute_cagr() returns NA
  # for a StartValue of exactly 0 (see compute_cagr()'s own comment), so a
  # 2021-2023 CAGR window has nothing to show either.
  Year = 2021:2023, Geography = "Ontario", Variable = "Multifactor productivity",
  Industry = "Business sector industries", IndustryLevel = "Aggregate", Value = c(0, 1, 2),
  UOM = "Index, 2017=100", stringsAsFactors = FALSE
)
save(mfp_data, file = ux_fixture_path)
Sys.setenv(MFP_DATA_FILE = ux_fixture_path)
ux_env <- new.env()
sys.source("app.R", envir = ux_env)

# Trends (app.R's trend_tab_server, id "trend"): rebasing to a base year the
# only series doesn't have leaves every row's DisplayValue NA -- upstream
# validate(need(nrow(scoped_raw()) > 0, ...)) doesn't catch this (scoped_raw()
# has 3 rows), so this exercises the renderPlotly-level gate specifically.
shiny::testServer(ux_env$server, {
  session$setInputs(
    `trend-geography` = "Ontario",
    `trend-variable` = "Multifactor productivity", `trend-industry` = "Business sector industries",
    `trend-year_range` = c(2021, 2023),
    `trend-view_mode` = "level", `trend-rebase_toggle` = TRUE, `trend-base_year` = 1999
  )
  session$flushReact()
  expect_validation_error(tryCatch(output[["trend-chart"]], error = function(e) e), "Trends")
})

# Rankings (id "ranking"): the only industry's start-year (2021) value is 0,
# so compute_cagr() returns NA for it -- every row is dropped, but
# ranking_data() itself still returned 1 row (nrow > 0), so the upstream
# validate() doesn't catch this either.
shiny::testServer(ux_env$server, {
  session$setInputs(
    `ranking-geography` = "Ontario", `ranking-variable` = "Multifactor productivity",
    `ranking-industry_level` = "Aggregate", `ranking-year_range` = c(2021, 2023)
  )
  session$flushReact()
  expect_validation_error(tryCatch(output[["ranking-chart"]], error = function(e) e), "Rankings")
})

# Compare (tab_module_server("bar", ...), id "bar"): active_pairs() starts
# out holding the default (Business sector) series on its own (see
# default_pair_row()) -- no Add-series click needed to reach this case.
shiny::testServer(ux_env$server, {
  session$setInputs(
    `bar-geography` = "Ontario",
    `bar-variable` = "Multifactor productivity", `bar-year_range` = c(2021, 2023),
    `bar-view_mode` = "level", `bar-rebase_toggle` = TRUE, `bar-base_year` = 1999
  )
  session$flushReact()
  expect_validation_error(tryCatch(output[["bar-chart"]], error = function(e) e), "Compare")
})
unlink(ux_fixture_path)

# == Data contract (see data_contract.R) ====================================
# validate_data_contract()/MFP_DATA_CONTRACT/RAW_STATCAN_CONTRACT are what
# turns a StatCan schema drift, a pipeline bug, or a 0-row/empty pull into a
# clear thrown error instead of a silently wrong chart -- these checks
# exercise that mechanism directly, plus the specific failure mode that
# motivated it (see the first check below).
cat("== Data contract: an empty (0-row) mfp_data_provincial.RData degrades to Application unavailable, not a broken page ==\n")
empty_fixture_path <- tempfile(fileext = ".RData")
mfp_data <- good_row()[0, ]
save(mfp_data, file = empty_fixture_path)
Sys.setenv(MFP_DATA_FILE = empty_fixture_path)
contract_env <- new.env()
sys.source("app.R", envir = contract_env)
page_html <- as.character(contract_env$ui(list()))
# Before validate_data_contract() existed, an empty-but-successfully-loaded
# mfp_data_provincial.RData sailed past every is.null() check in ui() and reached
# sliderInput(min = min(integer(0)), max = max(integer(0)), ...) -- Inf/-Inf
# bounds that don't error (confirmed empirically) but silently produce a
# garbled date-range slider instead of a clear error state. Confirming the
# *absence* of that slider's own JS library tag is a reasonable proxy for
# "ui() took the unavailable_page() branch and never got as far as building
# tab UIs that reference init_df at all".
stopifnot(grepl("Application unavailable", page_html, fixed = TRUE))
stopifnot(!grepl("ionRangeSlider", page_html, fixed = TRUE))
cat("empty data -> the existing \"Application unavailable\" page, no broken widgets built OK\n")
unlink(empty_fixture_path)

cat("== Data contract: validate_data_contract() catches each kind of drift it's meant to ==\n")
expect_contract_error <- function(df, contract, pattern, label) {
  res <- tryCatch({
    contract_env$validate_data_contract(df, contract, "test")
    NULL
  }, error = function(e) e)
  stopifnot(!is.null(res), grepl(pattern, conditionMessage(res), fixed = TRUE))
  cat(label, ": caught OK, message: ", conditionMessage(res), "\n", sep = "")
}

missing_col <- good_row()
missing_col$UOM <- NULL
expect_contract_error(missing_col, contract_env$MFP_DATA_CONTRACT, "missing column(s): UOM", "missing column")

na_key <- good_row()
na_key$Industry[2] <- NA
expect_contract_error(na_key, contract_env$MFP_DATA_CONTRACT, "column 'Industry' has 1 NA value", "NA in a key column")

dupe_key <- rbind(good_row(), good_row()[1, ])
expect_contract_error(dupe_key, contract_env$MFP_DATA_CONTRACT, "duplicate row(s)", "duplicate natural key")

na_geography <- good_row()
na_geography$Geography[2] <- NA
expect_contract_error(na_geography, contract_env$MFP_DATA_CONTRACT, "column 'Geography' has 1 NA value", "NA in the Geography key column")

# Regression check for the contract's own unique_key shape: table
# 36-10-0211-01 means the same (Year, Variable, Industry) can legitimately
# repeat once per province now, so unique_key had to grow from
# c("Year", "Variable", "Industry") to include "Geography" too (see
# data_contract.R) -- 2 rows identical on Year/Variable/Industry but
# differing only in Geography must NOT be flagged as duplicates, the mirror
# image of the dupe_key check above (which duplicates a row exactly,
# Geography included).
distinct_geography <- good_row()
quebec_row <- distinct_geography[1, ]
quebec_row$Geography <- "Quebec"
distinct_geography <- rbind(distinct_geography, quebec_row)
contract_env$validate_data_contract(distinct_geography, contract_env$MFP_DATA_CONTRACT, "test") # should not throw
cat("2 rows sharing Year/Variable/Industry but differing in Geography are not flagged as duplicates OK\n")

bad_enum <- good_row()
bad_enum$IndustryLevel[1] <- "3-digit"
expect_contract_error(bad_enum, contract_env$MFP_DATA_CONTRACT, "outside its expected set", "value outside a declared enum")

extra_col_ok <- good_row()
extra_col_ok$FutureColumn <- "whatever"
contract_env$validate_data_contract(extra_col_ok, contract_env$MFP_DATA_CONTRACT, "test") # should not throw
cat("forward-compatible: an unrecognized extra column is tolerated, not rejected OK\n")

raw_missing_col <- data.frame(REF_DATE = 2021, GEO = "Ontario", VALUE = 1, UOM = "x", stringsAsFactors = FALSE)
expect_contract_error(
  raw_missing_col, contract_env$RAW_STATCAN_CONTRACT,
  paste0(
    "missing column(s): Labour productivity measures and related measures, ",
    "North American Industry Classification System (NAICS), ",
    "Hierarchy for North American Industry Classification System (NAICS)"
  ),
  "raw StatCan pull missing a depended-on column"
)

cat("== Data contract: cached_load_mfp_data() is keyed on (path, mtime), not mtime alone ==\n")
# Regression check for a real bug this rewrite surfaced: keyed on mtime
# alone, a second distinct path read in the same process (two files
# happening to share an mtime, or -- as here -- simply any second path read
# after a first one) could silently serve the first path's cached data
# instead of ever being told apart. Only matters when one R process reads
# more than one path (MFP_DATA_FILE is fixed for the life of a real deployed
# app) -- which is exactly what this test file does throughout, swapping
# fixtures across many sys.source() environments.
path_a <- tempfile(fileext = ".RData")
mfp_data <- good_row()
save(mfp_data, file = path_a)
path_b <- tempfile(fileext = ".RData")
mfp_data <- good_row()
mfp_data$Value <- mfp_data$Value + 1000
save(mfp_data, file = path_b)
df_a <- contract_env$cached_load_mfp_data(path_a)
df_b <- contract_env$cached_load_mfp_data(path_b)
stopifnot(!identical(df_a$Value, df_b$Value))
cat("reading a second, different path returns that path's own data, not a stale cached copy OK\n")
unlink(c(path_a, path_b))

# == safe_load_mfp_data() recovery (its own section: needs a fresh env) =====
cat("== UX-state: safe_load_mfp_data() returns NULL + warns on a failed read, and recovers cleanly next time ==\n")
# Not a "safe_cached_load_mfp_data()" / RAW_DATA_STALE-flag mechanism -- no
# such function or reactiveVal exists anywhere in app.R (confirmed by
# grep); an earlier version of this check assumed one, and since it called
# a function that didn't exist, was crashing this entire script before it
# ever reached the summary below. safe_load_mfp_data() does something
# simpler: tryCatch(cached_load_mfp_data(...)) returning NULL and warn()ing
# on failure (see its own comment in app.R) -- there's no "serve the
# last-known-good copy while flagging it stale" fallback today. This checks
# that real, current contract instead: a failed read returns NULL and
# warns, and doesn't "poison" the shared cache -- the very next successful
# read for the same path works again with no restart needed. (Whether a
# last-known-good fallback would be a worthwhile *addition* is a product/UX
# call, not a bug -- see the audit summary.)
recovery_env <- new.env()
good_path <- tempfile(fileext = ".RData")
mfp_data <- good_row()
save(mfp_data, file = good_path)
Sys.setenv(MFP_DATA_FILE = good_path)
sys.source("app.R", envir = recovery_env)

first <- recovery_env$safe_load_mfp_data(good_path)
stopifnot(is.data.frame(first), nrow(first) == 3)

missing_path <- tempfile(fileext = ".RData") # never created -- simulates the file disappearing
warned <- FALSE
failed <- withCallingHandlers(
  recovery_env$safe_load_mfp_data(missing_path),
  warning = function(w) {
    warned <<- TRUE
    invokeRestart("muffleWarning")
  }
)
stopifnot(is.null(failed), warned)

recovered <- recovery_env$safe_load_mfp_data(good_path)
stopifnot(identical(first, recovered))
cat("a read failure returns NULL and warns; a later successful read for the same path recovers cleanly OK\n")
unlink(good_path)

cat("\nSUMMARY: all checks above passed (interactive-flow, UX-state, data-contract, and safe_load_mfp_data recovery).\n")
