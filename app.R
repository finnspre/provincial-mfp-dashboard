library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(DT)
library(htmltools)

# STATCAN_TABLE_ID / MFP_DATA_CONTRACT / validate_data_contract() -- shared
# with data_pipeline.R so both agree on exactly one definition of "what does
# this table's data look like" (see that file's own header comment). Plain
# relative path, not an absolute one -- Shiny always runs app.R with the
# app's own folder as the working directory (true both for a real deploy and
# for scripts/verify_app.R's sys.source(), run from this same project root),
# the same assumption versioned_asset()'s "www/..." paths below already make.
# local = TRUE -- not source()'s own default (FALSE, meaning "evaluate in
# .GlobalEnv regardless of where source() itself was called from") --
# so these definitions land in app.R's own environment (whatever that is:
# .GlobalEnv for a real Shiny deploy, but a throwaway per-test environment
# under scripts/verify_app.R's sys.source(), which runs several independent
# copies of app.R in the same R session). Confirmed empirically: without
# this, STATCAN_TABLE_ID et al. silently end up in .GlobalEnv instead of
# alongside the rest of app.R's own top-level bindings -- functions defined
# below still resolve them correctly by walking up the environment chain at
# call time, but only by accident (because .GlobalEnv happens to be a parent
# of the environment those functions close over) rather than because this
# file actually owns them, and every re-run leaks another stale copy into
# .GlobalEnv instead of being cleanly scoped to this file.
source("data_contract.R", local = TRUE)

MFP_DATA_FILE <- Sys.getenv("MFP_DATA_FILE", "mfp_data.RData")

# Appends a ?v=<mtime> cache-buster to a www/ asset path, so editing e.g.
# tree_select.js takes effect on a plain reload instead of silently
# serving a stale cached copy from before the edit (the static file's
# *name* doesn't change, and Shiny doesn't send cache-control headers that
# would force revalidation on every load, so without this a browser that
# already cached the old file can hang onto it across an ordinary reload).
versioned_asset <- function(path) {
  mtime <- file.mtime(file.path("www", path))
  paste0(path, "?v=", if (is.na(mtime)) "0" else as.integer(mtime))
}

# Categorical palette -- these are the real CSLS brand colours (the NHL-team
# nicknames are just internal mnemonics), so the *values* are fixed. The
# order below isn't the order they happen to have been typed in, though: it's
# the ordering (of the same 8 hex values, slot 1 held fixed since it also
# drives page chrome -- see below) that maximizes worst-case adjacent-pair
# separation under simulated colour-blindness. Computed by porting the
# dataviz skill's own OKLab + Machado-Oliveira-Fernandes (2009) CVD-simulation
# math to R and brute-forcing all 5,040 orderings that keep slot 1 fixed:
# the *previous* order had Canadiens Red directly beside Canucks Green (slots
# 2-3), which is the canonical red-green colour-blindness confusion pair --
# CVD deltaE 4.2 there, below the 6.0 hard floor, invisible to normal vision
# (deltaE 27.9) which is exactly why this needs computing, not eyeballing.
# This order clears worst-case adjacent CVD deltaE 24.6 (target >=8) and
# worst-case adjacent normal-vision deltaE 28.1 (floor >=15) -- re-run the
# same check before ever reordering this again.
CATEGORICAL_PALETTE <- c(
  "#012F72", # Maple Leaf Blue -- slot 1, held fixed: also the site-wide
             # primary accent (see BRAND_MAPLE_BLUE below -- default-state
             # buttons/links/tab text, and the single-series colour on
             # Trends/Rankings, all key off this slot)
  "#3C8745", # Canucks Green
  "#FFC550", # Flames Yellow
  "#CE2E2E", # Canadiens Red
  "#EF84EF", # Panther Pink
  "#7E1F86", # King Purple
  "#F74C16", # Oilers Orange
  "#0598D8"  # Jets Blue -- slot 8 (see BRAND_JETS_BLUE below -- csls.ca's
             # universal hover/focus/active-state colour: active tab fill,
             # link hover, tree-select hover/selected highlight)
)
# Hard cap on the Compare/Data tabs' active series list -- tied to
# length(CATEGORICAL_PALETTE) rather than a separate literal 8, so the two
# stay in sync automatically if the palette is ever resized: past this many
# series_color_map() would start reusing colours (see its own fallback
# below), which defeats the point of a colour-coded chart/chip list, so
# "Add series" is disabled once active_pairs() hits this count instead of
# letting the chart quietly degrade.
MAX_ACTIVE_SERIES <- length(CATEGORICAL_PALETTE)
# Secondary channel (colour-blind/print/grayscale accessibility) for series
# identity, cycled alongside CATEGORICAL_PALETTE by index -- see
# series_color_map() and the Compare tab's per-series trace loop. 6 values
# each (not 8) is deliberate: it keeps the dash/shape cycle out of phase with
# the 8-colour cycle, so a repeat of "solid + slot 1's colour" doesn't land on
# the same series index every time round.
LINE_DASH_STYLES <- c("solid", "dash", "dot", "dashdot", "longdash", "longdashdot")
MARKER_SYMBOLS <- c("circle", "diamond", "square", "triangle-up", "cross", "x")

# Page-chrome aliases for the two brand blues csls.ca actually names in its
# own :root (--color-maple-blue / --color-jets-blue -- see
# CSLS-Shiny-Style-Spec.md section 2). Reused by name from CATEGORICAL_PALETTE
# rather than re-typed as separate hex literals, so page chrome and the chart
# palette stay pinned to one source of truth. Chrome-only: the chart code
# below keeps referencing CATEGORICAL_PALETTE[1] directly (unchanged) for the
# Trends/Rankings single-series colour -- these two aliases are for the CSS
# in ui() (nav-pills, links, the tree-select dropdown, chip list) only.
BRAND_MAPLE_BLUE <- CATEGORICAL_PALETTE[1] # default state: buttons, links, tab/nav text
BRAND_JETS_BLUE <- CATEGORICAL_PALETTE[8]  # hover/focus/active state: tabs, links, highlights

INK_PRIMARY <- "#000000"   # csls.ca body-text black (was #0C0C0C "Senators Black" -- now the exact
                            # site value; only this UI-chrome/axis-text neutral changed, the chart
                            # series palette above is untouched -- see CSLS-Shiny-Style-Spec.md section 2)
INK_MUTED <- "#6B6B6B"     # Dark Grey -- already the exact csls.ca value; 5.33:1 vs CHART_SURFACE, clears WCAG AA text contrast
GRIDLINE <- "#D9D9D9"      # Light Grey -- already the exact csls.ca value -- deliberately low-contrast/recessive, not a bug (see display_axis_title() usage below)
CHART_SURFACE <- "#FFFFFF" # Snow White -- already the exact csls.ca value
FONT_FAMILY <- "Roboto, Arial, sans-serif" # matches csls.ca's --font-base fallback stack exactly

DEFAULT_VARIABLE <- "Multifactor productivity"
DEFAULT_INDUSTRY <- "Business sector"

# Table 36-10-0208-01 covers Canada only -- no provincial/territorial
# breakdown the way the old labour productivity table had (see
# data_pipeline.R) -- so there's no GEOGRAPHY_ORDER/DEFAULT_GEOGRAPHY here,
# and no Geography picker anywhere below: a dropdown with exactly one
# possible value would serve no purpose.
VARIABLE_ORDER <- c(
  "Multifactor productivity", "Labour productivity", "Capital productivity",
  "Real gross domestic product (GDP)", "Labour input", "Hours worked",
  "Labour composition",
  "Labour input of workers with primary or secondary education",
  "Labour input of workers with some or completed post-secondary certificate or diploma",
  "Labour input of workers with university degree or above",
  "Capital input", "Capital stock", "Capital composition",
  "Capital input of information and communications technologies",
  "Capital input of non-information and communications technologies",
  "Combined labour and capital inputs", "Gross domestic product (GDP)",
  "Labour compensation",
  "Labour compensation of workers with primary or secondary education",
  "Labour compensation of workers with some or completed post-secondary certificate or diploma",
  "Labour compensation of workers with university degree or above",
  "Capital cost",
  "Capital cost of information and communications technologies",
  "Capital cost of non-information and communications technologies",
  "Contribution of capital intensity to labour productivity growth",
  "Contribution of labour composition to labour productivity growth"
)

# The Growth Accounting tab's productivity decomposition: labour productivity
# (LP) growth in this table's own accounting framework equals the sum of the
# contribution of capital deepening (capital intensity), the contribution of
# labour composition, and multifactor productivity (MFP) growth -- MFP growth
# is the *residual* left over once those two contributions are removed from
# LP growth, not a separately-measured quantity, which is exactly why it's
# computed that way below (see growth_tab_server()) rather than read off the
# table's own "Multifactor productivity" series directly: StatCan's own MFP
# index and this residual agree almost exactly (a fraction of a percentage
# point apart, confirmed against the real data), but computing it as a
# residual guarantees the 3 contributions always add up to LP growth exactly
# -- the whole point of a decomposition chart -- rather than leaving a small
# unexplained gap for a reader to notice and wonder about.
#
# All 3 of these are chain-linked indices (UOM "Index, 2017=100"), the same
# as "Labour productivity" itself -- so a log-difference between consecutive
# years (not a simple percent change, which the rest of this app's "Annual
# percentage change" views use) is what makes the 3 contributions genuinely
# additive: LP_t/LP_(t-1) is (to within rounding) the product of the other
# 3 indices' own year-over-year ratios, so summing their log-ratios exactly
# reconstructs log(LP_t/LP_(t-1)) -- summing simple percent changes instead
# does not, and was confirmed empirically to leave a residual up to ~0.15
# percentage points even at the economy-wide "Business sector" level.
GROWTH_ACCOUNTING_VARS <- c(
  lp = "Labour productivity",
  cap = "Contribution of capital intensity to labour productivity growth",
  lab = "Contribution of labour composition to labour productivity growth"
)

# Colour-only identity for the 4 bars this tab's chart draws (see
# growth_tab_ui()'s sidebar legend and growth_tab_server()'s chart, the only
# 2 places these are used) -- first 4 slots of the same CATEGORICAL_PALETTE
# every other chart in this app draws from, so this tab's colours are still
# part of the one calibrated (CVD-safe) palette rather than a second,
# independent set of hex values.
GROWTH_ACCOUNTING_COLORS <- c(
  lp = CATEGORICAL_PALETTE[1], cap = CATEGORICAL_PALETTE[2],
  lab = CATEGORICAL_PALETTE[3], mfp = CATEGORICAL_PALETTE[4]
)

# The Growth Accounting tab's Interval picker -- how many years each bar on
# the x-axis covers, from 1 (today's original per-year behaviour) up to 10.
# Values are plain integers (not e.g. "5y" strings) since growth_tab_server()
# does arithmetic directly on the selected interval (period width, step size);
# selectInput() still reports it back as a string like every HTML <select>
# (see growth_interval()'s own as.integer() round-trip), same convention the
# Trends/Compare tabs' own numeric Base year <select> already relies on.
GROWTH_INTERVAL_CHOICES <- setNames(1:10, c("1 year (annual)", paste0(2:10, " years")))

# "Slightly more greyscale" treatment for the one truncated period a
# multi-year Interval can produce (see filtered_data()'s own comment on why
# at most one ever exists, always the oldest) -- how far blend_toward_grey()
# below blends a bar's colour toward grey; 0 = untouched, 1 = flat grey.
GROWTH_TRUNCATED_DESATURATION <- 0.55

# Blends a bar's normal series colour toward *that same colour's* own grey
# (its average-channel equivalent) rather than one flat grey shared by every
# series, so the 4 bars in the one truncated period (see
# GROWTH_TRUNCATED_DESATURATION above) are still each identifiable by hue --
# just visibly muted next to the vivid, full-length periods around them, per
# what was asked for ("slightly more greyscale", not "solid grey").
blend_toward_grey <- function(hex, amount = GROWTH_TRUNCATED_DESATURATION) {
  channels <- grDevices::col2rgb(hex)[, 1]
  grey <- mean(channels)
  blended <- channels * (1 - amount) + grey * amount
  grDevices::rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
}

# The Industry detail toggle on the Rankings tab -- selects a *maximum*
# level of detail, not an exact one, so "2-digit" still includes the
# Aggregate rows too (see industry_levels_upto() below). Table 36-10-0208-01
# has no tier deeper than this (see data_pipeline.R), so unlike the old
# labour productivity table there's no "3-digit" option to offer.
DEFAULT_INDUSTRY_LEVEL <- "Aggregate"
INDUSTRY_LEVEL_ORDER <- c("Aggregate", "2-digit")

# Rankings tab: row count above which the chart switches from a single
# fixed-height, one-side-labelled layout to a taller, scrollable one with
# labels split across both sides (see ranking_tab_server()'s output$chart
# and output$chart_container). Table 36-10-0208-01 tops out at 21
# industries total (5 Aggregate + 16 2-digit -- see data_pipeline.R), so in
# practice neither level comes close to crossing this today; kept as
# headroom rather than removed, in case a future table swap adds enough
# 2-digit detail to need it.
RANKING_CHART_ROW_THRESHOLD <- 40
RANKING_CHART_PX_PER_ROW <- 28
RANKING_CHART_TICKFONT_SPLIT <- 10

# Growth Accounting tab: period count above which the chart switches from
# filling the card's width to a fixed, wider-than-the-card pixel width
# inside its own horizontally-scrolling wrapper (see growth_tab_server()'s
# output$chart_container) -- same idea as RANKING_CHART_ROW_THRESHOLD above,
# just along the other axis. A "period" here is whatever the Interval
# picker currently divides the date range into (see GROWTH_INTERVAL_CHOICES/
# filtered_data()) -- 1-year-wide periods at Interval "1 year (annual)", up
# to 10-year-wide ones -- so how many periods a given date range produces
# varies with that pick; table 36-10-0208-01 spans up to 62 years of usable
# growth data per industry (1961-2023, minus the first year -- see
# GROWTH_ACCOUNTING_VARS), so the annual case in practice crosses this
# threshold at its default (full) date range even though a 10-year Interval
# over the same range would not. PX_PER_PERIOD budgets enough width per
# period for both bars (the labour productivity growth bar + the stacked
# "other factors" bar) plus the tight gap between them and a share of the
# wider gap to the next period -- see GROWTH_BAR_OFFSET/GROWTH_BAR_WIDTH
# below.
GROWTH_CHART_PERIOD_THRESHOLD <- 15
GROWTH_CHART_PX_PER_PERIOD <- 70

# Growth Accounting chart: the 2 bars per period (labour productivity
# growth, and the capital deepening/labour composition/MFP growth stack)
# are positioned by literal x-value arithmetic -- PosIndex - GROWTH_BAR_OFFSET
# and PosIndex + GROWTH_BAR_OFFSET respectively, each drawn at
# GROWTH_BAR_WIDTH wide -- rather than via barmode="group"/offsetgroup.
# PosIndex (see filtered_data()) is this chart's period's position along the
# x-axis, 1/2/3/..., not the calendar year(s) it actually spans -- a period
# can cover more than 1 year once the Interval picker is above "1 year", and
# the 1 truncated period a date range doesn't divide evenly can cover fewer
# years than the others (see filtered_data()'s own comment) -- so periods
# are always drawn evenly spaced/sized on the axis regardless of how many
# calendar years each actually represents, exactly like a bar chart of e.g.
# "1990s"/"2000s"/"2010s" would be; PosIndex's paired tickvals/ticktext (see
# output$chart) is what labels each position with its own real date range
# instead of a plain 1/2/3 axis. Confirmed empirically (a real headless-
# Chrome render, not just reading the plotly.js docs): offsetgroup only
# separates bars into different x-slots when barmode is "group" -- under
# "stack" or "relative" (needed here so the 3-series "other factors" bar
# actually stacks, see output$chart) every trace at a given x combines into
# one bar regardless of offsetgroup, so the 4 series all merged into a
# single bar instead of 2 side by side the first time this was tried.
# Explicit, different x-values per bar sidesteps that entirely: nothing
# here depends on offsetgroup at all any more.
# Geometry (derived once, not tuned by eye): with pair-offset d and per-bar
# width w, the gap *within* one period's pair is (2d - w) and the gap
# *between* one period's pair and the next is (1 - 2d - w) (1 = the spacing
# between whole PosIndex steps). Solving for a small intra-pair gap (~0.05)
# and a clearly bigger inter-pair gap (~0.25) gives d = 0.2, w = 0.35 below.
GROWTH_BAR_OFFSET <- 0.2
GROWTH_BAR_WIDTH <- 0.35

# Plain-language explanation shown below the Variable picker, keyed by name
# (not positionally paired with VARIABLE_ORDER) so a lookup miss is
# impossible to introduce by reordering one list and not the other.
# renderUI still treats an empty/missing entry as "show nothing" (see
# variable_definition_ui() below), so a future entry can still be blanked
# out for a self-explanatory variable the same way the old labour
# productivity dashboard did for "Total number of jobs".
VARIABLE_DEFINITIONS <- c(
  "Multifactor productivity" = "A measure of how efficiently an industry uses labour and capital together to produce output. Calculated by Statistics Canada as real GDP divided by combined labour and capital inputs.",
  "Labour productivity" = "A measure of how efficiently goods and services are produced by workers. Calculated by Statistics Canada as real value added divided by total hours worked.",
  "Capital productivity" = "A measure of how efficiently an industry uses its capital to produce output. Calculated by Statistics Canada as real GDP divided by capital input.",
  "Real gross domestic product (GDP)" = "The total dollar value of an industry's output minus the cost of the inputs (materials, energy, etc.) used to produce it. Adjusted by Statistics Canada to 2017 dollars by default, removing the effects of inflation.",
  "Labour input" = "A single measure of the total labour used in production. Calculated by Statistics Canada by combining hours worked across groups of workers (classified by education, experience, and employment type), weighted by hourly compensation.",
  "Hours worked" = "The total number of hours that a person devotes to work, whether paid or unpaid.",
  "Labour composition" = "A measure of how the skill mix of the workforce changes over time. Calculated as labour input divided by hours worked.",
  "Labour input of workers with primary or secondary education" = "The portion of total labour input contributed by workers whose highest education is high school or below.",
  "Labour input of workers with some or completed post-secondary certificate or diploma" = "The portion of total labour input from workers with some post-secondary education or a non-degree certificate or diploma (includes those who attended university without completing a bachelor's degree).",
  "Labour input of workers with university degree or above" = "The portion of total labour input from workers with a bachelor's degree or higher.",
  "Capital input" = "A measure of the productive services an industry gets from its capital assets (equipment, structures, inventories, and land) in a given year. Calculated by Statistics Canada by combining capital stocks, weighted by the cost of capital for each asset type.",
  "Capital stock" = "The dollar value of an industry's accumulated capital assets still in use, after accounting for depreciation. Estimated by Statistics Canada using the perpetual inventory method for most equipment and structures, and other methods for inventories and land.",
  "Capital composition" = "A measure of how the mix of capital assets changes over time. Calculated as capital input divided by capital stock. Rises when investment shifts toward shorter-lived assets like equipment, which deliver more service per dollar than longer-lived assets like buildings.",
  "Capital input of information and communications technologies" = "The portion of total capital input from computer hardware, software, and telecommunications equipment.",
  "Capital input of non-information and communications technologies" = "The portion of total capital input from all other capital assets (machinery, vehicles, buildings, and structures).",
  "Combined labour and capital inputs" = "A single measure combining labour input and capital input, weighted by each one's share of total production costs. Used as the denominator in the multifactor productivity calculation.",
  "Gross domestic product (GDP)" = "The dollar value of what an industry produces, minus the cost of the inputs (materials, energy, etc.) it used up to produce it. Measured in today's dollars, so it is affected by inflation.",
  "Labour compensation" = "All payments in cash or in-kind made by domestic producers to workers for services rendered.",
  "Labour compensation of workers with primary or secondary education" = "The portion of total labour compensation paid to workers whose highest education is high school or below.",
  "Labour compensation of workers with some or completed post-secondary certificate or diploma" = "The portion of total labour compensation paid to workers with some post-secondary education or a non-degree certificate or diploma.",
  "Labour compensation of workers with university degree or above" = "The portion of total labour compensation paid to workers with a bachelor's degree or higher.",
  "Capital cost" = "The income earned by the owners of capital (profit, depreciation, rent, and interest). Calculated by Statistics Canada as GDP (current dollars) minus labour compensation.",
  "Capital cost of information and communications technologies" = "The portion of total capital cost attributable to ICT assets, roughly what it would cost to rent that equipment and software for a year.",
  "Capital cost of non-information and communications technologies" = "The portion of total capital cost attributable to all other capital assets (machinery, vehicles, buildings, and structures).",
  "Contribution of capital intensity to labour productivity growth" = "The portion of labour productivity growth from workers having more capital to work with. Calculated as the growth in capital services per hour, multiplied by capital's share of total costs.",
  "Contribution of labour composition to labour productivity growth" = "The portion of labour productivity growth from the workforce becoming more educated or experienced. Calculated as the growth in labour composition, multiplied by labour's share of total costs."
)

# Plain-language explanation for the currently selected Variable, shown
# below the picker on the Trends tab only (trend_tab_server() is the one
# remaining `output$variable_definition <- renderUI(variable_definition_ui(input$variable))`
# call) -- the Compare/Rankings/Data tabs used to show the same text under
# their own Variable pickers too, which was redundant once a reader could
# always flip to Trends for it, so those 3 uiOutput()/renderUI() pairs were
# removed and this definition now lives in exactly one place.
# A small link that jumps to the Definitions tab -- the Trends tab's own
# inline "More" (right after its definition), the Compare/Rankings/Data
# tabs' "Definition" (right under their Variable picker), and the Growth
# Accounting tab's "Learn more" (folded into its own explanatory blurb) all
# share this one helper rather than 4 near-duplicate tags$a() calls drifting
# apart over time.
#
# Purely client-side (see the "goto-definition-link" delegated click
# listener in www/ui_helpers.js), not a Shiny actionLink()/observeEvent()
# round-trip -- switching tabs (and scrolling, when highlight = TRUE) is
# plain DOM/Bootstrap work with nothing for the server to compute, so it
# stays entirely in JS, same reasoning as the Definitions tab's own search
# box.
#
# highlight = FALSE (the Trends tab's own "More" -- see
# variable_definition_ui()) renders a `data-highlight="false"` attribute
# that tells the JS side to do nothing more than click the Definitions
# nav-link itself -- the *exact* same action a reader clicking that pill
# directly would trigger (no scroll, no highlight, no clearing whatever
# search filter is currently active), per an explicit user request that
# this one link read as pure navigation, not a jump to a specific entry.
# The other 4 instances all keep the default TRUE: `term` there must
# exactly match a VARIABLE_DEFINITIONS or GLOSSARY_EXTRA_TERMS key
# (case-insensitively -- the JS side lowercases both sides to match
# .definitions-item's own data-term) for the highlight to find anything; a
# term with no matching entry still switches tabs, just with nothing to
# highlight (see the JS's own bounded-retry comment).
goto_definition_link <- function(term, text, highlight = TRUE) {
  tags$a(
    href = "#", class = "goto-definition-link", `data-term` = term,
    `data-highlight` = if (!highlight) "false",
    text
  )
}

variable_definition_ui <- function(variable) {
  def <- VARIABLE_DEFINITIONS[[variable]]
  if (is.null(def) || !nzchar(def)) return(NULL)
  # Spells out the acronym just this once, right where a reader is most
  # likely to meet it cold -- "MFP" gets used on its own elsewhere in this
  # app (e.g. the Growth Accounting tab's legend/toasts) without ever being
  # expanded first. Display-only: `variable` itself stays the plain
  # "Multifactor productivity" (the actual StatCan variable name, matching
  # what the picker above and the Definitions tab both show), so this
  # doesn't touch VARIABLE_DEFINITIONS' lookup key.
  label <- if (variable == "Multifactor productivity") "Multifactor productivity (MFP)" else variable
  # " " (a plain space) before it, not a separate element/margin, is what
  # makes "More" read as the definition's own next sentence rather than a
  # visually separate line, per how this was asked for. highlight = FALSE --
  # see goto_definition_link()'s own comment: this one link is plain
  # navigation to the Definitions tab, not a jump to `variable`'s own entry
  # there, so `variable` here only ever matters for VARIABLE_DEFINITIONS'
  # own lookup above, never reaching the JS side at all.
  p(class = "text-muted small", strong(paste0(label, ": ")), def, " ", goto_definition_link(variable, "More", highlight = FALSE))
}

# Dashboard concepts shown at the end of the Definitions tab's glossary,
# after every VARIABLE_DEFINITIONS entry -- not StatCan variables
# themselves (they never appear as a value in the data), so folding them
# into VARIABLE_DEFINITIONS instead would be the wrong home for them: that
# vector's whole meaning is "keyed by the exact StatCan variable name", and
# ordered_unique() (see series_choices()) would just filter a non-variable
# key straight back out of the Variable picker anyway, silently relying on
# that filtering rather than keeping VARIABLE_DEFINITIONS honestly scoped to
# real variables. A separate named vector keeps that scoping exact while
# still letting the Definitions tab explain broader dashboard methodology
# alongside the variable-by-variable glossary.
GLOSSARY_EXTRA_TERMS <- c(
  "Growth accounting" = "A method Statistics Canada uses to explain what drives labour productivity growth. It divides labour productivity growth into the part coming from increases in capital intensity, increases in skill levels of workers (labour composition), and multifactor productivity (which captures everything else such as technological change, organizational improvements, or economies of scale). The residual portion of labour productivity growth not explained by capital intensity or labour composition is what's counted as multifactor productivity growth. The three pieces sum to total labour productivity growth, which is what lets you say how much of a given year's gain came from each source."
)

# One .definitions-item (a dt/dd pair wrapped in a div -- see
# definitions_tab_ui()'s own comment on why) for either a VARIABLE_ORDER
# entry or a GLOSSARY_EXTRA_TERMS one -- shared so both loops below stay in
# lockstep on the exact markup the search listener in www/ui_helpers.js
# depends on (the data-term attribute), rather than one drifting from the
# other if only one were ever edited later.
definitions_item <- function(term, def) {
  if (is.null(def) || !nzchar(def)) return(NULL)
  tags$div(class = "definitions-item", `data-term` = tolower(term), tags$dt(term), tags$dd(def))
}

# The Definitions tab -- a standing glossary of every VARIABLE_DEFINITIONS
# entry, in VARIABLE_ORDER (the same order the Variable picker itself lists
# them in), plus GLOSSARY_EXTRA_TERMS' broader dashboard concepts appended
# at the end, for a reader who wants to browse the full list up front rather
# than opening each variable one at a time off the Trends tab's picker.
# Skips the same "" -> hide entirely case variable_definition_ui() does
# above, so a future variable blanked out there (self-explanatory, e.g. the
# old labour productivity dashboard's "Total number of jobs") drops out of
# this list too rather than showing an empty definition.
#
# No `id`/NS() of its own, unlike every other *_tab_ui() in this file --
# this tab has no inputs, outputs, or per-session state at all (it just
# renders the same fixed glossary for everyone), so there's nothing here
# that would ever need namespacing; nav_panel() below calls it with no
# arguments, and there's no matching *_tab_server().
definitions_tab_ui <- function() {
  card(
    class = "definitions-tab-card",
    # Wrapped together rather than left as 2 direct card children -- bslib's
    # own card-body is a flex column with a fixed ~24px `gap` between every
    # direct child, applied independently of (i.e. on top of) each child's
    # own margin, so no amount of margin tweaking on the title/subtitle
    # alone could pull them closer than that 24px floor. Grouping both into
    # one wrapper makes them a single flex item from the card-body's own
    # point of view, so that 24px only ever applies *outside* this div (down
    # to the search box below); the small, tight gap actually wanted between
    # title and subtitle is then just ordinary collapsed block-margin inside
    # it (see the CSS below), free of the flex gap entirely.
    tags$div(
      class = "definitions-header",
      tags$h4(class = "definitions-title", "Definitions"),
      p(class = "definitions-subtitle text-muted small", "Plain-language explanations of terms used throughout this dashboard.")
    ),
    # Plain client-side filter -- see www/ui_helpers.js's "definitions-search-input"
    # input listener -- rather than a Shiny textInput()/renderUI() round-trip:
    # this tab has no server component at all (see definitions_tab_ui()'s own
    # comment above), and a search this simple (substring match against each
    # term's own name, nothing server-side to compute) doesn't need one --
    # every candidate row is already sitting in the DOM below, so JS just
    # shows/hides them directly. type="search" gets the browser's own native
    # clear ("x") button for free, same as a real search box.
    tags$div(
      class = "definitions-search",
      tags$input(
        type = "search", id = "definitions-search-input", class = "form-control",
        placeholder = "Search for a term...", autocomplete = "off",
        `aria-label` = "Search for a term"
      )
    ),
    tags$dl(
      class = "definitions-list",
      # One <div class="definitions-item"> per dt/dd pair (a dl's content
      # model allows grouping dt+dd inside a div) rather than flat sibling
      # dt/dd -- gives the search listener above one element per term to
      # toggle `hidden` on, and data-term (lowercased once here, not
      # repeatedly in JS on every keystroke) is what it matches the search
      # box's value against -- the term only, never the definition text,
      # per how this search is meant to work. See definitions_item().
      lapply(VARIABLE_ORDER, function(variable) definitions_item(variable, VARIABLE_DEFINITIONS[[variable]])),
      lapply(names(GLOSSARY_EXTRA_TERMS), function(term) definitions_item(term, GLOSSARY_EXTRA_TERMS[[term]]))
    ),
    # Shown only once a search leaves nothing visible -- same "a message,
    # not a silent blank result" convention the Trends/Rankings/Compare
    # tabs' own no-data guards already follow (see VARIABLE_DEFINITIONS'
    # comment above and scripts/verify_app.R's "UX-state" checks).
    p(id = "definitions-empty", class = "text-muted small", hidden = NA, "No terms match your search.")
  )
}

# Levels up to and including `level` -- e.g. "2-digit" resolves to
# c("Aggregate", "2-digit"), so picking a detail level always keeps the
# coarser rows too rather than switching to only that one level.
industry_levels_upto <- function(level) {
  idx <- match(level, INDUSTRY_LEVEL_ORDER)
  if (length(idx) == 0 || is.na(idx)) return(character(0))
  INDUSTRY_LEVEL_ORDER[seq_len(idx)]
}

# Immediate parent of each 2-digit sub-sector and special aggregation,
# derived once from the "Hierarchy for North American Industry
# Classification System (NAICS)" dot-path in Stats Canada table
# 36-10-0208-01 (that column isn't kept in mfp_data.RData -- only the
# Industry name and IndustryLevel survive the pipeline). Every 2-digit
# sub-sector nests under "Business sector", the table's sole aggregate/root
# -- unlike the old labour productivity table there's no business/
# non-business split here. The 4 "special aggregation" rows (a goods/
# services split and a durable/non-durable manufacturing split) aren't
# literally nested under anything in StatCan's own dot-path (see
# data_pipeline.R's IndustryDepth comment), but are real subsets of
# "Business sector" (the 2 manufacturing ones, of "Manufacturing"
# specifically) -- nested here for a more useful picker tree, not because
# the raw hierarchy encodes it. Used only to order/indent the Industry
# picker.
INDUSTRY_PARENT <- c(
  "Agriculture, forestry, fishing and hunting" = "Business sector",
  "Mining and oil and gas extraction" = "Business sector",
  "Utilities" = "Business sector",
  "Construction" = "Business sector",
  "Manufacturing" = "Business sector",
  "Wholesale trade" = "Business sector",
  "Retail trade" = "Business sector",
  "Transportation and warehousing" = "Business sector",
  "Information and cultural industries" = "Business sector",
  "Finance, insurance, real estate and renting and leasing" = "Business sector",
  "Professional, scientific and technical services" = "Business sector",
  "Other services (except public administration)" = "Business sector",
  "Administrative and support, waste management and remediation services" = "Business sector",
  "Arts, entertainment and recreation" = "Business sector",
  "Accommodation and food services" = "Business sector",
  "Other private services" = "Business sector",
  "Business sector, goods, special aggregation" = "Business sector",
  "Business sector, services, special aggregation" = "Business sector",
  "Non-durable manufacturing, special aggregation" = "Manufacturing",
  "Durable manufacturing, special aggregation" = "Manufacturing"
)

# Nested tree_data for the custom treeSelectInput widget (see
# www/tree_select.js for the paired Shiny.InputBinding), Industry's own
# case: each node is list(value=, label=, children=list(...)), built by
# walking INDUSTRY_PARENT from the single root aggregate ("Business
# sector"). Unlike a flat indented-label vector, the JS side gets real
# parent/child nesting, which is what lets it render a collapsible tree and
# auto-expand ancestors of a search match. Variable has no hierarchy of its
# own, so it builds tree_data with flat_tree_nodes() instead (below).
industry_tree_nodes <- function(df) {
  available <- unique(df$Industry)

  children_of <- function(parent) {
    sort(available[match(available, names(INDUSTRY_PARENT), 0L) > 0 &
                      INDUSTRY_PARENT[available] == parent])
  }
  build_node <- function(name) {
    list(value = name, label = name, children = lapply(children_of(name), build_node))
  }

  roots <- intersect("Business sector", available)
  nodes <- lapply(roots, build_node)

  # Anything present but not reachable from the roots above (e.g. a future
  # industry not yet mapped in INDUSTRY_PARENT) still shows up, as an
  # unindented top-level leaf, rather than silently disappearing from the
  # picker -- same "soft fail" behaviour the old flat-list version had.
  reachable <- unlist(lapply(nodes, flatten_tree_values), use.names = FALSE)
  leftover <- sort(setdiff(available, reachable))
  c(nodes, lapply(leftover, function(name) list(value = name, label = name, children = list())))
}

# Recursively collects every `value` in an industry_tree_nodes() node
# (including itself) -- used above to find industries not reachable from
# the 3 roots.
flatten_tree_values <- function(node) {
  c(node$value, unlist(lapply(node$children, flatten_tree_values), use.names = FALSE))
}

# tree_data for a treeSelectInput() with no hierarchy of its own -- every
# node is a root, `children = list()` -- used for Variable, where
# industry_tree_nodes()'s parent-walk doesn't apply. `choices` is expected
# already in display order (e.g. series_choices()'s VARIABLE_ORDER); unlike
# industry_tree_nodes(), this doesn't re-sort.
flat_tree_nodes <- function(choices) {
  lapply(choices, function(x) list(value = x, label = x, children = list()))
}

# UI generator for the collapsible tree-style dropdown picker used for
# Variable/Industry alike (Trends'/Rankings' sidebar; Industry alone on
# Compare/Data). Mirrors selectizeInput's single-value contract: the value
# Shiny sees is always a plain character(1), or "" for "nothing selected"
# -- never NULL/character(0) -- so req()/isTruthy() gating elsewhere didn't
# need to change just because the widget underneath did. All the
# interactive markup (toggle input, chevron, expandable rows) is built
# client-side by www/tree_select.js from the embedded JSON below; this only
# emits the skeleton the binding hydrates.
treeSelectInput <- function(inputId, label = NULL, tree_data, selected = NULL,
                             placeholder = "Search...", width = NULL) {
  selected <- if (is.null(selected) || identical(selected, character(0))) "" else selected
  div(
    class = "form-group shiny-input-container tree-select-input",
    style = css(width = validateCssUnit(width)),
    if (!is.null(label)) tags$label(class = "control-label", `for` = inputId, label),
    tags$div(
      id = inputId, class = "tree-select",
      `data-selected` = selected, `data-placeholder` = placeholder,
      tags$script(
        type = "application/json", class = "tree-select-data",
        HTML(jsonlite::toJSON(tree_data, auto_unbox = TRUE))
      )
    )
  )
}

# Server-side counterpart to updateSelectizeInput() etc. for the widget
# above -- uses the same session$sendInputMessage() plumbing every built-in
# update*Input() uses, which Shiny dispatches generically to the bound
# element's receiveMessage(el, data) JS method, so no bespoke
# session$sendCustomMessage()/session$onMessage() wiring is needed on
# either side.
updateTreeSelectInput <- function(session, inputId, tree_data = NULL, selected = NULL) {
  message <- dropNulls(list(tree_data = tree_data, selected = selected))
  session$sendInputMessage(inputId, message)
}

# Small local stand-in for shiny:::dropNulls (used internally by every
# built-in update*Input()) -- avoids reaching into shiny's internals with
# ::: for the sake of one two-line helper.
dropNulls <- function(x) x[!vapply(x, is.null, logical(1))]

# Thin wrapper around showNotification() -- every call site in this app
# passes only `ui`/`type`, so this is all that needs wrapping. Centralizes
# duration/behaviour tuning in one place instead of 4 call sites drifting.
# duration=12 (up from Shiny's own 5s default) -- these messages (esp. the
# ones interpolating a list of dropped industries/series) can run long, and
# 5s wasn't enough time to read one before it vanished. The close button
# (closeButton=TRUE is already Shiny's default) is what lets someone
# dismiss it sooner if they don't need the full 12s -- see the
# .shiny-notification-close styling in csls-shiny-theme.css, which is what
# actually makes that button visible/obvious; it already existed in the DOM
# before, just unstyled.
csls_notify <- function(ui, type = c("default", "message", "warning", "error"), duration = 12, ...) {
  showNotification(ui, type = match.arg(type), duration = duration, ...)
}

# data_pipeline.R pulls Stats Canada table 36-10-0208-01 then shapes/
# renames it into Year/Variable/Industry/IndustryLevel/Value/UOM columns.
load_mfp_data <- function(path = MFP_DATA_FILE) {
  if (!file.exists(path)) {
    stop(
      "mfp_data.RData not found. Run data_pipeline.R from this project folder ",
      "first to pull multifactor productivity data from Statistics Canada."
    )
  }
  e <- new.env()
  load(path, envir = e)
  df <- e$mfp_data

  # Fail loudly here -- not just in data_pipeline.R -- because mfp_data.RData
  # is its own process boundary: this file can be hand-edited, land from a
  # differently-shaped pipeline run, or simply be corrupt, independently of
  # whatever data_pipeline.R itself validated before saving it. safe_load_mfp_data()
  # (the only realistic caller of load_mfp_data() -- see its own comment)
  # already turns any thrown error here into a NULL sentinel that ui()/every
  # tab's scoped_raw() already know how to show a clean message for, so a
  # contract violation degrades exactly like a missing/corrupt file rather
  # than needing a UI state of its own. This is also what stops a 0-row
  # (e.g. an empty StatCan pull that made it all the way through) or
  # empty-but-technically-loaded mfp_data.RData from silently reaching ui()
  # and building sliderInput(min=Inf, max=-Inf, ...)-style broken widgets --
  # confirmed empirically to warn rather than error, so ui() previously
  # would have finished "successfully" with a garbled page instead of the
  # clean "Application unavailable" state this now produces.
  validate_data_contract(df, MFP_DATA_CONTRACT, paste0(path, " (loaded contract)"))

  # INDUSTRY_PARENT is a hand-maintained lookup, not derived from this data
  # pull -- industry_choices_tree() already fails *soft* for anything
  # missing from it (shows up unindented at the end of the picker instead
  # of disappearing), but that's easy to miss visually. Log it loudly too,
  # so a future StatCan rename/addition doesn't drift silently forever.
  unmapped <- setdiff(unique(df$Industry), c("Business sector", names(INDUSTRY_PARENT)))
  if (length(unmapped) > 0) {
    warning(
      "load_mfp_data(): ", length(unmapped), " industry name(s) not in INDUSTRY_PARENT -- ",
      "they'll still appear in pickers (unindented, at the end) instead of nested under their ",
      "true parent until INDUSTRY_PARENT is updated: ", paste(unmapped, collapse = ", "),
      call. = FALSE
    )
  }

  df
}
# Shared cache for load_mfp_data(), keyed on the file's mtime -- same
# invalidation check reactiveFileReader (below) already does internally, but
# usable from ui(), which runs per HTTP request *outside* any reactive
# context (reactiveFileReader requires one). Without this, ui() and
# RAW_DATA_READER each independently reloaded the RData file, so a repeat
# page view paid that parse cost again even though the server already had
# an identical copy sitting in RAW_DATA_READER's own reactive cache.
# session = NULL-style sharing: one mutable env for the whole R process, not
# per-session -- consistent with RAW_DATA_READER's own sharing model below.
MFP_DATA_CACHE <- new.env(parent = emptyenv())
cached_load_mfp_data <- function(path = MFP_DATA_FILE) {
  mtime <- file.mtime(path)
  # Keyed on (path, mtime), not mtime alone -- MFP_DATA_FILE never actually
  # changes within one running app process (it's set once, at source time,
  # from an env var), so this only matters for a caller that passes a
  # different `path` than the default within the same R session (as
  # scripts/verify_app.R's tests deliberately do, swapping in different
  # fixture paths against the same sourced app.R environment) -- without the
  # path check, two different files that happened to share an mtime would
  # silently serve each other's cached data instead of ever being told apart.
  if (is.null(MFP_DATA_CACHE$df) || !identical(MFP_DATA_CACHE$path, path) || !identical(MFP_DATA_CACHE$mtime, mtime)) {
    MFP_DATA_CACHE$df <- load_mfp_data(path)
    MFP_DATA_CACHE$mtime <- mtime
    MFP_DATA_CACHE$path <- path
  }
  MFP_DATA_CACHE$df
}

# tryCatch wrapper around cached_load_mfp_data() -- turns a missing/corrupt
# mfp_data.RData into a NULL sentinel instead of a thrown error, so neither
# consumer below (ui()'s per-request load, or RAW_DATA_READER's periodic
# poll) crashes outright. Both call this instead of cached_load_mfp_data()
# directly now. Self-healing: cached_load_mfp_data() only updates
# MFP_DATA_CACHE$mtime on success, so a failed load here doesn't poison the
# cache -- the very next call (next poll, or the next page request) just
# retries load_mfp_data() fresh, and picks it up the moment the file is
# fixed/reappears, no manual recovery step needed. See "Application
# unavailable" (unavailable_page(), below ui()) and the raw_data()-is-NULL
# validate() guards in each tab's scoped_raw().
safe_load_mfp_data <- function(path = MFP_DATA_FILE) {
  tryCatch(
    cached_load_mfp_data(path),
    error = function(e) {
      warning("safe_load_mfp_data(): ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

# Check mfp_data.RData for modifications every 6000 seconds (10 minutes) by default
# but allow tests to override this to avoid waiting 10 real minutes for a test to exercise the reactiveFileReader.
# Session = NULL means this reader isn't tied to (or
# torn down with) any one visitor's session.
RAW_DATA_POLL_MS <- as.numeric(Sys.getenv("RAW_DATA_POLL_MS", 10 * 60 * 1000))
# safe_load_mfp_data (not cached_load_mfp_data directly) -- see MFP_DATA_CACHE
# above for the shared in-memory load, and safe_load_mfp_data() for why this
# needs to never throw: if the file disappears/corrupts mid-session, every
# tab's scoped_raw() guards against raw_data() being NULL (see "No matching
# data" state) instead of this reactive's next poll crashing the whole app.
RAW_DATA_READER <- reactiveFileReader(RAW_DATA_POLL_MS, session = NULL, MFP_DATA_FILE, safe_load_mfp_data)

ordered_unique <- function(values, preferred_order) {
  known <- preferred_order[preferred_order %in% values]
  extra <- unique(values)[!unique(values) %in% known]
  c(known, extra)
}

# Every distinct value of a series dimension (Industry or Variable) present
# in the data, in a stable display order (preferred entries first, the rest
# following the data's own grouping).
series_choices <- function(df, dim_col, preferred_order = character(0)) {
  ordered_unique(df[[dim_col]], preferred_order)
}

# Assign palette colours to series in the order given.
# Overflow series share muted grey -- see series_style_map() below for how
# those overflow series still stay distinguishable from *each other*.
series_color_map <- function(series) {
  n <- length(series)
  colors <- if (n <= length(CATEGORICAL_PALETTE)) {
    CATEGORICAL_PALETTE[seq_len(n)]
  } else {
    c(CATEGORICAL_PALETTE, rep(INK_MUTED, n - length(CATEGORICAL_PALETTE)))
  }
  setNames(colors, series)
}

# Secondary channel (line dash / marker shape) for series identity, indexed
# 1:1 with series_color_map()'s colour assignment -- a colour-blind-friendly
# cue that doesn't depend on hue at all, and the mechanism that keeps
# overflow series (beyond the fixed 8 colours, which never cycle) distinct
# from *each other* despite sharing the same muted grey. LINE_DASH_STYLES/
# MARKER_SYMBOLS are 6-long (not 8), so the dash/shape cycle drifts out of
# phase with the 8-colour cycle instead of always pairing "solid" with slot 1.
series_style_map <- function(series) {
  n <- length(series)
  idx <- seq_len(n)
  list(
    dash = setNames(LINE_DASH_STYLES[((idx - 1) %% length(LINE_DASH_STYLES)) + 1], series),
    symbol = setNames(MARKER_SYMBOLS[((idx - 1) %% length(MARKER_SYMBOLS)) + 1], series)
  )
}

# A comparison series is a specific Industry (table 36-10-0208-01 has no
# Geography dimension worth keeping -- see data_pipeline.R -- so unlike the
# old labour productivity dashboard, a series here is just one industry, not
# an Industry+Geography pair). SeriesLabel/PairKey still exist as their own
# columns rather than reading Industry directly everywhere downstream --
# every chart/chip/CSV-export helper below is written generically against
# "however a series is labeled/keyed", which would need no changes at all if
# a future table swap ever reintroduced a second series dimension.
active_pairs_columns <- function(industry) {
  data.frame(Industry = industry, SeriesLabel = industry, PairKey = industry, stringsAsFactors = FALSE)
}

# The single active series a fresh session starts with. Used identically by
# ui() and server() so the two can never drift apart.
default_pair_row <- function() active_pairs_columns(DEFAULT_INDUSTRY)

# Axis wording for the currently active view -- growth mode ignores
# rebasing entirely (rebasing a series by a constant doesn't change its
# period-over-period % change, so there's nothing for it to reflect).
# Level mode uses the selected variable's own name/unit rather than a
# hardcoded label, since different variables carry different units.
display_axis_title <- function(view_mode, rebase_toggle, base_year, variable, uom) {
  if (identical(view_mode, "growth")) {
    "Annual growth (%)"
  } else if (isTRUE(rebase_toggle)) {
    paste0("Rebased index (", base_year, "=100)")
  } else {
    paste0(variable, " (", uom, ")")
  }
}

# Metric-specific number formatting for a chart's y-axis ticks and hover
# text -- both driven off this one helper so they never drift apart. Returns
# d3-format strings (Plotly's tick/hover formatting language): `tickformat`
# for the numeric part, `prefix`/`suffix` for a currency symbol or a percent
# sign. growth mode and the rebased-to-100 view override the variable's own
# UOM entirely (a growth rate or an index is never denominated in the
# underlying variable's dollars/jobs/hours), so those are checked first.
# Classification below is keyword-matched against the actual UOM strings
# data_pipeline.R's StatCan pull produces for table 36-10-0208-01 ("Index,
# 2017=100" / "Hours" / "Dollars" -- the dollar-denominated variables are
# expressed in millions per StatCan's own convention, same as the old
# labour productivity table's dollar variables before it). The "dollar" +
# "per" rate branch below doesn't match anything this table actually
# produces today (nothing here is a per-hour/per-unit rate the way unit
# labour cost was), but is left in place rather than removed -- it's
# generic, harmless dead code today, and would activate again unprompted if
# a future table or StatCan revision added a per-unit dollar variable. An
# unrecognized future UOM falls through to a plain thousands-separated
# number rather than erroring.
metric_format_spec <- function(uom, view_mode, rebase_toggle) {
  if (identical(view_mode, "growth")) {
    return(list(tickformat = ".1f", prefix = "", suffix = "%"))
  }
  if (isTRUE(rebase_toggle)) {
    return(list(tickformat = ".1f", prefix = "", suffix = ""))
  }
  uom <- if (is.null(uom)) "" else uom
  if (grepl("percent", uom, ignore.case = TRUE)) {
    list(tickformat = ".1f", prefix = "", suffix = "%")
  } else if (grepl("dollar", uom, ignore.case = TRUE) && grepl("per", uom, ignore.case = TRUE)) {
    # Rates -- productivity, compensation/hour, unit labour cost -- stay in
    # small, meaningful figures, so cents-level precision rather than an
    # abbreviated/rounded-off value.
    list(tickformat = ",.2f", prefix = "$", suffix = "")
  } else if (grepl("dollar", uom, ignore.case = TRUE)) {
    # Levels -- value added, total compensation -- already expressed "in
    # thousands of dollars" by the data itself, so whole dollars, comma-grouped.
    list(tickformat = ",.0f", prefix = "$", suffix = "")
  } else if (grepl("jobs|hours", uom, ignore.case = TRUE)) {
    list(tickformat = ",.0f", prefix = "", suffix = "")
  } else {
    list(tickformat = ",.1f", prefix = "", suffix = "")
  }
}

# Chart title for the Trend and Bar Chart tabs -- just the variable and the
# selected time frame, so it updates automatically as either changes rather
# than staying a static/generic title.
display_chart_title <- function(variable, year_range) {
  paste0(variable, " (", year_range[1], "-", year_range[2], ")")
}

# Shared row/column shaping for both the Data Table and the CSV export, so
# the download is always a WYSIWYG match of what's on screen. Exports the
# underlying dimensions (Industry, Variable, UOM) rather than the internal
# SeriesLabel convenience column. No Geography column -- see
# active_pairs_columns()'s own comment for why this table has none to export.
build_export_df <- function(df, rebase_toggle, base_year) {
  df <- df %>% arrange(SeriesLabel, Year)
  cols <- c("Year", "Industry", "Variable", "Value", "UOM", "GrowthPct")
  if (isTRUE(rebase_toggle)) cols <- c(cols, "RebasedValue")
  df[, cols]
}

export_column_labels <- function(cols, base_year, variable, uom) {
  labels <- c(
    Year = "Year", Industry = "Industry", Variable = "Variable",
    Value = paste0(variable, " (", uom, ")"),
    UOM = "Unit", GrowthPct = "Annual growth (%)"
  )
  if ("RebasedValue" %in% cols) {
    labels["RebasedValue"] <- paste0("Rebased index (", base_year, "=100)")
  }
  labels[cols]
}

# Rebases `value` to a percentage of `base_value` (100 = base year level).
# Returns NA -- not Inf/NaN -- whenever that's undefined: a missing base
# value, or a base value of exactly 0. A raw StatCan value of 0 genuinely
# occurs (e.g. a granular industry with no measured activity in a small
# province/year), and naive division would silently produce +/-Inf that
# slips straight past every `is.na()` guard in this file (is.na(Inf) is
# FALSE in R) and can blow out a chart's whole axis with one bad point.
safe_index_to_100 <- function(value, base_value) {
  ifelse(is.na(value) | is.na(base_value) | base_value == 0, NA_real_, value / base_value * 100)
}

# Compound annual growth rate from `start_value` to `end_value` over
# `n_years`. NA (not Inf/NaN) whenever it's undefined: a missing/non-
# positive start value (division by zero or a meaningless negative base),
# a missing end value, or a non-positive year span. A start value of 0 is
# the same "real StatCan zero" case safe_index_to_100() guards against --
# an end value of 0 is left alone (a genuine, meaningful -100% CAGR).
compute_cagr <- function(start_value, end_value, n_years) {
  bad_start <- is.na(start_value) | is.na(n_years) | start_value <= 0 | n_years <= 0
  ratio <- end_value / start_value
  ifelse(bad_start | is.na(ratio), NA_real_, ratio^(1 / n_years) - 1)
}

# True if `ancestor` is `industry` itself or one of its ancestors in the
# Industry hierarchy (see INDUSTRY_PARENT) -- used to flag when two active
# series would double-count each other for additive measures (labour
# compensation, capital cost, etc.): "Business sector" and "Manufacturing"
# aren't independent series, one contains the other.
industry_is_ancestor <- function(ancestor, industry) {
  seen <- character(0)
  current <- industry
  repeat {
    if (identical(current, ancestor)) return(TRUE)
    if (current %in% seen) return(FALSE) # cycle guard; shouldn't happen
    seen <- c(seen, current)
    # INDUSTRY_PARENT is a plain named character vector, not a list -- `[[`
    # errors ("subscript out of bounds") on a name it doesn't contain,
    # instead of returning NULL. `[` returns NA instead, which the root
    # category ("Business sector") hits once walked up to, since it has no
    # parent entry of its own -- nothing beyond it to bridge to, unlike the
    # old labour productivity table's 3-root hierarchy.
    parent <- unname(INDUSTRY_PARENT[current])
    if (is.na(parent)) return(FALSE)
    current <- parent
  }
}

# Provenance for whoever's looking at the numbers -- without this, two users
# (or the same user before/after a scheduled data refresh) could see
# different figures with no indication why. Rendered per-tab (via
# uiOutput(ns("data_asof")) + this in each tab's own renderUI) so it sits
# directly under that tab's own "Source: ..." line rather than as a single
# element trailing the whole tab region below the card.
data_asof_ui <- function() {
  p(
    # margin: 0 explicit -- this <p> sits one level inside its own
    # uiOutput()'s wrapper div, itself now nested inside a further wrapper
    # alongside the "Source" line (see trend_tab_ui() etc.) so the two read
    # as one tight block. bslib's own gap-spacing CSS has a rule for
    # exactly this shape (a <p> inside a directly-nested shiny-html-output)
    # but only when that output div is a *direct* child of the fill column
    # -- nested one level further than that, as it is now, it no longer
    # matches, so this is set explicitly rather than depended on.
    class = "text-muted small", style = "margin: 0;",
    "Data last refreshed: ", format(file.mtime(MFP_DATA_FILE), "%Y-%m-%d")
  )
}

# The "Source: Statistics Canada Table ..." + data_asof_ui() footer pair
# shared by all 4 tabs, immediately below their chart/table. Grouped into
# one wrapper div, not two separate top-level children -- bslib's own
# fill-layout puts a 24px `gap` between EVERY direct child of the fill
# column (chart / Source / data_asof), so as two siblings these would read
# with the same 24px gap between them as between the chart and "Source"
# above; one wrapper collapses that to a single gap (chart -> this pair),
# with the two lines themselves falling back to plain block stacking (0
# margin on either <p>, so they sit immediately consecutive -- visually one
# paragraph, still two elements for data_asof_ui() to independently
# re-render into). The reclaimed 24px goes straight to the chart above,
# which is the only flex:1 (fill) child in this column.
source_and_asof_ui <- function(ns) {
  tags$div(
    # margin-bottom explicit here -- bslib's own `.bslib-gap-spacing > p
    # { margin-bottom: 0 }` (what kept this flush before) only matches a
    # *direct* child of the fill column; nested one level down inside this
    # wrapper div, that rule no longer applies and this would otherwise
    # fall back to Bootstrap's default 1rem <p> margin.
    p(
      class = "text-muted small", style = "margin-bottom: 0;",
      paste0("Source: Statistics Canada Table ", STATCAN_TABLE_ID)
    ),
    uiOutput(ns("data_asof"))
  )
}

# Shared "Download" dropdown for the Trends, Rankings, Compare, and Data
# tabs -- replaces each tab's old single-purpose "Download CSV" button with
# a menu tailored to what that tab can actually offer:
#   - Trends/Rankings: chart-as-PNG + displayed data.
#   - Compare: chart-as-PNG + displayed data (kind == "bar" -- it has a
#     chart, but no standing notion of "the full dataset" independent of
#     the current Variable/pairs selection).
#   - Data: displayed data + full dataset (kind == "table" -- it renders a
#     table, not a chart, so no PNG option; and it's the one tab already
#     built around showing/exporting a whole scoped table, so it's the
#     natural home for a full-dataset export too).
#
# The PNG option needs no server-side downloadHandler: it's wired to
# downloadChartPng() in www/ui_helpers.js, which reaches into the already-
# rendered Plotly graph div client-side (via Plotly's own PNG exporter) --
# no round-trip to the server, and no server-side image rendering to keep in
# sync with what's on screen. The CSV option(s) are real downloadHandlers,
# same mechanism the old single button used, just re-styled as dropdown-item
# links (downloadLink(), not downloadButton(), so they don't carry their own
# conflicting btn styling inside the menu).
download_menu_ui <- function(ns, chart_id = ns("chart"), include_png = TRUE, full_dataset = FALSE) {
  tags$div(
    class = "dropdown download-dropdown",
    tags$button(
      class = "btn btn-default dropdown-toggle", type = "button",
      id = ns("download_menu_toggle"),
      `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
      icon("download"), " Download"
    ),
    tags$ul(
      class = "dropdown-menu", `aria-labelledby` = ns("download_menu_toggle"),
      if (isTRUE(include_png)) {
        tags$li(tags$button(
          class = "dropdown-item", type = "button",
          onclick = sprintf("downloadChartPng('%s')", chart_id),
          "Download chart as PNG"
        ))
      },
      tags$li(downloadLink(ns("download_csv"), "Download displayed data (.csv)", class = "dropdown-item")),
      if (isTRUE(full_dataset)) {
        tags$li(downloadLink(ns("download_full"), "Download full dataset (.csv)", class = "dropdown-item"))
      }
    )
  )
}

# The Trends tab: a focused single-series view (one Variable + one
# Industry -> one line), unlike the other 3 tabs which still compare
# multiple industries at once. Kept as its own dedicated module rather than
# another "kind" of tab_module_ui/
# tab_module_server below, since its sidebar shape and reactive pipeline
# are genuinely different (no active_pairs/Add-series/multi-color
# machinery), not just a different final render step.
trend_tab_ui <- function(id, init_df, variable_choices, industry_tree) {
  ns <- NS(id)

  card(
    # card-sidebar -- zeroes this card's own left padding (see the rule
    # itself, alongside the nav-pills CSS) so the sidebar's tinted box sits
    # flush with the card's true left edge instead of getting doubly inset
    # by both the card's own padding AND the sidebar's -- that double-inset
    # was what put the sidebar's visible edge ~40px to the right of the
    # page's own left edge (the intro text/nav-pills), instead of aligned
    # with it.
    class = "card-sidebar",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        # Variable/Industry both use the same treeSelectInput widget here
        # (see www/tree_select.js) rather than Shiny's selectizeInput, so
        # both behave identically -- click/tab in blanks the box for a
        # fresh search or scroll, picking an option (or not) is what
        # shows/restores the display text. Variable has no hierarchy of its
        # own, so its tree_data is just flat_tree_nodes() over the plain
        # choice list -- same flat-list rendering Industry's own leaf rows
        # already use. No Geography picker -- table 36-10-0208-01 covers
        # Canada only (see MFP_DATA_FILE's own comment up top).
        treeSelectInput(
          ns("variable"), "Variable",
          tree_data = flat_tree_nodes(variable_choices), selected = DEFAULT_VARIABLE,
          placeholder = "Search variables..."
        ),
        uiOutput(ns("variable_definition")),
        # Always the full Aggregate+2-digit tree -- no separate
        # industry_level toggle (none of the tabs have one). Collapsible
        # tree dropdown -- closed to just the root aggregate by default,
        # arrow to expand a branch, click a label to pick it.
        treeSelectInput(
          ns("industry"), "Industry",
          tree_data = industry_tree, selected = DEFAULT_INDUSTRY,
          placeholder = "Search industries..."
        ),
        # A native <details> disclosure -- zero extra JS dependencies,
        # keyboard-accessible by default. Verified (headless-browser +
        # shinytest2) that a sliderInput initialized while collapsed inside
        # this renders and syncs identically to one that was never hidden,
        # so no "re-init on open" workaround is needed.
        tags$details(
          id = ns("more_options"), class = "trend-more-options",
          tags$summary("More options"),
          sliderInput(
            ns("year_range"), "Date range",
            min = min(init_df$Year), max = max(init_df$Year),
            value = c(min(init_df$Year), max(init_df$Year)),
            step = 1, sep = ""
          ),
          radioButtons(
            ns("view_mode"), "View values as",
            choices = c("Level" = "level", "Annual percentage change" = "growth"),
            # Stacked rather than inline -- "Annual percentage change" is
            # too long to sit next to "Level" on one line in the sidebar.
            selected = "level", inline = FALSE
          ),
          conditionalPanel(
            "input.view_mode == 'level'", ns = ns,
            checkboxInput(ns("rebase_toggle"), "Set each series to 100 in a selected year", value = FALSE),
            conditionalPanel(
              "input.view_mode == 'level' && input.rebase_toggle == true", ns = ns,
              selectInput(
                ns("base_year"), "Base year",
                choices = sort(unique(init_df$Year)), selected = min(init_df$Year),
                selectize = FALSE
              )
            )
          ),
          download_menu_ui(ns)
        )
      ),
      plotlyOutput(ns("chart"), height = "100%"),
      # See source_and_asof_ui()'s own comment for why this is one wrapper
      # div rather than two separate top-level children here.
      source_and_asof_ui(ns)
    )
  )
}

trend_tab_server <- function(id, raw_data, variable_uom_lookup) {
  moduleServer(id, function(input, output, session) {

    # Keeps Variable/Industry/time-frame/base-year in sync with what's in
    # the data, preserving the user's current picks where still valid.
    # Unlike the multi-pair tabs, an invalid pick falls back to the
    # DEFAULT_* constants rather than going blank -- Industry is an
    # always-active single selection here, not an "add to compare" picker
    # that's allowed to sit empty.
    #
    # ignoreInit = TRUE: this only needs to fire when the underlying file
    # actually changes -- ui() just built this session's initial widgets
    # from the same raw_data() a moment ago, so re-running it again on
    # session connect (bindEvent()'s default) redid that work for nothing
    # and re-pushed an identical Industry tree over the websocket.
    observe({
      # req(), not a bare assignment -- raw_data() can be NULL if
      # safe_load_mfp_data() ever fails mid-session (see "No matching data"
      # state); this just leaves the pickers as they were rather than
      # erroring on df$Variable/df$Industry below.
      df <- req(raw_data())

      variable_choices <- series_choices(df, "Variable", VARIABLE_ORDER)
      new_variable <- if (is.null(input$variable) || !(input$variable %in% variable_choices)) {
        DEFAULT_VARIABLE
      } else {
        input$variable
      }
      updateTreeSelectInput(session, "variable", tree_data = flat_tree_nodes(variable_choices), selected = new_variable)

      new_industry <- if (is.null(input$industry) || !(input$industry %in% unique(df$Industry))) {
        DEFAULT_INDUSTRY
      } else {
        input$industry
      }
      updateTreeSelectInput(session, "industry", tree_data = industry_tree_nodes(df), selected = new_industry)

      year_min <- min(df$Year)
      year_max <- max(df$Year)
      current_range <- input$year_range
      range_value <- if (is.null(current_range)) {
        c(year_min, year_max)
      } else {
        c(max(current_range[1], year_min), min(current_range[2], year_max))
      }
      updateSliderInput(session, "year_range", min = year_min, max = year_max, value = range_value)

      year_choices <- sort(unique(df$Year))
      current_base <- suppressWarnings(as.numeric(input$base_year))
      new_base <- if (is.null(input$base_year) || is.na(current_base) || !(current_base %in% year_choices)) {
        min(year_choices)
      } else {
        current_base
      }
      updateSelectInput(session, "base_year", choices = year_choices, selected = new_base)
    }) |> bindEvent(raw_data(), once = FALSE, ignoreInit = TRUE)

    # Single (Variable, Industry) match -- no SeriesLabel filtering/looping
    # needed, but SeriesLabel is still added so build_export_df()/
    # export_column_labels() work completely unchanged.
    scoped_raw <- reactive({
      # NULL only if safe_load_mfp_data() failed mid-session (mfp_data.RData
      # deleted/corrupted after this session already connected) -- a
      # validate(), not req(), so every chart/table downstream shows this
      # message instead of just going blank. See "No matching data" state.
      validate(need(!is.null(raw_data()), "Data is temporarily unavailable -- please try again in a moment."))
      req(input$variable, input$industry)
      raw_data() %>%
        filter(Variable == input$variable, Industry == input$industry) %>%
        mutate(SeriesLabel = input$industry)
    })

    # UOM is 1:1 per Variable (see variable_uom_lookup() in server(), shared
    # across all 4 tabs) -- a small lookup keyed by Variable, not a fresh
    # filter() over the full raw_data() every time this tab's
    # Variable picker changes.
    variable_uom <- reactive({
      lookup <- variable_uom_lookup()
      lookup$UOM[lookup$Variable == input$variable][1]
    })

    history_with_growth <- reactive({
      validate(need(nrow(scoped_raw()) > 0, "No data for this variable/industry combination."))
      scoped_raw() %>%
        arrange(Year) %>%
        mutate(GrowthPct = (Value / lag(Value) - 1) * 100)
    })

    transform_result <- reactive({
      df <- history_with_growth()
      base_year_num <- suppressWarnings(as.numeric(input$base_year))

      if (isTRUE(input$rebase_toggle) && !is.na(base_year_num)) {
        base_row <- df %>% filter(Year == base_year_num)
        base_value <- if (nrow(base_row) > 0) base_row$Value[1] else NA_real_
        df$RebasedValue <- safe_index_to_100(df$Value, base_value)
        # "Missing" covers both no row at the base year at all AND a base
        # value of exactly 0 (rebasing to a 0 is undefined, not just rare --
        # see safe_index_to_100()) -- either way there's nothing to show.
        missing_base <- is.na(base_value) || isTRUE(base_value == 0)
      } else {
        df$RebasedValue <- NA_real_
        missing_base <- FALSE
      }

      df$DisplayValue <- if (identical(input$view_mode, "growth")) {
        df$GrowthPct
      } else if (isTRUE(input$rebase_toggle)) {
        df$RebasedValue
      } else {
        df$Value
      }

      list(df = df, missing_base = missing_base)
    })

    display_data <- reactive(transform_result()$df)

    rebase_missing <- reactive({
      if (identical(input$view_mode, "growth")) FALSE else isTRUE(transform_result()$missing_base)
    })

    observeEvent(rebase_missing(), {
      if (isTRUE(rebase_missing())) {
        csls_notify(
          paste0(
            "No ", input$base_year, " data for this series -- excluded from the rebased view."
          ),
          type = "warning"
        )
      }
    })

    filtered_data <- reactive({
      req(input$year_range)
      display_data() %>%
        filter(Year >= input$year_range[1], Year <= input$year_range[2])
    })

    output$chart <- renderPlotly({
      df <- filtered_data() %>% filter(!is.na(DisplayValue)) %>% arrange(Year)
      # Distinct from history_with_growth()'s own validate() above -- rows
      # can exist there (a real Variable/Industry match) but still
      # end up all-NA here, e.g. Growth view with only one year in range
      # (lag() has nothing to diff against). Previously a bare req(), which
      # blanked the chart with no explanation, indistinguishable from still
      # loading -- see "No matching data" state.
      validate(need(
        nrow(df) > 0,
        "No values to plot for this view -- try widening the year range, switching off Growth view, or turning off Rebase."
      ))
      axis_title <- display_axis_title(input$view_mode, input$rebase_toggle, input$base_year, input$variable, variable_uom())
      fmt <- metric_format_spec(variable_uom(), input$view_mode, input$rebase_toggle)
      line_color <- CATEGORICAL_PALETTE[1]
      # Chart title only states the variable/timeframe -- with a single
      # line there's no legend to identify which industry it is, so name it
      # as a subtitle. Value leads (bold) with the series name following on
      # its own line in the hover, same ordering as Compare's multi-series
      # tooltip -- see the comment there for why the name still needs to be
      # in the template even under "x unified" hovermode (its per-row
      # colour swatch isn't a substitute for text).
      subtitle <- input$industry

      plot_ly(
        data = df, x = ~Year, y = ~DisplayValue, name = subtitle,
        type = "scatter", mode = "lines+markers",
        line = list(color = line_color, width = 2),
        marker = list(color = line_color, size = 8),
        hovertemplate = paste0(
          "<b>%{y:", fmt$prefix, fmt$tickformat, "}", fmt$suffix, "</b><br>", subtitle, "<extra></extra>"
        )
      ) %>%
        layout(
          title = list(text = paste0(
            display_chart_title(input$variable, input$year_range),
            "<br><sup style='color:", INK_MUTED, "'>", subtitle, "</sup>"
          )),
          # nticks (not a fixed dtick) lets Plotly's own auto-tick engine
          # pick a clean step (2/5/10 years) that fits the actually-rendered
          # width, recalculated on every resize -- ~8 lands in the requested
          # 6-10 label range without hard-coding a tick-every-year step that
          # gets crowded over a long date range.
          xaxis = list(title = "Year", nticks = 8, tickformat = "d", gridcolor = GRIDLINE, color = INK_MUTED),
          yaxis = list(
            title = axis_title, gridcolor = GRIDLINE, color = INK_MUTED,
            tickformat = fmt$tickformat, tickprefix = fmt$prefix, ticksuffix = fmt$suffix
          ),
          paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
          font = list(color = INK_PRIMARY, family = FONT_FAMILY),
          showlegend = FALSE,
          hovermode = "x unified",
          # A visible zero line in growth mode -- "above/below flat" is the
          # first thing a reader wants out of an annual-% -change chart, and
          # Plotly's default zeroline is unstyled/easy to miss against the
          # gridlines otherwise.
          shapes = if (identical(input$view_mode, "growth")) {
            list(list(
              type = "line", xref = "paper", x0 = 0, x1 = 1,
              yref = "y", y0 = 0, y1 = 0,
              line = list(color = INK_MUTED, width = 1, dash = "dot")
            ))
          }
        )
    })

    output$variable_definition <- renderUI(variable_definition_ui(input$variable))

    # raw_data() is the dependency, not the value used -- reading it just
    # ties this to the same reactiveFileReader invalidation as this tab's
    # own data, so the "as of" date updates the moment a new pipeline run
    # lands, without this needing its own poll loop.
    output$data_asof <- renderUI({
      raw_data()
      data_asof_ui()
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        mode_part <- if (identical(input$view_mode, "growth")) {
          "annual-growth"
        } else if (isTRUE(input$rebase_toggle)) {
          paste0("rebased-", input$base_year)
        } else {
          "index"
        }
        sprintf(
          "productivity_%s_%s_%s_%s-%s_%s.csv",
          gsub("[^A-Za-z0-9]+", "-", input$variable),
          gsub("[^A-Za-z0-9]+", "-", input$industry),
          mode_part, input$year_range[1], input$year_range[2], format(Sys.Date(), "%Y%m%d")
        )
      },
      content = function(file) {
        df <- build_export_df(filtered_data(), input$rebase_toggle, input$base_year)
        write.csv(df, file, row.names = FALSE)
      }
    )
  })
}

# The Rankings tab: pick one Variable (+ a maximum Industry detail level),
# see every matching industry's compound annual growth rate (CAGR) over the
# full time span of the data, as a scatter plot. Unlike Trends/Compare
# there's no picker for a *specific* industry -- all industries up to the
# chosen detail level are shown at once -- so this gets its own dedicated
# module rather than another tab_module_ui/tab_module_server "kind", same
# reasoning as the Trends tab.
ranking_tab_ui <- function(id, init_df, variable_choices) {
  ns <- NS(id)

  card(
    # card-sidebar -- see the matching comment on the Trends tab's card().
    # ranking-tab-card mirrors the Data tab's table-tab-card (see
    # tab_module_ui()/its matching CSS): a no-op on its own, it only lets
    # this card scroll instead of clip once output$chart_container below
    # switches the chart to a real, content-driven pixel height.
    class = "card-sidebar ranking-tab-card",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        # Same treeSelectInput widget as the Trends tab's Variable picker
        # (see www/tree_select.js and the matching comment on the Trends
        # tab's sidebar) -- click/tab in blanks the box for a fresh search
        # or scroll, picking an option (or not) is what shows/restores the
        # display text. No Geography picker -- table 36-10-0208-01 covers
        # Canada only (see MFP_DATA_FILE's own comment up top).
        treeSelectInput(
          ns("variable"), "Variable",
          tree_data = flat_tree_nodes(variable_choices), selected = DEFAULT_VARIABLE,
          placeholder = "Search variables..."
        ),
        # renderUI (not a static goto_definition_link() call) since the
        # term it should jump to tracks whichever Variable is currently
        # selected -- see ranking_tab_server()'s own output$definition_link.
        uiOutput(ns("definition_link")),
        sliderInput(
          ns("year_range"), "Date range",
          min = min(init_df$Year), max = max(init_df$Year),
          value = c(min(init_df$Year), max(init_df$Year)),
          step = 1, sep = ""
        ),
        radioButtons(
          ns("industry_level"), "Industry classification level",
          choices = c(
            "Economy-wide and broad aggregates" = "Aggregate",
            "Sector level" = "2-digit"
          ),
          # Stacked rather than inline -- the longer labels above would
          # wrap awkwardly across a narrow sidebar as a horizontal row.
          selected = DEFAULT_INDUSTRY_LEVEL, inline = FALSE
        ),
        # Reuses the Trends tab's .trend-more-options styling (chevron
        # summary, no default browser triangle) -- the class name is
        # generic despite where it was first introduced.
        tags$details(
          id = ns("more_options"), class = "trend-more-options",
          tags$summary("More options"),
          radioButtons(
            ns("chart_type"), "Chart type",
            choices = c("Scatter" = "scatter", "Bar" = "bar"), selected = "bar", inline = TRUE
          ),
          download_menu_ui(ns)
        )
      ),
      # The actual plotlyOutput lives in output$chart_container (renderUI)
      # server-side instead of statically here -- once there are more
      # industries than fit on one screen (see RANKING_CHART_ROW_THRESHOLD),
      # it needs a real, content-driven pixel height instead of the usual
      # height="100%" fill, so the card above can scroll to it rather than
      # clipping it. Below that threshold it renders the exact same
      # plotlyOutput(height="100%") this replaces.
      uiOutput(ns("chart_container"), fill = TRUE),
      # See source_and_asof_ui()'s own comment for why this is one wrapper
      # div rather than two separate top-level children here.
      source_and_asof_ui(ns)
    )
  )
}

ranking_tab_server <- function(id, raw_data, variable_uom_lookup) {
  moduleServer(id, function(input, output, session) {

    # Same DEFAULT_*-fallback sync pattern as the Trends tab -- Variable is
    # an always-active single selection here too, never blank. ignoreInit =
    # TRUE -- see the matching comment on the Trends tab's own sync
    # observe(): ui() already built this session's initial widgets from the
    # same raw_data(), so this only needs to fire on a real data change.
    observe({
      # req(), not a bare assignment -- see the matching comment on the
      # Trends tab's own sync observe().
      df <- req(raw_data())

      variable_choices <- series_choices(df, "Variable", VARIABLE_ORDER)
      new_variable <- if (is.null(input$variable) || !(input$variable %in% variable_choices)) {
        DEFAULT_VARIABLE
      } else {
        input$variable
      }
      updateTreeSelectInput(session, "variable", tree_data = flat_tree_nodes(variable_choices), selected = new_variable)

      year_min <- min(df$Year)
      year_max <- max(df$Year)
      current_range <- input$year_range
      range_value <- if (is.null(current_range)) {
        c(year_min, year_max)
      } else {
        c(max(current_range[1], year_min), min(current_range[2], year_max))
      }
      updateSliderInput(session, "year_range", min = year_min, max = year_max, value = range_value)
    }) |> bindEvent(raw_data(), once = FALSE, ignoreInit = TRUE)

    # See goto_definition_link()'s own comment -- req() rather than a
    # DEFAULT_VARIABLE fallback since a NULL input$variable here is only
    # ever a brief moment before the session's first render finishes, not a
    # state this link needs to paper over.
    output$definition_link <- renderUI({
      req(input$variable)
      goto_definition_link(input$variable, "Definition")
    })

    scoped_raw <- reactive({
      # See the matching comment on the Trends tab's own scoped_raw().
      validate(need(!is.null(raw_data()), "Data is temporarily unavailable -- please try again in a moment."))
      req(input$variable, input$industry_level)
      raw_data() %>%
        filter(Variable == input$variable, IndustryLevel %in% industry_levels_upto(input$industry_level))
    })

    # UOM is 1:1 per Variable (see variable_uom_lookup() in server(), shared
    # across all 4 tabs) -- a small lookup keyed by Variable, not a fresh
    # filter() over the full raw_data() every time this tab's Variable
    # picker changes.
    variable_uom <- reactive({
      lookup <- variable_uom_lookup()
      lookup$UOM[lookup$Variable == input$variable][1]
    })

    # CAGR from the start to the end of the selected time frame.
    ranking_data <- reactive({
      df <- scoped_raw()
      validate(need(nrow(df) > 0, "No data for this variable."))
      rng <- req(input$year_range)
      start_year <- rng[1]
      end_year <- rng[2]
      validate(need(end_year > start_year, "Select a time frame spanning at least two years to compute CAGR."))

      start_df <- df %>% filter(Year == start_year) %>% select(Industry, StartValue = Value)
      end_df <- df %>% filter(Year == end_year) %>% select(Industry, EndValue = Value)
      joined <- inner_join(start_df, end_df, by = "Industry")
      joined$CAGR <- compute_cagr(joined$StartValue, joined$EndValue, end_year - start_year)
      joined
    })

    # A custom time frame can land on years some industries lack data for
    # (added/discontinued series) -- inner_join() above silently drops those
    # rows entirely. Separately, an industry can have a row at *both* years
    # but still end up with an undefined CAGR (e.g. a start value of exactly
    # 0 -- compute_cagr() returns NA rather than Inf for that). Both cases
    # mean "not shown", so both get surfaced the same way instead of one
    # silently shrinking the chart with no explanation.
    ranking_dropped_industries <- reactive({
      rd <- tryCatch(ranking_data(), error = function(e) NULL)
      if (is.null(rd)) return(character(0))
      all_industries <- unique(scoped_raw()$Industry)
      shown <- rd$Industry[!is.na(rd$CAGR)]
      setdiff(all_industries, shown)
    })

    observeEvent(ranking_dropped_industries(), {
      dropped <- ranking_dropped_industries()
      if (length(dropped) > 0) {
        csls_notify(
          paste0(
            "Missing start/end-year data for: ", paste(dropped, collapse = ", "),
            " -- excluded from the ranking."
          ),
          type = "warning"
        )
      }
    })

    # Shared by output$chart_container (which needs just the row count, to
    # decide how tall the chart should be) and output$chart (which needs
    # the actual data) -- factored out so both agree on exactly the same
    # rows, instead of the row count and the rendered chart risking drift
    # from two separately-filtered copies.
    ranking_chart_data <- reactive({
      rd <- ranking_data() %>% filter(!is.na(CAGR)) %>% arrange(CAGR)
      rd$Industry <- factor(rd$Industry, levels = rd$Industry)
      rd
    })

    # tryCatch mirrors ranking_dropped_industries() above -- ranking_data()
    # can itself be mid-validate() (e.g. "No data for this variable",
    # a time frame spanning under 2 years), and
    # that condition would otherwise propagate straight through here into
    # output$chart_container's own renderUI, replacing the plotlyOutput it
    # builds with a validation message of its own -- which would leave
    # output$chart with no output element left to render its more specific
    # "No industries have data..." message into. Falling back to 0 rows
    # instead just keeps the chart at its normal one-screen size, exactly
    # as if nothing were selected -- output$chart's own validate() below
    # still fires and shows the real message once it renders into that.
    ranking_chart_row_count <- reactive({
      tryCatch(nrow(ranking_chart_data()), error = function(e) 0L)
    })

    output$chart_container <- renderUI({
      n <- ranking_chart_row_count()
      if (n > RANKING_CHART_ROW_THRESHOLD) {
        # fill = FALSE -- a real, content-driven pixel height instead of
        # the usual 100%-of-card fill, so ranking-tab-card's own
        # overflow-y: auto (see the CSS below) can scroll to the rest of
        # it instead of clipping/squeezing every row into one screen.
        plotlyOutput(session$ns("chart"), height = paste0(n * RANKING_CHART_PX_PER_ROW, "px"), fill = FALSE)
      } else {
        # Today's exact behaviour, byte-for-byte, whenever there aren't
        # enough rows for that to be worth doing (in practice: Aggregate
        # and Sector level, ~3 and ~28 rows respectively).
        plotlyOutput(session$ns("chart"), height = "100%", fill = TRUE)
      }
    })

    output$chart <- renderPlotly({
      rd <- ranking_chart_data()
      # Distinct from ranking_data()'s own validate() -- rows can exist
      # there but still end up all-NA CAGR here (every matched industry
      # lacks a value at the start or end year, or has a 0 start value).
      # Previously a bare req() -- see "No matching data" state.
      validate(need(
        nrow(rd) > 0,
        "No industries have data at both the start and end of the selected time frame -- pick a different range."
      ))
      n <- nrow(rd)
      # Mirrors output$chart_container's own threshold check -- both need
      # to agree on whether this render is "too many rows for one side at
      # normal size" (see RANKING_CHART_ROW_THRESHOLD's own comment for
      # why 40, and why in practice only Sub-sector level crosses it).
      split_mode <- n > RANKING_CHART_ROW_THRESHOLD

      # Same ranked CAGR data either way -- scatter (one point per industry,
      # no bar length) reads better once the 3-digit level pulls in 100+
      # industries and overlapping bars get visually noisy; bar is the more
      # familiar shape for a shorter (e.g. Aggregate-level) list.
      p <- if (identical(input$chart_type, "bar")) {
        plot_ly(
          data = rd, x = ~CAGR * 100, y = ~Industry,
          type = "bar", orientation = "h",
          marker = list(color = CATEGORICAL_PALETTE[1]),
          hovertemplate = "%{y}: %{x:.2f}%<extra></extra>"
        )
      } else {
        plot_ly(
          data = rd, x = ~CAGR * 100, y = ~Industry,
          type = "scatter", mode = "markers",
          marker = list(color = CATEGORICAL_PALETTE[1], size = 9),
          hovertemplate = "%{y}: %{x:.2f}%<extra></extra>"
        )
      }

      # Below the threshold, this is exactly today's single left-side axis
      # at normal size. Above it, industries alternate by rank parity onto
      # a left axis (odd ranks) and a right axis (even ranks) -- alternating
      # rather than splitting top-half/bottom-half, because *adjacent* bars
      # are always exactly 1 row apart either way; only alternating actually
      # doubles the vertical room each side's own labels get, uniformly
      # across the whole list.
      if (split_mode) {
        levels_all <- levels(rd$Industry)
        rank <- seq_along(levels_all)
        left_labels <- levels_all[rank %% 2 == 1]
        right_labels <- levels_all[rank %% 2 == 0]
        axis_range <- c(-0.5, n - 0.5) # plotly's own default category-axis padding
        yaxis_left <- list(
          title = "", type = "category",
          categoryorder = "array", categoryarray = levels_all,
          tickmode = "array", tickvals = left_labels, ticktext = left_labels,
          range = axis_range, automargin = TRUE,
          gridcolor = GRIDLINE, color = INK_MUTED,
          tickfont = list(size = RANKING_CHART_TICKFONT_SPLIT)
        )
      } else {
        yaxis_left <- list(title = "", gridcolor = GRIDLINE, color = INK_MUTED)
      }

      p <- p %>% layout(
        title = paste0(
          input$variable, " by industry",
          " (", input$year_range[1], "-", input$year_range[2], ")"
        ),
        xaxis = list(title = "Compound annual growth rate (%)", gridcolor = GRIDLINE, color = INK_MUTED),
        yaxis = yaxis_left,
        paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
        font = list(color = INK_PRIMARY, family = FONT_FAMILY)
      )

      if (split_mode) {
        # Plotly.js only actually draws (and reserves automargin space
        # for) a y-axis that has at least one trace bound to it -- a bare
        # layout(yaxis2 = ...) with no trace ever assigned yaxis = "y2" is
        # silently never rendered at all, confirmed against the live app
        # (0 tick-label DOM nodes despite automargin = TRUE). This
        # invisible dummy trace exists purely to make yaxis2 real: NA x
        # values draw nothing, marker opacity 0/hoverinfo "skip"/
        # showlegend FALSE keep it from being seen, hovered, or listed.
        # inherit = FALSE stops it picking up the real trace's x/y/type
        # mapping from the original plot_ly() call above.
        p <- p %>% add_markers(
          data = data.frame(Industry = rd$Industry),
          x = NA_real_, y = ~Industry, yaxis = "y2",
          marker = list(opacity = 0), hoverinfo = "skip", showlegend = FALSE,
          inherit = FALSE
        )
        # type and range are repeated here to match yaxis_left exactly --
        # yaxis2 otherwise has nothing else to infer either from, and
        # needs to line its rows up with where the primary axis actually
        # drew each bar. showgrid = FALSE avoids doubled gridlines from
        # the overlay.
        p <- p %>% layout(yaxis2 = list(
          overlaying = "y", side = "right", type = "category",
          categoryorder = "array", categoryarray = levels(rd$Industry),
          tickmode = "array", tickvals = right_labels, ticktext = right_labels,
          range = axis_range, automargin = TRUE,
          showgrid = FALSE, color = INK_MUTED,
          tickfont = list(size = RANKING_CHART_TICKFONT_SPLIT)
        ))
      }
      p
    })

    # raw_data() is the dependency, not the value used -- reading it just
    # ties this to the same reactiveFileReader invalidation as this tab's
    # own data, so the "as of" date updates the moment a new pipeline run
    # lands, without this needing its own poll loop.
    output$data_asof <- renderUI({
      raw_data()
      data_asof_ui()
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        sprintf(
          "productivity_ranking_%s_%s_%s-%s_%s.csv",
          gsub("[^A-Za-z0-9]+", "-", input$variable),
          input$industry_level,
          input$year_range[1], input$year_range[2], format(Sys.Date(), "%Y%m%d")
        )
      },
      content = function(file) {
        out <- ranking_data() %>%
          arrange(desc(CAGR)) %>%
          transmute(
            Industry, Variable = input$variable,
            StartYear = input$year_range[1], StartValue, EndYear = input$year_range[2], EndValue,
            `CAGR (%)` = CAGR * 100
          )
        write.csv(out, file, row.names = FALSE)
      }
    )
  })
}

# One tab's worth of sidebar + main content, namespaced under `id` so each
# of the 2 remaining tabs (kind = "bar" | "table" -- Trends and Rankings now
# have their own dedicated modules above) gets its own fully independent
# Variable/Compare/Display state instead of sharing one page-level sidebar.
# `ns = ns` on conditionalPanel is what makes the condition strings below
# resolve against THIS tab's namespaced widgets client-side, without
# hand-building "input['id-view_mode']" strings.
tab_module_ui <- function(id, init_df, kind, variable_choices, industry_tree) {
  ns <- NS(id)

  main_panel <- switch(
    kind,
    # fill = FALSE is the actual, sanctioned way to opt a DT table out of
    # bslib's fill-to-available-height system -- DTOutput() defaults to
    # fill = TRUE, which is what was pinning it to a fixed pixel height via
    # an actively-enforced resize observer (confirmed: neither CSS
    # !important nor a JS override applied after the fact could beat it,
    # since it just gets silently re-imposed on the next resize tick).
    table = DTOutput(ns("chart"), fill = FALSE),
    plotlyOutput(ns("chart"), height = "100%")
  )

  card(
    # Only the Data tab's card gets its own internal scrollbar (see the
    # .tab-pane.active > .card.table-tab-card CSS override below) -- a DT
    # table's true height (rows + its own "Showing X of Y"/pagination
    # footer) is content-driven and routinely exceeds the fixed
    # viewport-height budget every card is otherwise pinned to, which
    # silently clipped the Source/Data-last-refreshed lines after the table
    # instead of leaving them reachable by scrolling.
    # card-sidebar -- see the matching comment on the Trends tab's card();
    # applies to both kinds sharing this module, alongside table-tab-card
    # which only kind == "table" also needs.
    class = paste(c("card-sidebar", if (kind == "table") "table-tab-card"), collapse = " "),
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        # Same treeSelectInput widget as the Trends/Rankings tabs' Variable/
        # Industry pickers (see www/tree_select.js and the matching comment
        # on the Trends tab's sidebar) -- click/tab in blanks the box for a
        # fresh search or scroll, picking an option (or not) is what
        # shows/restores the display text.
        treeSelectInput(
          ns("variable"), "Variable",
          tree_data = flat_tree_nodes(variable_choices), selected = DEFAULT_VARIABLE,
          placeholder = "Search variables..."
        ),
        # renderUI -- see the matching comment on the Rankings tab's own
        # output$definition_link (tab_module_server() below).
        uiOutput(ns("definition_link")),
        tags$strong("Compare"),
        # Collapsible tree dropdown -- closed to just the root aggregate by
        # default, arrow to expand a branch, click a label to pick it.
        # Natively supports an empty "nothing selected" state (reported as
        # ""), so unlike the old selectize picker this replaces, no leading
        # blank "" choice trick is needed to make the box start empty. No
        # Geography picker alongside it -- table 36-10-0208-01 covers Canada
        # only (see MFP_DATA_FILE's own comment up top), so a series here is
        # just one industry (see active_pairs_columns()).
        treeSelectInput(
          ns("pair_industry"), "Industry",
          tree_data = industry_tree, selected = NULL,
          placeholder = "Search industries..."
        ),
        # Starts disabled (the picker is blank on load) -- the server's
        # observe() on input$pair_industry takes over from here the moment
        # the session connects, see tab_module_server().
        actionButton(ns("add_pair"), "Add series", icon = icon("plus"), disabled = NA),
        # "N out of MAX series selected" -- reactive on active_pairs(), so it
        # updates in lockstep with the chip list and the disabled state above
        # instead of needing its own trigger.
        uiOutput(ns("series_count_label")),
        tags$strong("Series to compare"),
        # Chip list replaces the old checkboxGroupInput -- active_pairs()
        # drives this reactively via renderUI, so unlike the checkbox
        # widget there's no separate "sync the widget" step needed whenever
        # active_pairs() changes.
        uiOutput(ns("active_pairs_chips")),
        # Starts enabled -- active_pairs() has the default series in it on
        # load, unlike Add series' pickers which start blank. The server's
        # observe() on active_pairs() (see tab_module_server()) disables it
        # once the list is actually empty, same toggleDisabled mechanism
        # Add series uses.
        actionButton(ns("clear_pairs"), "Clear comparisons", icon = icon("trash-can"), class = "btn-sm"),
        if (kind == "table") {
          # Data tab: every control sits directly in the sidebar, always
          # visible -- no chevron disclosure (this is the one tab whose
          # sidebar is short enough not to need one). Download sits last,
          # after every control that shapes what it exports (Date
          # range/rebase/base year), same trailing spot it occupies inside
          # Compare's "More options". No "View values as" toggle -- level
          # vs. annual-growth only ever drove the *chart's* DisplayValue
          # (see transform_result()), which this tab's table export never
          # reads: build_export_df() always emits both the raw Value and
          # GrowthPct columns regardless.
          tagList(
            sliderInput(
              ns("year_range"), "Date range",
              min = min(init_df$Year), max = max(init_df$Year),
              value = c(min(init_df$Year), max(init_df$Year)),
              step = 1, sep = ""
            ),
            checkboxInput(ns("rebase_toggle"), "Set each series to 100 in a selected year", value = FALSE),
            conditionalPanel(
              "input.rebase_toggle == true", ns = ns,
              # A dropdown, not a slider, matching the Trends tab's Base
              # year picker -- a single specific year to jump to, not a
              # range to drag across.
              selectInput(
                ns("base_year"), "Base year",
                choices = sort(unique(init_df$Year)), selected = min(init_df$Year),
                selectize = FALSE
              )
            ),
            download_menu_ui(ns, include_png = FALSE, full_dataset = TRUE)
          )
        } else {
          # Compare (kind == "bar"): reuses the Trends/Rankings tabs'
          # .trend-more-options styling (chevron summary, no default
          # browser triangle) -- less-frequently-touched display controls
          # tucked away instead of always taking up sidebar space.
          tags$details(
            id = ns("more_options"), class = "trend-more-options",
            tags$summary("More options"),
            radioButtons(
              ns("chart_type"), "Chart type",
              choices = c("Trend line" = "line", "Bar chart" = "bar"),
              selected = "line", inline = TRUE
            ),
            radioButtons(
              ns("view_mode"), "View values as",
              choices = c("Level" = "level", "Annual percentage change" = "growth"),
              # Stacked rather than inline -- "Annual percentage change" is
              # too long to sit next to "Level" on one line in the sidebar.
              selected = "level", inline = FALSE
            ),
            sliderInput(
              ns("year_range"), "Date range",
              min = min(init_df$Year), max = max(init_df$Year),
              value = c(min(init_df$Year), max(init_df$Year)),
              step = 1, sep = ""
            ),
            conditionalPanel(
              "input.view_mode == 'level'", ns = ns,
              checkboxInput(ns("rebase_toggle"), "Set each series to 100 in a selected year", value = FALSE),
              conditionalPanel(
                "input.view_mode == 'level' && input.rebase_toggle == true", ns = ns,
                # A dropdown, not a slider, matching the Trends tab's Base
                # year picker -- a single specific year to jump to, not a
                # range to drag across.
                selectInput(
                  ns("base_year"), "Base year",
                  choices = sort(unique(init_df$Year)), selected = min(init_df$Year),
                  selectize = FALSE
                )
              )
            ),
            download_menu_ui(ns)
          )
        }
      ),
      main_panel,
      # See source_and_asof_ui()'s own comment for why this is one wrapper
      # div rather than two separate top-level children here.
      source_and_asof_ui(ns)
    )
  )
}

# Reactive pipeline shared by the 2 remaining tabs (kind = "bar" | "table"),
# only diverging at the final render step (kind-specific chart/table).
# Instantiated once per tab id so each tab's
# Variable/Compare/Display selections are fully independent -- module
# namespacing (see tab_module_ui) is what makes multiple simultaneous
# copies of the same widget ids possible in one page.
tab_module_server <- function(id, raw_data, kind, variable_uom_lookup) {
  moduleServer(id, function(input, output, session) {

    # Keeps the variable, time frame, and base year selectors in sync with
    # what's available in the data (added/removed whenever RAW_DATA_READER's
    # shared file-watcher picks up a new pipeline run), preserving the
    # user's current picks -- clamped to the new bounds -- where possible.
    # This only affects what's *offered* when adding a new series --
    # series already in active_pairs() are unaffected, since they're
    # already resolved to a concrete Industry.
    #
    # ignoreInit = TRUE -- see the matching comment on the Trends tab's own
    # sync observe(): ui() already built this session's initial widgets
    # (both Compare's and Data's -- this module runs once per kind) from the
    # same raw_data(), so this only needs to fire on a real data change, not
    # redundantly again for every new session.
    observe({
      # req(), not a bare assignment -- see the matching comment on the
      # Trends tab's own sync observe().
      df <- req(raw_data())

      variable_choices <- series_choices(df, "Variable", VARIABLE_ORDER)
      current_variable <- input$variable
      new_variable <- if (is.null(current_variable) || !(current_variable %in% variable_choices)) {
        DEFAULT_VARIABLE
      } else {
        current_variable
      }
      updateTreeSelectInput(session, "variable", tree_data = flat_tree_nodes(variable_choices), selected = new_variable)

      current_industry <- input$pair_industry
      new_industry <- if (is.null(current_industry) || !nzchar(current_industry) ||
                             !(current_industry %in% unique(df$Industry))) {
        ""
      } else {
        current_industry
      }
      updateTreeSelectInput(session, "pair_industry", tree_data = industry_tree_nodes(df), selected = new_industry)

      year_min <- min(df$Year)
      year_max <- max(df$Year)
      current_range <- input$year_range
      range_value <- if (is.null(current_range)) {
        c(year_min, year_max)
      } else {
        c(max(current_range[1], year_min), min(current_range[2], year_max))
      }
      updateSliderInput(session, "year_range", min = year_min, max = year_max, value = range_value)

      # base_year is a selectInput (see tab_module_ui()) -- its choices are
      # numeric Years, but like every HTML <select> its reported value
      # comes back as a string, hence the as.numeric() round-trip before
      # comparing/reselecting against year_choices (a numeric vector).
      # Same pattern trend_tab_server() uses for its own Base year picker.
      year_choices <- sort(unique(df$Year))
      current_base <- suppressWarnings(as.numeric(input$base_year))
      new_base <- if (is.null(input$base_year) || is.na(current_base) || !(current_base %in% year_choices)) {
        min(year_choices)
      } else {
        current_base
      }
      updateSelectInput(session, "base_year", choices = year_choices, selected = new_base)
    }) |> bindEvent(raw_data(), once = FALSE, ignoreInit = TRUE)

    # See goto_definition_link()'s own comment -- req() rather than a
    # DEFAULT_VARIABLE fallback since a NULL input$variable here is only
    # ever a brief moment before the session's first render finishes, not a
    # state this link needs to paper over. One output$definition_link
    # definition covers both kinds sharing this module (Compare and Data),
    # same as everything else in tab_module_server().
    output$definition_link <- renderUI({
      req(input$variable)
      goto_definition_link(input$variable, "Definition")
    })

    # The set of Industries currently being compared -- replaces the old
    # compare_mode-driven industries_multi/geos_multi/geo_single/
    # industry_single inputs with one free-form list (see
    # active_pairs_columns()'s own comment for why this is still called
    # "pairs"/PairKey rather than renamed now that Geography is gone).
    active_pairs <- reactiveVal(default_pair_row())

    # The MAX_ACTIVE_SERIES cap is a Compare-tab-only thing (kind == "bar")
    # -- it exists so every active series still gets its own distinct colour
    # off CATEGORICAL_PALETTE, which only the chart cares about. The Data
    # tab (kind == "table") just lists rows, so there's no colour budget to
    # protect and no reason to stop it at 8. Inf makes every `< max_series`
    # check below a no-op there instead of needing a separate branch per check.
    max_series <- if (kind == "bar") MAX_ACTIVE_SERIES else Inf

    # "Add series" only makes sense once the picker holds a real value AND
    # (Compare tab only) the list is under the max_series cap -- the picker
    # reports "" for "nothing selected" (treeSelectInput's shared contract,
    # see the comment on its definition), which isTruthy() treats as falsy
    # the same way req() elsewhere in this module already does, so this and
    # the req() inside the add_pair observer below always agree on what
    # counts as "ready".
    observe({
      ready <- isTruthy(input$pair_industry) && nrow(active_pairs()) < max_series
      session$sendCustomMessage("toggleDisabled", list(id = session$ns("add_pair"), disabled = !ready))
    })

    # "N out of MAX series selected" next to the Add series button -- the
    # only place the cap itself is explained, since the button just goes
    # disabled with no explanation on its own. Compare-tab-only, same as the
    # cap it's describing -- the Data tab has nothing to explain here.
    output$series_count_label <- renderUI({
      if (kind != "bar") return(NULL)
      n <- nrow(active_pairs())
      tags$p(
        class = "text-muted small",
        sprintf("%d out of %d series selected", n, max_series),
        if (n >= max_series) " -- remove one to add another." else NULL
      )
    })

    observeEvent(input$add_pair, {
      req(input$pair_industry)
      current <- active_pairs()
      # Belt-and-suspenders alongside the disabled state above -- guards the
      # rare case a click already in flight lands after the button's just
      # been disabled (e.g. a second series added elsewhere in the same
      # session tick), rather than trusting the UI toggle alone to never let
      # a 9th series through. A no-op on the Data tab, where max_series is Inf.
      req(nrow(current) < max_series)
      key <- input$pair_industry
      if (!(key %in% current$PairKey)) {
        # A hierarchy relationship (parent or child, either direction) means
        # the two series aren't independent -- one already includes the
        # other's activity, so comparing their *levels* isn't
        # apples-to-apples for additive measures like labour compensation or
        # capital cost. Informational only (not blocked): sometimes that
        # comparison is exactly the point (e.g. showing a sub-sector
        # diverging from its sector), so this just makes the overlap visible
        # rather than silently assuming it's a mistake.
        # vapply, not mapply -- mapply's default SIMPLIFY=TRUE returns a
        # zero-length *list* (not a logical(0) vector) when current$Industry
        # is empty (e.g. right after Clear comparisons), and `&` can't
        # operate against a list even an empty one ("operations are possible
        # only for numeric, logical or complex types"). vapply's declared
        # FUN.VALUE = logical(1) guarantees a real logical vector -- length
        # 0 included -- so this stays a no-op instead of crashing the app
        # the moment the very next series gets added to an empty list.
        related <- current[
          vapply(
            current$Industry,
            function(i) industry_is_ancestor(i, input$pair_industry) || industry_is_ancestor(input$pair_industry, i),
            logical(1), USE.NAMES = FALSE
          ),
        ]
        if (nrow(related) > 0) {
          csls_notify(
            paste0(
              "Heads up: \"", input$pair_industry, "\" overlaps with \"",
              paste(related$Industry, collapse = "\", \""), "\" in the industry hierarchy -- ",
              "one contains the other, so comparing levels isn't apples-to-apples for ",
              "additive measures like labour compensation or capital cost."
            ),
            type = "message"
          )
        }

        current <- rbind(current, active_pairs_columns(input$pair_industry))
        active_pairs(current)
      }

      # Clear the picker back to its empty/placeholder state after every
      # add -- so adding a series always ends with a clean search box ready
      # for the next pick, instead of leaving the just-added pick sitting
      # there looking like unconsumed input.
      updateTreeSelectInput(session, "pair_industry", selected = "")
    })

    # Chip list for the active series -- reactive on active_pairs(), so
    # unlike the old checkboxGroupInput this needs no explicit "sync the
    # widget" push whenever active_pairs() changes.
    output$active_pairs_chips <- renderUI({
      pairs <- active_pairs()
      if (nrow(pairs) == 0) {
        return(p(class = "text-muted small", "No series selected."))
      }
      pal <- colors()
      tags$div(
        class = "pair-chip-list",
        lapply(seq_len(nrow(pairs)), function(i) {
          row <- pairs[i, ]
          tags$span(
            class = "pair-chip",
            tags$span(class = "pair-chip-swatch", style = paste0("background-color:", pal[[row$SeriesLabel]], ";")),
            tags$span(class = "pair-chip-label", row$SeriesLabel),
            tags$button(
              type = "button", class = "pair-chip-remove",
              `aria-label` = paste("Remove", row$SeriesLabel),
              onclick = sprintf(
                "Shiny.setInputValue('%s', %s, {priority: 'event'})",
                session$ns("remove_pair"), jsonlite::toJSON(row$PairKey)
              ),
              "×"
            )
          )
        })
      )
    })

    # Clicking a chip's × is how a pair gets removed now, replacing the old
    # "uncheck a box" mechanism.
    observeEvent(input$remove_pair, {
      current <- active_pairs()
      active_pairs(current[current$PairKey != input$remove_pair, ])
    })

    # "Clear comparisons" empties the list in one click instead of clicking
    # every chip's × individually -- current[0, ] keeps the same columns
    # (Industry/SeriesLabel/PairKey) with zero rows, which is exactly what
    # the chip renderUI's nrow() == 0 empty-state branch and the downstream
    # selected_series()/history_with_growth() req() gates already expect
    # from "nothing selected".
    observeEvent(input$clear_pairs, {
      active_pairs(active_pairs()[0, ])
    })

    # Mirrors the add_pair readiness toggle above -- "Clear comparisons"
    # only makes sense once there's something to clear.
    observe({
      session$sendCustomMessage(
        "toggleDisabled",
        list(id = session$ns("clear_pairs"), disabled = nrow(active_pairs()) == 0)
      )
    })

    # Adds the SeriesLabel every downstream reactive/chart keys off of, so
    # the rest of the reactive chain doesn't need to know how a series is
    # identified -- it's already generic over "some series dimension" (see
    # active_pairs_columns()'s own comment).
    scoped_raw <- reactive({
      # See the matching comment on the Trends tab's own scoped_raw().
      validate(need(!is.null(raw_data()), "Data is temporarily unavailable -- please try again in a moment."))
      raw_data() %>%
        filter(Variable == input$variable) %>%
        mutate(SeriesLabel = Industry)
    })

    selected_series <- reactive(active_pairs()$SeriesLabel)

    # UOM is 1:1 per Variable (see variable_uom_lookup() in server(), shared
    # across all 4 tabs) -- a small lookup keyed by Variable, not a fresh
    # filter() over the full raw_data() every time this tab's
    # Variable picker changes.
    variable_uom <- reactive({
      lookup <- variable_uom_lookup()
      lookup$UOM[lookup$Variable == input$variable][1]
    })

    # Full history (not yet limited to the time-frame slider) for the
    # selected series, with year-over-year growth computed against each
    # series' actual prior year -- including years outside the currently
    # zoomed window.
    history_with_growth <- reactive({
      validate(need(nrow(scoped_raw()) > 0, "No data for this variable."))
      req(length(selected_series()) > 0)
      scoped_raw() %>%
        filter(SeriesLabel %in% selected_series()) %>%
        arrange(SeriesLabel, Year) %>%
        group_by(SeriesLabel) %>%
        mutate(GrowthPct = (Value / lag(Value) - 1) * 100) %>%
        ungroup()
    })

    # Adds RebasedValue (per series, relative to input$base_year, looked up
    # against the full history so the base year can sit outside the zoomed
    # time frame) and DisplayValue (whichever column the current view mode
    # calls for). Returns both the data and the list of series excluded from
    # rebasing for lacking the chosen base year.
    transform_result <- reactive({
      df <- history_with_growth()
      # input$base_year comes back as a string from its selectInput (see
      # tab_module_ui()) -- coerce to numeric before comparing against the
      # numeric Year column, same as trend_tab_server()'s transform_result().
      base_year_num <- suppressWarnings(as.numeric(input$base_year))

      if (isTRUE(input$rebase_toggle) && !is.na(base_year_num)) {
        base_values <- df %>%
          filter(Year == base_year_num) %>%
          select(SeriesLabel, BaseValue = Value)
        df <- df %>% left_join(base_values, by = "SeriesLabel")
        df$RebasedValue <- safe_index_to_100(df$Value, df$BaseValue)
        # "Missing" covers both a series having no row at the base year at
        # all (BaseValue NA after the join) AND a base value of exactly 0
        # (rebasing to a 0 is undefined -- see safe_index_to_100()).
        missing <- df %>% filter(is.na(BaseValue) | BaseValue == 0) %>% pull(SeriesLabel) %>% unique()
        df <- df %>% select(-BaseValue)
      } else {
        df$RebasedValue <- NA_real_
        missing <- character(0)
      }

      df$DisplayValue <- if (identical(input$view_mode, "growth")) {
        df$GrowthPct
      } else if (isTRUE(input$rebase_toggle)) {
        df$RebasedValue
      } else {
        df$Value
      }

      list(df = df, missing = missing)
    })

    display_data <- reactive(transform_result()$df)

    rebase_missing_series <- reactive({
      if (identical(input$view_mode, "growth") || !isTRUE(input$rebase_toggle)) {
        character(0)
      } else {
        transform_result()$missing
      }
    })

    observeEvent(rebase_missing_series(), {
      missing <- rebase_missing_series()
      if (length(missing) > 0) {
        csls_notify(
          paste0(
            "No ", input$base_year, " data for: ", paste(missing, collapse = ", "),
            " -- excluded from the rebased view."
          ),
          type = "warning"
        )
      }
    })

    filtered_data <- reactive({
      req(input$year_range)
      display_data() %>%
        filter(Year >= input$year_range[1], Year <= input$year_range[2])
    })

    # Colors are assigned in the order series were added to active_pairs(),
    # not from a fixed universe -- with 21 possible industries (see
    # data_pipeline.R) against an 8-colour palette (MAX_ACTIVE_SERIES), a
    # fixed domain would still mean most industries fall through to the
    # muted overflow color. This does mean a series' color can shift by one
    # slot if an earlier one is removed.
    colors <- reactive(series_color_map(active_pairs()$SeriesLabel))
    # Dash pattern / marker shape, indexed the same way as colors() -- the
    # colour-blind-safe secondary channel (see series_style_map()).
    styles <- reactive(series_style_map(active_pairs()$SeriesLabel))

    if (kind == "table") {
      # server = FALSE (client-side paging): the 8-series comparison cap
      # keeps this table small -- a few hundred rows at most -- so there's
      # no real cost to rendering it whole. It also sidesteps DT's default
      # server-side mode, whose page-change AJAX call back to this session
      # is what throws "DataTables warning ... Ajax error" specifically
      # when the app is iframe-embedded from Posit Connect Cloud (the
      # proxied/cross-origin hop breaks that follow-up request even though
      # the initial page load is fine); paging entirely client-side removes
      # that round-trip.
      output$chart <- renderDT({
        df <- build_export_df(filtered_data(), input$rebase_toggle, input$base_year)
        datatable(
          df, rownames = FALSE, options = list(pageLength = 15),
          colnames = unname(export_column_labels(names(df), input$base_year, input$variable, variable_uom()))
        )
      }, server = FALSE)
    } else {
      # kind == "bar" (the only other kind this shared module still serves)
      output$chart <- renderPlotly({
        df <- filtered_data() %>% filter(!is.na(DisplayValue))
        # Distinct from history_with_growth()'s own validate() -- rows can
        # exist there but still end up all-NA here (e.g. Growth view with
        # only one year in range). Previously a bare req() -- see "No
        # matching data" state.
        validate(need(
          nrow(df) > 0,
          "No values to plot for this view -- try widening the year range, switching off Growth view or Rebase, or adding a different series."
        ))
        pal <- colors()
        sty <- styles()
        series <- series_choices(df, "SeriesLabel")
        axis_title <- display_axis_title(input$view_mode, input$rebase_toggle, input$base_year, input$variable, variable_uom())
        fmt <- metric_format_spec(variable_uom(), input$view_mode, input$rebase_toggle)
        hover_spec <- paste0("%{y:", fmt$prefix, fmt$tickformat, "}", fmt$suffix)
        is_bar <- identical(input$chart_type, "bar")

        p <- plot_ly()
        for (s in series) {
          sd <- df %>% filter(SeriesLabel == s) %>% arrange(Year)
          if (nrow(sd) == 0) next
          # With hovermode "x unified" (below), Plotly shares one box across
          # every series at that X and keys each row with a colour/dash/
          # shape swatch drawn from the trace -- but *not* with the trace's
          # name as text, so the name still needs to be in the template
          # (confirmed empirically, not assumed). Value leads (bold), name
          # follows on its own line -- "the reader has the series [via the
          # swatch/legend] and wants the number" -- rather than the old
          # "name: value" ordering repeated as a single flat line.
          hovertemplate <- paste0("<b>", hover_spec, "</b><br>", s, "<extra></extra>")
          p <- if (is_bar) {
            p %>% add_trace(
              data = sd, x = ~Year, y = ~DisplayValue,
              type = "bar", name = s, legendgroup = s,
              marker = list(color = pal[[s]]),
              hovertemplate = hovertemplate
            )
          } else {
            p %>% add_trace(
              data = sd, x = ~Year, y = ~DisplayValue,
              type = "scatter", mode = "lines+markers", name = s, legendgroup = s,
              line = list(color = pal[[s]], width = 2, dash = sty$dash[[s]]),
              marker = list(color = pal[[s]], size = 8, symbol = sty$symbol[[s]]),
              hovertemplate = hovertemplate
            )
          }
        }

        # Direct end-of-line labels -- per the dataviz skill's rule these
        # *supplement* the legend (which always stays on for 2+ series),
        # they don't replace it, and only up to ~4 series before ends start
        # colliding. Line mode only (a bar chart's bars are already spatially
        # separated, nothing to label at "the end of"). Collision check is a
        # simplified numeric-proximity heuristic on the final data values,
        # not pixel-measured rendered positions (Shiny/R has no easy way to
        # read back actual rendered label geometry) -- any two series' last
        # points within 5% of the plotted y-range are treated as colliding,
        # and direct labels are skipped entirely for that render rather than
        # stacking illegibly; the legend + unified hover still carry identity.
        end_labels <- if (!is_bar && length(series) >= 2 && length(series) <= 4) {
          endpoints <- df %>%
            group_by(SeriesLabel) %>%
            filter(Year == max(Year)) %>%
            ungroup()
          y_range <- diff(range(df$DisplayValue, na.rm = TRUE))
          collide <- y_range > 0 && any(dist(endpoints$DisplayValue) < 0.05 * y_range)
          if (!collide) {
            lapply(seq_len(nrow(endpoints)), function(i) {
              row <- endpoints[i, ]
              list(
                x = row$Year, y = row$DisplayValue, xref = "x", yref = "y",
                text = paste0(" ", row$SeriesLabel), showarrow = FALSE,
                xanchor = "left", align = "left",
                font = list(color = pal[[row$SeriesLabel]], family = FONT_FAMILY, size = 11)
              )
            })
          }
        }

        p %>% layout(
          title = display_chart_title(input$variable, input$year_range),
          barmode = if (is_bar) "group" else NULL,
          xaxis = list(title = "Year", nticks = 8, tickformat = "d", gridcolor = GRIDLINE, color = INK_MUTED),
          yaxis = list(
            title = axis_title, gridcolor = GRIDLINE, color = INK_MUTED,
            tickformat = fmt$tickformat, tickprefix = fmt$prefix, ticksuffix = fmt$suffix
          ),
          paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
          font = list(color = INK_PRIMARY, family = FONT_FAMILY),
          legend = list(orientation = "h", y = -0.2),
          hovermode = "x unified",
          annotations = end_labels,
          shapes = if (identical(input$view_mode, "growth")) {
            list(list(
              type = "line", xref = "paper", x0 = 0, x1 = 1,
              yref = "y", y0 = 0, y1 = 0,
              line = list(color = INK_MUTED, width = 1, dash = "dot")
            ))
          }
        )
      })
    }

    # raw_data() is the dependency, not the value used -- reading it just
    # ties this to the same reactiveFileReader invalidation as this tab's
    # own data, so the "as of" date updates the moment a new pipeline run
    # lands, without this needing its own poll loop.
    output$data_asof <- renderUI({
      raw_data()
      data_asof_ui()
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        mode_part <- if (identical(input$view_mode, "growth")) {
          "annual-growth"
        } else if (isTRUE(input$rebase_toggle)) {
          paste0("rebased-", input$base_year)
        } else {
          "index"
        }
        sprintf(
          "productivity_%s_%s_%s-%s_%s.csv",
          gsub("[^A-Za-z0-9]+", "-", input$variable), mode_part,
          input$year_range[1], input$year_range[2], format(Sys.Date(), "%Y%m%d")
        )
      },
      content = function(file) {
        df <- build_export_df(filtered_data(), input$rebase_toggle, input$base_year)
        write.csv(df, file, row.names = FALSE)
      }
    )

    # Data-only (kind == "table", see the matching `if` in tab_module_ui()):
    # the entire underlying dataset -- every Variable/Industry/Year
    # combination, not just the currently selected series/variable/time
    # frame -- so this deliberately reads raw_data() directly rather than
    # any of the scoped_raw()/filtered_data() reactives the rest of this
    # module builds off of. No GrowthPct/RebasedValue columns here: those
    # are relative to *this tab's* current view-mode/rebase settings, which
    # don't have a single well-defined meaning across the whole dataset.
    if (kind == "table") {
      output$download_full <- downloadHandler(
        filename = function() {
          sprintf("productivity_full-dataset_%s.csv", format(Sys.Date(), "%Y%m%d"))
        },
        content = function(file) {
          out <- req(raw_data()) %>%
            arrange(Variable, Industry, Year) %>%
            select(Year, Industry, Variable, Value, UOM)
          write.csv(out, file, row.names = FALSE)
        }
      )
    }
  })
}

# The Growth Accounting tab: for one Industry at a time, a stacked/grouped
# bar chart showing what labour productivity (LP) growth is made up of each
# year -- see GROWTH_ACCOUNTING_VARS's own comment for the decomposition
# itself. No Variable picker (unlike every other tab) -- this tab always
# reads the same fixed set of 3 series, so there's nothing to choose there;
# Industry and the time frame are the only 2 things a reader can change.
growth_tab_ui <- function(id, init_df, industry_tree) {
  ns <- NS(id)

  card(
    # card-sidebar -- see the matching comment on the Trends tab's card().
    class = "card-sidebar",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("sidebar"),
        treeSelectInput(
          ns("industry"), "Industry",
          tree_data = industry_tree, selected = DEFAULT_INDUSTRY,
          placeholder = "Search industries..."
        ),
        sliderInput(
          ns("year_range"), "Date range",
          min = min(init_df$Year), max = max(init_df$Year),
          value = c(min(init_df$Year), max(init_df$Year)),
          step = 1, sep = ""
        ),
        # How many years each bar covers -- "1 year (annual)" (the default,
        # and this tab's original behaviour) up to "10 years". Periods are
        # counted backward from the date range's own *end* year (see
        # filtered_data()), not forward from its start, so choosing e.g. "5
        # years" over 2008-2020 gives 2015-2020/2010-2015/2008-2010, the
        # last (oldest) one shorter than the rest rather than the most
        # recent one -- a reader picking a date range is far more likely to
        # care about a full, untruncated *recent* period than a full oldest
        # one. selectize = FALSE, matching every other plain single-value
        # dropdown in this app (e.g. the Trends tab's Base year) -- nothing
        # here needs selectize's search box for a 10-item list.
        selectInput(
          ns("interval"), "Interval",
          choices = GROWTH_INTERVAL_CHOICES, selected = 1, selectize = FALSE
        ),
        # Each bar is identified purely by colour (no per-bar source
        # labelling on the chart itself, per how this tab is meant to read)
        # -- this is the one place that colour -> variable mapping is
        # actually spelled out. Static markup, not a renderUI: every colour
        # here is a fixed constant (GROWTH_ACCOUNTING_COLORS), nothing about
        # this legend depends on the current Industry/time-frame selection.
        # cap/lab read their label straight off GROWTH_ACCOUNTING_VARS --
        # StatCan's own exact variable names for this table (36-10-0208-01),
        # the same 2 strings that select the data (see scoped_raw()) --
        # rather than a shorter, separately-typed paraphrase, so the legend
        # can never drift from what's actually being filtered/plotted, and
        # matches what a reader would see naming that same Variable on the
        # Trends/Rankings/Compare/Data tabs' own pickers. lp/mfp don't: LP
        # growth is a growth *rate* of the "Labour productivity" variable,
        # not that variable itself, and MFP growth here is a computed
        # residual (see GROWTH_ACCOUNTING_VARS's own comment), not literally
        # StatCan's "Multifactor productivity" series -- so neither has a
        # single StatCan variable name to defer to the same way.
        tags$div(
          class = "growth-legend",
          tags$strong("What each colour shows"),
          tags$ul(
            class = "growth-legend-list",
            tags$li(
              tags$span(class = "growth-legend-swatch", style = paste0("background-color:", GROWTH_ACCOUNTING_COLORS[["lp"]], ";")),
              "Labour productivity growth"
            ),
            tags$li(
              tags$span(class = "growth-legend-swatch", style = paste0("background-color:", GROWTH_ACCOUNTING_COLORS[["cap"]], ";")),
              GROWTH_ACCOUNTING_VARS[["cap"]]
            ),
            tags$li(
              tags$span(class = "growth-legend-swatch", style = paste0("background-color:", GROWTH_ACCOUNTING_COLORS[["lab"]], ";")),
              GROWTH_ACCOUNTING_VARS[["lab"]]
            ),
            tags$li(
              tags$span(class = "growth-legend-swatch", style = paste0("background-color:", GROWTH_ACCOUNTING_COLORS[["mfp"]], ";")),
              "Multifactor productivity growth (residual)"
            )
          ),
          tags$p(
            class = "text-muted small",
            "Use this visualization to explore where labour productivity growth in Canada’s industries comes from.",
            " ", goto_definition_link("Growth accounting", "Learn more")
          )
        ),
        download_menu_ui(ns)
      ),
      # The actual plotlyOutput lives in output$chart_container (renderUI)
      # server-side instead of statically here -- once there are more
      # periods than comfortably fit on one screen (see
      # GROWTH_CHART_PERIOD_THRESHOLD), it needs a real, wider-than-the-card
      # pixel width inside its own horizontally-scrolling wrapper instead of
      # the usual 100%-fill, so every period's pair of bars keeps a legible
      # width instead of being squeezed thinner and thinner as the date
      # range widens (or the Interval picker shortens each period). Below
      # that threshold it renders the exact same plotlyOutput(height="100%") this
      # replaces.
      uiOutput(ns("chart_container"), fill = TRUE),
      # See source_and_asof_ui()'s own comment for why this is one wrapper
      # div rather than two separate top-level children here.
      source_and_asof_ui(ns)
    )
  )
}

growth_tab_server <- function(id, raw_data) {
  moduleServer(id, function(input, output, session) {

    # Keeps Industry/time-frame in sync with what's in the data, preserving
    # the user's current picks where still valid -- see the matching comment
    # on the Trends tab's own sync observe() for why ignoreInit = TRUE and
    # why req() (not a bare assignment) guards raw_data().
    observe({
      df <- req(raw_data())

      new_industry <- if (is.null(input$industry) || !(input$industry %in% unique(df$Industry))) {
        DEFAULT_INDUSTRY
      } else {
        input$industry
      }
      updateTreeSelectInput(session, "industry", tree_data = industry_tree_nodes(df), selected = new_industry)

      year_min <- min(df$Year)
      year_max <- max(df$Year)
      current_range <- input$year_range
      range_value <- if (is.null(current_range)) {
        c(year_min, year_max)
      } else {
        c(max(current_range[1], year_min), min(current_range[2], year_max))
      }
      updateSliderInput(session, "year_range", min = year_min, max = year_max, value = range_value)
    }) |> bindEvent(raw_data(), once = FALSE, ignoreInit = TRUE)

    # The 3 series this tab's decomposition needs, for the selected
    # Industry only -- see the matching comment on the Trends tab's own
    # scoped_raw() for why this is validate(), not req(), against raw_data()
    # itself.
    scoped_raw <- reactive({
      validate(need(!is.null(raw_data()), "Data is temporarily unavailable -- please try again in a moment."))
      req(input$industry)
      raw_data() %>% filter(Variable %in% GROWTH_ACCOUNTING_VARS, Industry == input$industry)
    })

    # One row per Year, the 3 raw indices aligned side by side -- match()
    # (not a join) both because it's a 3-way alignment (nothing in dplyr
    # does that in one step without tidyr, which this app doesn't otherwise
    # depend on) and because it makes a Year missing from one of the 3
    # series explicit as NA rather than silently dropped, the same way a
    # join would.
    aligned_indices <- reactive({
      df <- scoped_raw()
      validate(need(nrow(df) > 0, "No data for this industry."))
      years <- sort(unique(df$Year))
      value_for <- function(variable) {
        series <- df[df$Variable == variable, ]
        series$Value[match(years, series$Year)]
      }
      data.frame(
        Year = years,
        LP = value_for(GROWTH_ACCOUNTING_VARS[["lp"]]),
        Cap = value_for(GROWTH_ACCOUNTING_VARS[["cap"]]),
        Lab = value_for(GROWTH_ACCOUNTING_VARS[["lab"]])
      )
    })

    # input$interval as a clean, defensive integer >= 1 -- selectInput()
    # always reports its value back as a string (see GROWTH_INTERVAL_CHOICES'
    # own comment), and this is read from 3 different places below (the
    # period math itself, the chart's title/axis, and the CSV filename), so
    # it's coerced once here rather than 3 times over. Falls back to 1 (the
    # "1 year (annual)" choice) for anything unexpected (NULL before the
    # input first exists, or a somehow-invalid value) -- the safest of the
    # 10 choices to default to, since it's the one Interval that can never
    # produce a truncated period at all.
    growth_interval <- reactive({
      iv <- suppressWarnings(as.integer(input$interval))
      if (length(iv) == 0 || is.na(iv) || iv < 1) 1L else iv
    })

    # The decomposition itself, one row per *period* rather than per year --
    # see GROWTH_ACCOUNTING_VARS's own comment for why each series' own
    # log-difference is what's additive here, and why MFP growth is computed
    # as the residual rather than read off its own index; that identity
    # holds exactly over any span, not just a single year (a multi-year
    # log-difference is just the sum of the annual ones telescoped
    # together), which is what lets this same computation serve every
    # Interval choice unchanged. Each of the 3 log-differences is the *total*
    # growth over the whole period (b_start's index level straight to
    # b_end's, nothing averaged or divided by the period's width) -- an
    # average-annual-rate version (dividing every term by the period width)
    # was tried first, but was deliberately reverted: it makes a longer
    # period's bar height read as if growth had been *slower*, when really
    # the same total change is just being spread across more years -- i.e.
    # exactly the kind of distortion this chart exists to avoid, especially
    # since the whole point of the Interval picker is to compare periods of
    # different lengths (including the truncated one) on equal footing.
    # Total-over-the-period is also the more literal reading of "the
    # decomposition for 2015-2020" -- e.g. "labour productivity grew 12%
    # from 2015 to 2020", not "grew at an average of 2.4% a year".
    #
    # Periods are built backward from the date range's own end year in
    # growth_interval()-year steps (see the Interval picker's own comment in
    # growth_tab_ui() for why end-anchored, not start-anchored) and never
    # reach outside [input$year_range[1], input$year_range[2]] -- a period
    # that would otherwise start before the range's own start is clipped to
    # it instead, which is what can leave the very last (oldest, chronologically
    # first) period narrower than growth_interval() actually asked for
    # (Truncated below). At most 1 period can ever come up short this way:
    # every period from the end backward is a full growth_interval()-year
    # step until the *next* one would land at or before the start, so only
    # that final step can possibly be clipped.
    filtered_data <- reactive({
      req(input$year_range)
      df <- aligned_indices()
      interval <- growth_interval()
      start_yr <- input$year_range[1]
      end_yr <- input$year_range[2]

      # A period's own growth needs only its 2 endpoint years' index
      # levels -- match() (not a range filter) so a period is still
      # computable even if some year strictly *between* its endpoints is
      # missing from this industry's history, and so a genuinely missing
      # endpoint surfaces as a clean NA (dropped below) rather than an
      # error.
      value_at <- function(col, yr) df[[col]][match(yr, df$Year)]

      period_row <- function(b_start, b_end) {
        width <- b_end - b_start
        lp_g <- 100 * log(value_at("LP", b_end) / value_at("LP", b_start))
        cap_g <- 100 * log(value_at("Cap", b_end) / value_at("Cap", b_start))
        lab_g <- 100 * log(value_at("Lab", b_end) / value_at("Lab", b_start))
        data.frame(
          PeriodStart = b_start, PeriodEnd = b_end,
          # A 1-year period is labelled just its end year (e.g. "2020"),
          # matching this tab's original per-year axis exactly at Interval
          # "1 year" -- only a period wider than 1 year gets a "start-end"
          # range label.
          PeriodLabel = if (width == 1) as.character(b_end) else paste0(b_start, "-", b_end),
          LPGrowth = lp_g, CapitalDeepening = cap_g, LabourComposition = lab_g,
          MFPGrowth = lp_g - cap_g - lab_g,
          Truncated = width < interval
        )
      }

      rows <- list()
      b_end <- end_yr
      while (b_end > start_yr) {
        b_start <- max(b_end - interval, start_yr)
        rows[[length(rows) + 1]] <- period_row(b_start, b_end)
        b_end <- b_start
      }

      if (length(rows) == 0) {
        return(data.frame(
          PeriodStart = integer(0), PeriodEnd = integer(0), PeriodLabel = character(0),
          LPGrowth = numeric(0), CapitalDeepening = numeric(0), LabourComposition = numeric(0),
          MFPGrowth = numeric(0), Truncated = logical(0), PosIndex = integer(0)
        ))
      }
      # rows is newest-period-first (built walking backward from end_yr) --
      # rev() puts it in the chronological, oldest-first order this chart
      # (and every other chart in this app) reads left to right in.
      out <- do.call(rbind, rev(rows))
      # A period whose 2 endpoints aren't both present in this industry's
      # own history (value_at() returning NA -- e.g. "Other services"' data
      # ending in 2010, see GROWTH_ACCOUNTING_VARS's own comment) can't have
      # its growth computed at all -- dropped, the same "nothing to show"
      # outcome a genuinely missing year already got before Interval existed.
      out <- out[!is.na(out$LPGrowth) & !is.na(out$CapitalDeepening) & !is.na(out$LabourComposition), ]
      out$PosIndex <- seq_len(nrow(out))
      out
    })

    # A brief, non-blocking heads-up (see csls_notify()) that the oldest
    # period on screen is shorter than the others -- easy to miss otherwise,
    # since the muted-colour treatment (see output$chart) only reads as
    # *different*, not as *why*. tryCatch mirrors growth_chart_period_count()
    # below: filtered_data() can be mid-validate() (e.g. no data for this
    # industry), which would otherwise propagate straight through an any()
    # call and crash this reactive instead of just reporting "nothing to
    # flag" for that render.
    growth_has_truncated_period <- reactive({
      tryCatch(any(filtered_data()$Truncated), error = function(e) FALSE)
    })

    observeEvent(growth_has_truncated_period(), {
      if (isTRUE(growth_has_truncated_period())) {
        trunc_row <- filtered_data()[filtered_data()$Truncated, ][1, ]
        span <- trunc_row$PeriodEnd - trunc_row$PeriodStart
        csls_notify(
          sprintf(
            "The %s period only spans %d year%s -- shorter than the %d-year Interval used for the rest, so it's shown in muted colour.",
            trunc_row$PeriodLabel, span, if (span == 1) "" else "s", growth_interval()
          ),
          type = "message"
        )
      }
    })

    # Shared by output$chart_container (which needs just the period count,
    # to decide how wide the chart should be) and output$chart (which needs
    # the actual data) -- see the matching comment on the Rankings tab's own
    # ranking_chart_row_count() for why this is wrapped in tryCatch: a
    # validate() condition from filtered_data() (e.g. "No data for this
    # industry") would otherwise propagate into this renderUI too, replacing
    # the plotlyOutput it builds and leaving output$chart with nothing left
    # to render its own, more specific message into.
    growth_chart_period_count <- reactive({
      tryCatch(nrow(filtered_data()), error = function(e) 0L)
    })

    output$chart_container <- renderUI({
      n <- growth_chart_period_count()
      if (n > GROWTH_CHART_PERIOD_THRESHOLD) {
        px <- n * GROWTH_CHART_PX_PER_PERIOD
        # Outer div is the actual scroll viewport -- height:100% so it still
        # fills the same vertical space plotlyOutput(height="100%") always
        # has here (nothing about this chart needs *taller*, only *wider*,
        # unlike the Rankings tab's own analogous case); overflow-x: auto is
        # scoped to just this div, not the whole card (unlike table-tab-card/
        # ranking-tab-card's card-level overflow-y), so the sidebar beside it
        # is completely unaffected. Inner div pins the actual pixel width
        # (plus min-width, so a flex ancestor can't shrink it back down) that
        # the outer div then has something wider than itself to scroll to.
        # id'd (not just style-matched) so www/ui_helpers.js's own
        # "shown.bs.tab" scroll-hint listener can find this exact div
        # directly by id, without depending on a CSS-attribute selector or
        # this tab's nav-pill label text. overflow-anchor: none -- without
        # it, confirmed empirically (repeated tab visits, watching
        # scrollLeft) that the browser's own scroll-anchoring feature
        # nudges this div's scroll position by a few px on its own each
        # time Plotly re-measures/resizes the now-visible chart, compounding
        # a little further on every revisit -- exactly the kind of "layout
        # shifted, preserve what was on screen" adjustment scroll-anchoring
        # exists for, but not wanted here: this div's own content never
        # actually changes in a way a reader is looking at when it happens,
        # and it fights the scroll-hint listener's own attempt to land back
        # on exactly 0 every time.
        tags$div(
          id = session$ns("chart_scroll"),
          style = "overflow-x: auto; overflow-y: hidden; overflow-anchor: none; width: 100%; height: 100%;",
          tags$div(
            style = paste0("width:", px, "px; min-width:", px, "px; height: 100%;"),
            plotlyOutput(session$ns("chart"), height = "100%", width = "100%", fill = FALSE)
          )
        )
      } else {
        # Today's exact behaviour, byte-for-byte, whenever there aren't
        # enough years for that to be worth doing.
        plotlyOutput(session$ns("chart"), height = "100%", fill = TRUE)
      }
    })

    output$chart <- renderPlotly({
      df <- filtered_data()
      validate(need(
        nrow(df) > 0,
        "No values to plot for this view -- try widening the date range."
      ))
      col <- GROWTH_ACCOUNTING_COLORS
      interval <- growth_interval()

      # Per-point (not per-trace) marker colours -- ifelse() recycles the 2
      # scalar branches against df$Truncated, so every period gets that
      # series' normal colour except the (at most 1) truncated one, which
      # gets blend_toward_grey()'s muted version instead. This is what
      # actually renders the "slightly more greyscale" treatment; the notice
      # in growth_has_truncated_period()'s observeEvent above is what tells a
      # reader *why* one period looks different.
      point_colors <- function(base_color) {
        ifelse(df$Truncated, blend_toward_grey(base_color), base_color)
      }

      # 2 bars per period, positioned by literal x-value arithmetic
      # (PosIndex -/+ GROWTH_BAR_OFFSET) rather than offsetgroup -- see that
      # constant's own comment for why (offsetgroup doesn't separate bars at
      # all once barmode is "stack"/"relative", only under "group",
      # confirmed against a real render). barmode "relative" (a
      # layout-level, not per-trace, setting) is what makes the 3 "other
      # factors" traces -- sharing the same PosIndex + GROWTH_BAR_OFFSET
      # x-values -- stack into one bar: positive ones upward from zero,
      # negative ones (MFP growth, in a downturn) downward from zero, rather
      # than plain top-to-bottom cumulative ("stack" mode) which would draw
      # a negative MFP segment overlapping the positive ones instead of
      # visibly subtracting from them. The labour productivity growth
      # trace's different x-values (PosIndex - GROWTH_BAR_OFFSET) never
      # coincide with those, so it never combines with anything -- it just
      # renders as its own bar.
      #
      # text = ~PeriodLabel + "%{text}" in every hovertemplate -- with a
      # multi-year Interval a bar's x-position alone (see PosIndex above)
      # no longer reads as a specific date the way a whole Year tick used
      # to, so the period it belongs to is spelled out in the tooltip too,
      # not left to the tick label below it. textposition = "none" on every
      # trace -- confirmed empirically (a real render) that a bar trace's
      # own default textposition ("auto") draws `text` directly on/above
      # each bar the moment it's set at all, not just make it available to
      # hovertemplate's %{text} -- every bar was showing its own period
      # label stamped on top of it, on top of the *already-present* x-axis
      # tick label doing the same job, before this was added.
      plot_ly(data = df) %>%
        add_trace(
          x = ~PosIndex - GROWTH_BAR_OFFSET, y = ~LPGrowth, type = "bar", text = ~PeriodLabel,
          width = GROWTH_BAR_WIDTH, showlegend = FALSE, textposition = "none",
          marker = list(color = point_colors(col[["lp"]])),
          hovertemplate = "<b>%{y:.1f}%</b><br>Labour productivity growth<br>%{text}<extra></extra>"
        ) %>%
        add_trace(
          x = ~PosIndex + GROWTH_BAR_OFFSET, y = ~CapitalDeepening, type = "bar", text = ~PeriodLabel,
          width = GROWTH_BAR_WIDTH, showlegend = FALSE, textposition = "none",
          marker = list(color = point_colors(col[["cap"]])),
          # GROWTH_ACCOUNTING_VARS[["cap"]] -- StatCan's own exact variable
          # name, not a shorter paraphrase -- see the matching comment on
          # growth_tab_ui()'s legend for why.
          hovertemplate = paste0("<b>%{y:.1f} pp</b><br>", GROWTH_ACCOUNTING_VARS[["cap"]], "<br>%{text}<extra></extra>")
        ) %>%
        add_trace(
          x = ~PosIndex + GROWTH_BAR_OFFSET, y = ~LabourComposition, type = "bar", text = ~PeriodLabel,
          width = GROWTH_BAR_WIDTH, showlegend = FALSE, textposition = "none",
          marker = list(color = point_colors(col[["lab"]])),
          hovertemplate = paste0("<b>%{y:.1f} pp</b><br>", GROWTH_ACCOUNTING_VARS[["lab"]], "<br>%{text}<extra></extra>")
        ) %>%
        add_trace(
          x = ~PosIndex + GROWTH_BAR_OFFSET, y = ~MFPGrowth, type = "bar", text = ~PeriodLabel,
          width = GROWTH_BAR_WIDTH, showlegend = FALSE, textposition = "none",
          marker = list(color = point_colors(col[["mfp"]])),
          hovertemplate = "<b>%{y:.1f} pp</b><br>Multifactor productivity growth (residual)<br>%{text}<extra></extra>"
        ) %>%
        layout(
          title = paste0(
            "Labour productivity growth decomposition (", input$year_range[1], "-", input$year_range[2], ")",
            if (interval > 1) paste0(", ", interval, "-year periods") else "",
            "<br><sup style='color:", INK_MUTED, "'>", input$industry, "</sup>"
          ),
          barmode = "relative",
          xaxis = list(
            title = if (interval == 1) "Year" else "Period",
            # tickvals/ticktext (not dtick/tick0/tickformat) -- PosIndex is
            # this chart's own 1/2/3/... position for each period, not a
            # calendar year (see PosIndex's own comment in filtered_data()),
            # so every position is given its real date-range label
            # explicitly rather than relying on Plotly's numeric autoticking
            # to land on (and format as a plain year) the right values.
            tickvals = df$PosIndex, ticktext = df$PeriodLabel,
            gridcolor = GRIDLINE, color = INK_MUTED
          ),
          yaxis = list(
            # Just "Percentage points" (not "...per year"/"average annual")
            # -- every bar is the *total* growth over its own period, however
            # wide that period is (see filtered_data()'s own comment on why
            # this isn't divided down to an annual rate), so a per-year
            # framing on the axis itself would misdescribe it the moment
            # Interval is above "1 year".
            title = "Percentage points", gridcolor = GRIDLINE, color = INK_MUTED,
            ticksuffix = "%"
          ),
          paper_bgcolor = CHART_SURFACE, plot_bgcolor = CHART_SURFACE,
          font = list(color = INK_PRIMARY, family = FONT_FAMILY),
          showlegend = FALSE,
          # "closest" (Plotly's own single-point default), not "x unified"
          # like every other chart in this app -- unified hover groups by
          # exact x-match, but the 2 bars in one period deliberately sit at
          # 2 different x-values now (see GROWTH_BAR_OFFSET), so "x unified"
          # would only ever surface one bar's tooltip at a time anyway,
          # inconsistently depending on which bar's exact x the cursor was
          # nearest to -- "closest" is at least honest about that.
          hovermode = "closest",
          # A visible zero line -- this chart, unlike most others in this
          # app, defaults to a view where bars routinely sit below zero
          # (a recession year's MFP growth, or LP growth itself), so "above/
          # below flat" needs to be immediately legible rather than left to
          # Plotly's own unstyled default zeroline.
          shapes = list(list(
            type = "line", xref = "paper", x0 = 0, x1 = 1,
            yref = "y", y0 = 0, y1 = 0,
            line = list(color = INK_MUTED, width = 1, dash = "dot")
          ))
        )
      # No forced initial scroll position here -- left at the browser's own
      # default (scrollLeft 0, i.e. the oldest periods), deliberately, even
      # once there are more periods than fit on screen (see
      # GROWTH_CHART_PERIOD_THRESHOLD/output$chart_container): the y-axis
      # (title + tick labels) is drawn once, at the *left* edge of this
      # whole wide plot, same as any other Plotly chart -- it isn't a fixed
      # element outside the scrollable area, so starting scrolled to the
      # right (an earlier version of this chart auto-scrolled there, to
      # open on the most recent periods) scrolled the axis itself out of
      # view right along with the oldest ones. Left/default keeps the axis
      # always in view on open; see www/ui_helpers.js's own "shown.bs.tab"
      # listener for how a reader still discovers this chart scrolls
      # (a brief scroll-and-back nudge the first time this tab is shown),
      # now that the chart itself doesn't jump anywhere on its own.
    })

    # raw_data() is the dependency, not the value used -- see the matching
    # comment on the Trends tab's own data_asof output.
    output$data_asof <- renderUI({
      raw_data()
      data_asof_ui()
    })

    output$download_csv <- downloadHandler(
      filename = function() {
        sprintf(
          "growth_accounting_%s_%s-%s_%syr_%s.csv",
          gsub("[^A-Za-z0-9]+", "-", input$industry),
          input$year_range[1], input$year_range[2], growth_interval(), format(Sys.Date(), "%Y%m%d")
        )
      },
      content = function(file) {
        out <- filtered_data() %>%
          transmute(
            PeriodStart, PeriodEnd, PeriodLabel, Industry = input$industry,
            LPGrowth, CapitalDeepening, LabourComposition, MFPGrowth,
            Truncated = ifelse(Truncated, "Yes", "No")
          )
        # Column headers assigned after the fact (not as transmute()'s own
        # backtick-quoted names) so the capital/labour ones can be built
        # from GROWTH_ACCOUNTING_VARS -- StatCan's own exact variable names
        # -- rather than a separately-typed paraphrase; see the matching
        # comment on growth_tab_ui()'s legend for why. No "average annual"
        # on any of these -- every growth column is the *total* change from
        # Period start to Period end (see filtered_data()'s own comment on
        # why this isn't divided down to an annual rate), accurate as
        # written at Interval "1 year" too (the total over a 1-year period
        # is just that year's own growth).
        names(out) <- c(
          "Period start", "Period end", "Period", "Industry",
          "Labour productivity growth (%)",
          paste0(GROWTH_ACCOUNTING_VARS[["cap"]], " (pp)"),
          paste0(GROWTH_ACCOUNTING_VARS[["lab"]], " (pp)"),
          "Multifactor productivity growth, residual (pp)",
          "Shorter than Interval"
        )
        write.csv(out, file, row.names = FALSE)
      }
    )
  })
}

# "Application unavailable" -- ui()'s fallback when safe_load_mfp_data()
# can't produce a data frame at all (mfp_data.RData missing/corrupt). A
# standalone page_fillable(), same shape as trend_tab_ui()/tab_module_ui(),
# built with no dependency on init_df/variable_choices/industry_tree so it
# never itself touches the data that just failed to load. Reuses the real
# page's font link + theme stylesheet so it still looks like this app, not
# a bare error page.
unavailable_page <- function() {
  page_fillable(
    title = "Canadian Multifactor Productivity Dashboard",
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;600;800&display=swap"
      )
    ),
    tags$style(HTML(sprintf(
      "html { color-scheme: light; }
       body {
         padding: 2rem 2.5rem; background-color: %s; font-family: %s; color: %s;
         display: flex; align-items: center; justify-content: center; min-height: 100vh;
       }
       .app-unavailable-card { max-width: 480px; text-align: center; }
       .app-unavailable-card h1 { font-size: 22px; font-weight: 600; color: %s; margin-bottom: 0.75rem; }
       .app-unavailable-card p { font-size: 14px; font-weight: 300; color: %s; line-height: 1.6; }",
      GRIDLINE, FONT_FAMILY, INK_PRIMARY, BRAND_MAPLE_BLUE, INK_MUTED
    ))),
    tags$div(
      class = "card app-unavailable-card",
      tags$h1("Application unavailable"),
      tags$p(
        "The productivity dataset couldn't be loaded. If you're able to, try running ",
        tags$code("data_pipeline.R"), " from this project folder to regenerate ",
        tags$code("mfp_data.RData"), ", then reload this page. If this persists, contact the ",
        "site maintainer."
      )
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = versioned_asset("csls-shiny-theme.css"))
  )
}

ui <- function(request) {
  # ui() runs per HTTP
  # request, before any session/reactive context exists, so it can't read
  # a reactive value. Computed directly from the data at page-build time
  # (not via a server-side update*Input call) so the default selection is
  # already present in the first HTML the browser receives.
  # safe_load_mfp_data(), not cached_load_mfp_data() directly -- see
  # MFP_DATA_CACHE -- so a repeat page view doesn't pay the parse cost
  # again (only the first request after the file actually changes does),
  # and a missing/corrupt file short-circuits to unavailable_page() instead
  # of crashing UI generation with a raw R error.
  init_df <- safe_load_mfp_data()
  if (is.null(init_df)) return(unavailable_page())
  # Computed once and threaded through to the 4 tab-UI builders below,
  # instead of each of them independently recomputing the same
  # series_choices()/industry_tree_nodes() result from the same init_df --
  # this collapses what would otherwise be 4 series_choices() calls + 4
  # industry_tree_nodes() calls per page load down to 1 and 1 respectively.
  variable_choices <- series_choices(init_df, "Variable", VARIABLE_ORDER)
  industry_tree <- industry_tree_nodes(init_df)

  page_fillable(
    # Turns on Shiny's own built-in busy indicators (shiny >= 1.8; NOT a
    # bslib feature despite living right next to bslib's own page_fillable()
    # -- bslib::busy_indicators() doesn't exist, confirmed against the
    # installed package) -- spinners = a spinner overlay on any
    # plotlyOutput/DTOutput/uiOutput while it carries Shiny's own
    # .recalculating class (the "updating a chart" state), pulse = a
    # top-of-page progress bar for everything else Shiny is busy with (e.g.
    # RAW_DATA_READER's periodic re-poll). Both self-theme off
    # --shiny-spinner-color/--shiny-pulse-background below -- see the
    # tags$style() block right after this tag. Must be placed in the UI
    # itself; it is not a bs_theme()/options() setting.
    useBusyIndicators(),
    title = "Canadian Multifactor Productivity Dashboard", # browser tab title only -- no on-page heading
    # Roboto -- the exact font csls.ca loads (see CSLS-Shiny-Style-Spec.md
    # section 3) -- at the 4 weights the theme actually uses (300/400/600/
    # 800). FONT_FAMILY's own fallback stack (Arial, sans-serif) covers the
    # case this request is blocked, so nothing here depends on it loading.
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;600;800&display=swap"
      )
    ),
    # Brand colours for useBusyIndicators() above -- these are real CSS
    # custom properties shiny's own busy-indicators.css reads
    # (var(--shiny-spinner-color, ...)/var(--shiny-pulse-background, ...)),
    # confirmed by reading that stylesheet directly. Overriding them here is
    # deliberately simpler than introducing a bs_theme() object just for
    # this -- page_fillable() has never taken an explicit theme in this app,
    # and every other brand-colour override already works exactly this way
    # (a tags$style() block against named constants, see BRAND_MAPLE_BLUE/
    # BRAND_JETS_BLUE above). spinner_delay dropped from Shiny's stock 1s to
    # 300ms -- these recomputes are directly filter-driven, so the user
    # expects prompt affirmation, not a delay tuned to avoid flicker on
    # sub-second background work.
    tags$style(HTML(sprintf(
      ":root {
         --shiny-spinner-color: %s;
         --shiny-spinner-delay: 300ms;
         --shiny-pulse-background: linear-gradient(120deg, transparent, %s, %s, transparent);
       }",
      BRAND_JETS_BLUE, BRAND_MAPLE_BLUE, BRAND_JETS_BLUE
    ))),
    # "Starting the application" / "Loading data" splash -- literal
    # server-rendered HTML (not built by JS), so it's part of the very first
    # response ui(request) sends and is visible before Shiny's JS bundle has
    # even downloaded, let alone connected. Hidden by www/ui_helpers.js on
    # the first shiny:idle event (see there for why not
    # shiny:sessioninitialized). Copy is fixed rather than conditioned on
    # cache warmth -- this app deploys to Posit Connect Cloud, which can
    # spin up a cold instance in response to the very first request after a
    # period of inactivity, so the wording needs to read right whether the
    # user waited 200ms or several seconds; nothing on this page can render
    # any *earlier* than this even in the cold-instance case, since zero
    # HTTP bytes exist yet for the browser to show until this response
    # starts arriving.
    tags$div(
      id = "app-splash",
      tags$div(class = "app-splash-spinner"),
      tags$p(class = "app-splash-text", "Loading dashboard…"),
      tags$p(class = "app-splash-subtext", "This may take a few seconds on the first visit.")
    ),
    tags$style(HTML(sprintf(
      "#app-splash {
         position: fixed; inset: 0; z-index: 100000;
         display: flex; flex-direction: column; align-items: center; justify-content: center;
         gap: 0.75rem; background: %s; font-family: %s;
         transition: opacity 280ms ease;
       }
       #app-splash.app-splash-hidden { opacity: 0; pointer-events: none; }
       .app-splash-spinner {
         width: 40px; height: 40px; border-radius: 50%%;
         border: 4px solid %s; border-top-color: %s;
         animation: app-splash-spin 0.8s linear infinite;
       }
       @keyframes app-splash-spin { to { transform: rotate(360deg); } }
       .app-splash-text { font-size: 16px; font-weight: 600; color: %s; margin: 0; }
       .app-splash-subtext { font-size: 13px; font-weight: 300; color: %s; margin: 0; }",
      CHART_SURFACE, FONT_FAMILY, GRIDLINE, BRAND_JETS_BLUE, INK_PRIMARY, INK_MUTED
    ))),
    # page() renders straight into <body> with no margin/padding of its
    # own, so the intro text would otherwise butt right up against the
    # browser edge -- give the whole page some breathing room on top/sides.
    #
    # nav-pills only puts a visible background on the *active* pill by
    # default -- give every pill a border + margin so all 4 read as
    # distinct, fully-rounded (pill/capsule-shaped, not just rounded-corner)
    # buttons with a gap between them, not one continuous flush bar with
    # just the selected segment highlighted. margin-bottom on the row
    # itself puts a little breathing room between the buttons and the card
    # below them.
    #
    # Colors/font reuse the same named constants the charts use (see top of
    # file) so the page chrome and the plots stay a single source of truth
    # instead of a second set of hardcoded hex values drifting out of sync.
    #
    # color-scheme: light tells the browser this page is deliberately
    # light-themed -- without it, some browsers' "auto dark mode for
    # websites" feature (on for some users at the OS/browser level) inverts
    # our light grey/white palette into a dark one on its own.
    tags$style(HTML(sprintf(
      "html { color-scheme: light; }
       /* This body{padding:...} rule's own padding half never actually
          applied -- page_fillable() puts a real class, .bslib-page-fill,
          on <body> itself, and that bslib stylesheet's own
          `padding: var(--bslib-spacer, 1rem)` rule (a single-class
          selector, specificity 0,1,0) beats a bare `body` element
          selector (0,0,1) regardless of which stylesheet loads later --
          confirmed against the live page via CDP's getMatchedStylesForNode,
          which shows bslib's rule as the one actually winning. Every
          margin the rest of this page was ever visually tuned against
          (sidebar/card left edge, etc.) was tuned against THAT padding
          (a uniform ~24px on all four sides), not the 32px/40px this line
          appears to ask for -- so left/right are deliberately left alone
          below (re-specifying them here would be the actual behaviour
          change); only the redundant bottom inset is trimmed, via the
          higher-specificity body.bslib-page-fill override right after
          this rule, to let the sidebar/chart card reach closer to the
          bottom of the page as requested. */
       body { background-color: %s; font-family: %s; color: %s; }
       /* See the comment above -- this narrowly out-specifies bslib's own
          .bslib-page-fill padding rule for just padding-bottom (the
          property this selector sets), while padding-top/left/right fall
          through unchanged to bslib's rule exactly as they already did. */
       body.bslib-page-fill { padding-bottom: 12px; }
       /* .card's own radius/padding/background come from csls-shiny-theme.css
          (section 8: 16px radius, 40px padding, white) -- no local override
          needed here now that that stylesheet is loaded (see tags$head()
          above and the trailing tags$link() below). card-sidebar (added to
          the Trends/Rankings/Compare/Data cards -- see trend_tab_ui() etc.)
          overrides just the left side of that 40px padding down to 0: those
          4 cards all wrap a layout_sidebar(), and the sidebar's own tinted
          box was landing 40px right of the page's actual left edge (the
          intro text/nav-pills below), double-inset by both the card's
          padding and the sidebar's own -- zeroing the card's left padding
          here lets the sidebar box sit flush with the card's true edge
          (which already lines up with the page edge) instead. Every other
          side of the card's padding (top/right/bottom) is untouched.
          .card.card-sidebar (2 classes), not just .card-sidebar -- this
          block is rendered before csls-shiny-theme.css's own .card padding
          rule (that stylesheet is deliberately the LAST tag on the page --
          see the tags$link() comment below), so a same-specificity
          single-class rule here would lose that cascade tie (later same-
          specificity source wins) even though it's the one meant to win;
          the extra .card doubles this selector's specificity so it wins
          regardless of source order. */
       .card.card-sidebar { padding-left: 0; }
       /* gap, not a per-link margin -- a margin on .nav-link put its own
          0.5rem of whitespace on the *outside* ends of the row too (before
          the first pill, after the last), so the row of buttons started
          8px right of the page's actual left edge (the intro text just
          above it). gap only ever sits *between* flex items (.nav-pills is
          Bootstrap 5's .nav, already display:flex), so the same spacing
          between pills no longer touches the row's own outer edges. */
       .nav-pills { margin-bottom: 1rem; gap: 0.75rem; }
       .nav-pills .nav-link {
         border: .5px solid rgba(0,0,0,.24); border-radius: 999px;
         color: %s; background-color: %s; font-weight: 400;
         transition: background-color 280ms ease, color 280ms ease, border-color 280ms ease;
       }
       /* csls.ca's interaction rule: every hover/focus state turns jets
          blue (see CSLS-Shiny-Style-Spec.md section 2) -- applied here to
          both the resting-tab hover and the persistent active tab, so the
          justified pill nav reads as the same control family as the rest
          of the page's links/buttons instead of keeping its own maple-only
          palette. */
       .nav-pills .nav-link:hover, .nav-pills .nav-link:focus-visible {
         border-color: %s; color: %s; background-color: rgba(5,152,216,.1); outline: 0;
       }
       .nav-pills .nav-link.active, .nav-pills .nav-link.active:hover {
         background-color: %s; color: %s; font-weight: 600; border-color: %s;
       }
       .trend-more-options > summary { cursor: pointer; list-style: none; display: flex; align-items: center;
         gap: 0.35rem; color: %s; font-size: 0.9rem; margin-bottom: 0.5rem; transition: color 280ms ease; }
       .trend-more-options > summary:hover { color: %s; }
       .trend-more-options > summary::-webkit-details-marker { display: none; }
       .trend-more-options > summary::before { content: '\\25B8'; transition: transform 0.15s ease; }
       .trend-more-options[open] > summary::before { transform: rotate(90deg); }
       /* page_fillable() makes <body> a fill-height flex column, but
          navset_pill()'s own .tab-content/.tab-pane wrapper markup is all
          plain block, none of it a flex container passing that fill
          height further down -- without these, the card (which bslib DOES
          mark fill-aware) never receives a stretched height to fill.
          .tab-content sits directly under <body> now (see the ul.nav/
          .tab-content split just above, pulled apart so the logo can sit
          beside just the nav-pills row) rather than nested inside
          navset_pill()'s own .tabbable wrapper div, so flex:1 1 auto on
          .tab-content alone is what grows it to fill the remaining body
          height -- no .tabbable rule needed any more. */
       .tab-content { flex: 1 1 auto; display: flex; flex-direction: column; min-height: 0; }
       .tab-content > .tab-pane.active { flex: 1 1 auto; display: flex; flex-direction: column; min-height: 0; }
       /* bslib's own .card.html-fill-item CSS defaults to flex: 0 1 auto
          (content-sized, not growing) -- override just the one directly
          inside our now-fillable tab pane so it actually claims the space
          the layers above are now correctly offering it. margin-bottom: 0
          strips a stray ~24px bslib puts below every .card by default --
          on most pages that's a gap before the *next* section, but this
          card is the last thing on the page, so it was just dead space
          between the sidebar/chart and the bottom edge for no visual
          purpose. */
       .tab-pane.active > .card { flex: 1 1 auto; min-height: 0; margin-bottom: 0; }",
      CHART_SURFACE, FONT_FAMILY, INK_PRIMARY,
      BRAND_MAPLE_BLUE, CHART_SURFACE,
      BRAND_JETS_BLUE, BRAND_JETS_BLUE,
      BRAND_JETS_BLUE, CHART_SURFACE, BRAND_JETS_BLUE,
      BRAND_MAPLE_BLUE,
      BRAND_JETS_BLUE
    ))),
    # A second, separate tags$style() -- sprintf() (used above and below for
    # the color-token substitutions) refuses to run on a format string past
    # 8192 characters, which one single CSS block spanning the whole page
    # eventually hit. This block has no %s color tokens of its own, so it's
    # plain HTML(), not sprintf(), and splitting it out here also keeps
    # each remaining sprintf() call comfortably under that limit.
    tags$style(HTML(
      "/* page_fillable() pins the whole page to viewport height with no
          page-level scrolling (by design, so Trends/Compare's Plotly
          charts fill exactly one screen) -- fine for a chart sized to fill
          100% of its card, but a DT table's true height (rows plus its own
          Showing-X-of-Y/pagination footer) is content-driven and routinely
          exceeds that fixed budget. Without this, anything past the budget
          was silently clipped, which is what made the Source/Data-last-
          refreshed lines after the table disappear or overlap DT's own
          footer instead of just being scrolled to. Scoped to just the Data
          tab's card (see class = if (kind == table) ... in
          tab_module_ui()) -- Trends/Compare keep the plain clipped-to-
          viewport behavior, which is what makes their charts fill height
          instead of pushing the page taller. */
       .tab-pane.active > .card.table-tab-card { overflow-y: auto; }
       /* Same mechanism, for the Rankings tab (see ranking_tab_ui()'s
          ranking-tab-card class and ranking_tab_server()'s
          output$chart_container) -- a no-op whenever that chart is small
          enough to just fill the card at its normal height=100%, and only
          produces a real scrollbar once there are more industries than fit
          on one screen, so every one of them stays fully legible instead
          of being squeezed/clipped into a fixed budget. */
       .tab-pane.active > .card.ranking-tab-card { overflow-y: auto; }
       /* Same mechanism again, for the Definitions tab (see
          definitions_tab_ui()'s definitions-tab-card class) -- unlike the
          other 3 cards this scrolls, this one has no sidebar/chart at all,
          just a plain glossary that routinely runs longer than one screen
          (26 variables' worth of definitions), so it needs the same
          card-level scrollbar rather than being clipped to the viewport. */
       .tab-pane.active > .card.definitions-tab-card { overflow-y: auto; }
       /* Every tab's Download dropdown (see download_menu_ui()) -- full
          width so the toggle button lines up with the other sidebar
          controls above it instead of sizing to its own label. */
       .download-dropdown { width: 100%; }
       .download-dropdown .dropdown-menu { width: 100%; }
       /* Bootstrap's .dropdown-item defaults to white-space: nowrap, sized
          for a normal wide dropdown -- fine there, but this menu is pinned
          to the sidebar's own (narrow) width above, so a label like
          'Download displayed data (.csv)' would run past the sidebar edge
          and get clipped instead of wrapping, the way every other sidebar
          control's label already does. */
       .download-dropdown .dropdown-item { white-space: normal; }"
    )),
    tags$style(HTML(sprintf(
      "/* Collapsible search/scroll dropdown (see www/tree_select.js), used
          for Variable/Industry on Trends/Rankings and for Industry on
          Compare/Data -- toggle styled to csls.ca's own form-control spec
          (42px tall, 8px radius, 14px/weight-300 text, rgba(0,0,0,.24)
          hairline border, jets blue on hover/focus -- CSLS-Shiny-Style-Spec.md
          section 4) so it reads as the same control family as every other
          picker, selectize ones included. The toggle is a real text
          <input> -- clicking or tabbing into it opens the panel and blanks
          the box (rather than leaving the current label there to be
          typed over) so you can search or scroll fresh, same as
          selectize's own single-select behaviour; picking a row (or not)
          is what shows/restores the display text, so there's no second
          search box to style separately. Chevron is a plain
          absolutely-positioned span next to the input (reusing
          .trend-more-options' rotate-on-open technique above) rather than
          an ::after pseudo-element -- browsers don't render generated
          content on replaced elements like <input>. A childless row hides
          its own arrow (.tree-select-row.no-children below) -- for
          Industry's leaf rows that's visibility:hidden, keeping the blank
          space so the label still lines up with sibling branch rows'
          visible arrows one level up; a genuinely flat list (Variable --
          every row childless, no sibling ever has a visible arrow to align
          against) instead collapses that gutter away entirely via
          .tree-select-flat, so it doesn't read as a stray indent in front
          of every label. */
       .tree-select { position: relative; width: 100%%; }
       .tree-select-toggle {
         display: block; width: 100%%; height: 42px; min-height: 42px; margin: 0;
         padding: 10px 36px 10px 16px;
         border: .5px solid rgba(0,0,0,.24); border-radius: 8px;
         background-color: %s; color: %s;
         font-size: 14px; font-weight: 300; line-height: 1.4;
         transition: border-color 280ms ease;
       }
       /* Native placeholder attribute now that the toggle is a real
          <input> -- keys off the same muted token the site's own
          .form-control::placeholder rule uses. opacity:1 overrides
          Firefox's default (0.54) so it matches other pickers exactly. */
       .tree-select-toggle::placeholder { color: %s; opacity: 1; }
       .tree-select-toggle:hover, .tree-select-toggle:focus,
       .tree-select-toggle[aria-expanded=\"true\"] { border-color: %s; outline: 0; }
       .tree-select-chevron {
         position: absolute; right: 16px; top: 50%%; color: %s; pointer-events: none;
         font-size: 14px; line-height: 1;
         transform: translateY(-50%%) rotate(90deg); transition: transform 0.15s ease;
       }
       .tree-select-toggle[aria-expanded=\"true\"] ~ .tree-select-chevron { transform: translateY(-50%%) rotate(-90deg); }
       .tree-select-panel {
         position: absolute; z-index: 20; top: calc(100%% + 0.25rem); left: 0; width: 100%%;
         max-height: 320px; overflow-y: auto; background: %s; border: 0;
         border-radius: 8px; box-shadow: 0 14px 36px rgba(0,0,0,.16);
       }
       .tree-select-list, .tree-select-children { list-style: none; margin: 0; padding: 0; }
       .tree-select-row { display: flex; align-items: center; gap: 0.35rem; padding: 0.15rem 0.5rem; }
       .tree-select-arrow { cursor: pointer; transition: transform 0.15s ease; color: %s; width: 1em; text-align: center; }
       .tree-select-row.expanded > .tree-select-arrow { transform: rotate(90deg); }
       .tree-select-row.no-children > .tree-select-arrow { visibility: hidden; }
       /* display:none, not visibility:hidden -- unlike a leaf row's own
          arrow above, a flat list's arrow gutter isn't reserving space to
          stay aligned with anything (there's no branch row anywhere in
          it), so it should take up no room at all rather than a blank
          1em + gap indent in front of every label. */
       .tree-select-flat .tree-select-arrow { display: none; }
       .tree-select-label {
         cursor: pointer; flex: 1 1 auto; padding: 10px 16px; border-radius: 4px;
         font-size: 14px; font-weight: 300; transition: background-color 280ms ease, color 280ms ease;
       }
       /* Matches csls.ca's own dropdown-option treatment exactly (see
          .selectize-dropdown .active / .option:hover in
          csls-shiny-theme.css): a jets-blue tint background with maple-blue
          text, not a solid fill -- for both the row being hovered and the
          one currently selected. */
       .tree-select-label:hover, .tree-select-label:focus-visible,
       .tree-select-row.selected > .tree-select-label { background: rgba(5,152,216,.1); color: %s; }
       .tree-select-node[hidden] { display: none; }
       /* Removable chip list for the Compare/Data 'Series to compare' list
          -- styled like csls.ca's own selectize multi-value chips
          (.selectize-control.multi .selectize-input > .item in
          csls-shiny-theme.css: tint background, maple text, 4px radius)
          rather than echoing the pill-shaped nav/button radius. */
       .pair-chip-list { display: flex; flex-wrap: wrap; gap: 0.4rem; margin: 0.5rem 0; }
       .pair-chip {
         display: inline-flex; align-items: center; gap: 0.35rem; padding: 2px 8px 2px 10px;
         border: 0; border-radius: 4px;
         background: rgba(5,152,216,.1); color: %s; font-size: 13px; font-weight: 400;
       }
       .pair-chip-swatch { width: 0.6rem; height: 0.6rem; border-radius: 50%%; flex-shrink: 0; }
       .pair-chip-remove {
         border: none; background: transparent; color: %s; border-radius: 50%%;
         width: 1.1rem; height: 1.1rem; line-height: 1; cursor: pointer; padding: 0;
         transition: background-color 280ms ease, color 280ms ease;
       }
       .pair-chip-remove:hover { background: rgba(5,152,216,.1); color: %s; }",
      CHART_SURFACE, INK_PRIMARY,
      INK_MUTED,
      BRAND_JETS_BLUE,
      INK_PRIMARY,
      CHART_SURFACE,
      INK_MUTED,
      BRAND_MAPLE_BLUE,
      BRAND_MAPLE_BLUE,
      INK_MUTED,
      BRAND_JETS_BLUE
    ))),
    # The Growth Accounting tab's sidebar legend -- swatches styled like the
    # Compare/Data tabs' pair-chip-swatch (a small solid circle) rather than
    # reintroducing a second swatch shape, but laid out as a plain list
    # (each row is a fixed colour -> label mapping, nothing to remove/click)
    # instead of the chip-list's inline flex-wrap layout.
    tags$style(HTML(
      ".growth-legend { margin: 0.75rem 0; }
       .growth-legend-list { list-style: none; margin: 0.4rem 0; padding: 0; }
       .growth-legend-list li { display: flex; align-items: center; gap: 0.5rem; padding: 2px 0; font-size: 13px; }
       .growth-legend-swatch { width: 0.7rem; height: 0.7rem; border-radius: 50%; flex-shrink: 0; }
       /* The 'Use this visualization...' blurb right under the legend list
          -- its default browser/Bootstrap margins (p's own margin-top: 0,
          margin-bottom: 1rem) left it sitting almost flush against the
          legend above (just the list's own 0.4rem margin-bottom) while
          opening up more room than wanted below, since that 1rem bottom
          margin collapses with .growth-legend's own margin-bottom (0.75rem)
          to whichever is bigger -- explicit margins here move the text down
          a bit relative to the legend, and up relative to the Download
          button below by not leaving a bigger collapsed margin than
          .growth-legend's own for the sidebar's own item-to-item gap (see
          the next rule) to add to. */
       .growth-legend p { margin: 0.75rem 0 0; }
       /* The sidebar's own flex `gap` between direct children (bslib's
          sidebar-content, ~2.25rem) applies between .growth-legend and the
          Download button below it regardless of either one's own margin --
          confirmed via the live cascade (same mechanism as the Definitions
          tab's card-body gap, see [[mfp-definitions-tab]] memory) that
          margin tweaks alone can't close that gap below a fixed floor. A
          small negative margin-top pulls the Download button up into that
          gap instead, scoped via the adjacent-sibling combinator so this
          only ever matches the Growth Accounting tab's own Download button
          (the only .download-dropdown that directly follows a
          .growth-legend), not the other 4 tabs' otherwise-identical
          download_menu_ui() markup. */
       .growth-legend + .download-dropdown { margin-top: -0.75rem; }"
    )),
    # The Definitions tab (see definitions_tab_ui()'s own comment on why
    # title+subtitle are wrapped in .definitions-header to begin with). h4's
    # ~0.5rem margin-bottom (Bootstrap's own default heading rule;
    # csls-shiny-theme.css's h1-h6 rule only zeroes margin-top) is the one
    # bit of spacing actually worth zeroing by hand here -- bslib's own
    # bundled CSS (`.bslib-card .card-body p`) already zeroes a <p>'s own
    # margin-top for free inside any card, descendant combinator so it
    # reaches straight through the .definitions-header wrapper div too, and
    # already out-specifies a bare `.definitions-subtitle` rule trying to
    # set one explicitly (confirmed via the live cascade: a margin-top set
    # here is silently never applied) -- so definitions-subtitle needs no
    # margin-top rule of its own at all; the small gap the title and
    # subtitle actually end up with is just ordinary line-height leading on
    # each, which alone already reads as "just underneath", not flush.
    # definitions-search's own top margin is trimmed slightly too, nudging
    # the search box (and everything below it, which cascades in normal
    # flow from there) up in turn so the whole block reads as one
    # tightened-up unit rather than the old gap just reappearing lower down.
    tags$style(HTML(
      ".definitions-title { margin-bottom: 0; }
       .definitions-search { margin: 0.75rem 0 1rem; max-width: 320px; }"
    )),
    # The glossary itself -- a plain <dl>, one .definitions-item (a dt/dd
    # pair, see definitions_tab_ui()'s own comment on why each is wrapped in
    # a div) per variable, spaced so each entry reads as its own block
    # rather than running into the next one the way a browser's unstyled
    # <dl> (dt/dd flush together, no gap before the next dt) would.
    # .definitions-item[hidden] is the search box's own doing (see
    # www/ui_helpers.js's "definitions-search-input" listener) -- spelled
    # out explicitly rather than relying on the bare [hidden] the browser
    # already applies by default, since an author-origin display rule on
    # the same element (were one ever added here) would otherwise be free
    # to override that UA-level default regardless of specificity.
    tags$style(HTML(
      ".definitions-list { margin: 0; }
       .definitions-item { margin-top: 1rem; border-radius: 6px; transition: background-color 400ms ease, box-shadow 400ms ease; }
       .definitions-item:first-child { margin-top: 0; }
       .definitions-item[hidden] { display: none; }
       .definitions-item dt { font-weight: 600; margin: 0; }
       .definitions-item dd { margin: 0.25rem 0 0 0; }
       /* The brief highlight a goto_definition_link() jump lands on (see
          the 'goto-definition-link' click handler in www/ui_helpers.js) --
          background-color + box-shadow only, deliberately no padding/margin
          change, so adding or removing this class never shifts this item's
          own box size or nudges its neighbours -- just a soft colour wash
          that fades in on arrival and back out once the reader scrolls. The
          box-shadow's spread is what gives the highlight some breathing
          room around the text without that padding change. */
       .definitions-item-highlight { background-color: rgba(5,152,216,.1); box-shadow: 0 0 0 10px rgba(5,152,216,.1); }"
    )),
    # goto_definition_link()'s 5 instances (Trends' "More", Compare/
    # Rankings/Data's "Definition", Growth Accounting's "Learn more") --
    # deliberately the plain browser-default hyperlink look (blue,
    # underlined) instead of this app's own house link style (maple blue,
    # no underline -- see the plain `a` rule in csls-shiny-theme.css) at the
    # user's explicit request, so these read unmistakably as links planted
    # inside otherwise-plain muted text, rather than blending into it the
    # way the site's understated house style would. #0000EE is the actual
    # browser-default `:link` blue, not a colour picked by eye. `:visited`
    # is pinned to the same blue rather than left to the browser's own
    # default purple -- every instance shares the literal href="#" (see
    # goto_definition_link()), and :visited matches by URL, not by DOM node,
    # so without this, clicking any *one* of these 5 links would turn every
    # other one purple too, mislabelling links the reader never actually
    # clicked as "already visited". Specificity-wise this needs no
    # special-casing to win over csls-shiny-theme.css's own plain `a`/
    # `a:hover` rules -- 1 class alone already out-specifies a bare type
    # selector or type+pseudo-class pair, regardless of source order.
    tags$style(HTML(
      ".goto-definition-link, .goto-definition-link:visited { color: #0000EE; text-decoration: underline; }
       .goto-definition-link:hover, .goto-definition-link:focus-visible { color: #0000EE; text-decoration: underline; }"
    )),
    # The Compare/Rankings/Data tabs' own "Definition" link specifically
    # (scoped to its 3 renderUI() output ids -- see ranking_tab_server()'s
    # and tab_module_server()'s output$definition_link -- so this doesn't
    # touch the Trends/Growth Accounting tabs' own goto-definition-link
    # instances, which stay inline text with no positioning of their own).
    # Both output divs render with `display: contents` (bslib's own default
    # for a shiny-html-output inside a fillable sidebar -- confirmed via the
    # live cascade that the wrapper div itself generates no box at all, so
    # this rule targets the <a> directly rather than the wrapper), which is
    # also why margin here works exactly like it did for the Growth
    # Accounting tab's Download button (see that CSS's own comment): the
    # <a> becomes a direct flex item of the sidebar's own column layout,
    # picking up that same fixed item-to-item gap no ordinary margin alone
    # can close, hence the negative margin-top rather than a positive one.
    # A small positive margin-left is the "slightly more inward" nudge off
    # the Variable picker's own left edge (which this <a> otherwise sits
    # perfectly flush against, being a sibling flex item at the same
    # indent) -- purely a placement preference, not fixing a misalignment.
    tags$style(HTML(
      "#bar-definition_link .goto-definition-link,
       #ranking-definition_link .goto-definition-link,
       #table-definition_link .goto-definition-link {
         margin: -1rem 0 0 0.25rem;
       }"
    )),
    tags$script(src = versioned_asset("tree_select.js")),
    tags$script(src = versioned_asset("ui_helpers.js")),
    # Logo, left of the nav-pills row (no on-page title text anymore -- the
    # page(title=...) string above is only ever the *browser tab* title).
    # app-header-row has no horizontal margin/padding of its own, so it
    # inherits the page's left edge from <body>'s own padding (see the body
    # rules above) -- the logo lines up flush-left with the card/sidebar
    # edge below it, same as every other top-level element on this page.
    # The nav-pills row is pulled into that same flex row via flex:1, so
    # the pills now fill the width remaining *after* the logo + gap instead
    # of the full page width -- justified/equal-width across that narrower
    # space, shifted right just enough to leave the logo room.
    #
    # No margin-bottom of its own on app-header-row -- page_fillable()'s
    # own body already puts a structural `gap` between every direct child
    # (confirmed via getMatchedStylesForNode: body picks up
    # .bslib-page-fill's `gap: var(--bslib-spacer, 1rem)`), so an explicit
    # margin here was pure double-spacing on top of that, stacking with it
    # to push the card further from the buttons than intended. margin-top
    # nudges the logo/buttons down slightly from the page's own top edge
    # (independent of that structural gap, which sits *after* this row).
    # tab-content's small negative margin-top pulls the card up into part
    # of that same structural gap, tightening the row-to-card space a
    # little without touching --bslib-spacer itself (which also governs
    # the chart-to-Source-line gap inside the card, left alone).
    #
    # navset_pill (button-styled tabs) + nav-justified (stretched to fill
    # the available width in equal-width segments) instead of the default
    # left-aligned, content-width nav-tabs underline style.
    #
    # Depends on bslib's internal navset_pill() markup exposing a
    # "ul.nav"/".tab-content" pair to split apart -- if a future bslib
    # version restructures that, find() just matches nothing and
    # selectedTags() then returns an empty list (verified: this drops the
    # tabs/content entirely rather than erroring), so a bslib upgrade is a
    # "check this still renders" item, not a silent breakage risk.
    tags$style(HTML(
      ".app-header-row { display: flex; align-items: center; gap: 0.75rem; margin-top: 0.5rem; }
       /* Wide logo (icon + wordmark, ~3.46:1) -- height fixed to match the
          nav-pills row, width left auto so it scales at its own natural
          aspect ratio instead of being squashed into the old square 40px
          icon box. */
       .app-header-logo { height: 52px; width: auto; flex-shrink: 0; }
       .app-header-row .nav-pills { flex: 1 1 auto; margin-bottom: 0; }
       .tab-content { margin-top: -1rem; }"
    )),
    (function() {
      navset <- tagQuery(
        navset_pill(
          nav_panel("Trends", trend_tab_ui("trend", init_df, variable_choices, industry_tree)),
          nav_panel("Compare", tab_module_ui("bar", init_df, "bar", variable_choices, industry_tree)),
          nav_panel("Rankings", ranking_tab_ui("ranking", init_df, variable_choices)),
          nav_panel("Growth Accounting", growth_tab_ui("growth", init_df, industry_tree)),
          nav_panel("Data", tab_module_ui("table", init_df, "table", variable_choices, industry_tree)),
          nav_panel("Definitions", definitions_tab_ui())
        )
      )
      nav_ul <- navset$find("ul.nav")$addClass("nav-justified")$selectedTags()
      tab_content <- navset$find(".tab-content")$selectedTags()
      tagList(
        tags$div(
          class = "app-header-row",
          tags$img(
            src = versioned_asset("csls-logo-wide.png"),
            alt = "Centre for the Study of Living Standards",
            class = "app-header-logo"
          ),
          nav_ul
        ),
        tab_content
      )
    })(),
    # csls-shiny-theme.css (www/) is the drop-in stylesheet that makes every
    # Bootstrap control -- inputs, selects, checkboxes/radios, buttons, the
    # DT table, the ionRangeSlider year picker -- match csls.ca's own
    # metrics (42px/8px-radius controls, 24px checkboxes, pill buttons,
    # etc.) instead of Bootstrap's defaults. Placed as the LAST tag in the
    # whole page rather than up in tags$head() with the Google Fonts link
    # above: a <link rel="stylesheet"> works from anywhere in the document,
    # and putting it last in DOM order is what actually guarantees it wins
    # the cascade over bslib's own Bootstrap bundle for same-specificity
    # selectors (bslib injects that bundle's <link> into <head> itself, at a
    # point in the render pipeline this file doesn't control) -- no
    # !important needed anywhere in that stylesheet as a result. Loaded via
    # versioned_asset() like every other www/ asset here, so an edit to it
    # takes effect on reload instead of serving a stale cached copy.
    tags$link(rel = "stylesheet", type = "text/css", href = versioned_asset("csls-shiny-theme.css"))
  )
}

server <- function(input, output, session) {
  # Off by default in Shiny -- without this, a dropped websocket (a brief
  # network blip, not a crashed R process) never even attempts to resume;
  # it goes straight to the "Connection lost" full-page state (see
  # #shiny-disconnected-overlay styling in csls-shiny-theme.css) with no
  # chance to recover in place. This lets Shiny's own reconnect attempt (its
  # "Attempting to reconnect... Try now" toast, restyled alongside every
  # other showNotification() in this app) run first; the click-to-reload
  # overlay stays as the fallback for when reconnection genuinely can't
  # succeed (e.g. the R process itself died, as in the mapply/`&` crash
  # fixed earlier -- reconnecting to a dead process can't help, so that
  # class of error still ends up at the overlay regardless of this setting).
  session$allowReconnect(TRUE)

  raw_data <- RAW_DATA_READER # single shared reactiveFileReader, not duplicated per tab

  # UOM is 1:1 per Variable in this table (StatCan's own convention here,
  # confirmed against the real data) -- one small shared lookup (26 rows),
  # recomputed only when raw_data() itself changes, instead of each of the
  # 4 tabs below independently re-filtering the full raw_data() on every
  # Variable pick just to read off one constant.
  variable_uom_lookup <- reactive(distinct(req(raw_data()), Variable, UOM))

  trend_tab_server("trend", raw_data, variable_uom_lookup)
  ranking_tab_server("ranking", raw_data, variable_uom_lookup)
  tab_module_server("bar", raw_data, "bar", variable_uom_lookup)
  tab_module_server("table", raw_data, "table", variable_uom_lookup)
  growth_tab_server("growth", raw_data)
}

shinyApp(ui, server)
