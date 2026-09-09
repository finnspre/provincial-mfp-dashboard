#install packages
library("cansim")
library("dplyr")

#define function that returns folder location of current script
script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg))))
  }
  frame_files <- Filter(Negate(is.null), lapply(sys.frames(), function(f) f$ofile))
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[[length(frame_files)]])))
  }

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    doc_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(doc_path)) {
      return(dirname(normalizePath(doc_path)))
    }
  }
  stop("Could not determine script location. Run this with Rscript or source(), or setwd() to this script's folder first.")
}

# STATCAN_TABLE_ID / RAW_STATCAN_CONTRACT / MFP_DATA_CONTRACT / validate_data_contract()
# -- shared with app.R so both agree on exactly one definition of "what does
# this table's data look like" (see that file's own header comment).
# local = TRUE so these land in this script's own execution environment
# rather than unconditionally in .GlobalEnv (source()'s own default) -- see
# the matching comment on app.R's identical source() call for why that
# distinction matters even though, for a plain `Rscript data_pipeline.R` run
# specifically, the two happen to be the same environment.
source(file.path(script_dir(), "data_contract.R"), local = TRUE)

#retrieve data from statscan table 0208 (multifactor productivity)
mfp_data <- get_cansim(STATCAN_TABLE_ID)

# Fail loudly, immediately, if StatCan has renamed/dropped a column this
# script depends on below -- before the rename()/mutate() chain gets a
# chance to fail with a more cryptic "object not found" partway through, or
# (worse) silently produce a wrong-shaped result that only surfaces as a
# strange chart in the app much later.
validate_data_contract(mfp_data, RAW_STATCAN_CONTRACT, paste0("raw get_cansim(\"", STATCAN_TABLE_ID, "\") pull"))

# This table's Geography dimension carries exactly one value ("Canada") --
# there is no provincial/territorial breakdown the way the old labour
# productivity table had, so unlike that table's GEO column, this one isn't
# kept as a real column at all: a column that can only ever say "Canada"
# gives every picker/export a dimension with nothing to distinguish, so it's
# dropped here rather than carried through as dead weight. If StatCan ever
# adds a geography breakdown to this table, GEOGRAPHY_ORDER-style picker
# support would need to be reintroduced in app.R at that point.
#
# Hierarchy for the NAICS classification column is a dot-path of ancestor
# IDs (e.g. "1.11"), so its number of segments is the depth: 1 = "Business
# sector" (the whole-business-sector total) plus 4 special aggregations
# (goods/services split, durable/non-durable manufacturing split) that sit
# alongside it at the same depth without nesting under it in the dot-path;
# 2 = the major NAICS sub-sectors nested directly under "Business sector".
# Unlike the old labour productivity table, this table has no deeper tier to
# drop -- every row that exists is kept.
#
# Stored as a real column -- not a bare vector kept "in lockstep" with
# mfp_data's row order by convention -- so that subsetting mfp_data below can
# never desync it from the rows it actually describes, however mfp_data ends
# up filtered/reordered in the future.
naics_col <- "North American Industry Classification System (NAICS)"
naics_hierarchy_col <- "Hierarchy for North American Industry Classification System (NAICS)"
mfp_data$IndustryDepth <- lengths(strsplit(as.character(mfp_data[[naics_hierarchy_col]]), "[.]"))

# Shape data frame to be more readable for app.R
mfp_data <- mfp_data %>%
  rename(
    Variable = `Multifactor productivity and related variables`,
    Industry = !!naics_col
  ) %>%
  mutate(
    Year = as.integer(REF_DATE),
    Value = as.numeric(VALUE),
    Variable = as.character(Variable),
    Industry = as.character(Industry),
    IndustryLevel = ifelse(IndustryDepth == 1, "Aggregate", "2-digit")
  ) %>%
  select(Year, Variable, Industry, IndustryLevel, Value, UOM)

# Fail loudly, before save(), if the cleaning above produced anything other
# than exactly the shape app.R's load_mfp_data() is entitled to assume -- see
# MFP_DATA_CONTRACT's own comment in data_contract.R. This is what stops a
# pipeline bug from silently overwriting a good mfp_data.RData with a bad
# one: save() below never runs unless this passes.
validate_data_contract(mfp_data, MFP_DATA_CONTRACT, "data_pipeline.R output (pre-save)")

#save the multifactor productivity data frame next to this script
save(mfp_data, file = file.path(script_dir(), "mfp_data.RData"))
