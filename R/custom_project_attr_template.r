# Custom Project Attributes Template
#
# PURPOSE
# - Define custom stand-year outputs that run inside vegClass `main()` / `vegOut()`.
#
# IMPORTANT RULES
# - main() discovers and runs ALL functions in this script.
# - Each runnable function must use signature: function(data, context)
# - Each runnable function must return exactly one-row data.frame
# - If output column names are a duplicate, a note will appear in terminal and
#   the name will be given a "_2" at the end
#
# INPUTS PROVIDED TO EACH FUNCTION
# - data: stand-year slice from FVS_TreeList (one CaseID + one Year)
# - context: list of upstream outputs (for example context$plotvals)
#
# QUICK START
# 1) Keep one active function below and edit it for your project.
# 2) Rename output columns to your custom names.
# 3) Replace placeholder calculations with your own logic.
# 4) Keep helper logic inside the function body (avoid extra top-level functions).

custom_project_attr <- function(data, context) {
  # Step 1: Declare all custom output columns with default NA values.
  out <- data.frame(
    CUST_EXAMPLE_NUMERIC = NA_real_,
    CUST_EXAMPLE_TEXT = NA_character_,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Step 2: Basic guards. Return defaults if required inputs are missing.
  if (is.null(data) || nrow(data) == 0) return(out)
  if (is.null(context) || is.null(context$plotvals) || is.null(context$plotvals[["ALL"]])) return(out)

  # Step 3: Pull inputs from context$plotvals[['ALL']] when needed.
  all_vals <- context$plotvals[["ALL"]]
  qmd <- as.numeric(all_vals["QMD"])
  cc <- as.numeric(all_vals["CC"])

  # Step 4: Pull inputs directly from stand-year TREELIST `data` when needed.
  if (!all(c("DBH", "TPA") %in% colnames(data))) return(out)
  dbh <- as.numeric(data[["DBH"]])
  tpa <- as.numeric(data[["TPA"]])

  # Step 5: Replace these example calculations with your project logic.
  live <- is.finite(dbh) & is.finite(tpa) & dbh > 0 & tpa > 0
  if (any(live)) {
    out$CUST_EXAMPLE_NUMERIC <- round(stats::weighted.mean(dbh[live], w = tpa[live], na.rm = TRUE), 2)
  }

  if (is.finite(cc) && is.finite(qmd) && qmd > 0) {
    out$CUST_EXAMPLE_TEXT <- if ((cc / qmd) > 2) "HIGH" else "LOW"
  }

  out
}

# -----------------------------------------------------------------------------
# OPTIONAL REFERENCE: available plotAttr keys in context$plotvals[["ALL"]]
# -----------------------------------------------------------------------------
# N          = number of tree records used in plotAttr calculations
# BA         = basal area per acre
# TPA        = trees per acre
# QMD        = quadratic mean diameter
# UNCC       = uncorrected canopy cover percent
# CC         = corrected canopy cover percent
# ZSDI       = Zeide SDI
# RSDI       = Reineke SDI
# BA_WT_DIA  = basal-area-weighted diameter
# BA_WT_HT   = basal-area-weighted height
# AVE_HT     = average height (Region 1 context)
# SSSIZE     = TPA-weighted advanced-regeneration height (Region 8 context)
# SSTPA      = advanced-regeneration trees per acre (Region 8 context)
# SSBA       = advanced-regeneration basal area (Region 8 context)
# NMSIZE     = non-merchantable BA-weighted diameter (Region 8 context)
# NMBA       = non-merchantable basal area (Region 8 context)
# PWSIZE     = pulpwood BA-weighted diameter (Region 8 context)
# PWBA       = pulpwood basal area (Region 8 context)
# STSIZE     = sawtimber BA-weighted diameter (Region 8 context)
# STBA       = sawtimber basal area (Region 8 context)

# -----------------------------------------------------------------------------
# OPTIONAL SECOND FUNCTION TEMPLATE (commented out)
# -----------------------------------------------------------------------------
# If you need a second output function, copy, rename, and uncomment this block.
#
# custom_project_attr_2 <- function(data, context) {
#   out <- data.frame(
#     CUST_SECOND_EXAMPLE = NA_real_,
#     stringsAsFactors = FALSE,
#     check.names = FALSE
#   )
#
#   # Add your guards and calculation logic.
#
#   out
# }
