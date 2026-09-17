### Use this script to run vegClass without the App ###
### Script will also populate App with all inputs below ###

##NOTE##
#run "devtools::document()" in the R console to get access to the help files through R
#once that is run, you should be able to type in "?" with a function name to see
#the R documentation e.g. "?main" and press enter


### Scroll down to USER INPUTS ###

# Set working directory to the nearest folder containing an .Rproj file.
file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
start_dir <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

project_dir <- start_dir
repeat {
  if (length(list.files(project_dir, pattern = "\\.Rproj$", full.names = TRUE)) > 0) break
  parent_dir <- dirname(project_dir)
  if (identical(parent_dir, project_dir)) stop("No .Rproj file found in current directory or parent directories.")
  project_dir <- parent_dir
}
setwd(project_dir)

# Ensure required packages are installed and loaded.
cran_packages <- c("foreach", "doSNOW", "RSQLite", "devtools")
missing_packages <- cran_packages[!vapply(
  cran_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(lapply(cran_packages, library, character.only = TRUE))

if (!requireNamespace("vegClass2.0", quietly = TRUE)) {
  stop("Package 'vegClass2.0' is not installed. Install it first, then rerun this script.")
}

########----------USER INPUTS------------########

# All file paths must have forward slashes (/)

# Set your input database (.db)
input <- "E:/vegClass2.0_Draft/vegClass2.0_Draft/FVS_Out_20260709_091330.db"

# Set your output directory
output_dir <- "G:/FVS_batch/PostProcessing/Outputs_batch_improved2"

# Set output CSV file name
output_csv_name <- "BLK_HILLS_vegClass10110_MPSG.csv"

# Set number of cores to do parallel processing on. Currently set up to select half
# of the machines cores but it can be set to any number in the main function.
num_cores<- parallel::detectCores()/2

# Set run title(s) to process. Use c("run1", "run2") for multiple runs.
runTitles <- c("10110_NG")

# Set to TRUE to process all runtitles in the input database.
allRuns <- FALSE

# Region selection:
# - Use 1, 2, 3, 8, or "MPSG" for built-in vegClass outputs.
# - Note that R8 classification was built for a custom project in North Carolina.
# - Use "CUSTOM" or NULL if you only want custom output scripts.
# - Custom output scripts can also be added on top of any built-in region.
region <- "MPSG"

# Set MPSG cover type algorithm (only used when region = "MPSG") Valid values are 1, 2, and 3.
MPSGcovTyp <- 2

# Add optional output groups from the main workflow.
addHSS <- FALSE
addCompute <- FALSE
addPotFire <- FALSE
addFuels <- FALSE
addCarbon <- FALSE
addVolume <- FALSE

# Overwrite existing output csv if it already exists.
overwriteOut <- TRUE

# DBH thresholds used when volume outputs are included.
vol1DBH <- 0
vol2DBH <- 4
vol3DBH <- 9

# Set the first year to include in outputs.
startYear <- 2024

# Inventory database and stand table (needed for region 1 and if MPSGcovTyp = 1 ).
InvDB <- "G:/FVS_batch/Inputs/Combined/AllBKNF_Combined_our_inpt.db"
InvStandTbl <- "FVS_STANDINIT"

# Optional custom attribute order form path (xlsx). Keep NULL if not used.
customVars <- NULL

# Optional output attributes to exclude from final CSV (case-insensitive).
# Example: excludeAttributes <- c("QMD_TOP20", "ZDSI", "RSDI")
excludeAttributes <- NULL

# Optional custom output scripts.
# If script is in local R/ folder, provide script name (for example "custom_project_attr.r").
# If script is elsewhere, provide full path (for example "C:/path/to/custom_project_attr.r").
# You can provide one value or multiple values with c(...).
# set to NULL if none used
customOutputScripts <- c("custom_project_attr_template.r")

# Remove case indices from input DB after processing.
removeCaseIndices <- FALSE

########---------- END USER INPUTS ------------########

# Creating output file name and location. Do not change.
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output <- file.path(output_dir, output_csv_name)

########---------- RUNNING MAIN FUNCTION ------------########

vegClass2.0::main(input = input,
  num_cores = num_cores,
  output = output,
  runTitles = runTitles,
  allRuns = allRuns,
  region = region,
  MPSGcovTyp = MPSGcovTyp,
  addHSS = addHSS,
  addCompute = addCompute,
  addPotFire = addPotFire,
  addFuels = addFuels,
  addCarbon = addCarbon,
  addVolume = addVolume,
  overwriteOut = overwriteOut,
  vol1DBH = vol1DBH,
  vol2DBH = vol2DBH,
  vol3DBH = vol3DBH,
  startYear = startYear,
  InvDB = InvDB,
  InvStandTbl = InvStandTbl,
  customVars = customVars,
  excludeAttributes = excludeAttributes,
  customOutputScripts = customOutputScripts,
  removeCaseIndices = removeCaseIndices)
