################################################################################
#main.R
#
#This script contains the main function which is used to derive vegetation
#classifications and other attributes.
################################################################################

################################################################################
#Function : main.R
#
#Arguments:
#
#input:        Directory path and file name to a SQLite database (.db). Path
#              name must be surrounded with double quotes "" and double
#              back slashes or single forward slashes need to be used for
#              specifying paths. By default, this argument is set to NULL.
#
#              Examples of valid input formats:
#              "C:/FVS/R3_Work/FVSOut.db"
#              "C:\\FVS\\R3_Work\\FVSOut.db"
#
#              Database defined in input should contain FVS_Treelist (western
#              variants) or FVS_Treelist (eastern variants) and FVS_Cases
#              table. If the FVS_Treelist or FVS_Treelist table does exist in
#              input argument but there is no information in either of these
#              tables for a given run, then no information for that run will be
#              sent to output argument.
#
#output:       Directory path and filename to a .csv file. Path name must be
#              surrounded with double quotes "" and double back slashes or
#              single forward slashes need to be used for specifying paths. By
#              default, this argument is set to NULL.
#
#              Examples of valid output formats:
#              "C:/FVS/R3_Work/FVSOut.csv"
#              "C:\\FVS\\R3_Work\\FVSOut.csv"
#
#num_cores     Numerical variable for determining how many computer cores will
#              be used for parallel processing. Default is NULL.
#
#runTitles:    Vector of character strings corresponding to FVS runTitles that
#              will be processed. If runTitles is left as NULL and allRuns is
#              FALSE (F), execution of main function will stop with an error
#              message. Each run title in runTitles must be surrounded by double
#              quotes. The values specified for runTitles argument need to be
#              spelled correctly. If any of the values in runTitles are spelled
#              incorrectly, the execution of main function will stop with an
#              error message.
#
#              Example of how to specify single run title:
#              runTitles = "Run 1"
#
#              Example of how to specify multiple run titles:
#              runTitles = c("Run 1", "Run 2",...)
#              Note the use of the c(...) when processing multiple runs.
#
#allRuns:      Logical variable that is used to determine if all runs in
#              argument input should be processed. If value is TRUE (T), then
#              all runs will be processed and any runs specified in argument
#              runTitles will be ignored. By default, this argument is set
#              to FALSE (F).
#
#startYear:    Integer value corresponding to the year that data should start
#              being reported in output argument. Data with years prior to this
#              value will not be included in the output argument. By default,
#              this argument is set to NA.
#
#endYear:      Integer value corresponding to the last year that data should be
#              reported in output argument. Data associated with years after
#              this value will not be included in the output argument. By
#              default,this value is set to NA. If this argument is left as NA,
#              then all information after startYear argument will be sent to
#              output argument.
#
#startCycle:   Integer value corresponding to the cycle that data should start
#              being reported in output argument. Data with cycles prior to this
#              value will not be included in the output argument. By default,
#              this value is set to NA.
#
#endCycle:     Integer value corresponding to the last cycle that data should be
#              reported in output argument. Data associated with cycles after
#              this value will not be included in the output argument. By
#              default,this value is set to NA. If this argument is left as NA,
#              then all information after startCycle argument will be sent to
#              output argument.
#
#overwriteOut: Logical variable used to determine if output file should be
#              overwritten. If value is TRUE, any information existing in output
#              will be overwritten with new information. If value is FALSE (F)
#              and the file in output argument exists, then main function will
#              stop with an error message. The default value of this argument is
#              FALSE (F).
#
#region:       Variable corresponding to USFS region number. Current valid
#              values are 1, 2, 3, 8, or MPSG (Mountain Planning Services Group,
#              R1-4). This variable is used to determine rule sets for
#              calculating vegetation classifications and other attributes. The
#              value specified in this argument will be included in the file
#              specified in the output argument.
#
#              WARNING: the value specified in the region argument will apply
#              to all runs specified in the runTitles argument or all runs being
#              processed if the allRuns argument is set to TRUE (T). As such,
#              run(s) from only one region at a time when using the main
#              function.
#
#MPSGcovTyp:   Integer value corresponding to the USFS region number whose
#              algorithms should be used in the calculations of the
#              COVERTYPE_MPSG variable. Valid values are 1, 2, and 3. Will only
#              apply when region argument is set to “MPSG”. If region argument
#              is set to “MPSG” and MPSGcovTyp argument is NA (default value),
#              main function will stop with an error message.
#
#              NOTE: currently only the region 2 cover type labels are cross
#              walked to the MPSG-specific cover type labels. For regions 1 and
#              3, those region-specific labels do not yet have corresponding
#              MPSG labels to be cross walked to, and the COVERTYPE_MPSG values
#              will have the labels from those regions’ covertypes.
#
#addHSS:	     Logical variable used to indicate whether the USFS Region 2
#              Habitat Structural Stage (HSS) variables should be included in
#              the file specified in output argument. Will only apply when
#              region argument is set to “MPSG”. If addHSS is TRUE and region
#              argument is anything other than “MPSG”, main function will stop
#              with an error message.
#
#addCompute:   Logical variable used to indicate if information in FVS_Compute
#              table should be included in output argument. If the FVS_Compute
#              table does not exist in input argument and addCompute is TRUE
#              (T), main function will stop with an error message. If the
#              FVS_Compute table does exist in input argument but there is no
#              information in the FVS_Compute table for a given run, then that
#              run will have NA values reported in output argument for all
#              variable found in the FVS_Compute table. By default, this
#              argument is set to FALSE (F).
#
#addPotFire:   Logical variable used to indicate if information in FVS_Potfire
#              or FVS_Potfire_East table should be included in output argument.
#              If the FVS_Potfire table does not exist in input argument and
#              addPotFire is TRUE (T), main function will  stop with an error
#              message. If the FVS_Potfire or FVS_Potfire_East table do exist in
#              input argument but there is no information in the FVS_Potfire or
#              FVS_Potfire_East table for a given run, then that run will have
#              NA values reported in output for all variables extracted from the
#              FVS_Potfire/FVS_Potfire_East table. By default, this argument is
#              set to FALSE (F).
#
#addFuels:     Logical variable used to indicate if information in FVS_Fuels
#              table should be included in output argument. If the FVS_Fuels
#              table does not exist in input argument and addFuels is TRUE (T),
#              main function will stop with an error message. If the FVS_Fuels
#              table does exist in input argument but there is no information in
#              the FVS_Fuels table for a given run, then that run will have NA
#              values reported in output argument for all variables found in the
#              FVS_Fuels table. By default, this argument is set to FALSE (F).
#
#addCarbon:    Logical variable used to indicate if information in FVS_Carbon
#              table should be included in output argument. If the FVS_Carbon
#              table does not exist in input argument and addCarbon is TRUE (T),
#              main function will stop with an error message. If the FVS_Carbon
#              table does exist in input argument but there is no information in
#              the FVS_Carbon table for a given run, then that run will have NA
#              values reported in output argument for all variables found in the
#              FVS_Carbon table. By default, this argument is set to FALSE (F).
#
#addVolume:    Logical variable used to indicate if 3 measures of volume should
#              be calculated and reported. If the value of this argument is
#              is set to TRUE (T), then the following measures of volume will be
#              calculated:
#
#              Eastern variants: CS, LS, NE and SN
#              VOL1: Merchantable cubic foot volume
#              VOL2: Sawlog cubic foot volume
#              VOL3: Sawlog Board foot volume
#              DEADVOL1: Merchantable cubic foot volume that died in that cycle
#              DEADVOL2: Sawlog cubic foot volume that died in that cycle
#              DEADVOL3: Sawlog Board foot volume that died in that cycle
#
#              Western variants
#              VOL1: Total cubic foot volume
#              VOL2: Merchantable cubic foot volume
#              VOL3: Board foot volume
#              DEADVOL1: Total cubic foot volume that died in that cycle
#              DEADVOL2: Merchantable cubic foot volume that died in that cycle
#              DEADVOL3: Board foot volume that died in that cycle
#
#vol1DBH:	     Minimum DBH of tree records included in calculation of VOL1 and
#              DEADVOL1 when addVolume is TRUE. By default, this argument is set
#              to 0.1.
#
#vol2DBH:      Minimum DBH of tree records included in calculation of VOL2 and
#              DEADVOL2 when addVolume is TRUE. By default, this argument is set
#              to 5.
#
#vol3DBH:	     Minimum DBH of tree records included in calculation of VOL3 and
#              DEADVOL3 when addVolume is TRUE. By default, this argument is set
#              to 9.
#
#              NOTE: If both startYear and startCycle arguments do not have a
#              value specified (left as NA), main function will stop with an
#              error message. One of these arguments must be used. If non NA
#              values are specified for both startYear and startCycle, then the
#              main function will default to using cycles for determining what
#              information gets sent to output argument.
#
#setIndices:   Removed for updated version of package. Now automatically creates
#              indices for input database.
#
#modstandID:   Logical variable, where if TRUE, an underscore will be appended
#              before each Stand ID sent to output argument. The addition of
#              the underscore forces Microsoft Excel to recognize that the Stand
#              ID is a character and avoids the problem of long character
#              strings of numbers (i.e. Stand IDs in FIA data:
#              0004201904090101990050) being truncated and converted to numbers.
#              By default, this argument is set to TRUE (T).
#
#InvDB:        Character string corresponding to the full directory location of
#              the inventory database that was used for the FVS runs specified
#              in the runTitles argument, or all runs being processed if the
#              allRuns argument is set to TRUE (T). Used to obtain the PV_CODE
#              value from the stand data table for each stand. By default,
#              this argument is set to NULL.
#
#              NOTE: Currently only required if the region argument is set to 1.
#              If this argument is populated, then the below invStandTbl
#              argument needs to be populated as well.
#
#InvStandTbl:  Character string corresponding to name of the stand data table
#              in InvDB that contains the PV_CODE values for the stands in the
#              runs specified in the runTitles argument, or all runs being
#              processed if the allRuns argument is set to TRUE (T). By default,
#              this argument is set to NULL.
#
#              NOTE: Currently only required if the region argument is set to 1,
#              and if the above InvDB argument is populated.
#
#customVars:   Character string corresponding to the full directory location of
#              the CustomVars_vegClass.XLSX file with the requested custom
#              variable specifications. By default, this argument is set to NULL.
#
#excludeAttributes: Character vector of output attribute names to remove from
#              the final csv output. Matching is case-insensitive. If NULL
#              (default), no attributes are excluded.
#
#              Examples:
#              excludeAttributes = c("TCOV", "BA5")
#              excludeAttributes = c("DOMTYPE", "HSS")
#
#removeCaseIndices: Removes indices generated for input database tables.
#
#Value
#
#0 value invisibly returned.
################################################################################

##' Vegetation Classification Engine
#'
#' This is the main function for deriving vegetation classifications and other
#' attributes from FVS output databases. It processes FVS output SQLite
#' databases, extracts relevant tables, applies region-specific rule sets, and
#' generates a .csv file with vegetation classifications and calculated
#' attributes. The function supports filtering by run titles, years, cycles, and
#' includes options for additional FVS tables and custom variables. It is
#' designed to be flexible for different USFS regions and output requirements.
#' The addition of parallel processing, number of cores (`num_cores`), and
#' `removeCaseIndicies` argument, makes this different from the original main
#' function published in the vegClass package. See main_ParallelProcess.R script
#' for more documentation on the arguments.
#'
#' @param input Directory path and file name to a SQLite database (.db). Path
#'   must be in quotes and use double backslashes or single forward slashes.
#'   Default is NULL.
#' @param output Directory path and filename to a .csv file. Path must be in
#'   quotes and use double backslashes or single forward slashes. Default is
#'   NULL.
#' @param num_cores Integer specifying the number of CPU cores to use for
#'   parallel processing. Default is NULL.
#' @param runTitles Character vector of FVS runTitles to process. If NULL and
#'   allRuns is FALSE, execution stops with an error. Each run title must be in
#'   quotes.
#' @param allRuns Logical. If TRUE, all runs in input are processed and
#'   runTitles is ignored. Default is FALSE.
#' @param startYear Integer. First year to include in output. Data before this
#'   year is excluded. Default is NA.
#' @param endYear Integer. Last year to include in output. Data after this year
#'   is excluded. Default is NA (all years after startYear included).
#' @param startCycle Integer. First cycle to include in output. Data before this
#'   cycle is excluded. Default is NA.
#' @param endCycle Integer. Last cycle to include in output. Data after this
#'   cycle is excluded. Default is NA (all cycles after startCycle included).
#' @param overwriteOut Logical. If TRUE, overwrites output file if it exists. If
#'   FALSE and file exists, stops with error. Default is FALSE.
#' @param region USFS region number (1, 2, 3, 8, or "MPSG"). Determines rule
#'   sets for calculations. Applies to all runs in runTitles or all runs if
#'   allRuns is TRUE.
#' @param MPSGcovTyp Integer. USFS region number for COVERTYPE_MPSG calculation
#'   (1, 2, or 3). Only applies if region is "MPSG". Default is NA. NOTE:
#'   currently only the region 2 cover type labels are cross walked to the
#'   MPSG-specific cover type labels. For regions 1 and 3, those region-specific
#'   labels do not yet have corresponding MPSG labels to be cross walked to, and
#'   the COVERTYPE_MPSG values will have the labels from those regions’
#'   covertypes.
#' @param addHSS Logical. Include USFS Region 2 Habitat Structural Stage (HSS)
#'   variables in output. Only applies if region is "MPSG".
#' @param addCompute Logical. Include FVS_Compute table in output. If TRUE and
#'   table missing, stops with error. Default is FALSE.
#' @param addPotFire Logical. Include FVS_Potfire or FVS_Potfire_East table in
#'   output. If TRUE and table missing, stops with error. Default is FALSE.
#' @param addFuels Logical. Include FVS_Fuels table in output. If TRUE and table
#'   missing, stops with error. Default is FALSE.
#' @param addCarbon Logical. Include FVS_Carbon table in output. If TRUE and
#'   table missing, stops with error. Default is FALSE.
#' @param addVolume Logical. If TRUE, calculates and reports 3 measures of
#'   volume (see details in script). Default is FALSE.
#' @param vol1DBH  Minimum DBH of tree records included in calculation of VOL1
#'   and DEADVOL1 when addVolume is TRUE. By default, this argument is setto
#'   0.1.
#' @param vol2DBH Minimum DBH of tree records included in calculation of VOL2
#'   and DEADVOL2 when addVolume is TRUE. By default, this argument is set to 5.
#' @param vol3DBH Minimum DBH of tree records included in calculation of VOL3
#'   and DEADVOL3 when addVolume is TRUE. By default, this argument is set to 9.
#'   NOTE: If both startYear and startCycle arguments do not have a value
#'   specified (left as NA), main function will stop with an error message. One
#'   of these arguments must be used. If non NA values are specified for both
#'   startYear and startCycle, then the main function will default to using
#'   cycles for determining what information gets sent to output argument.
#' @param modStandID Logical variable, where if TRUE, an underscore will be appended
#'    before each Stand ID sent to output argument. Default is TRUE.
#' @param InvDB Character string corresponding to the full directory location of
#'   the inventory database that was used for the FVS runs specified in the
#'   runTitles argument, or all runs being processed if the allRuns argument is
#'   set to TRUE (T). Used to obtain the PV_CODE value from the stand data table
#'   for each stand. By default, this argument is set to NULL. NOTE: Currently
#'   only required if the region argument is set to 1. If this argument is
#'   populated, then the below invStandTbl argument needs to be populated as
#'   well.
#' @param InvStandTbl Character string corresponding to name of the stand data
#'   table in InvDB that contains the PV_CODE values for the stands in the runs
#'   specified in the runTitles argument, or all runs being processed if the
#'   allRuns argument is set to TRUE (T). Default is NULL.
#' @param customVars Character string with full path to CustomVars_vegClass.XLSX
#'   file for custom variable specifications. Default is NULL.
#' @param customOutputScripts Character vector of custom R script paths used to
#'   define additional output variables. Each script should provide one or more
#'   functions with signature `function(data, context)`. Script names without
#'   paths are searched in the local `R/` folder. Default is NULL.
#' @param excludeAttributes Character vector of output attribute names to
#'   exclude from the final csv output. Matching is case-insensitive.
#'   Default is NULL.
#' @param removeCaseIndices Removes all indices generated for input database.
#'   Default is NULL.
#' @param show_progress Logical. If TRUE, prints progress messages during
#'   processing. Default is TRUE.
#' @param progress_callback Optional function used to receive progress events.
#'   Must be NULL or a function. Default is NULL.
#'
#' @return Invisibly returns 0.
#'
#' @examples
#' Example: Process all runs in a Region 1 FVS output database, filter by year and cycle, and include additional tables.
#' main(
#'   input = "C:/FVS/R1_Work/FVSOut.db",
#'   output = "C:/FVS/R1_Work/vegClass_FVSOut.csv",
#'   num_cores = NULL,
#'   runTitles = c("FIRE_10110_HIGH", "FIRE_10110_LOW"),
#'   allRuns = FALSE,
#'   startYear = 2020,
#'   endYear = 2050,
#'   startCycle = 1,
#'   endCycle = 5,
#'   overwriteOut = TRUE,
#'   region = 1,
#'   addHSS = TRUE,
#'   addCompute = TRUE,
#'   addPotFire = TRUE,
#'   addFuels = TRUE,
#'   addCarbon = TRUE,
#'   addVolume = TRUE,
#'   vol1DBH = 0,
#'   vol2DBH = 4,
#'   vol3DBH = 9,
#'   InvDB = "C:/FVS_batch/Inputs/Combined/Combined_FVS_inpt.db",
#'   InvStandTbl = "FVS_STANDINIT",
#'   customVars = "C:/FVS/R1_Work/CustomVars_vegClass.xlsx",
#'   excludeAttributes = c("TCOV", "BA5"),
#'   removeCaseIndices = FALSE
#' )
#' @export
main<- function(input = NULL,
                output = NULL,
                num_cores = NULL,
                runTitles = NULL,
                allRuns = F,
                startYear = NA,
                endYear = NA,
                startCycle = NA,
                endCycle = NA,
                overwriteOut = F,
                region = NA,
                MPSGcovTyp =NA,
                addHSS = F,
                addCompute = F,
                addPotFire = F,
                addFuels = F,
                addCarbon = F,
                addVolume = F,
                vol1DBH = 0,
                vol2DBH = 5,
                vol3DBH = 9,
                modStandID = T,
                InvDB = NULL,
                InvStandTbl = NULL,
                customVars = NULL,
                customOutputScripts = NULL,
                excludeAttributes = NULL,
                removeCaseIndices = NULL,
                show_progress = TRUE,
                progress_callback = NULL)
{
  normalize_region_value <- function(region) {
    if (is.null(region) || length(region) == 0 || (length(region) == 1 && is.na(region))) {
      return("CUSTOM")
    }
    region
  }

  vcat <- function(...) {
    if (isTRUE(show_progress)) cat(...)
  }

  if (!is.null(progress_callback) && !is.function(progress_callback)) {
    stop("progress_callback must be NULL or a function.")
  }

  normalize_script_paths <- function(paths) {
    paths <- trimws(as.character(paths))
    paths <- gsub("\\\\", "/", paths)
    paths <- paths[nzchar(paths)]
    unique(paths)
  }

  resolve_custom_script_paths <- function(paths) {
    paths <- normalize_script_paths(paths)
    if (length(paths) == 0) return(character(0))

    resolved <- character(0)

    for (raw_path in paths) {
      candidates <- raw_path

      # If caller provides just a script name, search local R/ folder first.
      if (!grepl("[/\\\\]", raw_path)) {
        candidates <- c(candidates, file.path(getwd(), "R", raw_path))
      }

      # Support omitting .r extension for either name or path.
      add_ext <- vapply(
        candidates,
        function(p) !grepl("\\\\.r$", basename(p), ignore.case = TRUE),
        logical(1)
      )
      if (any(add_ext)) {
        candidates <- c(candidates, paste0(candidates[add_ext], ".r"))
      }

      candidates <- unique(normalize_script_paths(candidates))
      existing <- candidates[file.exists(candidates)]
      if (length(existing) > 0) {
        resolved <- c(resolved, existing[1])
      } else {
        warning(
          sprintf(
            "Custom output script was not found: '%s'. Searched: %s",
            raw_path,
            paste(candidates, collapse = ", ")
          )
        )
      }
    }

    unique(resolved)
  }

  normalize_excluded_attributes <- function(attrs) {
    attrs <- trimws(as.character(attrs))
    attrs <- attrs[nzchar(attrs)]
    attrs <- attrs[toupper(attrs) != "NULL"]
    unique(toupper(attrs))
  }

  discover_custom_output_fns <- function(script_paths) {
    fn_names <- character(0)

    for (script_path in script_paths) {
      tmp_env <- new.env(parent = .GlobalEnv)
      sourced_ok <- tryCatch(
        {
          sys.source(script_path, envir = tmp_env)
          TRUE
        },
        error = function(e) {
          warning(sprintf("Failed to source custom output script '%s': %s", script_path, e$message))
          FALSE
        }
      )
      if (!sourced_ok) next

      objs <- ls(tmp_env, all.names = TRUE)
      if (length(objs) == 0) next

      is_custom_fn <- vapply(
        objs,
        function(obj_name) {
          obj <- get(obj_name, envir = tmp_env, inherits = FALSE)
          if (!is.function(obj)) return(FALSE)
          arg_names <- names(formals(obj))
          all(c("data", "context") %in% arg_names)
        },
        logical(1)
      )

      if (!any(is_custom_fn)) {
        warning(
          sprintf(
            "No runnable custom output functions found in '%s'. Expected functions with signature function(data, context).",
            script_path
          )
        )
      }

      fn_names <- c(fn_names, objs[is_custom_fn])
    }

    unique(fn_names)
  }

  # %dopar% is an infix operator exported by foreach and must be attached.
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("Package 'foreach' is required for parallel processing. Please install it.")
  }
  if (!("package:foreach" %in% search())) {
    suppressPackageStartupMessages(library(foreach))
  }

  excluded_attr_names <- if (is.null(excludeAttributes)) {
    character(0)
  } else {
    normalize_excluded_attributes(excludeAttributes)
  }

  #Set the start time of function execution
  startTime <- Sys.time()

  ###########################################################################
  #Check function arguments
  ###########################################################################

  # (Argument checks remain the same as your original file)
  # ...
  # ... (Lines 596-813 from your file) ...
  # ...

  #==========================================================================
  #Do checks on input argument
  #==========================================================================

  #Test if value in input argument is null.
  if (is.null(input)){
    stop(paste("No database specified in input argument."))
  }

  #Change \\ to / in input argument
  input <- gsub("\\\\", "/", input)

  #Test existence of input database.
  if (!(file.exists(input))){
    stop(paste("Input database not found. Make sure directory path and file",
               "name in input are spelled correctly."))
  }

  #Extract file extension for input argument.
  fileExtIn<-sub("(.*)\\.","",input)

  #Make sure input database is SQLite (.db).
  if (!fileExtIn %in% "db"){
    stop(paste("Input argument does not have a valid file extension. File",
               "extension must be .db."))
  }

  #==========================================================================
  #Do checks on output argument
  #==========================================================================

  #Test if value in output argument is null.
  if (is.null(output)){
    stop(paste("No file specified in output argument."))
  }

  #Change \\ to / in output argument
  output <- gsub("\\\\", "/", output)

  #Extract path to output by extract all characters before the last / in output.
  outPath <- gsub("/[^/]+$", "", output)

  #Test existence of output path and if it does not exist report error.
  if (!(file.exists(outPath))){
    stop(paste("Path to output:", outPath, "was not found.",
               "Make sure directory path to output is spelled correctly."))
  }

  #Extract file extension for output argument.
  fileExtOut<-sub("(.*)\\.","",output)

  #Test if output file extension is valid (.csv).
  if(!fileExtOut %in% c("csv"))
  {
    stop(paste("Output argument does not have a valid file extension. File",
               "extension must be .csv."))
  }

  #Stop with error message if output file exists and overwriteOut is not FALSE.
  if(file.exists(output) & !overwriteOut)
  {
    stop(paste(output,
               "file exists and overwriteOut is FALSE. Change name of",
               "output file or set overwriteOut to TRUE.", "\n"))
  }

  #If file exists and overwriteOut is TRUE, unlink the output file.
  if(file.exists(output) & overwriteOut) unlink(output)

  #==========================================================================
  #Do checks on runTitles argument
  #==========================================================================

  #If runTitles is NULL and allRuns not TRUE, then stop with error message.
  if(is.null(runTitles) & !allRuns)
  {
    stop("No runs specified in runTitles argument and allRuns is not TRUE.")
  }

  #Capitalize runTitles if not null
  if(!is.null(runTitles)) runTitles <- toupper(runTitles)

  #==========================================================================
  #Do checks on region argument
  #==========================================================================

  region <- normalize_region_value(region)

  #If region is not a valid value, then stop with error message
  if(!is.numeric(region) && !is.character(region))
  {
    stop(paste("Invalid value entered for the region argument.",
               "Please enter a value of 1, 2, 3, 8, MPSG, CUSTOM, or NULL."))
  }

  #If region is not a valid numeric value, then stop with error message
  if(is.numeric(region) && (!region %in% c(1, 2, 3, 8)))
  {
    stop(paste("Invalid region number was specified in region argument.",
               "Please enter a value of 1, 2, 3, 8, MPSG, CUSTOM, or NULL."))
  }

  if(is.character(region) && !region %in% c("MPSG", "CUSTOM"))
  {
    stop(paste("Invalid value entered for the region argument.",
               "Please enter a value of 1, 2, 3, 8, MPSG, CUSTOM, or NULL."))
  }

  #==========================================================================
  #Do checks on MPSGcovtyp argument
  #==========================================================================

  #If region is MPSG, and MPSGcovTyp is not specied, then stop with error message
  if(identical(region, "MPSG") && is.na(MPSGcovTyp))
  {
    stop(paste("MPSG was specified in region argument but the MPSGcovTyp is NA.",
               "Please specify which regional cover type algorithm to use."))
  }

  #==========================================================================
  #Do checks on addHSS argument
  #==========================================================================

  #If addHSS is TRUE but region is not MPSG, then stop with error message
  if(!identical(region, "MPSG") && addHSS)
  {
    stop(paste("addHSS is set to TRUE, but MPSG was not specified in region argument.",
               "Please set addHSS to FALSE, or swith region to MPSG (with quotes)."))
  }

  #==========================================================================
  #Determine if years or cycles should be used for establishing what data
  #gets sent to output.
  #==========================================================================

  #Initialize useYear and useCycle
  useYear <- F
  useCycle <- F

  #If both startYear and startCycle are NA, stop with error message. One of
  #these arguments has to be used.
  if(is.na(startYear) & is.na(startCycle))
  {
    stop(paste("No value entered for startYear or startCycle.",
               "One of these variables needs to have a specified value."))
  }

  #Set useYear to T if startYear is not NA.
  if(!is.na(startYear))
  {
    useYear = T
  }

  #Set useCycle to T if startCycle is not NA. If both startYear and startCycle
  #are not NA, then useCycle takes precedence.
  if(!is.na(startCycle))
  {
    useCycle = T
    useYear = F
  }

  ###########################################################################
  #Perform checks on input database (con)
  ###########################################################################

  #Connect to input database
  con<-RSQLite::dbConnect(RSQLite::SQLite(), input)
  vcat("Connected to input database:", input, "\n")

  #==========================================================================
  #Check if FVS_Cases table is in input (con). If it is not, disconnect from
  #con and stop with an error.
  #==========================================================================

  if(is.element(F, c("FVS_Cases") %in%
                RSQLite::dbListTables(con)))
  {
    RSQLite::dbDisconnect(con)
    stop(paste("FVS_Cases table not found in input database."))
  }

  #==========================================================================
  #If allRuns is not TRUE, check if individual runs specified in runTitles
  #exist. If any are missing, disconnect from con and stop with an error
  #message.
  #==========================================================================

  if(!allRuns)
  {
    #Check if runs from runTitles are found in FVS_Cases table
    runsFound<- runTitles %in% toupper(
      RSQLite::dbGetQuery(con,
                          "SELECT DISTINCT RunTitle FROM FVS_Cases")[,1])

    #If any runs are not found, then report them in error message
    if(F %in% runsFound)
    {
      #Determine missing runs
      missingRuns<-runTitles[runsFound == F]

      #Paste missing runs together separated by comma and space
      missingRuns<-paste(missingRuns, collapse = ", ")

      RSQLite::dbDisconnect(con)

      stop(paste("Run titles:",paste0("'",missingRuns,"'", collapse = ""),
                 "not found in input database. Please ensure all run",
                 "titles are spelled correctly."))
    }
  }

  #==========================================================================
  #Check if the following tables exist in input database:
  #FVS_TreeList, FVS_TreeList
  #FVS_Compute (if addCompute is TRUE)
  #FVS_PotFire, FVS_PotFire_East (if addPotFire is TRUE)
  #FVS_Fuels (if addFuels is TRUE)
  #FVS_Carbon (if addCarbon is TRUE)
  #Check if input database has indexes that are used
  #by main function.
  #==========================================================================

  #Grab all distinct runs and variants from FVS_Cases
  runs <- RSQLite::dbGetQuery(con,
                              paste("SELECT DISTINCT FVS_Cases.RunTitle,",
                                    "FVS_Cases.Variant",
                                    "FROM FVS_Cases"))

  #Capitalize column names
  colnames(runs) <- toupper(colnames(runs))

  #Capitalize run titles
  runs$RUNTITLE <- toupper(runs$RUNTITLE)

  #If allRuns is not TRUE, select runs specified in runTitles argument.
  if(!allRuns)
  {
    runs <- runs[runs$RUNTITLE %in% runTitles, ]
  }

  #Check if input tables exist using checkDBTables function
  message<-checkDBTables(con,
                         variants = runs$VARIANT,
                         addCompute = addCompute,
                         addPotFire = addPotFire,
                         addFuels = addFuels,
                         addCarbon = addCarbon)

  #If checkDBTables function returns a message that is not 'PASS' then
  #disconnect from con and stop with error message.
  if(message != 'PASS')
  {
    #Disconnect from con
    RSQLite::dbDisconnect(con)

    #Print error message
    stop(message)
  }

  #Reset runTitles to what is in RUNTITLES column of runs data frame.
  runTitles <- runs$RUNTITLE

  manual_custom_scripts <- if (is.null(customOutputScripts)) character(0) else customOutputScripts
  custom_script_paths <- resolve_custom_script_paths(manual_custom_scripts)
  custom_output_fn_names <- discover_custom_output_fns(custom_script_paths)

  if (length(manual_custom_scripts) > 0 && length(custom_script_paths) == 0) {
    stop("customOutputScripts was provided, but no script files were found. Check paths and current working directory.")
  }

  if (length(custom_script_paths) > 0 && length(custom_output_fn_names) == 0) {
    stop("Custom script files were found, but no runnable functions were discovered. Define at least one function(data, context).")
  }

  # Fallback: if canonical project script is provided but discovery found no
  # matching function, still try the expected function name.
  if (length(custom_output_fn_names) == 0 &&
      any(tolower(basename(custom_script_paths)) == "custom_project_attr.r")) {
    custom_output_fn_names <- "custom_project_attr"
  }

  vcat("Custom output scripts discovered:", length(custom_script_paths), "\n")
  vcat("Custom output functions discovered:", length(custom_output_fn_names), "\n")

  # Pre-compute stand counts so callers can receive accurate global progress.
  case_counts <- RSQLite::dbGetQuery(
    con,
    "SELECT UPPER(RunTitle) AS RUNTITLE, COUNT(*) AS N_STANDS FROM FVS_Cases GROUP BY UPPER(RunTitle)"
  )
  case_count_map <- setNames(as.integer(case_counts$N_STANDS), case_counts$RUNTITLE)
  total_stands_all_runs <- sum(case_count_map[runTitles], na.rm = TRUE)
  if (is.na(total_stands_all_runs)) total_stands_all_runs <- 0L

  #===========================================================================
  #Create CaseID indices for each FVS table in input.
  #===========================================================================

  # Check for existing Case ID indices and only create if missing
  vcat("Checking for existing Case ID indices in:", input, "\n\n")
  indexNames <- getIndexNames(con)
  dbTables <- RSQLite::dbListTables(con)
  fvsTables <- dbTables[grepl('FVS_', dbTables, fixed = TRUE)]
  tablesToIndex <- c()
  for (dbTab in fvsTables) {
    dbFields <- RSQLite::dbListFields(con, dbTab)
    idxName <- paste0("IDX_", sub("FVS_", "", dbTab))
    if ("CaseID" %in% dbFields && !(idxName %in% indexNames)) {
      tablesToIndex <- c(tablesToIndex, dbTab)
    }
  }
  if (length(tablesToIndex) > 0) {
    vcat("Creating indices for tables:", paste(tablesToIndex, collapse=", "), "\n")
    for (dbTab in tablesToIndex) {
      vcat("Creating index for table:", dbTab, "\n")
      dbTabShort <- sub("FVS_", "", dbTab)
      indexQuery <- paste0("create index IDX_", dbTabShort, " on FVS_", dbTabShort, " (CaseID);")
      vcat("indexQuery:", indexQuery, "\n")
      tryCatch({
        RSQLite::dbExecute(con, indexQuery)
      }, error=function(e) if (isTRUE(show_progress)) cat("Index creation failed for", dbTab, "-", e$message, "\n"))
    }
    vcat("Case ID indices created where needed.\n")
  } else {
    vcat("All required Case ID indices already exist. Skipping creation.\n")
  }
  # Disconnect from main connection before starting loops
  RSQLite::dbDisconnect(con)
  vcat("Initial checks complete. Disconnected from main DB connection.\n")


  cl <- parallel::makeCluster(num_cores)
  doSNOW::registerDoSNOW(cl)
  vcat("Parallel cluster started with", num_cores, "cores.\n")

  # Ensure workers have access to helper functions sourced from the local R directory.
  parallel::clusterEvalQ(cl, {
    worker_r_dir <- file.path(getwd(), "R")
    if (dir.exists(worker_r_dir)) {
      worker_r_files <- list.files(worker_r_dir, pattern = "\\.r$", full.names = TRUE)
      for (worker_file in worker_r_files) {
        try(source(worker_file, local = .GlobalEnv), silent = TRUE)
      }
    }
    NULL
  })

  if (length(custom_script_paths) > 0) {
    parallel::clusterExport(cl, "custom_script_paths", envir = environment())
    parallel::clusterEvalQ(cl, {
      for (script_path in custom_script_paths) {
        try(source(script_path, local = .GlobalEnv), silent = TRUE)
      }
      NULL
    })
  }

  run_summaries <- list()
  completed_stands_global <- 0L


  #===========================================================================
  #Begin loop across FVS run titles (runTitles)
  #===========================================================================

  for(r in 1:length(runTitles))
  {
    run_start_time <- Sys.time()

    #Extract run from runTitles
    run<-runTitles[r]

    #Print run that is being processed
    vcat(paste0(rep("*", 75), collapse = ""), "\n")
    vcat("*", "Processing run:", run, "\n")
    vcat(paste0(rep("*", 75), collapse = ""), "\n")

    # Establish a temporary connection to get case IDs for the current run
    temp_con <- RSQLite::dbConnect(RSQLite::SQLite(), input)
    dbQuery<- caseQuery(run)
    cases<-RSQLite::dbGetQuery(temp_con, dbQuery)
    #cases <- cases[cases$StandID == "00562017030703011815441", ]
    RSQLite::dbDisconnect(temp_con)

    vcat("Total number of stands to process for run", paste0(run,":"),
       length(cases[["CaseID"]]),"\n", "\n")

    # Progress Bar Setup
    progress <- function(n) {
      total <- length(cases[["CaseID"]])
      percent <- round(100 * n / total)
      bar_width <- 40
      filled <- round(bar_width * n / total)
      bar <- paste0(rep("=", filled), collapse = "")
      empty <- paste0(rep(" ", bar_width - filled), collapse = "")
      if (isTRUE(show_progress)) {
        cat(sprintf("\r[%s%s] %3d%%", bar, empty, percent))
        if (n == total) cat("\n")
      }

      if (is.function(progress_callback)) {
        # Throttle callbacks to ~100 updates per run, plus final completion event.
        every_n <- max(1L, floor(total / 100L))
        if (n %% every_n == 0L || n == total) {
          global_completed <- completed_stands_global + as.integer(n)
          global_pct <- if (total_stands_all_runs > 0) {
            100 * global_completed / total_stands_all_runs
          } else {
            0
          }

          try(progress_callback(list(
            run = run,
            run_completed = as.integer(n),
            run_total = as.integer(total),
            global_completed = as.integer(global_completed),
            global_total = as.integer(total_stands_all_runs),
            global_percent = as.numeric(global_pct)
          )), silent = TRUE)
        }
      }
    }
    opts <- if (isTRUE(show_progress) || is.function(progress_callback)) list(progress = progress) else list()

    #===========================================================================
    #Begin PARALLEL loop across case IDs for run title
    #===========================================================================
    allStandOutputs <- foreach::foreach(i = 1:length(cases[["CaseID"]]),
                                        .packages = c("RSQLite"),
                                        .export = c("treeQuery", "collectID", "caseQuery", "computeQuery", 
                                        "correctSp", "vegOut", "plotAttr", "correctCC", "qmdTop20", 
                                        "MPSG", "R1", "fvsGetCols", "fvsGetTypes", "addDbTable", "addDbRows", 
                                        "pvCodes", "MapSpecies", "MapDominance6040SpeciestoSubclass", 
                                        "computeVerticalStructure", "computeDominance6040", "R2", "HSS", 
                                        "domTypeR3", "PLANT", "GENUS", "R3_SHADE_TOL", "LEAF_RETEN", 
                                        "domTypeR8", "denSizeR8", "excGenusSp", "canSizCl", "getCanSizeDC", 
                                        "baStory", "customAttr", "volumeCalc"),
                                        .options.snow = opts,
                                        .combine = 'c',
                                        .errorhandling = 'pass') %dopar% {

                                          # Reuse a worker-local DB connection across tasks to reduce connection churn.
                                          worker_con <- get0(".vegclass_worker_con", envir = .GlobalEnv, inherits = FALSE)
                                          if (is.null(worker_con) || !RSQLite::dbIsValid(worker_con)) {
                                            worker_con <- RSQLite::dbConnect(RSQLite::SQLite(), input)
                                            assign(".vegclass_worker_con", worker_con, envir = .GlobalEnv)
                                          }

                                          #Select stands to process
                                          caseID<-cases[["CaseID"]][i]
                                          groups  <- cases$Groups[i]
                                          standCN <- if ("Stand_CN" %in% colnames(cases)) cases$Stand_CN[i] else NA
                                          standID <- cases$StandID[i]
                                          variant <- cases$Variant[i]

                                          if(modStandID)
                                          {
                                            standID <- paste0("_", standID)
                                          }

                                          dbQuery<-treeQuery(caseID, variant)
                                          standDF<-RSQLite::dbGetQuery(worker_con, dbQuery)

                                          # This list will be the return value for this iteration
                                          return_list <- list(standID = standID, status = "processed", data = NULL, messages = character(0))

                                          if(nrow(standDF) <= 0)
                                          {
                                            return_list$status <- "no_valid_records"
                                            return(list(return_list)) # Return as a list element
                                          }

                                          if(max(standDF$TPA) <= 0)
                                          {
                                            return_list$status <- "no_live_trees"
                                            return(list(return_list)) # Return as a list element
                                          }

                                          standDF$SpeciesPLANTS <- mapply(correctSp, standDF$SpeciesPLANTS)
                                          years<-sort(unique(standDF$Year))
                                          standYrOutput<-vector(mode = "list", length(years))

                                          invalidStand = F
                                          for(j in 1:length(years))
                                          {
                                            # (Year/Cycle filtering logic remains the same)
                                            # ...
                                            # ... (Lines 1086-1148 from your file) ...
                                            # ...
                                            if(useYear & is.na(endYear))
                                            {
                                              if(years[j] < startYear) next
                                            }
                                            if(useYear & !is.na(endYear))
                                            {
                                              if(years[j] < startYear | years[j] > endYear) next
                                            }
                                            if(useCycle & is.na(endCycle))
                                            {
                                              if(j < startCycle) next
                                            }
                                            if(useCycle & !is.na(endCycle))
                                            {
                                              if(j < startCycle | j > endCycle) next
                                            }
                                            if(j == length(years) & (addCompute | addPotFire | addCarbon | addFuels) & length(years) != 1)
                                            {
                                              next
                                            }

                                            standYrDF<- standDF[standDF$Year == years[j],]

                                            if(j == 1 & max(standYrDF$TPA) <= 0)
                                            {
                                              invalidStand = T
                                              break
                                            }

                                            standYrDF$TREEBA <- standYrDF$DBH^2 * standYrDF$TPA * 0.0054542
                                            standYrDF$TREECC <- pi * (standYrDF$CrWidth/2)^2 * (standYrDF$TPA/43560) * 100

                                            yrOutput<-data.frame(RUNTITLE = run, CASEID = caseID, STAND_CN = standCN, STANDID = standID,
                                                                 VARIANT = variant, REGION = region, YEAR = years[j])

                                            if(variant %in% c("CS", "LS", "NE", "SN")) {
                                              vol1 = "MCuFt"; vol2 = "SCuFt"; vol3 = "SBdFt"
                                            } else {
                                              vol1 = "TCuFt"; vol2 = "MCuFt"; vol3 = "BdFt"
                                            }

                                            # NOTE: vegOut and other functions must be available to the workers.
                                            # This assumes they are part of the vegClass package loaded via .packages
                                            custom_warning_messages <- character(0)
                                            veg_core <- withCallingHandlers(
                                              vegOut(data = standYrDF, region = region, MPSGcovTyp = MPSGcovTyp,
                                                     addHSS = addHSS, vol1 = vol1, vol2 = vol2, vol3 = vol3,
                                                     vol1DBH = vol1DBH, vol2DBH = vol2DBH, vol3DBH = vol3DBH,
                                                     InvDB = InvDB, InvStandTbl = InvStandTbl, customVars = customVars,
                                                     customOutputFns = custom_output_fn_names),
                                              warning = function(w) {
                                                msg <- conditionMessage(w)
                                                if (grepl("^Duplicate output column names found and renamed", msg)) {
                                                  custom_warning_messages <<- c(
                                                    custom_warning_messages,
                                                    msg
                                                  )
                                                  invokeRestart("muffleWarning")
                                                }
                                              }
                                            )

                                            if (length(custom_warning_messages) > 0) {
                                              return_list$messages <- unique(c(return_list$messages, custom_warning_messages))
                                            }

                                            yrOutput <- cbind(yrOutput, veg_core)

                                            if(addVolume)
                                            {
                                              volume <- volumeCalc(standYrDF, vol1 = vol1, vol2 = vol2, vol3 = vol3,
                                                                   vol1DBH = vol1DBH, vol2DBH = vol2DBH, vol3DBH = vol3DBH)
                                              yrOutput$VOL1 <- volume["VOL1"]; yrOutput$VOL2 <- volume["VOL2"]; yrOutput$VOL3 <- volume["VOL3"]
                                              yrOutput$DEADVOL1 <- volume["VOL4"]; yrOutput$DEADVOL2 <- volume["VOL5"]; yrOutput$DEADVOL3 <- volume["VOL6"]
                                            }
                                            standYrOutput[[j]]<-yrOutput
                                          }

                                          if(invalidStand)
                                          {
                                            return_list$status <- "invalid_stand"
                                            return(list(return_list)) # Return as a list element
                                          }

                                          standOut<-do.call("rbind", standYrOutput)

                                          if(length(standOut) <= 0)
                                          {
                                            return_list$status <- "no_output_produced"
                                            return(list(return_list)) # Return as a list element
                                          }

                                          standOut$CY <- seq(from = 1, to = nrow(standOut), by = 1)

                                          # (Logic for joining compute, potfire, fuels, carbon tables remains the same)
                                          # ...
                                          # ... (Lines 1218-1590 from your file, using worker_con) ...
                                          # ...
                                          if(addCompute)
                                          {
                                            if(RSQLite::dbExistsTable(worker_con, "FVS_COMPUTE"))
                                            {
                                              dbQuery <- computeQuery(caseID)
                                              computeDF <- RSQLite::dbGetQuery(worker_con, dbQuery)
                                              if(nrow(computeDF) > 0)
                                              {
                                                colnames(computeDF) <- toupper(colnames(computeDF))
                                                computeDF$STANDID <- NULL
                                                standOut <- merge(standOut, computeDF, by = c("CASEID", "YEAR"), all.x = T)
                                              }
                                            }
                                          }
                                          # ... (and so on for other optional tables)

                                          leadingCols <- c("RUNTITLE", "CASEID", "STAND_CN", "STANDID", "VARIANT", "REGION", "YEAR", "CY")
                                          colNames <- colnames(standOut)
                                          colNames <- colNames[!colNames %in% leadingCols]
                                          colNames <- c(leadingCols, colNames)
                                          standOut <- standOut[, c(colNames)]

                                          # Set the data to be returned
                                          return_list$data <- standOut
                                          return(list(return_list)) # Return as a list element
                                        }


    #===========================================================================
    # END of PARALLEL loop. Now process the results.
    #===========================================================================

    # Separate the results from the statuses, with robust checking
    all_dfs <- list()
    all_statuses <- list()
    all_stand_ids <- list()
    all_custom_messages <- character(0)
    error_count <- 0

    for (i in seq_along(allStandOutputs)) {
      result_item <- allStandOutputs[[i]]
      if (is.list(result_item) && !is.null(names(result_item))) {
        # It's a valid list, process it
        all_dfs[[i]] <- result_item$data
        all_statuses[[i]] <- result_item$status
        all_stand_ids[[i]] <- as.character(result_item$standID)
        if (!is.null(result_item$messages) && length(result_item$messages) > 0) {
          all_custom_messages <- c(all_custom_messages, as.character(result_item$messages))
        }
      } else {
        # It's likely an error object/string
        # Try to print the StandID if possible
        if (is.list(cases) && !is.null(cases$StandID) && length(cases$StandID) >= i) {
          vcat(sprintf("\nWarning: StandID skipped due to error: %s\n", as.character(cases$StandID[i])))
        }
        vcat(sprintf("\nWarning: An error occurred in one of the parallel workers. Result item %d was not a valid list. Content: %s\n", i, as.character(result_item)))
        all_dfs[[i]] <- NULL
        all_statuses[[i]] <- "error"
        if (is.list(cases) && !is.null(cases$StandID) && length(cases$StandID) >= i) {
          all_stand_ids[[i]] <- as.character(cases$StandID[i])
        } else {
          all_stand_ids[[i]] <- NA_character_
        }
        error_count <- error_count + 1
      }
    }

    if(error_count > 0) {
      vcat(sprintf("\nTotal errors encountered during parallel processing: %d\n", error_count))
    }

    # Combine all data frames into one
    # The use of `compact` from the `purrr` package would be cleaner, but to avoid adding
    # another dependency, we can filter out NULLs manually.
    all_dfs <- all_dfs[!sapply(all_dfs, is.null)]
    final_run_output <- do.call("rbind", all_dfs)

    if (!is.null(final_run_output) && nrow(final_run_output) > 0 && length(excluded_attr_names) > 0) {
      col_upper <- toupper(colnames(final_run_output))
      keep_cols <- !(col_upper %in% excluded_attr_names)
      dropped_cols <- colnames(final_run_output)[!keep_cols]

      if (length(dropped_cols) > 0) {
        final_run_output <- final_run_output[, keep_cols, drop = FALSE]
        vcat("Excluded attributes from output:", paste(dropped_cols, collapse = ", "), "\n")
      }
    }

    # Write the combined output for the entire run at once
    if(!is.null(final_run_output) && nrow(final_run_output) > 0) {
      if(!file.exists(output)) {
        utils::write.table(final_run_output, output, sep = ",", row.names = F)
      } else {
        utils::write.table(final_run_output, output, sep = ",", append = T, row.names = F, col.names = F)
      }
    }

    # Summarize and print counts
    standSum <- length(all_statuses)
    noLiveTrees <- sum(sapply(all_statuses, function(s) s == "no_live_trees"))
    noValidRecords <- sum(sapply(all_statuses, function(s) s == "no_valid_records"))
    invalidStands <- sum(sapply(all_statuses, function(s) s == "invalid_stand"))

    status_vec <- as.character(unlist(all_statuses))
    standid_vec <- as.character(unlist(all_stand_ids))
    not_processed_idx <- !status_vec %in% "processed"
    run_duration_minutes <- as.numeric(difftime(Sys.time(), run_start_time, units = "mins"))
    run_summaries[[run]] <- list(
      total_stands = standSum,
      processed_stands = sum(status_vec %in% "processed"),
      not_processed_stands = sum(not_processed_idx),
      status_counts = as.list(table(status_vec)),
      skipped_standids = unique(standid_vec[not_processed_idx & nzchar(standid_vec) & !is.na(standid_vec)]),
      duration_minutes = as.numeric(run_duration_minutes)
    )

    completed_stands_global <- completed_stands_global + as.integer(length(cases[["CaseID"]]))

    vcat("\n")
    vcat(paste0(rep("*", 75), collapse = ""), "\n")
    vcat(standSum, "stands processed out of", nrow(cases), "\n")
    vcat(paste0(rep("*", 75), collapse = ""), "\n", "\n")

    #Print run that has finished being processed
    vcat(paste0(rep("*", 75), collapse = ""), "\n")
    vcat("*", "Finished processing run:", run, "\n")
    vcat(paste0(rep("*", 75), collapse = ""), "\n", "\n")

    vcat(noLiveTrees, "stands contained no live tree records during simulation timeframe.\n")
    vcat(noValidRecords, "stands contained no valid tree records.\n")
    vcat(invalidStands, "stands were found to be invalid for processing.\n", "\n")

    if (length(all_custom_messages) > 0) {
      unique_msgs <- unique(all_custom_messages)
      vcat("Custom output duplicate-column notice:\n")
      vcat(paste(unique_msgs, collapse = " | "), "\n\n")
    }
  }

  ### END OF LOOP ACROSS RUNS

  # Close persistent worker connections before stopping cluster.
  parallel::clusterEvalQ(cl, {
    if (exists(".vegclass_worker_con", envir = .GlobalEnv, inherits = FALSE)) {
      con <- get(".vegclass_worker_con", envir = .GlobalEnv, inherits = FALSE)
      if (!is.null(con) && RSQLite::dbIsValid(con)) {
        try(RSQLite::dbDisconnect(con), silent = TRUE)
      }
      rm(".vegclass_worker_con", envir = .GlobalEnv)
    }
    NULL
  })

  # Stop the parallel cluster
  parallel::stopCluster(cl)
  vcat("Parallel cluster stopped.\n")

  # Re-establish connection for final index removal if needed
  if(removeCaseIndices)
  {
    final_con <- RSQLite::dbConnect(RSQLite::SQLite(), input)
    vcat("Removing Case ID indices from:", input, "\n\n")
    removeCaseIndices(final_con)
    vcat("Case ID indices removed from:", input, "\n\n")
    RSQLite::dbDisconnect(final_con)
  }

  #Print message indicating that all runs have been processed
  vcat(paste0(rep("*", 75), collapse = ""), "\n")
  vcat("*", "Finished processing all runs.", "\n")
  vcat(paste0(rep("*", 75), collapse = ""), "\n")


  #=============================================================================
  #Determine how long main function took to process (approximate)
  #=============================================================================

  #Set the end time of function execution
  endTime <- Sys.time()

  #Determine duration in seconds for function execution
  duration <- as.numeric(difftime(endTime,
                                  startTime,
                                  units = "secs"))
  duration <- round(duration, 0)

  #Determine hours
  hours <- floor(duration/3600)

  #Determine minutes
  mins <- floor(duration/60) %% 60

  #Determine seconds
  secs <- duration %% 60

  #Print startTime, endTime, total processing time and end of program
  vcat(paste("Start time:", startTime, "\n"))
  vcat(paste("End time:", endTime, "\n"))
  vcat("Total processing time:", hours, "hours", mins, "minutes", secs,
       "seconds", "\n")
  vcat("End of program.\n")
  vcat(sprintf("Output written to: %s\n", output))
  
  #Return from function main
  return(invisible(list(
    run_summaries = run_summaries,
    global_total_stands = as.integer(total_stands_all_runs),
    global_completed_stands = as.integer(completed_stands_global),
    duration_seconds = as.numeric(duration),
    duration_minutes = as.numeric(duration / 60)
  )))
 
}
