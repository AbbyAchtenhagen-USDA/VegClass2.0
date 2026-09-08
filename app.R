# Ensure required packages are installed and loaded.
cran_packages <- c("shiny", "bslib", "fs", "DT", "plotly", "tools")
missing_packages <- cran_packages[!(cran_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages, dependencies = TRUE)
}

invisible(lapply(cran_packages, library, character.only = TRUE))

# This app lives at package root and manually sources scripts from R/ below.
# Disable Shiny's automatic R/ autoload to avoid duplicate sourcing warnings.
options(shiny.autoload.r = FALSE)

# Source all vegClass scripts from the local package R directory.
# Source all .r files from the specified directory
r_files_path <- file.path(getwd(), "R")
r_files <- list.files(r_files_path, pattern = "\\.r$", full.names = TRUE)
for (file in r_files) {
  source(file)
}

extract_quoted_assignment <- function(lines, var_name) {
  pattern <- paste0("^\\s*", var_name, "\\s*<-\\s*\"([^\"]+)\"")
  hit <- regexec(pattern, lines)
  m <- regmatches(lines, hit)
  vals <- vapply(m, function(x) if (length(x) > 1) x[2] else "", character(1))
  vals <- vals[nzchar(vals)]
  if (length(vals) > 0) vals[1] else NULL
}

extract_assignment_expression <- function(lines, var_name) {
  pattern <- paste0("^\\s*", var_name, "\\s*<-\\s*(.+)$")
  hit <- regexec(pattern, lines)
  m <- regmatches(lines, hit)
  vals <- vapply(m, function(x) if (length(x) > 1) trimws(x[2]) else "", character(1))
  vals <- vals[nzchar(vals)]
  if (length(vals) > 0) vals[1] else NULL
}

extract_main_value <- function(lines, arg_name) {
  pattern <- paste0(arg_name, "\\s*=\\s*([^,\\n\\r]+)")
  hit <- regexec(pattern, lines)
  m <- regmatches(lines, hit)
  vals <- vapply(m, function(x) if (length(x) > 1) trimws(x[2]) else "", character(1))
  vals <- vals[nzchar(vals)]
  if (length(vals) > 0) vals[1] else NULL
}

extract_main_quoted_value <- function(lines, arg_name) {
  text_blob <- paste(lines, collapse = "\n")
  pattern <- paste0(arg_name, "\\s*=\\s*([\"'])(.*?)\\1")
  hit <- regexec(pattern, text_blob, perl = TRUE)
  m <- regmatches(text_blob, hit)

  if (length(m) == 0 || length(m[[1]]) < 3) {
    return(NULL)
  }

  val <- trimws(m[[1]][3])
  if (nzchar(val)) val else NULL
}

extract_main_c_vector <- function(lines, arg_name) {
  # Capture c(...) argument values from a main(...) call, including multi-line vectors.
  text_blob <- paste(lines, collapse = "\n")
  pattern <- paste0(arg_name, "\\s*=\\s*c\\(([\\s\\S]*?)\\)")
  hit <- regexec(pattern, text_blob, perl = TRUE)
  m <- regmatches(text_blob, hit)

  if (length(m) == 0 || length(m[[1]]) < 2) {
    return(NULL)
  }

  inside <- trimws(m[[1]][2])
  if (!nzchar(inside)) {
    return(character(0))
  }

  vals <- strsplit(inside, ",", fixed = TRUE)[[1]]
  vals <- trimws(vals)
  vals <- gsub("^['\"]|['\"]$", "", vals)
  vals <- vals[nzchar(vals)]
  vals
}

extract_path_list <- function(value) {
  if (is.null(value)) return(character(0))

  value <- trimws(as.character(value))
  value <- value[nzchar(value)]
  value <- value[!(toupper(value) %in% c("NULL", "CHARACTER(0)"))]
  if (length(value) == 0) return(character(0))

  value <- sub("^c\\((.*)\\)$", "\\1", value[1], perl = TRUE)
  if (!nzchar(value) || toupper(value) %in% c("NULL", "CHARACTER(0)")) {
    return(character(0))
  }
  value <- gsub("^['\"]|['\"]$", "", value)
  parts <- unlist(strsplit(value, "[;,\n\r]+", perl = TRUE), use.names = FALSE)
  parts <- trimws(parts)
  parts <- gsub("^['\"]|['\"]$", "", parts)
  parts <- parts[nzchar(parts)]
  parts <- parts[!(toupper(parts) %in% c("NULL", "CHARACTER(0)"))]
  parts
}

parse_assignment_logical <- function(lines, var_name) {
  raw_val <- extract_assignment_expression(lines, var_name)
  if (is.null(raw_val)) return(NULL)

  normalized <- toupper(gsub("^['\"]|['\"]$", "", trimws(raw_val)))
  if (normalized %in% c("TRUE", "T")) return(TRUE)
  if (normalized %in% c("FALSE", "F")) return(FALSE)
  NULL
}

parse_assignment_numeric <- function(lines, var_name) {
  raw_val <- extract_assignment_expression(lines, var_name)
  if (is.null(raw_val)) return(NULL)

  numeric_val <- suppressWarnings(as.numeric(gsub("^['\"]|['\"]$", "", trimws(raw_val))))
  if (is.na(numeric_val)) NULL else numeric_val
}

parse_assignment_string <- function(lines, var_name) {
  raw_val <- extract_assignment_expression(lines, var_name)
  if (is.null(raw_val)) return(NULL)

  cleaned <- gsub("^['\"]|['\"]$", "", trimws(raw_val))
  if (!nzchar(cleaned) || toupper(cleaned) == "NULL") NULL else cleaned
}

normalize_attribute_names <- function(values) {
  values <- toupper(trimws(as.character(values %||% character(0))))
  values <- values[nzchar(values)]
  values <- values[values != "NULL"]
  unique(values)
}

read_markdown_attribute_names <- function(md_path, section_heading = "Attribute List") {
  if (!file.exists(md_path)) return(character(0))

  lines <- readLines(md_path, warn = FALSE, encoding = "UTF-8")
  section_pattern <- paste0("^##\\s+", section_heading, "\\s*$")
  start_idx <- grep(section_pattern, lines, ignore.case = TRUE)
  if (length(start_idx) == 0) return(character(0))

  start_idx <- start_idx[1]
  end_candidates <- grep("^##\\s+", lines)
  end_candidates <- end_candidates[end_candidates > start_idx]
  end_idx <- if (length(end_candidates) > 0) end_candidates[1] - 1 else length(lines)
  if (end_idx <= start_idx) return(character(0))

  section_lines <- lines[(start_idx + 1):end_idx]
  heading_lines <- section_lines[grepl("^###\\s+", section_lines)]
  if (length(heading_lines) == 0) return(character(0))

  tokens <- unlist(regmatches(heading_lines, gregexpr("`[^`]+`", heading_lines, perl = TRUE)), use.names = FALSE)
  tokens <- gsub("^`|`$", "", tokens)
  tokens <- normalize_attribute_names(tokens)
  tokens[!grepl("*", tokens, fixed = TRUE)]
}

get_region_doc_filename <- function(region_value) {
  switch(
    toupper(trimws(as.character(region_value %||% ""))),
    "1" = "region_1.md",
    "2" = "region_2.md",
    "3" = "region_3.md",
    "8" = "region_8.md",
    "MPSG" = "region_mpsg.md",
    NULL
  )
}

read_output_column_names <- function(csv_path) {
  if (is.null(csv_path) || !nzchar(csv_path) || !file.exists(csv_path)) {
    return(character(0))
  }

  cols <- tryCatch(
    names(utils::read.csv(csv_path, nrows = 0, check.names = FALSE)),
    error = function(e) character(0)
  )

  normalize_attribute_names(cols)
}

load_run_defaults <- function() {
  detected_cores <- suppressWarnings(as.integer(parallel::detectCores()))
  if (is.na(detected_cores) || detected_cores < 1) detected_cores <- 1L

  defaults <- list(
    input_db = NULL,
    output_dir = NULL,
    output_csv_name = NULL,
    runTitles = "10110_NG_10110",
    allRuns = FALSE,
    region = "1",
    MPSGcovTyp = "",
    addHSS = FALSE,
    addCompute = FALSE,
    addPotFire = FALSE,
    addFuels = FALSE,
    addCarbon = FALSE,
    addVolume = FALSE,
    InvDB = NULL,
    InvStandTbl = "FVS_STANDINIT",
    customVars = NULL,
    enableCustomOutputScripts = FALSE,
    customOutputScripts = character(0),
    excludeAttributes = character(0),
    startYear = NA_real_,
    endYear = NA_real_,
    startCycle = NA_real_,
    endCycle = NA_real_,
    overwriteOut = TRUE,
    vol1DBH = 0,
    vol2DBH = 4,
    vol3DBH = 9,
    removeCaseIndices = FALSE,
    num_cores = max(1L, floor(detected_cores / 2))
  )

  run_defaults_path <- file.path(getwd(), "run_vegClass.R")
  if (!file.exists(run_defaults_path)) {
    return(defaults)
  }

  lines <- readLines(run_defaults_path, warn = FALSE)

  defaults$input_db <- parse_assignment_string(lines, "input")
  if (is.null(defaults$input_db)) defaults$input_db <- parse_assignment_string(lines, "input_db")

  defaults$output_dir <- parse_assignment_string(lines, "output_dir")
  defaults$output_csv_name <- parse_assignment_string(lines, "output_csv_name")

  run_titles_raw <- extract_assignment_expression(lines, "runTitles")
  run_titles_vec <- extract_path_list(run_titles_raw)
  if (!is.null(run_titles_vec) && length(run_titles_vec) > 0) {
    defaults$runTitles <- paste(run_titles_vec, collapse = ", ")
  } else {
    run_titles_raw <- extract_main_value(lines, "runTitles")
    if (!is.null(run_titles_raw)) {
      run_titles_clean <- gsub('^"|"$', "", trimws(run_titles_raw))
      if (nzchar(run_titles_clean)) defaults$runTitles <- run_titles_clean
    }
  }

  all_runs_raw <- parse_assignment_logical(lines, "allRuns")
  if (!is.null(all_runs_raw)) defaults$allRuns <- all_runs_raw

  region_raw <- parse_assignment_string(lines, "region")
  if (is.null(region_raw)) region_raw <- extract_assignment_expression(lines, "region")
  if (!is.null(region_raw)) {
    region_clean <- gsub('"', "", trimws(region_raw))
    defaults$region <- if (!nzchar(region_clean) || toupper(region_clean) == "NULL") "CUSTOM" else region_clean
  }

  mpsg_cov_raw <- extract_assignment_expression(lines, "MPSGcovTyp")
  if (!is.null(mpsg_cov_raw)) defaults$MPSGcovTyp <- gsub('"', "", trimws(mpsg_cov_raw))

  add_hss_raw <- parse_assignment_logical(lines, "addHSS")
  if (!is.null(add_hss_raw)) defaults$addHSS <- add_hss_raw

  add_compute_raw <- parse_assignment_logical(lines, "addCompute")
  if (!is.null(add_compute_raw)) defaults$addCompute <- add_compute_raw

  add_potfire_raw <- parse_assignment_logical(lines, "addPotFire")
  if (!is.null(add_potfire_raw)) defaults$addPotFire <- add_potfire_raw

  add_fuels_raw <- parse_assignment_logical(lines, "addFuels")
  if (!is.null(add_fuels_raw)) defaults$addFuels <- add_fuels_raw

  add_carbon_raw <- parse_assignment_logical(lines, "addCarbon")
  if (!is.null(add_carbon_raw)) defaults$addCarbon <- add_carbon_raw

  add_volume_raw <- parse_assignment_logical(lines, "addVolume")
  if (!is.null(add_volume_raw)) defaults$addVolume <- add_volume_raw

  defaults$InvDB <- parse_assignment_string(lines, "InvDB")

  inv_tbl <- parse_assignment_string(lines, "InvStandTbl")
  if (!is.null(inv_tbl)) defaults$InvStandTbl <- inv_tbl

  defaults$customVars <- parse_assignment_string(lines, "customVars")

  custom_output_scripts_raw <- extract_assignment_expression(lines, "customOutputScripts")
  if (is.null(custom_output_scripts_raw)) {
    custom_output_scripts_raw <- extract_assignment_expression(lines, "custom_output_scripts")
  }
  if (!is.null(custom_output_scripts_raw)) {
    custom_output_scripts_vec <- extract_path_list(custom_output_scripts_raw)
    if (length(custom_output_scripts_vec) > 0) {
      defaults$customOutputScripts <- unique(c(
        defaults$customOutputScripts,
        normalize_r_path(custom_output_scripts_vec)
      ))
      defaults$enableCustomOutputScripts <- TRUE
    }
  }

  exclude_attrs_raw <- extract_assignment_expression(lines, "excludeAttributes")
  if (is.null(exclude_attrs_raw)) exclude_attrs_raw <- extract_assignment_expression(lines, "exclude_attributes")
  if (is.null(exclude_attrs_raw)) exclude_attrs_raw <- extract_main_value(lines, "excludeAttributes")
  if (!is.null(exclude_attrs_raw)) {
    exclude_attrs_vec <- extract_path_list(exclude_attrs_raw)
    if (length(exclude_attrs_vec) > 0) {
      defaults$excludeAttributes <- unique(toupper(trimws(as.character(exclude_attrs_vec))))
      defaults$excludeAttributes <- defaults$excludeAttributes[nzchar(defaults$excludeAttributes)]
    }
  }

  start_year_raw <- parse_assignment_numeric(lines, "startYear")
  if (!is.null(start_year_raw)) defaults$startYear <- start_year_raw

  end_year_raw <- parse_assignment_numeric(lines, "endYear")
  if (!is.null(end_year_raw)) defaults$endYear <- end_year_raw

  start_cycle_raw <- parse_assignment_numeric(lines, "startCycle")
  if (!is.null(start_cycle_raw)) defaults$startCycle <- start_cycle_raw

  end_cycle_raw <- parse_assignment_numeric(lines, "endCycle")
  if (!is.null(end_cycle_raw)) defaults$endCycle <- end_cycle_raw

  overwrite_raw <- parse_assignment_logical(lines, "overwriteOut")
  if (!is.null(overwrite_raw)) defaults$overwriteOut <- overwrite_raw

  remove_indices_raw <- parse_assignment_logical(lines, "removeCaseIndices")
  if (!is.null(remove_indices_raw)) defaults$removeCaseIndices <- remove_indices_raw

  num_cores_raw <- extract_assignment_expression(lines, "num_cores")
  if (!is.null(num_cores_raw)) {
    eval_env <- list2env(
      list(
        detectCores = parallel::detectCores
      ),
      parent = baseenv()
    )

    num_cores_num <- suppressWarnings(tryCatch(
      as.numeric(eval(parse(text = num_cores_raw), envir = eval_env)),
      error = function(e) NA_real_
    ))
    if (!is.na(num_cores_num) && num_cores_num >= 1) {
      defaults$num_cores <- as.integer(floor(num_cores_num))
    }
  }

  for (dbh_name in c("vol1DBH", "vol2DBH", "vol3DBH")) {
    dbh_raw <- parse_assignment_numeric(lines, dbh_name)
    if (!is.null(dbh_raw)) defaults[[dbh_name]] <- dbh_raw
  }

  defaults
}

`%||%` <- function(x, y) if (is.null(x)) y else x

normalize_r_path <- function(path) {
  path <- as.character(path %||% "")
  path[is.na(path)] <- ""
  path <- trimws(path)
  gsub("\\\\", "/", path, fixed = FALSE)
}

get_attribute_units <- function(attr_name, region_value = NULL) {
  attr <- toupper(trimws(as.character(attr_name %||% "")))
  region_norm <- toupper(trimws(as.character(region_value %||% "")))

  if (region_norm %in% c("1", "1.0")) region_norm <- "1"
  if (region_norm %in% c("2", "2.0")) region_norm <- "2"
  if (region_norm %in% c("3", "3.0")) region_norm <- "3"
  if (region_norm %in% c("8", "8.0")) region_norm <- "8"
  if (region_norm %in% c("MPS", "MPSG")) region_norm <- "MPSG"

  if (attr %in% c("CAN_COV", "DOM_TYPE_R2_CC1", "DOM_TYPE_R2_CC2", "DOM_TYPE_R2_CC3", "XDCC1", "XDCC2")) {
    return("percent canopy cover (%)")
  }

  if (attr %in% c("BA", "BA_STM", "NMBA", "PWBA", "STBA")) {
    return("square feet per acre (ft^2/ac)")
  }

  if (attr %in% c("TPA", "TPA_STM", "SSTPA")) {
    return("trees per acre (trees/ac)")
  }

  if (attr %in% c("QMD", "QMD_STM", "QMD_TOP20", "BA_WT_DIA", "SIZECLASS_NTG", "NMSIZE", "PWSIZE", "STSIZE", "SSSIZE")) {
    return("diameter/size in inches (in)")
  }

  if (attr == "BA_WT_HT") {
    return("height in source DB units (typically feet)")
  }

  if (attr %in% c("ZSDI", "RSDI", "ZSDI_STM", "RSDI_STM")) {
    return("unitless stand density index")
  }

  if (attr %in% c(
    "DOM6040", "COVERTYPE_R1", "VEGTYPE", "VERTICAL_STRUCTURE", "STRCLSSTR",
    "DOM_TYPE_R2", "COVERTYPE_R2", "TREE_SIZE_CLASS_R2", "CROWN_CLASS_R2", "HSS1_4C", "HSS1_5",
    "DOM_TYPE", "DCC1", "DCC2", "CAN_SIZCL", "CAN_SZTMB", "CAN_SZWDL", "BA_STORY",
    "SSDOMSPP", "NMDOMSPP", "PWDOMSPP", "STDOMSPP", "DOMTYPE", "VEGCLASS",
    "COVERTYPE", "TREE_SIZE_CLASS", "CROWN_CLASS"
  )) {
    return("categorical code/class (unitless)")
  }

  "value-specific; verify source attribute units"
}

run_defaults <- load_run_defaults()

ui <- page_fillable(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  padding = 0,
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js"),
    tags$script(HTML("\n      if (window.mermaid) {\n        mermaid.initialize({ startOnLoad: false, securityLevel: 'loose', theme: 'default', flowchart: { htmlLabels: true } });\n      }\n\n      function rerenderMermaidById(id) {\n        setTimeout(function() {\n          var el = document.getElementById(id);\n          if (!el || !window.mermaid) return;\n          var source = el.dataset.mermaidSource;\n          if (!source) {\n            source = (el.textContent || '').trim();\n            if (source) el.dataset.mermaidSource = source;\n          }\n\n          if (source) {\n            el.textContent = source;\n          }\n\n          try {\n            el.removeAttribute('data-processed');\n            mermaid.run({ nodes: [el] });\n          } catch (e) {\n            console.error('Mermaid render failed:', e);\n          }\n        }, 70);\n      }\n\n      Shiny.addCustomMessageHandler('renderMermaid', function(x) {\n        rerenderMermaidById(x.id);\n      });\n\n      Shiny.addCustomMessageHandler('zoomMermaidById', function(x) {\n        var el = document.getElementById(x.id);\n        if (!el) return;\n\n        var current = parseFloat(el.dataset.zoomScale || '1');\n        if (!isFinite(current) || current <= 0) current = 1;\n\n        var next = current;\n        if (x && x.reset) {\n          next = 1;\n        } else {\n          var factor = Number((x && x.scale) || 1);\n          if (!isFinite(factor) || factor <= 0) factor = 1;\n          next = current * factor;\n        }\n\n        next = Math.max(0.5, Math.min(3, next));\n        el.dataset.zoomScale = String(next);\n        el.style.transformOrigin = 'top left';\n        el.style.transform = 'scale(' + next + ')';\n      });\n\n      Shiny.addCustomMessageHandler('setFlowchartModalClass', function(x) {\n        setTimeout(function() {\n          var modal = document.querySelector('.modal.show');\n          if (!modal) return;\n          var dialog = modal.querySelector('.modal-dialog');\n          if (!dialog) return;\n          dialog.classList.add('flowchart-modal');\n        }, 20);\n      });\n\n      Shiny.addCustomMessageHandler('toggleFlowchartModal', function(x) {\n        var modal = document.querySelector('.modal.show');\n        if (!modal) return;\n        var dialog = modal.querySelector('.modal-dialog');\n        if (!dialog) return;\n\n        dialog.classList.toggle('flowchart-expanded');\n\n        if (dialog.classList.contains('flowchart-expanded')) {\n          dialog.style.width = '';\n          dialog.style.maxWidth = '';\n          var content = dialog.querySelector('.modal-content');\n          var wrap = dialog.querySelector('.flowchart-wrap');\n          if (content) content.style.maxHeight = '';\n          if (wrap) {\n            wrap.style.minHeight = '';\n            wrap.style.maxHeight = '';\n          }\n        }\n\n        rerenderMermaidById(x.id);\n      });\n\n      Shiny.addCustomMessageHandler('enableFlowchartResize', function(x) {\n        setTimeout(function() {\n          var modal = document.querySelector('.modal.show');\n          if (!modal) return;\n          var dialog = modal.querySelector('.modal-dialog');\n          if (!dialog) return;\n          var content = dialog.querySelector('.modal-content');\n          var wrap = dialog.querySelector('.flowchart-wrap');\n          var handle = dialog.querySelector('.flowchart-resize-handle');\n          if (!content || !wrap || !handle || handle.dataset.bound === '1') return;\n\n          handle.dataset.bound = '1';\n\n          handle.addEventListener('mousedown', function(ev) {\n            ev.preventDefault();\n            ev.stopPropagation();\n\n            var startX = ev.clientX;\n            var startY = ev.clientY;\n            var startW = dialog.getBoundingClientRect().width;\n            var startH = content.getBoundingClientRect().height;\n\n            dialog.classList.remove('flowchart-expanded');\n            document.body.classList.add('flowchart-resizing');\n\n            function onMove(mev) {\n              var dx = mev.clientX - startX;\n              var dy = mev.clientY - startY;\n\n              var newW = Math.max(760, Math.min(window.innerWidth - 20, startW + dx));\n              var newH = Math.max(520, Math.min(window.innerHeight - 20, startH + dy));\n\n              dialog.style.width = newW + 'px';\n              dialog.style.maxWidth = newW + 'px';\n              content.style.maxHeight = newH + 'px';\n\n              var wrapHeight = Math.max(320, newH - 230);\n              wrap.style.minHeight = wrapHeight + 'px';\n              wrap.style.maxHeight = wrapHeight + 'px';\n            }\n\n            function onUp() {\n              document.removeEventListener('mousemove', onMove);\n              document.removeEventListener('mouseup', onUp);\n              document.body.classList.remove('flowchart-resizing');\n              rerenderMermaidById(x.id);\n            }\n\n            document.addEventListener('mousemove', onMove);\n            document.addEventListener('mouseup', onUp);\n          });\n        }, 40);\n      });\n    ")),
    tags$style(HTML("\n      .custom-top-bar {\n        background-color: #2C3E50;\n        color: #FFFFFF;\n        padding: 12px 20px;\n        font-size: 22px;\n        width: 100%;\n      }\n      .main-panel-tabs > .nav {\n        background-color: #2C3E50;\n        padding: 0 10px;\n        margin-bottom: 15px;\n        border-radius: 4px;\n      }\n      .main-panel-tabs > .nav .nav-link {\n        color: rgba(255,255,255,0.7);\n        border: none !important;\n        border-radius: 0;\n        padding: 12px 20px !important;\n        font-size: 16px;\n      }\n      .main-panel-tabs > .nav .nav-link:hover {\n        color: rgba(255,255,255,0.9);\n      }\n      .main-panel-tabs > .nav .nav-link.active {\n        color: #FFFFFF !important;\n        border-bottom: 3px solid #18BC9C !important;\n        background: transparent !important;\n      }\n      .status-block {\n        background: #F8F9FA;\n        border: 1px solid #DEE2E6;\n        border-radius: 6px;\n        padding: 10px;\n        margin-bottom: 10px;\n      }\n      .flowchart-wrap {\n        border: 1px solid #DEE2E6;\n        border-radius: 6px;\n        background: #FFFFFF;\n        padding: 10px;\n        min-height: 420px;\n        max-height: 55vh;\n        overflow: auto;\n      }\n      .flowchart-wrap .mermaid {\n        width: 100%;\n      }\n      .flowchart-wrap .mermaid svg {\n        display: block;\n        margin-left: auto;\n        margin-right: auto;\n      }\n      .modal-dialog.flowchart-modal {\n        width: 95vw;\n        max-width: 95vw;\n      }\n      .modal-dialog.flowchart-modal .modal-content {\n        position: relative;\n        max-height: 90vh;\n      }\n      .modal-dialog.flowchart-modal.flowchart-expanded {\n        width: 99vw;\n        max-width: 99vw;\n        margin: 0.5rem auto;\n      }\n      .modal-dialog.flowchart-modal.flowchart-expanded .modal-content {\n        max-height: 97vh;\n      }\n      .modal-dialog.flowchart-modal.flowchart-expanded .flowchart-wrap {\n        min-height: 70vh;\n        max-height: 80vh;\n      }\n      .flowchart-resize-handle {\n        position: absolute;\n        right: 10px;\n        bottom: 10px;\n        width: 18px;\n        height: 18px;\n        cursor: nwse-resize;\n        border-right: 2px solid #7F8C8D;\n        border-bottom: 2px solid #7F8C8D;\n        opacity: 0.75;\n        z-index: 5;\n      }\n      .flowchart-resize-handle:hover {\n        opacity: 1;\n      }\n      .flowchart-resizing {\n        user-select: none;\n      }\n    "))
  ),
  tags$script(HTML("\n    function setViewOutputSidebar(tabValue) {\n      var layout = document.querySelector('.bslib-sidebar-layout');\n      if (!layout) return;\n      if (tabValue === 'view_output_tab') {\n        layout.classList.add('hide-view-output-sidebar');\n      } else {\n        layout.classList.remove('hide-view-output-sidebar');\n      }\n    }\n\n    $(document).on('shiny:inputchanged', function(e) {\n      if (e && e.name === 'main_tabs') {\n        setViewOutputSidebar(e.value);\n      }\n    });\n\n    document.addEventListener('shiny:connected', function() {\n      setTimeout(function() {\n        if (window.Shiny && Shiny.shinyapp && Shiny.shinyapp.$inputValues) {\n          setViewOutputSidebar(Shiny.shinyapp.$inputValues.main_tabs);\n        }\n      }, 60);\n    });\n  ")),
  tags$style(HTML("\n    .bslib-sidebar-layout.hide-view-output-sidebar > .sidebar {\n      display: none !important;\n    }\n    .bslib-sidebar-layout.hide-view-output-sidebar {\n      grid-template-columns: minmax(0, 1fr) !important;\n    }\n    .bslib-sidebar-layout.hide-view-output-sidebar > .main {\n      width: 100% !important;\n      max-width: 100% !important;\n    }\n  ")),
  tags$style(HTML("\n    .output-preview-scroll {\n      width: 100%;\n      overflow-x: auto;\n    }\n    .output-preview-scroll .dataTables_wrapper .dataTables_scrollBody {\n      overflow-x: auto !important;\n    }\n  ")),
  tags$style(HTML("\n    .condensed-log-box {\n      max-height: 210px;\n      overflow-y: auto;\n      border: 1px solid #DEE2E6;\n      border-radius: 6px;\n      background: #F8F9FA;\n      padding: 6px 8px;\n    }\n    .condensed-log-box pre {\n      margin: 0;\n      font-size: 0.82rem;\n      line-height: 1.25;\n      white-space: pre-wrap;\n      word-break: break-word;\n    }\n  ")),
  tags$style(HTML("\n    .terminal-log-box {\n      max-height: 420px;\n    }\n  ")),
  tags$style(HTML("\n    .flowchart-layout {\n      display: flex;\n      gap: 12px;\n      align-items: stretch;\n    }\n    .flowchart-layout .flowchart-wrap {\n      flex: 1 1 auto;\n      min-width: 0;\n    }\n    .flowchart-legend {\n      flex: 0 0 34%;\n      max-width: 34%;\n      border: 1px solid #DEE2E6;\n      border-radius: 6px;\n      background: #F8F9FA;\n      padding: 10px;\n      min-height: 420px;\n      max-height: 55vh;\n      overflow: auto;\n      font-size: 0.92rem;\n      line-height: 1.35;\n    }\n    .flowchart-legend h6 {\n      margin-top: 0;\n      margin-bottom: 8px;\n      font-weight: 700;\n    }\n    .flowchart-legend p {\n      margin-bottom: 8px;\n    }\n    .flowchart-legend ul {\n      margin-bottom: 10px;\n      padding-left: 18px;\n    }\n    .flowchart-legend li {\n      margin-bottom: 6px;\n    }\n    .modal-dialog.flowchart-modal.flowchart-expanded .flowchart-legend {\n      min-height: 70vh;\n      max-height: 80vh;\n    }\n    @media (max-width: 1100px) {\n      .flowchart-layout {\n        flex-direction: column;\n      }\n      .flowchart-legend {\n        flex: 1 1 auto;\n        max-width: none;\n        min-height: 180px;\n      }\n    }\n  ")),
  tags$style(HTML("\n    #about_flowchart_mermaid .label foreignObject div {\n      width: 190px;\n      max-width: 190px;\n      white-space: normal !important;\n      overflow-wrap: anywhere;\n      word-break: break-word;\n      text-align: center;\n      line-height: 1.2;\n    }\n  ")),

  div(class = "custom-top-bar", "FVS Post-Processing - VegClass"),

  layout_sidebar(
    class = "p-3",
    border = FALSE,
    sidebar = sidebar(
      width = 460,
      conditionalPanel(
        condition = "input.main_tabs == 'about_tab'",
        h5("Workflow"),
        p("Use the tabs to configure directories, set processing options, and run vegClass in batch mode across all discovered .db files.")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'config_tab'",
        div(
          class = "status-block",
          strong("Defaults Loaded from run_vegClass.R"),
          p(class = "text-muted mb-0", "Input and output fields below are auto-populated from run_vegClass.R when available. You can edit any value in the UI before running.")
        ),
        h5("Input and Output"),
        actionButton("browse_input_db", "Select Input Database"),
        textInput("inputDbPath", "Input Database Path", run_defaults$input_db %||% ""),
        p(class = "text-muted", "Path to a single FVS output SQLite database (.db) containing FVS_Cases and tree list data."),
        actionButton("browse_output_dir", "Select Output Directory"),
        textInput("outputDirPath", "Output Directory Path", run_defaults$output_dir %||% ""),
        p(class = "text-muted", "Directory where the output .csv file will be written."),
        textInput("outputCsvName", "Output CSV Name", run_defaults$output_csv_name %||% ""),
        p(class = "text-muted", "Editable output filename. If blank, the app uses the input DB base name with .csv extension."),
        textOutput("output_csv_target"),
        hr(),
        h5("Run Options"),
        selectizeInput("runTitles", "Run Titles", choices = NULL, selected = NULL, multiple = TRUE),
        p(class = "text-muted", "One or more FVS RunTitle values to process. Ignored when Process All Runs is selected."),
        checkboxInput("allRuns", "Process All Runs", run_defaults$allRuns),
        p(class = "text-muted", "If checked, all runs in FVS_Cases are processed and Run Titles are ignored."),
        selectInput(
          "region",
          "Region",
          choices = c(
            "1" = "1",
            "2" = "2",
            "3" = "3",
            "8 (Developed for a project in North Carolina. Not official USFS R8 classification)" = "8",
            "MPSG" = "MPSG",
            "CUSTOM" = "CUSTOM"
          ),
          selected = run_defaults$region
        ),
        p(class = "text-muted", "Region ruleset used for vegetation classifications and derived attributes for all selected runs. Use CUSTOM to skip built-in region outputs and run custom-only outputs."),
        uiOutput("MPSGcovTyp_ui"),
        hr(),
        h5("Additional Modules"),
        checkboxInput("addHSS", "Add HSS", run_defaults$addHSS),
        p(class = "text-muted", "Include Region 2 Habitat Structural Stage outputs (MPSG only)."),
        checkboxInput("addCompute", "Add Compute", run_defaults$addCompute),
        p(class = "text-muted", "Include variables from the FVS_Compute table."),
        checkboxInput("addPotFire", "Add Potential Fire", run_defaults$addPotFire),
        p(class = "text-muted", "Include variables from FVS_PotFire or FVS_PotFire_East tables."),
        checkboxInput("addFuels", "Add Fuels", run_defaults$addFuels),
        p(class = "text-muted", "Include variables from the FVS_Fuels table."),
        checkboxInput("addCarbon", "Add Carbon", run_defaults$addCarbon),
        p(class = "text-muted", "Include variables from the FVS_Carbon table."),
        checkboxInput("addVolume", "Add Volume", run_defaults$addVolume),
        p(class = "text-muted", "Calculate VOL1-3 and DEADVOL1-3 using variant-specific volume definitions."),
        hr(),
        h5("Volume DBH"),
        numericInput("vol1DBH", "Volume 1 DBH", run_defaults$vol1DBH),
        p(class = "text-muted", "Minimum DBH threshold used when calculating VOL1 and DEADVOL1."),
        numericInput("vol2DBH", "Volume 2 DBH", run_defaults$vol2DBH),
        p(class = "text-muted", "Minimum DBH threshold used when calculating VOL2 and DEADVOL2."),
        numericInput("vol3DBH", "Volume 3 DBH", run_defaults$vol3DBH),
        p(class = "text-muted", "Minimum DBH threshold used when calculating VOL3 and DEADVOL3."),
        hr(),
        h5("Other Settings"),
        textOutput("cpu_core_info"),
        numericInput("num_cores", "Parallel Cores", run_defaults$num_cores, min = 1, step = 1),
        p(class = "text-muted", "Number of CPU cores used for parallel stand processing."),
        numericInput("startYear", "Start Year", run_defaults$startYear),
        p(class = "text-muted", "First simulation year to include in output when filtering by year."),
        numericInput("endYear", "End Year", run_defaults$endYear),
        p(class = "text-muted", "Last simulation year to include in output. Leave blank to include all years after Start Year."),
        numericInput("startCycle", "Start Cycle", run_defaults$startCycle),
        p(class = "text-muted", "First simulation cycle to include in output. If provided, cycle filtering takes precedence over year filtering."),
        numericInput("endCycle", "End Cycle", run_defaults$endCycle),
        p(class = "text-muted", "Last simulation cycle to include in output. Leave blank to include all cycles after Start Cycle."),
        checkboxInput("overwriteOut", "Overwrite Output", run_defaults$overwriteOut),
        p(class = "text-muted", "If checked, replaces an existing output file with the same name."),
        checkboxInput("removeCaseIndices", "Remove Case Indices", run_defaults$removeCaseIndices),
        p(class = "text-muted", "If checked, removes CaseID indices that were created for processing."),
        hr(),
        h5("Inventory Settings"),
        actionButton("browse_invDB", "Select Inventory DB"),
        textInput("invDBPath", "Inventory DB Path", run_defaults$InvDB %||% ""),
        helpText("Region 1 only: inventory database used to look up PV_CODE values for processed stands."),
        textInput("invStandTbl", "Inventory Stand Table", run_defaults$InvStandTbl),
        helpText("Region 1 only: stand table name in Inventory DB (for example, FVS_STANDINIT) containing stand records used for PV_CODE matching."),
        hr(),
        h5("Custom Variables"),
        actionButton("browse_customVars", "Select Custom Vars File"),
        textInput("customVarsPath", "Custom Vars Path", run_defaults$customVars %||% ""),
        p(class = "text-muted", "Path to CustomVars_vegClass.xlsx defining additional custom output variables."),
        hr(),
        h5("Custom Output Script"),
        checkboxInput(
          "enableCustomOutputScripts",
          "Add Custom Project Attributes from Script",
          run_defaults$enableCustomOutputScripts
        ),
        p(class = "text-muted", "If checked, custom attribute columns returned by custom script function(s) are appended to output rows."),
        textInput(
          "customOutputScriptsPath",
          "Custom Output Script Path",
          paste(run_defaults$customOutputScripts %||% character(0), collapse = ", ")
        ),
        p(class = "text-muted", "Optional .r file path(s) for custom attribute outputs. Separate multiple paths with commas or new lines.")
        ,hr(),
        h5("Output Attribute Filter"),
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap; margin-bottom: 8px;",
          actionButton("selectCoreExcludeAttrs", "Select All Core Attributes"),
          actionButton("clearExcludeAttrs", "Clear Selection")
        ),
        selectizeInput(
          "excludeAttributes",
          "Exclude Attributes",
          choices = NULL,
          selected = run_defaults$excludeAttributes %||% character(0),
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            create = TRUE,
            placeholder = "Select or type attribute names to exclude"
          )
        ),
        p(class = "text-muted", "Select attributes to remove from the final CSV. Use 'Select All Core Attributes' to add every documented core attribute at once. The list includes documented core and region outputs, and expands to include actual output CSV columns when a preview file is available.")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'run_tab'",
        h5("Run VegClass"),
        p("This will process the selected input database and write one output CSV file."),
        actionButton("runMain", "Run Processing", class = "btn-danger w-100")
      ),
      conditionalPanel(
        condition = "input.main_tabs == 'plot_tab'",
        h5("Plot Options"),
        uiOutput("plot_runtitle_ui"),
        selectInput(
          "plot_type",
          "Plot Type",
          choices = c(
            "Scatter Plot" = "scatter",
            "Bar Plot" = "bar",
            "Time Series (Per Run)" = "timeseries",
            "Categorical Composition (%)" = "composition"
          ),
          selected = "scatter"
        ),
        conditionalPanel(
          condition = "input.plot_type == 'scatter' || input.plot_type == 'bar'",
          uiOutput("plot_x_ui"),
          uiOutput("plot_color_ui")
        ),
        conditionalPanel(
          condition = "input.plot_type == 'scatter'",
          uiOutput("plot_y_ui"),
          checkboxInput("plot_add_trendline", "Add Trend Line", FALSE)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'bar'",
          uiOutput("plot_bar_value_ui"),
          selectInput(
            "plot_bar_fun",
            "Bar Aggregation (for selected value)",
            choices = c("Count" = "count", "Sum" = "sum", "Mean" = "mean", "Median" = "median"),
            selected = "count"
          ),
          selectInput(
            "plot_bar_mode",
            "Bar Mode",
            choices = c("Grouped" = "group", "Stacked" = "stack"),
            selected = "group"
          )
        ),
        conditionalPanel(
          condition = "input.plot_type == 'timeseries'",
          uiOutput("plot_ts_value_ui"),
          selectInput(
            "plot_ts_fun",
            "Time-Series Aggregation",
            choices = c("Count" = "count", "Sum" = "sum", "Mean" = "mean", "Median" = "median"),
            selected = "mean"
          ),
          checkboxInput("plot_ts_markers", "Show Markers", TRUE)
        ),
        conditionalPanel(
          condition = "input.plot_type == 'composition'",
          uiOutput("plot_comp_x_ui"),
          uiOutput("plot_comp_group_ui"),
          uiOutput("plot_comp_split_run_ui")
        )
      )
    ),

    div(
      class = "main-panel-tabs h-100",
      navset_tab(
        id = "main_tabs",
        nav_panel(
          title = tagList(icon("circle-info"), " About"),
          value = "about_tab",
          card(
            card_header("Application Overview"),
            p("This app runs vegClass post-processing against FVS SQLite databases and writes CSV outputs."),
            tags$ul(
              tags$li("Configure directories and run options on the '1. Configure' tab."),
              tags$li("Review region-specific documentation before execution."),
              tags$li("Use '2. Run VegClass' to process the selected input database and write the output CSV."),
              tags$li("Use '3. View Output' to preview output rows and inspect attribute logic details."),
              tags$li("Use '4. Plot Output' to create scatter, bar, time-series, and composition plots from the output CSV.")
            )
          ),
          card(
            card_header("Simplified vegClass Workflow"),
            p(class = "text-muted", "This overview walks through the vegClass workflow: the raw FVS_TreeList columns it reads, the core metrics it calculates, region-specific outputs, optional module outputs, and final CSV assembly."),
            div(
              style = "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:8px;",
              actionButton("aboutZoomIn", "Zoom In", icon = icon("magnifying-glass-plus"), class = "btn btn-outline-secondary btn-sm"),
              actionButton("aboutZoomOut", "Zoom Out", icon = icon("magnifying-glass-minus"), class = "btn btn-outline-secondary btn-sm"),
              actionButton("aboutZoomReset", "Reset", icon = icon("arrows-rotate"), class = "btn btn-outline-secondary btn-sm")
            ),
            div(
              class = "flowchart-wrap",
              div(
                id = "about_flowchart_mermaid",
                class = "mermaid",
                HTML(
                  paste(
                    c(
                      "flowchart TB",
                      "A[Input DB\\nand run filters] --> ACASE[FVS_Cases columns used:\\nRunTitle, CaseID, StandID,\\nVariant, Groups, Stand_CN]",
                      "A --> ATBL[FVS_TreeList columns used:\\nCaseID, StandID, Year, SpeciesPLANTS, TPA, MortPA, DBH, Ht, CrWidth,\\nWest variants: TCuFt, MCuFt, BdFt,\\nEast variants: MCuFt, SCuFt, SBdFt]",
                      "ACASE --> B[Run main processing]",
                      "ATBL --> B",
                      "B --> C[Compute core stand metrics]",
                      "C --> COREO[Core outputs:\\nCAN_COV, BA, TPA,\\nQMD, ZSDI, RSDI,\\nBA_WT_DIA, BA_WT_HT,\\nBA_STM, TPA_STM, QMD_STM,\\nZSDI_STM, RSDI_STM,\\nQMD_TOP20]",
                      "C --> D{Region selection}",
                      "D --> R1O[Region 1 outputs:\\nDOM6040, COVERTYPE_R1, VEGTYPE,\\nVERTICAL_STRUCTURE, SIZECLASS_NTG, STRCLSSTR]",
                      "D --> R2O[Region 2 outputs:\\nDOM_TYPE_R2,\\nDOM_TYPE_R2_CC1, DOM_TYPE_R2_CC2, DOM_TYPE_R2_CC3,\\nCOVERTYPE_R2,\\nTREE_SIZE_CLASS_R2, CROWN_CLASS_R2]",
                      "D --> R3O[Region 3 outputs:\\nDOM_TYPE, DCC1, XDCC1\\nDCC2, XDCC2,\\nCAN_SIZCL, CAN_SZTMB,\\nCAN_SZWDL, BA_STORY]",
                      "D --> R8O[Region 8 outputs:\\nSSDOMSPP, NMDOMSPP,\\nPWDOMSPP, STDOMSPP, DOMTYPE,\\nSSSIZE, SSTPA, NMBA, NMSIZE,\\nPWBA, PWSIZE, STBA, STSIZE,\\nVEGCLASS]",
                      "D --> MPO[MPSG outputs:\\nCOVERTYPE, TREE_SIZE_CLASS,\\nCROWN_CLASS, VERTICAL_STRUCTURE,\\nHSS1_4C, HSS1_5, \\nwhen add HSS = TRUE]",
                      "D --> CUO[CUSTOM outputs:\\ncustom vars and\\ncustom script columns]",
                      "B --> OPT{Optional modules}",
                      "OPT --> OALL[Optional module outputs:\\naddCompute: FVS_Compute\\naddPotFire: FVS_PotFire\\naddFuels: FVS_Fuels\\naddCarbon: FVS_Carbon\\naddVolume: VOL1, VOL2, VOL3\\nDEADVOL\\nadd HSS: HSS1_4C, HSS1_5]",
                      "COREO --> M[Merge columns]",
                      "R1O --> M[Merge columns]",
                      "R2O --> M",
                      "R3O --> M",
                      "R8O --> M",
                      "MPO --> M",
                      "CUO --> M",
                      "OALL --> M",
                      "M --> X[Exclude selected\\nattributes]",
                      "X --> Y[Write final CSV]",
                      "Y --> Z[Preview table\\nand build plots]",
                      "classDef core fill:#E8F1FF,stroke:#2B5CAA,stroke-width:1.4px,color:#0E2A52",
                      "classDef region fill:#EAF9F0,stroke:#1D7D46,stroke-width:1.2px,color:#0B3E24",
                      "classDef out fill:#FFF9D6,stroke:#8A7A00,stroke-width:1.2px,color:#4A4300",
                      "classDef opt fill:#F0E8FF,stroke:#6D3FB0,stroke-width:1.2px,color:#391A66",
                      "classDef src fill:#E7F7FF,stroke:#1E6A8D,stroke-width:1.2px,color:#08364D",
                      "class A,ACASE,ATBL src",
                      "class B,C,D,M,X,Y,Z core",
                      "class R1O,R2O,R3O,R8O,MPO,CUO,COREO out",
                      "class OPT,OALL opt"
                    ),
                    collapse = "\n"
                  )
                )
              )
            )
          )
        ),
        nav_panel(
          title = tagList(icon("sliders"), " 1. Configure"),
          value = "config_tab",
          card(
            card_header("Current Configuration"),
            div(class = "status-block", strong("Input Database:"), verbatimTextOutput("input_db_path", placeholder = TRUE)),
            div(class = "status-block", strong("Output Directory:"), verbatimTextOutput("output_dir_path", placeholder = TRUE)),
            div(class = "status-block", strong("Inventory DB:"), verbatimTextOutput("invDB_path", placeholder = TRUE)),
            div(class = "status-block", strong("Custom Vars File:"), verbatimTextOutput("customVars_path", placeholder = TRUE)),
            div(class = "status-block", strong("Custom Output Script(s):"), verbatimTextOutput("customOutputScripts_path", placeholder = TRUE)),
            div(class = "status-block", strong("Excluded Attributes:"), verbatimTextOutput("excludedAttributes_value", placeholder = TRUE))
          ),
          card(
            card_header("Region-Specific Description"),
            htmlOutput("region_description")
          )
        ),
        nav_panel(
          title = tagList(icon("play"), " 2. Run VegClass"),
          value = "run_tab",
          card(
            card_header("Selected Input Database"),
            verbatimTextOutput("db_preview")
          ),
          card(
            card_header("Processing Log"),
            div(class = "condensed-log-box", verbatimTextOutput("log"))
          ),
          card(
            card_header("Terminal Output"),
            p(class = "text-muted", "Console output captured from main() during the run."),
            div(class = "condensed-log-box terminal-log-box", verbatimTextOutput("terminal_log"))
          )
        ),
        nav_panel(
          title = tagList(icon("table"), " 3. View Output"),
          value = "view_output_tab",
          card(
            card_header("Output CSV Preview"),
            textInput("outputPreviewPath", "Output CSV Path (paste path to preview)", ""),
            div(class = "status-block", strong("Resolved Output File:"), verbatimTextOutput("resolved_output_path", placeholder = TRUE)),
            verbatimTextOutput("output_preview_status"),
            p(class = "text-muted", "Click a table cell to view region-specific logic notes for that attribute."),
            div(class = "output-preview-scroll", DTOutput("output_preview"))
          )
        ),
        nav_panel(
          title = tagList(icon("chart-column"), " 4. Plot Output"),
          value = "plot_tab",
          card(
            card_header("Output Plot Builder"),
            verbatimTextOutput("plot_status"),
            plotlyOutput("output_plot", height = "520px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  docs_dir_path <- file.path(dirname(r_files_path), "region_descriptions")
  input_db_selected <- reactiveVal(normalize_r_path(run_defaults$input_db %||% ""))
  output_dir_selected <- reactiveVal(normalize_r_path(run_defaults$output_dir %||% ""))
  default_run_titles <- trimws(strsplit(run_defaults$runTitles %||% "", ",")[[1]])
  default_run_titles <- default_run_titles[nzchar(default_run_titles)]
  default_excluded_attributes <- normalize_attribute_names(run_defaults$excludeAttributes)
  exclude_attributes_selected <- reactiveVal(default_excluded_attributes)

  updateSelectizeInput(
    session,
    "runTitles",
    choices = default_run_titles,
    selected = default_run_titles,
    server = TRUE
  )

  observeEvent(input$main_tabs, {
    if (identical(input$main_tabs, "about_tab")) {
      session$sendCustomMessage("renderMermaid", list(id = "about_flowchart_mermaid"))
    }
  }, ignoreInit = FALSE)

  observeEvent(input$aboutZoomIn, {
    session$sendCustomMessage("zoomMermaidById", list(id = "about_flowchart_mermaid", scale = 1.2))
  }, ignoreInit = TRUE)

  observeEvent(input$aboutZoomOut, {
    session$sendCustomMessage("zoomMermaidById", list(id = "about_flowchart_mermaid", scale = 0.85))
  }, ignoreInit = TRUE)

  observeEvent(input$aboutZoomReset, {
    session$sendCustomMessage("zoomMermaidById", list(id = "about_flowchart_mermaid", reset = TRUE))
  }, ignoreInit = TRUE)

  observeEvent(input$browse_input_db, {
    selected <- tryCatch(file.choose(new = FALSE), error = function(e) "")
    selected <- normalize_r_path(selected)
    if (!nzchar(selected)) return()
    input_db_selected(selected)
    updateTextInput(session, "inputDbPath", value = selected)
  }, ignoreInit = TRUE)

  observeEvent(input$browse_output_dir, {
    selected <- tryCatch(utils::choose.dir(default = getwd(), caption = "Select Output Directory"), error = function(e) "")
    selected <- normalize_r_path(selected)
    if (!nzchar(selected)) return()
    output_dir_selected(selected)
    updateTextInput(session, "outputDirPath", value = selected)
  }, ignoreInit = TRUE)

  input_db_path <- reactive({
    manual_val <- normalize_r_path(input$inputDbPath %||% "")
    if (nzchar(manual_val)) return(manual_val)

    selected_val <- normalize_r_path(input_db_selected())
    if (nzchar(selected_val)) return(selected_val)

    normalize_r_path(run_defaults$input_db)
  })

  observeEvent(input_db_path(), {
    db_path <- normalize_r_path(input_db_path())

    if (!nzchar(db_path) || !file.exists(db_path)) {
      updateSelectizeInput(
        session,
        "runTitles",
        choices = default_run_titles,
        selected = default_run_titles,
        server = TRUE
      )
      return()
    }

    run_titles_db <- tryCatch({
      con <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
      on.exit(RSQLite::dbDisconnect(con), add = TRUE)

      if (!RSQLite::dbExistsTable(con, "FVS_Cases")) {
        return(character(0))
      }

      out <- RSQLite::dbGetQuery(con, "SELECT DISTINCT RunTitle FROM FVS_Cases ORDER BY RunTitle")
      vals <- trimws(as.character(out$RunTitle))
      vals[nzchar(vals)]
    }, error = function(e) {
      character(0)
    })

    current_selected <- input$runTitles %||% character(0)
    current_selected <- trimws(as.character(current_selected))
    current_selected <- current_selected[nzchar(current_selected)]

    available_titles <- unique(c(run_titles_db, default_run_titles))

    if (length(current_selected) > 0) {
      selected_titles <- intersect(current_selected, available_titles)
    } else {
      selected_titles <- intersect(default_run_titles, available_titles)
    }

    updateSelectizeInput(
      session,
      "runTitles",
      choices = available_titles,
      selected = selected_titles,
      server = TRUE
    )
  }, ignoreInit = FALSE)

  output_dir_path <- reactive({
    manual_val <- normalize_r_path(input$outputDirPath %||% "")
    if (nzchar(manual_val)) return(manual_val)

    selected_val <- normalize_r_path(output_dir_selected())
    if (nzchar(selected_val)) return(selected_val)

    normalize_r_path(run_defaults$output_dir)
  })

  inv_db_selected <- reactiveVal(NULL)

  observeEvent(input$browse_invDB, {
    selected <- tryCatch(file.choose(new = FALSE), error = function(e) "")
    selected <- normalize_r_path(selected)
    if (!nzchar(selected)) return()
    inv_db_selected(selected)
    updateTextInput(session, "invDBPath", value = selected)
  }, ignoreInit = TRUE)

  inv_db_path <- reactive({
    manual_val <- normalize_r_path(input$invDBPath %||% "")
    if (nzchar(manual_val)) return(manual_val)

    selected <- inv_db_selected()
    if (is.null(selected) || !nzchar(selected)) normalize_r_path(run_defaults$InvDB) else normalize_r_path(selected)
  })

  custom_vars_path <- reactive({
    manual_val <- trimws(input$customVarsPath %||% "")
    if (nzchar(manual_val)) manual_val else NULL
  })

  observeEvent(input$browse_customVars, {
    selected <- tryCatch(file.choose(new = FALSE), error = function(e) "")
    selected <- normalize_r_path(selected)
    if (!nzchar(selected)) return()
    updateTextInput(session, "customVarsPath", value = selected)
  }, ignoreInit = TRUE)

  custom_output_scripts_path <- reactive({
    manual_val <- trimws(input$customOutputScriptsPath %||% "")
    if (!nzchar(manual_val)) return(character(0))

    scripts <- extract_path_list(manual_val)
    if (length(scripts) == 0) return(character(0))

    unique(normalize_r_path(scripts))
  })

  resolve_custom_script_paths <- function(scripts) {
    scripts <- unique(normalize_r_path(scripts))
    if (length(scripts) == 0) return(character(0))

    resolved <- character(0)

    for (raw_path in scripts) {
      candidates <- raw_path

      # Allow entering just a script name if file lives in local R/ folder.
      if (!grepl("[/\\\\]", raw_path)) {
        candidates <- c(candidates, file.path(getwd(), "R", raw_path))
      }

      # Allow omitting .r extension.
      add_ext <- vapply(
        candidates,
        function(p) !grepl("\\.r$", basename(p), ignore.case = TRUE),
        logical(1)
      )
      if (any(add_ext)) {
        candidates <- c(candidates, paste0(candidates[add_ext], ".r"))
      }

      candidates <- unique(normalize_r_path(candidates))
      existing <- candidates[file.exists(candidates)]
      if (length(existing) > 0) {
        resolved <- c(resolved, existing[1])
      }
    }

    unique(resolved)
  }

  resolved_custom_output_scripts <- reactive({
    scripts <- custom_output_scripts_path()
    resolve_custom_script_paths(scripts)
  })

  observeEvent(input$excludeAttributes, {
    exclude_attributes_selected(normalize_attribute_names(input$excludeAttributes))
  }, ignoreInit = TRUE)

  excluded_attributes <- reactive({
    exclude_attributes_selected()
  })

  resolved_custom_vars_path <- reactive({
    path <- custom_vars_path()
    if (is.null(path) || !nzchar(path) || !file.exists(path)) NULL else path
  })

  selected_output_dir <- reactive({
    output_dir_path()
  })

  output$input_db_path <- renderText({
    val <- input_db_path()
    if (is.null(val) || !nzchar(val)) "No file selected" else val
  })
  output$output_dir_path <- renderText({
    val <- selected_output_dir()
    if (is.null(val) || !nzchar(val)) "No directory selected" else val
  })
  output$output_csv_target <- renderText({
    input_db <- input_db_path()
    output_parent_dir <- selected_output_dir()

    out_base <- if (!is.null(input_db) && nzchar(input_db)) {
      tools::file_path_sans_ext(basename(input_db))
    } else {
      "output"
    }

    output_csv_name <- trimws(input$outputCsvName %||% "")
    if (!nzchar(output_csv_name)) {
      output_csv_name <- paste0(out_base, ".csv")
    } else if (!grepl("\\.csv$", output_csv_name, ignore.case = TRUE)) {
      output_csv_name <- paste0(output_csv_name, ".csv")
    }

    if (is.null(output_parent_dir) || !nzchar(output_parent_dir)) {
      paste0("Effective output file: ", output_csv_name)
    } else {
      paste0("Effective output file: ", file.path(output_parent_dir, output_csv_name))
    }
  })
  output$cpu_core_info <- renderText({
    detected_cores <- suppressWarnings(as.integer(parallel::detectCores()))
    if (is.na(detected_cores) || detected_cores < 1) detected_cores <- 1L
    paste0("Detected CPU cores: ", detected_cores, " | Default parallel cores: ", max(1L, floor(detected_cores / 2)))
  })
  output$invDB_path <- renderText({
    val <- normalize_r_path(input$invDBPath %||% "")
    if (!nzchar(val)) "" else val
  })
  output$customVars_path <- renderText({
    val <- resolved_custom_vars_path()
    if (is.null(val)) "No file selected" else val
  })
  output$customOutputScripts_path <- renderText({
    if (!isTRUE(input$enableCustomOutputScripts)) {
      return("Disabled")
    }

    val <- resolved_custom_output_scripts()
    if (length(val) == 0) "No file selected" else paste(val, collapse = "\n")
  })
  output$excludedAttributes_value <- renderText({
    attrs <- excluded_attributes()
    if (length(attrs) == 0) "None" else paste(attrs, collapse = ", ")
  })

  output$MPSGcovTyp_ui <- renderUI({
    if (identical(input$region, "MPSG")) {
      tagList(
        textInput("MPSGcovTyp", "MPSG Cover Type", run_defaults$MPSGcovTyp %||% ""),
        p(class = "text-muted", "For MPSG only: choose regional cover type algorithm source (valid values: 1, 2, or 3).")
      )
    }
  })

  output$region_description <- renderUI({
    req(input$region)

    if (identical(input$region, "CUSTOM")) {
      return(HTML("<p>CUSTOM runs skip the built-in region logic and only return optional custom outputs, such as custom variable definitions or custom output scripts.</p>"))
    }

    region_md_name <- get_region_doc_filename(input$region)

    if (is.null(region_md_name)) {
      return(HTML("<p>No region description mapping is configured for this region.</p>"))
    }

    render_md_file <- function(path, missing_label) {
      if (!file.exists(path)) {
        return(p(sprintf("%s not found: %s", missing_label, path)))
      }

      md_text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

      # Prefer commonmark when available; fall back to markdown; else show raw text.
      if (requireNamespace("commonmark", quietly = TRUE)) {
        return(HTML(commonmark::markdown_html(md_text)))
      }

      if (requireNamespace("markdown", quietly = TRUE)) {
        return(HTML(markdown::markdownToHTML(text = md_text, fragment.only = TRUE)))
      }

      tagList(
        p("Install 'commonmark' or 'markdown' to render rich formatting. Showing plain text for now."),
        tags$pre(style = "white-space: pre-wrap; margin-bottom: 0;", md_text)
      )
    }

    region_md_path <- file.path(docs_dir_path, region_md_name)
    core_md_path <- file.path(docs_dir_path, "core_attributes.md")

    tagList(
      div(
        style = "max-height: 460px; overflow-y: auto; border: 1px solid #ddd; border-radius: 6px; background: #fff; padding: 12px;",
        h5(sprintf("Region %s Description", input$region)),
        render_md_file(region_md_path, "Region description file"),
        tags$hr(),
        h5("Core Attributes"),
        render_md_file(core_md_path, "Core attributes file")
      )
    )
  })

  selected_db <- reactiveVal(NULL)
  log_val <- reactiveVal("Waiting for scan or run.")
  terminal_log_val <- reactiveVal("Terminal output will appear here after a run.")
  last_output_file <- reactiveVal(NULL)
  run_in_progress <- reactiveVal(FALSE)

  observe({
    selected_db(input_db_path())
  })

  output$db_preview <- renderText({
    db_file <- selected_db()
    if (is.null(db_file) || !nzchar(db_file)) {
      return("No input database selected yet.")
    }
    db_file
  })

  resolved_output_file <- reactive({
    manual_output_path <- normalize_r_path(input$outputPreviewPath %||% "")
    if (nzchar(manual_output_path)) {
      if (file.exists(manual_output_path)) {
        return(manual_output_path)
      }
      return(NULL)
    }

    preferred <- last_output_file()
    if (!is.null(preferred) && nzchar(preferred) && file.exists(preferred)) {
      return(preferred)
    }

    input_db <- selected_db()
    output_parent_dir <- selected_output_dir()
    if (is.null(input_db) || !nzchar(input_db) || !file.exists(input_db) ||
        is.null(output_parent_dir) || !nzchar(output_parent_dir)) {
      return(NULL)
    }

    out_base <- tools::file_path_sans_ext(basename(input_db))
    output_csv_name <- trimws(input$outputCsvName)
    if (!nzchar(output_csv_name)) {
      output_csv_name <- paste0(out_base, ".csv")
    } else if (!grepl("\\.csv$", output_csv_name, ignore.case = TRUE)) {
      output_csv_name <- paste0(output_csv_name, ".csv")
    }

    candidate <- file.path(output_parent_dir, output_csv_name)
    if (file.exists(candidate)) candidate else NULL
  })

  core_exclude_attributes <- reactive({
    read_markdown_attribute_names(
      file.path(docs_dir_path, "core_attributes.md"),
      section_heading = "Attribute List"
    )
  })

  region_exclude_attributes <- reactive({
    region_md_name <- get_region_doc_filename(input$region %||% run_defaults$region)
    if (is.null(region_md_name)) return(character(0))

    read_markdown_attribute_names(
      file.path(docs_dir_path, region_md_name),
      section_heading = "Attribute List"
    )
  })

  output_file_exclude_attributes <- reactive({
    read_output_column_names(resolved_output_file())
  })

  available_exclude_attributes <- reactive({
    documented_core <- core_exclude_attributes()
    documented_region <- region_exclude_attributes()
    output_cols <- sort(output_file_exclude_attributes())
    current_selected <- excluded_attributes()
    extra_selected <- setdiff(current_selected, unique(c(documented_core, documented_region, output_cols)))

    unique(c(documented_core, documented_region, output_cols, extra_selected))
  })

  observe({
    choices <- available_exclude_attributes()
    selected_vals <- excluded_attributes()

    updateSelectizeInput(
      session,
      "excludeAttributes",
      choices = choices,
      selected = selected_vals,
      server = TRUE
    )
  })

  observeEvent(input$selectCoreExcludeAttrs, {
    selected_vals <- unique(c(excluded_attributes(), core_exclude_attributes()))
    exclude_attributes_selected(selected_vals)
    updateSelectizeInput(session, "excludeAttributes", selected = selected_vals, server = TRUE)
  }, ignoreInit = TRUE)

  observeEvent(input$clearExcludeAttrs, {
    exclude_attributes_selected(character(0))
    updateSelectizeInput(session, "excludeAttributes", selected = character(0), server = TRUE)
  }, ignoreInit = TRUE)

  output$resolved_output_path <- renderText({
    path <- resolved_output_file()
    if (is.null(path)) "No output CSV found at the configured location yet." else path
  })

  output$output_preview_status <- renderText({
    if (isTRUE(run_in_progress())) {
      return("Processing is in progress. Output preview will update when the run finishes.")
    }

    manual_output_path <- normalize_r_path(input$outputPreviewPath %||% "")
    if (nzchar(manual_output_path) && !file.exists(manual_output_path)) {
      return(sprintf("The pasted output CSV path was not found: %s", manual_output_path))
    }

    path <- resolved_output_file()
    if (is.null(path)) {
      return("Run processing first, or point to an output directory/name where a CSV already exists.")
    }

    size <- file.info(path)$size
    sprintf("Previewing: %s (%.1f KB)", basename(path), as.numeric(size) / 1024)
  })

  preview_df <- reactive({
    req(!isTRUE(run_in_progress()))

    path <- resolved_output_file()
    req(!is.null(path), file.exists(path))

    df <- tryCatch(
      read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) {
        data.frame(`CSV read error` = e$message, check.names = FALSE)
      }
    )

    if (nrow(df) == 0) {
      df <- data.frame(`Status` = "CSV exists but has no rows.", check.names = FALSE)
    }

    df
  })

  coerce_numeric <- function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  }

  build_axis_tick_values <- function(values, max_ticks = 12L) {
    vals <- suppressWarnings(as.numeric(values))
    vals <- vals[is.finite(vals)]
    uniq <- sort(unique(vals))

    if (length(uniq) <= max_ticks) {
      return(uniq)
    }

    idx <- unique(round(seq(1, length(uniq), length.out = max_ticks)))
    uniq[idx]
  }

  get_numeric_like_cols <- function(df) {
    if (is.null(df) || ncol(df) == 0) return(character(0))
    names(df)[vapply(df, function(col) {
      vals <- coerce_numeric(col)
      any(!is.na(vals))
    }, logical(1))]
  }

  find_col_ignore_case <- function(df, candidates) {
    if (is.null(df) || ncol(df) == 0) return(NULL)
    nms <- colnames(df)
    nms_upper <- toupper(nms)
    for (cand in candidates) {
      idx <- match(toupper(cand), nms_upper)
      if (!is.na(idx)) return(nms[[idx]])
    }
    NULL
  }

  plot_runtitle_col <- reactive({
    df <- preview_df()
    find_col_ignore_case(df, c("RUNTITLE", "RUN_TITLE", "RUN TITLE"))
  })

  available_plot_runtitles <- reactive({
    df <- preview_df()
    run_col <- plot_runtitle_col()
    if (is.null(run_col) || !nzchar(run_col)) return(character(0))

    run_vals <- trimws(as.character(df[[run_col]]))
    run_vals <- run_vals[nzchar(run_vals)]
    unique(run_vals)
  })

  selected_plot_runtitles <- reactive({
    available <- available_plot_runtitles()
    if (length(available) == 0) return(character(0))

    selected <- input$plot_runtitles %||% character(0)
    selected <- trimws(as.character(selected))
    selected <- selected[nzchar(selected)]
    selected <- intersect(selected, available)

    if (length(selected) == 0) {
      return(available)
    }

    selected
  })

  plot_df <- reactive({
    df <- preview_df()
    run_col <- plot_runtitle_col()

    if (is.null(run_col) || !nzchar(run_col)) {
      return(df)
    }

    selected_runs <- selected_plot_runtitles()
    if (length(selected_runs) == 0) {
      return(df)
    }

    run_vals <- trimws(as.character(df[[run_col]]))
    keep <- run_vals %in% selected_runs
    filtered <- df[keep, , drop = FALSE]
    filtered
  })

  output$plot_runtitle_ui <- renderUI({
    run_col <- plot_runtitle_col()

    if (is.null(run_col)) {
      return(helpText("No RunTitle column found in this output file. Plotting will use all rows."))
    }

    run_vals <- available_plot_runtitles()

    if (length(run_vals) == 0) {
      return(helpText("RunTitle column exists but has no non-empty values."))
    }

    selectizeInput(
      "plot_runtitles",
      "RunTitles to Display",
      choices = run_vals,
      selected = run_vals,
      multiple = TRUE,
      options = list(placeholder = "Select one or more RunTitles")
    )
  })

  output$plot_x_ui <- renderUI({
    df <- plot_df()
    choices <- colnames(df)

    if (length(choices) == 0) {
      return(helpText("No columns available. Run processing first to load output data."))
    }

    plot_type_now <- input$plot_type %||% "scatter"
    preferred <- choices[[1]]

    if (identical(plot_type_now, "bar")) {
      numeric_cols <- get_numeric_like_cols(df)
      categorical_cols <- setdiff(choices, numeric_cols)
      bar_choices <- unique(c(if ("YEAR" %in% choices) "YEAR" else character(0), categorical_cols))

      if (length(bar_choices) == 0) {
        return(helpText("No categorical columns or YEAR are available for a bar chart X axis."))
      }

      preferred <- if ("YEAR" %in% bar_choices) "YEAR" else bar_choices[[1]]
      return(selectInput("plot_x", "X Axis", choices = bar_choices, selected = preferred))
    }

    if (identical(plot_type_now, "scatter") || identical(plot_type_now, "bar")) {
      if ("YEAR" %in% choices) {
        preferred <- "YEAR"
      } else if ("CY" %in% choices) {
        preferred <- "CY"
      } else if ("CYCLE" %in% choices) {
        preferred <- "CYCLE"
      }
    }

    selectInput("plot_x", "X Axis", choices = choices, selected = preferred)
  })

  output$plot_y_ui <- renderUI({
    df <- plot_df()
    numeric_cols <- get_numeric_like_cols(df)

    if (length(numeric_cols) < 1) {
      return(helpText("No numeric columns are available for a scatter plot."))
    }

    numeric_upper <- toupper(numeric_cols)
    canopy_candidates <- c("CAN_COV", "CC", "STCC")
    canopy_idx <- match(canopy_candidates, numeric_upper)
    canopy_idx <- canopy_idx[!is.na(canopy_idx)]

    default_y <- if (length(canopy_idx) > 0) {
      numeric_cols[[canopy_idx[[1]]]]
    } else if (length(numeric_cols) >= 2) {
      numeric_cols[[2]]
    } else {
      numeric_cols[[1]]
    }

    selectInput("plot_y", "Y Axis (numeric)", choices = numeric_cols, selected = default_y)
  })

  output$plot_color_ui <- renderUI({
    df <- plot_df()
    cols <- colnames(df)
    numeric_cols <- get_numeric_like_cols(df)
    categorical_cols <- setdiff(cols, numeric_cols)

    if (length(cols) == 0) {
      return(helpText("No columns available for color grouping."))
    }

    preferred <- if ("DOMTYPE" %in% categorical_cols) {
      "DOMTYPE"
    } else if ("COVERTYPE" %in% categorical_cols) {
      "COVERTYPE"
    } else {
      "__none__"
    }

    choices <- c("None" = "__none__")
    if (length(categorical_cols) > 0) {
      choices <- c(choices, setNames(categorical_cols, categorical_cols))
    }

    selectInput("plot_color_by", "Color By", choices = choices, selected = preferred)
  })

  output$plot_bar_value_ui <- renderUI({
    df <- plot_df()
    numeric_cols <- get_numeric_like_cols(df)

    choices <- c("Count Rows" = "__count__")
    if (length(numeric_cols) > 0) {
      choices <- c(choices, setNames(numeric_cols, numeric_cols))
    }

    selectInput("plot_bar_value", "Bar Value", choices = choices, selected = "__count__")
  })

  output$plot_ts_value_ui <- renderUI({
    df <- plot_df()
    numeric_cols <- get_numeric_like_cols(df)

    choices <- c("Count Rows" = "__count__")
    if (length(numeric_cols) > 0) {
      choices <- c(choices, setNames(numeric_cols, numeric_cols))
    }

    numeric_upper <- toupper(numeric_cols)
    canopy_candidates <- c("CAN_COV", "CC", "STCC")
    canopy_idx <- match(canopy_candidates, numeric_upper)
    canopy_idx <- canopy_idx[!is.na(canopy_idx)]

    default_choice <- if (length(canopy_idx) > 0) {
      numeric_cols[[canopy_idx[[1]]]]
    } else if (length(numeric_cols) > 0) {
      numeric_cols[[1]]
    } else {
      "__count__"
    }

    selectInput("plot_ts_value", "Time-Series Value", choices = choices, selected = default_choice)
  })

  output$plot_comp_x_ui <- renderUI({
    df <- plot_df()
    cols <- colnames(df)
    if (length(cols) == 0) {
      return(helpText("No columns available for composition plotting."))
    }

    preferred <- if ("YEAR" %in% cols) "YEAR" else cols[[1]]
    selectInput("plot_comp_x", "Composition X Axis", choices = cols, selected = preferred)
  })

  output$plot_comp_group_ui <- renderUI({
    df <- plot_df()
    cols <- colnames(df)
    if (length(cols) == 0) {
      return(helpText("No columns available for composition groups."))
    }

    preferred <- if ("DOMTYPE" %in% cols) "DOMTYPE" else if ("COVERTYPE" %in% cols) "COVERTYPE" else cols[[1]]
    selectInput("plot_comp_group", "Composition Category", choices = cols, selected = preferred)
  })

  output$plot_comp_split_run_ui <- renderUI({
    run_col <- plot_runtitle_col()
    selected_runs <- selected_plot_runtitles()

    if (is.null(run_col) || !nzchar(run_col)) {
      return(helpText("RunTitle column not available for splitting composition bars by run."))
    }

    if (length(selected_runs) <= 1) {
      return(helpText("Select 2+ RunTitles to split composition bars by RunTitle."))
    }

    checkboxInput("plot_comp_split_run", "Separate bars by RunTitle", FALSE)
  })

  get_attribute_explanation <- function(region_value, attr_name, attr_value = NULL) {
    region_norm <- toupper(trimws(as.character(region_value %||% "")))
    attr <- toupper(trimws(as.character(attr_name %||% "")))
    attr_value_text <- trimws(as.character(attr_value %||% ""))
    units_label <- get_attribute_units(attr, region_norm)
    no_units_attrs <- c("RUNTITLE", "CASEID", "STAND_CN", "STANDID", "VARIANT", "REGION", "YEAR", "CY")

    expand_explanation_terms <- local({
      seen_terms <- character(0)

      replace_on_first_mention <- function(text, key, long_name) {
        key_pattern <- gsub("([.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1", key, perl = TRUE)
        pattern <- paste0("(?<![A-Za-z0-9_])", key_pattern, "(?![A-Za-z0-9_])")

        if (!grepl(pattern, text, perl = TRUE)) {
          return(text)
        }

        if (key %in% seen_terms) {
          return(text)
        }

        seen_terms <<- c(seen_terms, key)
        sub(pattern, paste0(long_name, " (", key, ")"), text, perl = TRUE)
      }

      function(text) {
        fixed_replacements <- c(
          "CAN_COV" = "stand canopy cover",
          "QMD_TOP20" = "top-twenty-percent quadratic mean diameter",
          "BA_WT_DIA" = "basal-area-weighted average diameter at breast height",
          "BA_WT_HT" = "basal-area-weighted average height",
          "SSTPA" = "advanced-regeneration trees per acre",
          "SSSIZE" = "advanced-regeneration mean height",
          "NMBA" = "non-merchantable basal area",
          "NMSIZE" = "non-merchantable basal-area-weighted diameter",
          "PWBA" = "pulpwood basal area",
          "PWSIZE" = "pulpwood basal-area-weighted diameter",
          "STBA" = "sawtimber basal area",
          "STSIZE" = "sawtimber basal-area-weighted diameter",
          "SSDOMSPP" = "advanced-regeneration dominant species grouping",
          "NMDOMSPP" = "non-merchantable dominant species grouping",
          "PWDOMSPP" = "pulpwood dominant species grouping",
          "STDOMSPP" = "sawtimber dominant species grouping",
          "DOMTYPE" = "whole-stand dominant species grouping",
          "VEGCLASS" = "size-density class",
          "BAWTD" = "basal-area-weighted diameter",
          "TREEBA" = "tree basal area",
          "TEXPF" = "tree expansion factor",
          "DOM_TYPE_R2" = "Region 2 dominant type",
          "DOM6040" = "Region 1 dominance subclass",
          "SIZECLASS_NTG" = "Region 1 size class",
          "STRCLSSTR" = "Region 1 structure class",
          "VERTICAL_STRUCTURE" = "vertical structure",
          "TREE_SIZE_CLASS_R2" = "Region 2 tree size class",
          "CROWN_CLASS_R2" = "Region 2 crown class",
          "HSS1_4C" = "habitat structural stage one through four-C",
          "HSS1_5" = "habitat structural stage one through five",
          "DCC1" = "primary dominance component",
          "XDCC1" = "primary dominance canopy cover share",
          "DCC2" = "secondary dominance component",
          "XDCC2" = "secondary dominance canopy cover share",
          "CAN_SIZCL" = "midscale canopy size class",
          "CAN_SZTMB" = "timberland canopy size class",
          "CAN_SZWDL" = "woodland canopy size class",
          "BA_STORY" = "basal-area storiedness",
          "COVERTYPE_R1" = "Region 1 cover type",
          "COVERTYPE_R2" = "Region 2 cover type"
        )

        token_replacements <- c(
          "TPA" = "trees per acre",
          "BA" = "basal area",
          "DBH" = "diameter at breast height",
          "QMD" = "quadratic mean diameter",
          "RSDI" = "Reineke stand density index",
          "ZSDI" = "Zeide stand density index",
          "STCC" = "total stand canopy cover percentage",
          "vol1" = "volume field 1",
          "vol2" = "volume field 2",
          "vol3" = "volume field 3"
        )

        out <- as.character(text %||% "")
        for (k in names(fixed_replacements)) {
          out <- replace_on_first_mention(out, k, fixed_replacements[[k]])
        }
        for (k in names(token_replacements)) {
          out <- replace_on_first_mention(out, k, token_replacements[[k]])
        }
        out
      }
    })

    with_units <- function(text) {
      text <- expand_explanation_terms(text)
      if (attr %in% no_units_attrs) {
        return(text)
      }
      paste0("Units: ", units_label, ".\n\n", text)
    }

    if (region_norm %in% c("1", "1.0")) {
      region_norm <- "1"
    } else if (region_norm %in% c("2", "2.0")) {
      region_norm <- "2"
    } else if (region_norm %in% c("3", "3.0")) {
      region_norm <- "3"
    } else if (region_norm %in% c("8", "8.0")) {
      region_norm <- "8"
    } else if (region_norm %in% c("MPSG", "MPS")) {
      region_norm <- "MPSG"
    }

    common_value_text <- switch(
      attr,
      "CAN_COV" = paste0(
        "Stand canopy cover percentage. This measures the percentage of ground area covered by tree crowns after correcting for crown overlap. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the stand is estimated to have that percent canopy cover.") else ""
      ),
      "BA" = paste0(
        "Stand basal area. This is the total cross-sectional tree area per acre across all trees. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the stand has that many square feet of basal area per acre when all trees are included.") else ""
      ),
      "TPA" = paste0(
        "Stand trees per acre across all trees. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the stand is carrying that many trees per acre when all trees are included.") else ""
      ),
      "QMD" = paste0(
        "Stand quadratic mean diameter for all trees. This is the diameter of a tree with the average basal area, so larger trees influence it more strongly than a simple arithmetic average. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the stand-level quadratic mean diameter for all trees is that many inches.") else ""
      ),
      "ZSDI" = paste0(
        "Zeide stand density index for all trees. This index combines trees per acre and quadratic mean diameter to represent relative crowding/competition in the stand. Higher values generally indicate denser, more competitive conditions; lower values indicate more open conditions. ",
        "It is best used for comparing stands or changes through time under the same variant and methodology. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " is the Zeide SDI estimate for all trees in this row.") else ""
      ),
      "RSDI" = paste0(
        "Reineke stand density index for all trees. This index standardizes stand density using trees per acre and quadratic mean diameter so stands of different average tree size can be compared on a common density scale. ",
        "Higher values generally indicate greater stand occupancy and competition pressure. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " is the Reineke SDI estimate for all trees in this row.") else ""
      ),
      "BA_STM" = paste0(
        "Stand basal area for stems only, where diameter at breast height is at least one inch. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the stand has that many square feet of basal area per acre for stem-sized trees only.") else ""
      ),
      "TPA_STM" = paste0(
        "Stand trees per acre for stems only, where diameter at breast height is at least one inch. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the stand is carrying that many stem-sized trees per acre.") else ""
      ),
      "QMD_STM" = paste0(
        "Stand quadratic mean diameter for stems only, where diameter at breast height is at least one inch. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the quadratic mean diameter for stem-sized trees is that many inches.") else ""
      ),
      "ZSDI_STM" = paste0(
        "Zeide stand density index for stems only, where diameter at breast height is at least one inch. This is the same Zeide crowding concept, but restricted to stem-sized trees. ",
        "Higher values generally indicate denser stem-level competition. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " is the Zeide SDI estimate for stem-sized trees in this row.") else ""
      ),
      "RSDI_STM" = paste0(
        "Reineke stand density index for stems only, where diameter at breast height is at least one inch. This is the Reineke density standardization applied only to stem-sized trees. ",
        "Higher values generally indicate greater stem-level occupancy and competition. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " is the Reineke SDI estimate for stem-sized trees in this row.") else ""
      ),
      "QMD_TOP20" = paste0(
        "Quadratic mean diameter for the top twenty percent of stand trees per acre contribution, with a minimum target of twenty trees per acre. This emphasizes the larger portion of the stand. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the larger-diameter top portion of the stand has that quadratic mean diameter in inches.") else ""
      ),
      "BA_WT_DIA" = paste0(
        "Basal-area-weighted average diameter at breast height. Trees with more basal area contribute more weight to this average. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the stand's basal-area-weighted average diameter is that many inches.") else ""
      ),
      "BA_WT_HT" = paste0(
        "Basal-area-weighted average height. Trees with more basal area contribute more weight to this average height. ",
        if (nzchar(attr_value_text)) paste0("The clicked value ", attr_value_text, " means the stand's basal-area-weighted average height is that amount.") else ""
      ),
      NULL
    )

    r1_map <- list(
      VEGTYPE = "Region 1 vegetation type is built by combining the mapped habitat group with an abbreviated Region 1 cover type. If the habitat code is missing, this can remain empty.",
      COVERTYPE_R1 = "Region 1 cover type starts from dominance subclass mapping, then can be switched from mixed mesic conifer to dry Douglas-fir when habitat code falls in dry habitat sets.",
      DOM6040 = "Region 1 dominance subclass uses a sixty-forty style rule. The function first decides whether to use trees-per-acre proportions or basal-area proportions, then applies species tie-breaks using diameter and height.",
      VERTICAL_STRUCTURE = "Region 1 vertical structure first checks low-stocked stands, then assigns one layer for low basal area with high trees-per-acre, otherwise computes canopy layering from basal-area distribution by diameter classes.",
      SIZECLASS_NTG = "Region 1 size class uses stand basal-area-weighted diameter and assigns one of fixed diameter ranges from seedling up to twenty-five inches and greater.",
      STRCLSSTR = "Region 1 structure class combines stand canopy cover and stand basal-area-weighted diameter. It first tests open-canopy conditions, then assigns a letter class from a diameter-by-cover decision matrix."
    )

    r2_map <- list(
      DOM_TYPE_R2 = "Region 2 dominant type is based on species canopy cover ranking. If stand canopy cover is under ten percent it is set to NONE; otherwise it concatenates the first one, two, or three most dominant species.",
      DOM_TYPE_R2_CC1 = "Canopy cover percentage of the most dominant species in Region 2. Derived from the highest species canopy contribution after sorting.",
      DOM_TYPE_R2_CC2 = "Canopy cover percentage of the second most dominant species in Region 2. If fewer than two species are present, this becomes zero (or NONE in very open stands).",
      DOM_TYPE_R2_CC3 = "Canopy cover percentage of the third most dominant species in Region 2. If fewer than three species are present, this becomes zero (or NONE in very open stands).",
      COVERTYPE_R2 = paste(c(
        "Region 2 cover type maps dominant species to standard type groups, then applies group override checks where combined spruce-fir, pinyon-juniper, or Douglas-fir groups can overrule single-species mapping.",
        "Possible assigned COVERTYPE_R2 codes and meanings:",
        "- NONE: no cover type assigned (very open canopy, stand canopy cover below 10%).",
        "- TAA: aspen.",
        "- TSF: spruce-fir.",
        "- TDF: Douglas-fir.",
        "- TCW: cottonwood.",
        "- TPJ: pinyon-juniper.",
        "- TWF: white fir.",
        "- TBS: blue spruce.",
        "- TLP: lodgepole pine.",
        "- TLI: limber pine.",
        "- TPP: ponderosa pine.",
        "- TBC: bristlecone pine.",
        "- TBO: bur oak.",
        "- TAE: ash.",
        "- TER: eastern red-cedar.",
        "- TJP: jack pine.",
        "- TJF: Jeffrey pine.",
        "- TPB: paper birch.",
        "- TSC: salt cedar.",
        "- TSP: Scotch pine.",
        "- TWP: southwestern white pine.",
        "- TWB: whitebark pine.",
        "- TWS: white spruce.",
        "- TGO: Gambel oak.",
        "- OTH: other or unknown fallback group."
      ), collapse = "\n"),
      TREE_SIZE_CLASS_R2 = "Region 2 tree size class bins canopy cover into diameter classes, compares grouped cover totals, and uses tie-break rules to assign final class.",
      CROWN_CLASS_R2 = "Region 2 crown class is assigned from stand canopy cover thresholds: ten to less than forty, forty to less than seventy, and seventy or greater.",
      HSS1_4C = "Habitat structural stage (one through four-C scheme). Computed by the habitat-stage helper using stand structure context.",
      HSS1_5 = "Habitat structural stage (one through five scheme). Computed by the habitat-stage helper using stand structure context."
    )

    r3_map <- list(
      DOM_TYPE = "Region 3 dominant type follows the LEAD decision sequence: sparse-stand test, then species dominance checks, genus dominance checks, and evergreen-deciduous plus shade-tolerance fallback rules.",
      DCC1 = "Primary dominance component from Region 3 dominant-type logic. This is the first species, genus, or category selected by the LEAD rule sequence.",
      XDCC1 = "Canopy cover share associated with the primary dominance component (DCC1), corrected by the canopy-cover correction routine.",
      DCC2 = "Secondary dominance component from Region 3 dominant-type logic when a two-part dominance label is assigned.",
      XDCC2 = "Canopy cover share associated with the secondary dominance component (DCC2), corrected by the canopy-cover correction routine.",
      CAN_SIZCL = "Region 3 midscale canopy size class. Uses canopy by diameter class, then assigns the class with adjustments for sparse canopies.",
      CAN_SZTMB = "Region 3 timberland canopy size class. Starts from canopy class maxima then applies timberland-specific adjustment rules.",
      CAN_SZWDL = "Region 3 woodland canopy size class. Starts from canopy class maxima then applies woodland-specific adjustment rules.",
      BA_STORY = "Region 3 basal-area storiedness. Tests sparse conditions first, then checks large-tree dominance and sliding diameter windows to classify one-, two-, or three-story structure."
    )

    r8_map <- list(
      SSDOMSPP = "Region 8 advance-regeneration dominant species grouping, based only on the advanced-regeneration group (DBH < 1.5) and assigned with the seventy-percent dominance rules using one species, the top two species, or the top three species.",
        SSSIZE = "Region 8 advanced-regeneration mean height, based only on the advanced-regeneration group (DBH < 1.5) and used to summarize the typical height of that small-tree group at the stand level.",
      SSTPA = "Region 8 trees per acre for advance regeneration, calculated as the total TEXPF (tree expansion factor) from the advanced-regeneration group (DBH < 1.5).",
      NMDOMSPP = "Region 8 non-merchantable dominant species grouping, based only on the non-merchantable group (DBH >= 1.5 and vol1 <= 0) and assigned with the seventy-percent dominance rules.",
      NMSIZE = "Region 8 size metric for non-merchantable trees, computed only from the non-merchantable group (DBH >= 1.5 and vol1 <= 0) as BAWTD (basal-area-weighted diameter) divided by NMBA (non-merchantable basal area).",
      NMBA = "Region 8 basal area for non-merchantable trees, based only on the non-merchantable group (DBH >= 1.5 and vol1 <= 0).",
      PWDOMSPP = "Region 8 pulpwood dominant species grouping, based only on the pulpwood group (vol1 > 0 and vol2 <= 0) and assigned with the seventy-percent dominance rules.",
      PWSIZE = "Region 8 size metric for pulpwood trees, computed only from the pulpwood group (vol1 > 0 and vol2 <= 0) as BAWTD divided by PWBA.",
      PWBA = "Region 8 basal area for pulpwood trees, based only on the pulpwood group (vol1 > 0 and vol2 <= 0).",
      STDOMSPP = "Region 8 sawtimber dominant species grouping, based only on the sawtimber group (vol3 > 0) and assigned with the seventy-percent dominance rules.",
      STSIZE = "Region 8 size metric for sawtimber trees, computed only from the sawtimber group (vol3 > 0) as BAWTD divided by STBA.",
      STBA = "Region 8 basal area for sawtimber trees, based only on the sawtimber group (vol3 > 0).",
      DOMTYPE = "Region 8 whole-stand dominant species grouping, assigned with the seventy-percent dominance rules using whole-stand basal area across species rather than a single Region 8 group.",
      VEGCLASS = "Region 8 size-density class, using the advance-regeneration size-class path when BA < 10 and otherwise selecting the class from the largest basal-area group among NMBA, PWBA, and STBA when BA >= 10."
    )

    mpsg_map <- list(
      COVERTYPE = "Mountain Planning Services Group cover type. If canopy is very open it becomes NONE; otherwise it uses selected cover-type logic path based on chosen source ruleset.",
      TREE_SIZE_CLASS = "Mountain Planning Services Group tree size class from stand basal-area-weighted diameter thresholds.",
      CROWN_CLASS = "Mountain Planning Services Group canopy class from stand canopy-cover thresholds.",
      VERTICAL_STRUCTURE = "Mountain Planning Services Group vertical structure from the canopy layering routine, with normalization for output classes.",
      HSS1_4C = "Optional habitat structural stage output (one through four-C scheme) when habitat-stage option is enabled.",
      HSS1_5 = "Optional habitat structural stage output (one through five scheme) when habitat-stage option is enabled."
    )

    region_map <- switch(
      region_norm,
      "1" = r1_map,
      "2" = r2_map,
      "3" = r3_map,
      "8" = r8_map,
      "MPSG" = mpsg_map,
      list()
    )

    if (!is.null(common_value_text)) {
      return(with_units(common_value_text))
    }
    if (!is.null(region_map[[attr]])) {
      return(with_units(region_map[[attr]]))
    }

    with_units("No static logic note is currently defined for this attribute and region.")
  }

  get_attribute_flowchart_mermaid <- function(region_value, attr_name, attr_value = NULL, row_data = NULL) {
    region_norm <- toupper(trimws(as.character(region_value %||% "")))
    attr <- toupper(trimws(as.character(attr_name %||% "")))
    selected_value <- trimws(as.character(attr_value %||% ""))
    value_norm <- toupper(trimws(as.character(attr_value %||% "")))

    if (region_norm %in% c("1", "1.0")) region_norm <- "1"
    if (region_norm %in% c("2", "2.0")) region_norm <- "2"
    if (region_norm %in% c("3", "3.0")) region_norm <- "3"
    if (region_norm %in% c("8", "8.0")) region_norm <- "8"
    if (region_norm %in% c("MPS", "MPSG")) region_norm <- "MPSG"

    row_values <- if (is.null(row_data)) {
      list()
    } else if (is.data.frame(row_data) && nrow(row_data) > 0) {
      as.list(row_data[1, , drop = TRUE])
    } else if (is.list(row_data)) {
      row_data
    } else {
      list()
    }

    get_row_text <- function(names_vec, default = "") {
      nms <- names(row_values)
      if (length(nms) == 0) return(default)
      upper_nms <- toupper(nms)
      for (nm in names_vec) {
        idx <- match(toupper(nm), upper_nms)
        if (!is.na(idx)) {
          val <- row_values[[idx]]
          if (length(val) == 0 || is.null(val)) next
          out <- trimws(as.character(val[[1]]))
          if (nzchar(out) && toupper(out) != "NA") return(out)
        }
      }
      default
    }

    get_row_num <- function(names_vec) {
      txt <- get_row_text(names_vec, default = "")
      if (!nzchar(txt)) return(NA_real_)
      suppressWarnings(as.numeric(txt))
    }

    fmt_num <- function(x, digits = 2) {
      if (is.na(x)) return("unknown")
      format(round(x, digits), nsmall = digits, trim = TRUE)
    }

    # Shared row context used by multiple region-specific flowchart branches.
    stcc <- get_row_num(c("CAN_COV", "CC", "STCC"))
    tpa <- get_row_num(c("TPA"))
    ba <- get_row_num(c("BA"))

    wrap_mermaid_text <- function(x, width = 70L) {
      if (is.null(x) || !nzchar(as.character(x))) return("")
      wrapped <- strwrap(as.character(x), width = width, exdent = 0, simplify = TRUE)
      if (length(wrapped) == 0) return(as.character(x))
      paste(wrapped, collapse = "<br/>")
    }

    safe_mermaid_text <- function(x) {
      out <- as.character(x %||% "")
      if (length(out) == 0) out <- ""
      out <- out[[1]]
      if (is.na(out)) out <- ""
      # Keep labels parser-safe for Mermaid node text.
      out <- gsub("\\[|\\]", "", out, perl = TRUE)
      out <- gsub("-->|->", " to ", out, perl = TRUE)
      out <- gsub(";", ",", out, fixed = TRUE)
      out <- gsub("\\{|\\}", "", out, perl = TRUE)
      out <- gsub("\\|", " ", out, perl = TRUE)
      out <- gsub('"', "'", out, fixed = TRUE)
      # Final strict whitelist for Mermaid labels, preserving equation and percent symbols.
      out <- gsub("[^A-Za-z0-9 ,.:+*/^=()_%<>?-]", " ", out, perl = TRUE)
      out <- gsub("\\s+", " ", out, perl = TRUE)
      out <- trimws(out)
      wrap_mermaid_text(out, width = 70L)
    }

    safe_mermaid_equation_text <- function(x) {
      out <- as.character(x %||% "")
      if (length(out) == 0) out <- ""
      out <- out[[1]]
      if (is.na(out)) out <- ""
      # Keep arithmetic symbols for equation labels while removing parser-sensitive characters.
      out <- gsub("\\[|\\]", "", out, perl = TRUE)
      out <- gsub("\\{|\\}", "", out, perl = TRUE)
      out <- gsub("-->|->", " to ", out, perl = TRUE)
      out <- gsub("\\|", " ", out, perl = TRUE)
      out <- gsub('"', "'", out, fixed = TRUE)
      out <- gsub("[^A-Za-z0-9 ,.:+*/^=()_%<>?-]", " ", out, perl = TRUE)
      out <- gsub("\\s+", " ", out, perl = TRUE)
      out <- trimws(out)
      wrap_mermaid_text(out, width = 70L)
    }

    expand_terms <- local({
      seen_terms <- character(0)

      replace_on_first_mention <- function(text, key, long_name) {
        key_pattern <- gsub("([.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1", key, perl = TRUE)
        pattern <- paste0("(?<![A-Za-z0-9_])", key_pattern, "(?![A-Za-z0-9_])")

        if (!grepl(pattern, text, perl = TRUE)) {
          return(text)
        }

        if (key %in% seen_terms) {
          return(text)
        }

        seen_terms <<- c(seen_terms, key)
        sub(pattern, paste0(long_name, " (", key, ")"), text, perl = TRUE)
      }

      function(line) {
        # Expand each abbreviation only once per rendered flowchart.
        fixed_replacements <- c(
          "CAN_COV" = "corrected stand canopy cover",
          "TREECC" = "tree canopy cover contribution",
          "UNCC" = "uncorrected stand canopy cover",
          "TREEBA" = "tree basal area",
          "DBHSQ" = "sum of diameter squared times trees per acre",
          "TPASUM" = "accumulated trees per acre",
          "TPA20" = "top-twenty-percent trees per acre target",
          "SSTPA" = "advanced-regeneration trees per acre",
          "SSSIZE" = "advanced-regeneration mean height",
          "NMBA" = "non-merchantable basal area",
          "NMSIZE" = "non-merchantable basal-area-weighted diameter",
          "PWBA" = "pulpwood basal area",
          "PWSIZE" = "pulpwood basal-area-weighted diameter",
          "STBA" = "sawtimber basal area",
          "STSIZE" = "sawtimber basal-area-weighted diameter",
          "BA_STM" = "stem-only basal area",
          "TPA_STM" = "stem-only trees per acre",
          "QMD_STM" = "stem-only quadratic mean diameter",
          "ZSDI_STM" = "stem-only Zeide stand density index",
          "RSDI_STM" = "stem-only Reineke stand density index",
          "STCC" = "total stand canopy cover percentage",
          "TEXPF" = "tree expansion factor",
          "TREEBA" = "tree basal area",
          "BAWTD" = "basal-area-weighted diameter",
          "BAWTH" = "basal-area-weighted height",
          "vol1" = "volume field 1",
          "vol2" = "volume field 2",
          "vol3" = "volume field 3",
          "SP1" = "first ranked species",
          "SP2" = "second ranked species",
          "SP3" = "third ranked species",
          "DOM_TYPE_R2" = "Region 2 dominant species label",
          "DOM_TYPE" = "dominant type",
          "COVERTYPE_R2" = "Region 2 cover type",
          "COVERTYPE_R1" = "Region 1 cover type",
          "TREE_SIZE_CLASS_R2" = "Region 2 tree size class",
          "CROWN_CLASS_R2" = "Region 2 crown class",
          "HSS1_4C" = "habitat structural stage one through four-C",
          "HSS1_5" = "habitat structural stage one through five",
          "QMD_TOP20" = "top-twenty-percent quadratic mean diameter",
          "BA_WT_DIA" = "basal-area-weighted diameter",
          "BA_WT_HT" = "basal-area-weighted height"
        )

        token_replacements <- c(
          "QMD" = "quadratic mean diameter",
          "TPA" = "trees per acre",
          "BA" = "stand basal area",
          "DBH" = "diameter at breast height",
          "CC" = "canopy cover"
        )

        out <- line
        for (k in names(fixed_replacements)) {
          out <- replace_on_first_mention(out, k, fixed_replacements[[k]])
        }
        for (k in names(token_replacements)) {
          out <- replace_on_first_mention(out, k, token_replacements[[k]])
        }

        out
      }
    })

    expand_conditions <- function(line) {
      out <- line
      # Preserve symbolic comparisons and normalize common shorthand.
      out <- gsub("(?i)\\bGE\\s*([0-9.]+)\\s*percent\\b", ">= \\1%", out, perl = TRUE)
      out <- gsub("(?i)\\bGT\\s*([0-9.]+)\\s*percent\\b", "> \\1%", out, perl = TRUE)
      out <- gsub("(?i)\\bLE\\s*([0-9.]+)\\s*percent\\b", "<= \\1%", out, perl = TRUE)
      out <- gsub("(?i)\\bLT\\s*([0-9.]+)\\s*percent\\b", "< \\1%", out, perl = TRUE)
      out <- gsub("(?i)\\bGE\\s*([0-9.]+)", ">= \\1", out, perl = TRUE)
      out <- gsub("(?i)\\bGT\\s*([0-9.]+)", "> \\1", out, perl = TRUE)
      out <- gsub("(?i)\\bLE\\s*([0-9.]+)", "<= \\1", out, perl = TRUE)
      out <- gsub("(?i)\\bLT\\s*([0-9.]+)", "< \\1", out, perl = TRUE)
      out <- gsub("(?i)\\b([0-9.]+)\\s*percent\\b", "\\1%", out, perl = TRUE)
      out <- gsub("(?i)\\bpercent\\b", "%", out, perl = TRUE)
      out <- gsub(">=\\s*([0-9.]+)", ">= \\1", out, perl = TRUE)
      out <- gsub("<=\\s*([0-9.]+)", "<= \\1", out, perl = TRUE)
      out <- gsub(">\\s*([0-9.]+)", "> \\1", out, perl = TRUE)
      out <- gsub("<\\s*([0-9.]+)", "< \\1", out, perl = TRUE)
      out <- gsub("==\\s*([0-9.]+)", "= \\1", out, perl = TRUE)
      out <- gsub("=\\s*([0-9.]+)", "= \\1", out, perl = TRUE)
      out
    }

    strip_source_refs <- function(line) {
      out <- line
      # Remove inline source-file references such as R/file.r:123 or R/file.r:123-130.
      out <- gsub("\\s+R/[A-Za-z0-9_./-]+:[0-9]+(-[0-9]+)?", "", out, perl = TRUE)
      out
    }

    prune_lines_to_selected_value <- function(lines, selected_value) {
      target <- toupper(trimws(as.character(selected_value %||% "")))
      if (!nzchar(target)) return(lines)

      edge_src <- function(edge) {
        if (is.null(edge)) return(NA_character_)
        src <- edge["src"]
        if (length(src) == 0 || is.na(src) || !nzchar(src)) return(NA_character_)
        as.character(src)
      }

      edge_dst <- function(edge) {
        if (is.null(edge)) return(NA_character_)
        dst <- edge["dst"]
        if (length(dst) == 0 || is.na(dst) || !nzchar(dst)) return(NA_character_)
        as.character(dst)
      }

      parse_edge <- function(line) {
        m <- regexec(
          "^\\s*([A-Za-z][A-Za-z0-9_]*)[^-]*--[^>]*-->\\s*([A-Za-z][A-Za-z0-9_]*)",
          line,
          perl = TRUE
        )
        hit <- regmatches(line, m)[[1]]
        if (length(hit) == 3) {
          c(src = hit[2], dst = hit[3])
        } else {
          NULL
        }
      }

      parse_destination_label <- function(line) {
        m <- regexec("-->\\s*([A-Za-z][A-Za-z0-9_]*)\\[([^\\]]+)\\]", line, perl = TRUE)
        hit <- regmatches(line, m)[[1]]
        if (length(hit) == 3) {
          c(id = hit[2], label = toupper(hit[3]))
        } else {
          NULL
        }
      }

      edges <- lapply(lines, parse_edge)
      edge_idx <- which(vapply(edges, function(x) !is.null(x), logical(1)))
      if (length(edge_idx) == 0) return(lines)

      sources <- vapply(edges[edge_idx], edge_src, character(1))
      targets <- vapply(edges[edge_idx], edge_dst, character(1))
      valid_idx <- !is.na(sources) & !is.na(targets)
      edge_idx <- edge_idx[valid_idx]
      sources <- sources[valid_idx]
      targets <- targets[valid_idx]
      if (length(edge_idx) == 0) return(lines)
      nodes <- unique(c(sources, targets))
      if (length(nodes) == 0) return(lines)

      start_node <- sources[1]
      if (!start_node %in% nodes) return(lines)

      node_labels <- setNames(rep("", length(nodes)), nodes)
      for (i in edge_idx) {
        parsed <- parse_destination_label(lines[[i]])
        if (is.null(parsed)) next
        node_labels[[parsed[["id"]]]] <- parsed[["label"]]
      }

      label_hits <- vapply(node_labels, function(lbl) {
        if (!nzchar(lbl)) return(FALSE)
        # Prefer terminal assignment-style matches to avoid catching threshold literals.
        pattern <- paste0("(=|TO|REMAINS|EQUALS|OUTPUT|MAPS TO)\\s*", gsub("([.^$|()\\[\\]{}*+?\\\\-])", "\\\\\\1", target, perl = TRUE), "(\\b|$)")
        grepl(pattern, lbl, perl = TRUE)
      }, logical(1))

      candidate_nodes <- names(node_labels)[label_hits]

      # Prefer terminal nodes that explicitly assign the selected output value.
      final_output_nodes <- names(node_labels)[vapply(node_labels, function(lbl) {
        if (!nzchar(lbl)) return(FALSE)
        grepl("SET OUTPUT VALUE", lbl, fixed = TRUE) && grepl(target, lbl, fixed = TRUE)
      }, logical(1))]
      if (length(final_output_nodes) > 0) {
        candidate_nodes <- final_output_nodes
      }

      if (length(candidate_nodes) == 0) {
        # Fallback to broader text match if no assignment-style match is found.
        candidate_nodes <- names(node_labels)[vapply(node_labels, function(lbl) {
          nzchar(lbl) && grepl(target, lbl, fixed = TRUE)
        }, logical(1))]
      }
      if (length(candidate_nodes) == 0) return(lines)

      prev_node <- setNames(rep(NA_character_, length(nodes)), nodes)
      prev_edge <- setNames(rep(NA_integer_, length(nodes)), nodes)
      visited <- setNames(rep(FALSE, length(nodes)), nodes)
      queue <- c(start_node)
      visited[[start_node]] <- TRUE

      while (length(queue) > 0) {
        current <- queue[[1]]
        queue <- queue[-1]

        outgoing <- edge_idx[vapply(edge_idx, function(i) {
          src_i <- edge_src(edges[[i]])
          !is.na(src_i) && identical(src_i, current)
        }, logical(1))]

        for (i in outgoing) {
          nxt <- edge_dst(edges[[i]])
          if (is.na(nxt) || !(nxt %in% names(visited))) next
          if (!isTRUE(visited[[nxt]])) {
            visited[[nxt]] <- TRUE
            prev_node[[nxt]] <- current
            prev_edge[[nxt]] <- i
            queue <- c(queue, nxt)
          }
        }
      }

      reachable <- candidate_nodes[vapply(candidate_nodes, function(n) isTRUE(visited[[n]]), logical(1))]
      if (length(reachable) == 0) return(lines)

      path_length <- vapply(reachable, function(n) {
        len <- 0L
        current <- n
        while ((current %in% names(prev_node)) && !is.na(prev_node[[current]]) && nzchar(prev_node[[current]])) {
          len <- len + 1L
          current <- prev_node[[current]]
        }
        len
      }, integer(1))

      chosen <- reachable[[which.min(path_length)]]
      keep_edges <- integer(0)
      current <- chosen
      while ((current %in% names(prev_edge)) && !is.na(prev_edge[[current]])) {
        keep_edges <- c(prev_edge[[current]], keep_edges)
        current <- prev_node[[current]]
      }

      if (length(keep_edges) == 0) return(lines)
      lines[unique(keep_edges)]
    }

    mk <- function(lines) {
      sanitize_square_labels <- function(text) {
        gm <- gregexpr("\\[[^\\]]*\\]", text, perl = TRUE)
        hits <- regmatches(text, gm)[[1]]
        if (length(hits) == 0 || (length(hits) == 1 && identical(hits[[1]], ""))) {
          return(text)
        }
        repl <- vapply(hits, function(seg) {
          txt <- sub("^\\[", "", seg, perl = TRUE)
          txt <- sub("\\]$", "", txt, perl = TRUE)
          paste0("[\"", safe_mermaid_equation_text(txt), "\"]")
        }, character(1))
        regmatches(text, gm) <- list(repl)
        text
      }

      sanitize_brace_labels <- function(text) {
        gm <- gregexpr("\\{[^\\}]*\\}", text, perl = TRUE)
        hits <- regmatches(text, gm)[[1]]
        if (length(hits) == 0 || (length(hits) == 1 && identical(hits[[1]], ""))) {
          return(text)
        }
        repl <- vapply(hits, function(seg) {
          txt <- sub("^\\{", "", seg, perl = TRUE)
          txt <- sub("\\}$", "", txt, perl = TRUE)
          paste0("{\"", safe_mermaid_equation_text(txt), "\"}")
        }, character(1))
        regmatches(text, gm) <- list(repl)
        text
      }

      lines_to_render <- prune_lines_to_selected_value(lines, value_norm)
      readable_lines <- vapply(
        lines_to_render,
        function(x) {
          out <- strip_source_refs(expand_conditions(expand_terms(x)))

          # Sanitize labels while preserving arithmetic notation.
          out <- sanitize_square_labels(out)
          out <- sanitize_brace_labels(out)

          out
        },
        character(1)
      )
      paste(c("flowchart TD", paste0("  ", readable_lines)), collapse = "\n")
    }

    get_r2_covertype_value_flow <- function(code) {
      target <- toupper(trimws(as.character(code %||% "")))
      if (!nzchar(target)) return(NULL)

      family_by_output <- c(
        "TAA" = "T217", "TSF" = "T206", "TDF" = "T210", "TCW" = "T235", "TPJ" = "T239",
        "TWF" = "T211", "TBS" = "T216", "TLP" = "T218", "TLI" = "T219", "TPP" = "T237",
        "TBC" = "T209", "TBO" = "T236", "TAE" = "TASH", "TER" = "T045", "TJP" = "T001",
        "TJF" = "T247", "TPB" = "T252", "TSC" = "TSCD", "TSP" = "TSCP", "TWP" = "TSWP",
        "TWB" = "T208", "TWS" = "T201", "TGO" = "S413", "OTH" = "T999"
      )

      dom_label <- get_row_text(c("DOM_TYPE_R2"), "")
      dom_parts <- trimws(strsplit(dom_label, ":", fixed = TRUE)[[1]])
      if (length(dom_parts) < 3) dom_parts <- c(dom_parts, rep("", 3 - length(dom_parts)))
      sp1 <- if (length(dom_parts) >= 1) dom_parts[[1]] else ""
      sp2 <- if (length(dom_parts) >= 2) dom_parts[[2]] else ""
      sp3 <- if (length(dom_parts) >= 3) dom_parts[[3]] else ""

      stcc <- get_row_num(c("CAN_COV", "CC", "STCC"))
      cc1 <- get_row_num(c("DOM_TYPE_R2_CC1"))
      cc2 <- get_row_num(c("DOM_TYPE_R2_CC2"))
      cc3 <- get_row_num(c("DOM_TYPE_R2_CC3"))
      if (is.na(cc1)) cc1 <- 0
      if (is.na(cc2)) cc2 <- 0
      if (is.na(cc3)) cc3 <- 0

      in_set <- function(sp, set_codes) nzchar(sp) && sp %in% set_codes
      sf_set <- c("PIEN", "ABLA", "ABLAA", "ABAR2", "ABBI2")
      pj_set <- c("PIED", "JUSC2", "SAUT3", "JUNIP", "JUOS", "JUMO")
      df_set <- c("PSME", "PSMEG", "ABCO")
      totsf <- (if (in_set(sp1, sf_set)) cc1 else 0) + (if (in_set(sp2, sf_set)) cc2 else 0) + (if (in_set(sp3, sf_set)) cc3 else 0)
      totpj <- (if (in_set(sp1, pj_set)) cc1 else 0) + (if (in_set(sp2, pj_set)) cc2 else 0) + (if (in_set(sp3, pj_set)) cc3 else 0)
      totdf <- (if (in_set(sp1, df_set)) cc1 else 0) + (if (in_set(sp2, df_set)) cc2 else 0) + (if (in_set(sp3, df_set)) cc3 else 0)

      initial_family <- if (sp1 == "POTR5") "T217" else if (sp1 == "PIPU") "T216" else if (sp1 == "PIAR") "T209" else if (sp1 == "QUMA2") "T236" else if (sp1 %in% c("POAN3", "PODE3", "POAC5", "PODEW", "POFR2", "PODEM", "POSA", "POBA2", "POPUL")) "T235" else if (sp1 %in% c("PSME", "PSMEG")) "T210" else if (sp1 == "ABCO" && (sp2 %in% c("PSME", "PSMEG") || sp3 %in% c("PSME", "PSMEG"))) "T210" else if (sp1 == "JUVI") "T045" else if (sp1 == "QUGA" && !(sp1 %in% c("PIED", "JUSC2", "JUNIP", "JUOS", "JUMO", "SAUT3"))) "S413" else if (sp1 == "FRPE") "TASH" else if (sp1 == "PIBA2") "T001" else if (sp1 == "PIJE") "T247" else if (sp1 %in% c("PICO", "PICOL")) "T218" else if (sp1 == "PIFL2") "T219" else if (sp1 == "BEPA") "T252" else if (sp1 %in% c("PIED", "JUSC2", "SAUT3", "JUNIP", "JUOS", "JUMO")) "T239" else if (sp1 %in% c("PIPO", "PIPOS", "PIPOS2")) "T237" else if (sp1 == "TARA") "TSCD" else if (sp1 == "PISY") "TSCP" else if (sp1 %in% c("PIEN", "ABLA", "ABLAA", "ABAR2", "ABBI2")) "T206" else if (sp1 == "PIST3") "TSWP" else if (sp1 == "ABCO" && !(sp2 %in% c("PSME", "PSMEG") || sp3 %in% c("PSME", "PSMEG"))) "T211" else if (sp1 == "PIAL") "T208" else if (sp1 == "PIGL") "T201" else "unknown"

      target_family <- if (target == "NONE") "NONE" else unname(family_by_output[target])
      if (is.na(target_family) || !nzchar(target_family)) return(NULL)

      open_canopy <- !is.na(stcc) && stcc < 10
      special_other <- sp1 %in% c("2TB", "2TN")
      white_fir_is_top <- sp1 == "ABCO"
      douglas_fir_in_second_or_third <- sp2 %in% c("PSME", "PSMEG") || sp3 %in% c("PSME", "PSMEG")
      spruce_fir_override <- totsf > cc1 && totsf > cc2 && totsf > cc3 && totsf > 0
      pinyon_juniper_override <- totpj > cc1 && totpj > cc2 && totpj > cc3 && totpj > 0
      douglas_fir_override <- totdf > cc1 && totdf > cc2 && totdf > cc3 && totdf > 0

      yes_no <- function(x) if (isTRUE(x)) "yes" else "no"

      rule_text <- c(
        paste0(
          "Rule check: total stand canopy cover percent less than 10? ", yes_no(open_canopy),
          " (value = ", fmt_num(stcc), ")"
        ),
        paste0(
          "Rule check: top-ranked species is white fir code ABCO? ", yes_no(white_fir_is_top),
          " ; second or third ranked species is Douglas-fir code PSME or PSMEG? ", yes_no(douglas_fir_in_second_or_third)
        ),
        paste0(
          "Rule check: combined spruce-fir canopy total greater than each top-three individual canopy value and greater than zero? ",
          yes_no(spruce_fir_override),
          " (combined = ", fmt_num(totsf),
          " ; first = ", fmt_num(cc1),
          " ; second = ", fmt_num(cc2),
          " ; third = ", fmt_num(cc3), ")"
        ),
        paste0(
          "Rule check: combined pinyon-juniper canopy total greater than each top-three individual canopy value and greater than zero? ",
          yes_no(pinyon_juniper_override),
          " (combined = ", fmt_num(totpj),
          " ; first = ", fmt_num(cc1),
          " ; second = ", fmt_num(cc2),
          " ; third = ", fmt_num(cc3), ")"
        ),
        paste0(
          "Rule check: combined Douglas-fir canopy total greater than each top-three individual canopy value and greater than zero? ",
          yes_no(douglas_fir_override),
          " (combined = ", fmt_num(totdf),
          " ; first = ", fmt_num(cc1),
          " ; second = ", fmt_num(cc2),
          " ; third = ", fmt_num(cc3), ")"
        ),
        paste0(
          "Rule check: top-ranked species is special other code 2TB or 2TN? ", yes_no(special_other)
        )
      )

      stage_values <- c(
        paste0("Stage values: selected output value = ", target, " ; total stand canopy cover percent = ", fmt_num(stcc)),
        paste0(
          "Stage values: dominant species sequence = ", if (nzchar(dom_label)) dom_label else "unknown",
          " ; first ranked species = ", if (nzchar(sp1)) sp1 else "none",
          " ; second ranked species = ", if (nzchar(sp2)) sp2 else "none",
          " ; third ranked species = ", if (nzchar(sp3)) sp3 else "none"
        ),
        paste0(
          "Stage values: canopy percent for first ranked species = ", fmt_num(cc1),
          " ; second ranked species = ", fmt_num(cc2),
          " ; third ranked species = ", fmt_num(cc3)
        ),
        paste0(
          "Stage values: combined canopy percent by grouped species families -> spruce-fir group = ", fmt_num(totsf),
          " ; pinyon-juniper group = ", fmt_num(totpj),
          " ; Douglas-fir group = ", fmt_num(totdf)
        ),
        paste0("Stage values: initial forest type family code = ", initial_family, " ; final forest type family code = ", target_family)
      )

      code_after_initial <- initial_family
      code_after_sf <- if (spruce_fir_override) "T206" else code_after_initial
      code_after_pj <- if (pinyon_juniper_override) "T239" else code_after_sf
      code_after_df <- if (douglas_fir_override) "T210" else code_after_pj
      code_after_special <- if (special_other) "T999" else code_after_df

      yn_open <- if (open_canopy) "yes" else "no"
      yn_white_fir <- if (white_fir_is_top) "yes" else "no"
      yn_df_second_third <- if (douglas_fir_in_second_or_third) "yes" else "no"
      yn_sf <- if (spruce_fir_override) "yes" else "no"
      yn_pj <- if (pinyon_juniper_override) "yes" else "no"
      yn_df <- if (douglas_fir_override) "yes" else "no"
      yn_special <- if (special_other) "yes" else "no"

      edge_open <- safe_mermaid_text(paste0(yn_open, " , canopy percent = ", fmt_num(stcc)))
      edge_white_fir <- safe_mermaid_text(paste0(yn_white_fir, " , first ranked species = ", if (nzchar(sp1)) sp1 else "none"))
      edge_df_second_third <- safe_mermaid_text(paste0(yn_df_second_third, " , second = ", if (nzchar(sp2)) sp2 else "none", " , third = ", if (nzchar(sp3)) sp3 else "none"))
      edge_sf <- safe_mermaid_text(paste0(yn_sf, " , combined spruce fir = ", fmt_num(totsf), " , first = ", fmt_num(cc1), " , second = ", fmt_num(cc2), " , third = ", fmt_num(cc3)))
      edge_pj <- safe_mermaid_text(paste0(yn_pj, " , combined pinyon juniper = ", fmt_num(totpj), " , first = ", fmt_num(cc1), " , second = ", fmt_num(cc2), " , third = ", fmt_num(cc3)))
      edge_df <- safe_mermaid_text(paste0(yn_df, " , combined Douglas fir = ", fmt_num(totdf), " , first = ", fmt_num(cc1), " , second = ", fmt_num(cc2), " , third = ", fmt_num(cc3)))
      edge_special <- safe_mermaid_text(paste0(yn_special, " , first ranked species = ", if (nzchar(sp1)) sp1 else "none"))

      if (target == "NONE") {
        return(mk(c(
          "A[Call Region 2 logic] --> B[Compute total stand canopy cover percent]",
          paste0("B --> C{Is total stand canopy cover percent less than 10?}"),
          paste0("C -- ", edge_open, " --> D[Carry forward selected output value NONE]"),
          "D --> E[Set Region 2 cover type equals NONE]"
        )))
      }

      if (target == "OTH") {
        return(mk(c(
          "A[Call Region 2 logic] --> B[Resolve first second and third ranked species with canopy percentages]",
          paste0("B --> C{Is total stand canopy cover percent less than 10?}"),
          paste0("C -- ", edge_open, " --> D[Continue classification because canopy percent is at least 10]"),
          "D --> E{Is first ranked species special other code 2TB or 2TN?}",
          paste0("E -- ", edge_special, " --> F[Carry current family code T999]"),
          "F --> G[Crosswalk family code T999 to Region 2 output OTH]",
          "G --> H[Set Region 2 cover type equals OTH]"
        )))
      }

      mk(c(
        "A[Call Region 2 logic] --> B[Resolve first second and third ranked species with canopy percentages]",
        paste0("B --> C[", safe_mermaid_text(stage_values[[2]]), "]"),
        paste0("C --> D[", safe_mermaid_text(stage_values[[3]]), "]"),
        "D --> E{Is total stand canopy cover percent less than 10?}",
        paste0("E -- ", edge_open, " --> F[Continue classification because canopy percent is at least 10]"),
        "F --> G{Is first ranked species white fir code ABCO?}",
        paste0("G -- ", edge_white_fir, " --> H{If white fir is first ranked species is second or third ranked species Douglas fir code PSME or PSMEG?}"),
        paste0("H -- ", edge_df_second_third, " --> I[Carry family code after initial mapping and white fir exception = ", code_after_initial, "]"),
        "I --> J{Does combined spruce fir canopy exceed each top three species canopy and exceed zero?}",
        paste0("J -- ", edge_sf, " --> K[Carry family code after spruce fir override = ", code_after_sf, "]"),
        "K --> L{Does combined pinyon juniper canopy exceed each top three species canopy and exceed zero?}",
        paste0("L -- ", edge_pj, " --> M[Carry family code after pinyon juniper override = ", code_after_pj, "]"),
        "M --> N{Does combined Douglas fir canopy exceed each top three species canopy and exceed zero?}",
        paste0("N -- ", edge_df, " --> O[Carry family code after Douglas fir override = ", code_after_df, "]"),
        "O --> P{Is first ranked species special other code 2TB or 2TN?}",
        paste0("P -- ", edge_special, " --> Q[Carry final family code after special other rule = ", code_after_special, "]"),
        paste0("Q --> R[Crosswalk final family code to Region 2 output value = ", target, "]")
      ))
    }

    get_non_covertype_value_flow <- function(region_key, attr_key, selected_value) {
      if (!nzchar(selected_value)) return(NULL)
      if (attr_key == "COVERTYPE_R2") return(NULL)

      yn <- function(flag, extra = "") {
        prefix <- if (is.na(flag)) "unknown" else if (isTRUE(flag)) "yes" else "no"
        lab <- trimws(paste(prefix, extra))
        safe_mermaid_text(lab)
      }

      stcc <- get_row_num(c("CAN_COV", "CC", "STCC"))
      ba <- get_row_num(c("BA"))
      tpa <- get_row_num(c("TPA"))
      dbh_wt <- get_row_num(c("BA_WT_DIA", "STBAWTDBH"))
      dom_r2 <- get_row_text(c("DOM_TYPE_R2"), "")
      dom_parts <- if (nzchar(dom_r2)) trimws(strsplit(dom_r2, ":", fixed = TRUE)[[1]]) else character(0)
      n_dom <- length(dom_parts)

      if (attr_key == "CAN_COV") {
        return(mk(c(
          "A[Call plotAttr for all trees] --> B[Compute TREECC for each tree]",
          "B --> C[Equation: TREECC = pi * (CrWidth / 2)^2 * (TPA / 43560) * 100]",
          "C --> D[Sum TREECC across trees to UNCC]",
          "D --> E[Equation: CAN_COV = 100 * (1 - exp(-0.01 * UNCC))]",
          paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (attr_key %in% c("BA", "BA_STM")) {
        stem_only <- attr_key == "BA_STM"
        dbh_rule <- if (stem_only) "at least 1 inch" else "at least 0 inch"
        return(mk(c(
          paste0("A[Call plotAttr with diameter filter ", dbh_rule, "] --> B[Compute TREEBA for each tree]"),
          "B --> C[Equation: TREEBA = DBH^2 * TPA * 0.005454154]",
          "C --> D[Sum TREEBA across trees to BA]",
          paste0("D --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (attr_key %in% c("TPA", "TPA_STM")) {
        stem_only <- attr_key == "TPA_STM"
        dbh_rule <- if (stem_only) "at least 1 inch" else "at least 0 inch"
        return(mk(c(
          paste0("A[Call plotAttr with diameter filter ", dbh_rule, "] --> B[Read TPA from each included tree record]"),
          "B --> C[Sum TPA across included trees]",
          paste0("C --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (attr_key %in% c("QMD", "QMD_STM")) {
        stem_only <- attr_key == "QMD_STM"
        dbh_rule <- if (stem_only) "at least 1 inch" else "at least 0 inch"
        return(mk(c(
          paste0("A[Call plotAttr with diameter filter ", dbh_rule, "] --> B[Compute DBHSQ for each included tree]"),
          "B --> C[Sum DBHSQ across included trees]",
          "C --> D[Sum TPA across included trees to TPASUM]",
          "D --> E[Equation: QMD = sqrt(DBHSQ / TPASUM)]",
          paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (attr_key %in% c("ZSDI", "ZSDI_STM")) {
        stem_only <- attr_key == "ZSDI_STM"
        dbh_rule <- if (stem_only) "at least 1 inch" else "at least 0 inch"
        return(mk(c(
          paste0("A[Call plotAttr with diameter filter ", dbh_rule, "] --> B[Compute ZSDI contribution for each included tree]"),
          "B --> C[Equation: ZSDI contribution per tree = TPA * (DBH / 10)^1.605]",
          "C --> D[Sum ZSDI contributions across included trees]",
          paste0("D --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (attr_key %in% c("RSDI", "RSDI_STM")) {
        stem_only <- attr_key == "RSDI_STM"
        dbh_rule <- if (stem_only) "at least 1 inch" else "at least 0 inch"
        return(mk(c(
          paste0("A[Call plotAttr with diameter filter ", dbh_rule, "] --> B[Compute TPA and QMD for the stand]"),
          "B --> C[Equation: RSDI = TPA * (QMD / 10)^1.605]",
          paste0("C --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (attr_key == "BA_WT_DIA") {
        return(mk(c(
          "A[Call plotAttr for all trees] --> B[Compute TREEBA for each tree]",
          "B --> C[Equation: TREEBA = DBH^2 * TPA * 0.005454154]",
          "C --> D[Equation: BAWTD = sum(DBH * TREEBA)]",
          "D --> E[Use BA as the denominator]",
          "E --> F[Equation: BA_WT_DIA = BAWTD / BA]",
          paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (attr_key == "BA_WT_HT") {
        return(mk(c(
          "A[Call plotAttr for all trees] --> B[Compute TREEBA for each tree]",
          "B --> C[Equation: TREEBA = DBH^2 * TPA * 0.005454154]",
          "C --> D[Equation: BAWTH = sum(Ht * TREEBA)]",
          "D --> E[Use BA as the denominator]",
          "E --> F[Equation: BA_WT_HT = BAWTH / BA]",
          paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (attr_key == "QMD_TOP20") {
        min_dbh <- if (!is.na(stcc) && stcc >= 10) 0.2 else 0.1
        tpa20 <- if (!is.na(tpa)) max(tpa * 0.20, 20) else NA_real_
        cc_yes <- !is.na(stcc) && stcc >= 10
        cc_path_text <- if (!is.na(stcc) && stcc >= 10) {
          paste0("Yes: CAN_COV is at least 10 percent, so minimum included DBH = 0.2 inches (CAN_COV = ", fmt_num(stcc), ")")
        } else if (!is.na(stcc) && stcc < 10) {
          paste0("No: CAN_COV is less than 10 percent, so minimum included DBH = 0.1 inches (CAN_COV = ", fmt_num(stcc), ")")
        } else {
          "No: CAN_COV value is unavailable, so the helper default path sets minimum included DBH to 0.1 inches"
        }
        return(mk(c(
          "A[Call qmdTop20 helper] --> B[Read stand inputs from this row]",
          paste0("B --> B1[Input value: CAN_COV = ", fmt_num(stcc), "]"),
          paste0("B1 --> B2[Input value: TPA = ", fmt_num(tpa), "]"),
          "B2 --> C[Sort trees from largest DBH to smallest DBH]",
          "C --> D{Is CAN_COV at least 10 percent?}",
          paste0("D -- ", if (cc_yes) "yes" else "no", " --> E[", safe_mermaid_text(cc_path_text), "]"),
          paste0("E --> F[Derived value: minimum included DBH = ", fmt_num(min_dbh), " inches]"),
          "F --> G[Equation: TPA20 = max(0.20 * TPA, 20)]",
          paste0("G --> G1[Derived value: TPA20 = ", fmt_num(tpa20), "]"),
          "G1 --> H[Keep only trees with DBH at or above minimum included DBH]",
          "H --> I[Accumulate DBHSQ and TPASUM until TPA20 is met]",
          "I --> J[If the last tree would overshoot TPA20, keep a partial TPA contribution so TPASUM equals TPA20]",
          "J --> K[Equation: QMD_TOP20 = sqrt(DBHSQ / TPASUM)]",
          paste0("K --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (region_key == "2" && attr_key == "DOM_TYPE_R2") {
        open_canopy <- !is.na(stcc) && stcc < 10
        cc1 <- get_row_num(c("DOM_TYPE_R2_CC1"))
        cc2 <- get_row_num(c("DOM_TYPE_R2_CC2"))
        cc3 <- get_row_num(c("DOM_TYPE_R2_CC3"))
        sp1 <- if (length(dom_parts) >= 1) dom_parts[[1]] else ""
        sp2 <- if (length(dom_parts) >= 2) dom_parts[[2]] else ""
        sp3 <- if (length(dom_parts) >= 3) dom_parts[[3]] else ""
        has_three <- n_dom >= 3
        has_two <- n_dom == 2

        path <- c(
          "A[Call Region 2 dominant type logic] --> B[Get total stand canopy cover]",
          paste0("B --> B_VAL[Carried value: total stand canopy cover = ", fmt_num(stcc), "]"),
          paste0("B_VAL --> B_VAL1[Carried value: first ranked species = ", safe_mermaid_text(if (nzchar(sp1)) sp1 else "none"), "]"),
          paste0("B_VAL1 --> B_VAL2[Carried value: first canopy share = ", fmt_num(cc1), "]"),
          paste0("B_VAL2 --> B_VAL3[Carried value: second ranked species = ", safe_mermaid_text(if (nzchar(sp2)) sp2 else "none"), "]"),
          paste0("B_VAL3 --> B_VAL4[Carried value: second canopy share = ", fmt_num(cc2), "]"),
          paste0("B_VAL4 --> B_VAL5[Carried value: third ranked species = ", safe_mermaid_text(if (nzchar(sp3)) sp3 else "none"), "]"),
          paste0("B_VAL5 --> B_VAL6[Carried value: third canopy share = ", fmt_num(cc3), "]"),
          "B_VAL6 --> C0[Sort species canopy shares from largest to smallest]",
          "C0 --> C{Is total stand canopy cover less than 10%?}",
          paste0("C -- ", yn(open_canopy, paste0("value = ", fmt_num(stcc))), " --> D")
        )

        if (open_canopy) {
          path <- c(path,
                    "D[Result: Dominant type is NONE for open canopy]",
                    "D --> FINAL[Set output value = NONE]"
          )
        } else {
          path <- c(path,
                    "D[Build dominant species sequence from sorted canopy shares]",
                    paste0("D --> D_VAL[Carried value: dominant species sequence = ", safe_mermaid_text(dom_r2), "]"),
                    "D_VAL --> E{Are three or more species present in dominant species sequence?}",
                    paste0("E -- ", yn(has_three, paste0("count = ", n_dom)), " --> F")
          )
          if (has_three) {
            path <- c(path,
                      "F[Result: Use top three species for dominant type]",
                      paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else {
            path <- c(path,
                      "F{Are exactly two species present?}",
                      paste0("F -- ", yn(has_two, paste0("count = ", n_dom)), " --> G")
            )
            if (has_two) {
              path <- c(path,
                        "G[Result: Use top two species for dominant type]",
                        paste0("G --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
              )
            } else {
              path <- c(path,
                        "G[Result: Use top species for dominant type]",
                        paste0("G --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
              )
            }
          }
        }
        return(mk(path))
      }

      if (region_key == "2" && attr_key %in% c("DOM_TYPE_R2_CC1", "DOM_TYPE_R2_CC2", "DOM_TYPE_R2_CC3")) {
        val_num <- suppressWarnings(as.numeric(selected_value))
        open_canopy <- !is.na(stcc) && stcc < 10
        target_rank <- if (attr_key == "DOM_TYPE_R2_CC1") 1 else if (attr_key == "DOM_TYPE_R2_CC2") 2 else 3
        enough_species <- n_dom >= target_rank

        path <- c(
          "A[Call Region 2 canopy share logic] --> B[Get total stand canopy cover and dominant species sequence]",
          paste0("B --> B_VAL1[Carried value: total stand canopy cover = ", fmt_num(stcc), "]"),
          paste0("B_VAL1 --> B_VAL2[Carried value: dominant species sequence = ", safe_mermaid_text(dom_r2), "]"),
          "B_VAL2 --> C{Is total stand canopy cover less than 10%?}",
          paste0("C -- ", yn(open_canopy, paste0("value = ", fmt_num(stcc))), " --> D")
        )

        if (open_canopy) {
          path <- c(path,
                    "D[Result: Canopy share is NONE for open canopy]",
                    "D --> FINAL[Set output value = NONE]"
          )
        } else {
          path <- c(path,
                    paste0("D{Are at least ", target_rank, " dominant species available?}"),
                    paste0("D -- ", yn(enough_species, paste0("species count = ", n_dom)), " --> E")
          )
          if (enough_species) {
            path <- c(path,
                      paste0("E[Result: Use rank ", target_rank, " canopy share from sorted species list]"),
                      paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else {
            path <- c(path,
                      "E[Result: Default canopy share is 0 because rank is not available]",
                      "E --> FINAL[Set output value = 0]"
            )
          }
        }
        return(mk(path))
      }

      if (region_key == "2" && attr_key == "TREE_SIZE_CLASS_R2") {
        open_canopy <- !is.na(stcc) && stcc < 10

        path <- c(
          "A[Call Region 2 tree size class logic] --> B[Get total stand canopy cover and canopy by diameter classes]",
          paste0("B --> B_VAL[Carried value: total stand canopy cover = ", fmt_num(stcc), "]"),
          "B_VAL --> C{Is canopy open (cover < 10%) and size class 'n'?}",
          paste0("C -- ", yn(open_canopy && selected_value == "n"), " --> D")
        )

        if (open_canopy && selected_value == "n") {
          path <- c(path,
                    "D[Result: Tree size class is 'n' for open canopy]",
                    "D --> FINAL[Set output value = n]"
          )
        } else {
          path <- c(path,
                    "D[Result: Group canopy by diameter classes (seedling, small, medium, large, very-large)]",
                    "D --> E[Apply tie-breaking rules to find the dominant size class]",
                    paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        }
        return(mk(path))
      }

      if (region_key == "2" && attr_key == "CROWN_CLASS_R2") {
        c1 <- !is.na(stcc) && stcc >= 10 && stcc < 40
        c2 <- !is.na(stcc) && stcc >= 40 && stcc < 70
        c3 <- !is.na(stcc) && stcc >= 70

        path <- c(
          "A[Call Region 2 crown class logic] --> B[Get total stand canopy cover]",
          paste0("B --> B_VAL[Carried value: total stand canopy cover = ", fmt_num(stcc), "]"),
          "B_VAL --> C{Is canopy cover >= 10% and < 40%?}",
          paste0("C -- ", yn(c1, paste0("value = ", fmt_num(stcc))), " --> D")
        )

        if (c1) {
          path <- c(path,
                    "D[Result: Crown class is 1]",
                    "D --> FINAL[Set output value = 1]"
          )
        } else {
          path <- c(path,
                    "D{Is canopy cover >= 40% and < 70%?}",
                    paste0("D -- ", yn(c2, paste0("value = ", fmt_num(stcc))), " --> E")
          )
          if (c2) {
            path <- c(path,
                      "E[Result: Crown class is 2]",
                      "E --> FINAL[Set output value = 2]"
            )
          } else {
            path <- c(path,
                      "E{Is canopy cover >= 70%?}",
                      paste0("E -- ", yn(c3, paste0("value = ", fmt_num(stcc))), " --> F")
            )
            if (c3) {
              path <- c(path,
                        "F[Result: Crown class is 3]",
                        "F --> FINAL[Set output value = 3]"
              )
            } else {
              path <- c(path,
                        "F[Result: No class assigned (cover < 10%)]",
                        paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
              )
            }
          }
        }
        return(mk(path))
      }

      if (region_key == "2" && attr_key %in% c("HSS1_4C", "HSS1_5")) {
        is_14c <- attr_key == "HSS1_4C"
        selected_hss <- toupper(trimws(as.character(selected_value %||% "")))

        path <- c("A[Call HSS helper function] --> B[Get stand attributes]")

        if (is_14c) {
          # Show only the decision branch used to produce the selected HSS1_4C value.
          grp_size_class <- get_row_text(c("TREE_SIZE_CLASS_R2"), "unknown")
          stcc_val <- get_row_num(c("CAN_COV", "CC", "STCC"))
          stcc_lt_10 <- !is.na(stcc_val) && stcc_val < 10
          grp_upper <- toupper(trimws(grp_size_class))

          get_optional_num <- function(candidates) {
            out <- get_row_num(candidates)
            if (is.na(out)) return(NA_real_)
            out
          }
          fmt_present <- function(label, value) {
            if (is.na(value)) return(NULL)
            paste0(label, "=", fmt_num(value))
          }

          # These canopy-bin totals are not always present in row-level outputs.
          bin_e <- get_optional_num(c("covTsize_E", "COVTSIZE_E", "CC_BIN_E", "TREECC_BIN_E", "CCBYDIAMCLASS_E"))
          bin_s <- get_optional_num(c("covTsize_S", "COVTSIZE_S", "CC_BIN_S", "TREECC_BIN_S", "CCBYDIAMCLASS_S"))
          bin_m <- get_optional_num(c("covTsize_M", "COVTSIZE_M", "CC_BIN_M", "TREECC_BIN_M", "CCBYDIAMCLASS_M"))
          bin_l <- get_optional_num(c("covTsize_L", "COVTSIZE_L", "CC_BIN_L", "TREECC_BIN_L", "CCBYDIAMCLASS_L"))
          bin_v <- get_optional_num(c("covTsize_V", "COVTSIZE_V", "CC_BIN_V", "TREECC_BIN_V", "CCBYDIAMCLASS_V"))

          bin_es <- get_optional_num(c("GroupES", "GROUP_ES", "ES_BIN", "COV_GROUP_ES"))
          bin_lv <- get_optional_num(c("GroupLV", "GROUP_LV", "LV_BIN", "COV_GROUP_LV"))
          if (is.na(bin_es) && !is.na(bin_e) && !is.na(bin_s)) bin_es <- bin_e + bin_s
          if (is.na(bin_lv) && !is.na(bin_l) && !is.na(bin_v)) bin_lv <- bin_l + bin_v

          have_bin_values <- any(!is.na(c(bin_e, bin_s, bin_m, bin_l, bin_v, bin_es, bin_lv)))
          bin_source_txt <- if (have_bin_values) {
            "row-level canopy totals"
          } else {
            "not available in selected summary row"
          }

          path <- c(path, "B --> C{Is STCC < 10?}")
          if (stcc_lt_10) {
            path <- c(path,
                      paste0("C -- yes", if (!is.na(stcc_val)) paste0(", STCC = ", fmt_num(stcc_val)) else "", " --> D[grpSizeClass = N]"),
                      "D --> H_VAL1"
            )
          } else {
            path <- c(path,
                      paste0("C -- no", if (!is.na(stcc_val)) paste0(", STCC = ", fmt_num(stcc_val)) else "", " --> E[Build canopy bins E/S/M/L/V]"),
                      "E --> F[Compute ES and LV totals]",
                      "F --> G[Apply grpSizeClass precedence]",
                      "G --> H_VAL1"
            )
          }

          stcc_gt0_lt40 <- !is.na(stcc_val) && stcc_val > 0 && stcc_val < 40
          stcc_ge40_lt70 <- !is.na(stcc_val) && stcc_val >= 40 && stcc_val < 70
          stcc_ge70 <- !is.na(stcc_val) && stcc_val >= 70

          derived_hss <- if (grp_upper == "N") {
            "1"
          } else if (grp_upper == "E") {
            "2"
          } else if (grp_upper %in% c("S", "M") && stcc_gt0_lt40) {
            "3A"
          } else if (grp_upper %in% c("S", "M") && stcc_ge40_lt70) {
            "3B"
          } else if (grp_upper %in% c("S", "M") && stcc_ge70) {
            "3C"
          } else if (grp_upper %in% c("L", "V") && stcc_gt0_lt40) {
            "4A"
          } else if (grp_upper %in% c("L", "V") && stcc_ge40_lt70) {
            "4B"
          } else if (grp_upper %in% c("L", "V") && stcc_ge70) {
            "4C"
          } else {
            ""
          }

          target_hss <- if (selected_hss %in% c("1", "2", "3A", "3B", "3C", "4A", "4B", "4C")) selected_hss else derived_hss

          stcc_band <- if (is.na(stcc_val)) {
            "unknown"
          } else if (stcc_val < 10) {
            "<10"
          } else if (stcc_val < 40) {
            "0-<40"
          } else if (stcc_val < 70) {
            "40-<70"
          } else {
            ">=70"
          }

          converged_parts <- c(
            paste0("grpSizeClass=", safe_mermaid_text(grp_size_class)),
            fmt_present("STCC", stcc_val),
            if (!is.na(stcc_val)) paste0("STCC band=", stcc_band) else NULL
          )
          bin_parts <- c(
            fmt_present("E", bin_e),
            fmt_present("S", bin_s),
            fmt_present("M", bin_m),
            fmt_present("L", bin_l),
            fmt_present("V", bin_v),
            fmt_present("ES", bin_es),
            fmt_present("LV", bin_lv)
          )

          path <- c(path,
                    paste0("H_VAL1[Converged state from prior branch: ", paste(converged_parts, collapse = ", "), "]"))
          if (length(bin_parts) > 0) {
            path <- c(path,
                      paste0("H_VAL1 --> H_VAL2[Carry in bins: ", paste(bin_parts, collapse = ", "), " (", bin_source_txt, ")]"),
                      paste0("H_VAL2 --> H_VAL3[Carry in target stage = ", safe_mermaid_text(if (nzchar(target_hss)) target_hss else selected_hss), "]")
            )
          } else {
            path <- c(path,
                      paste0("H_VAL1 --> H_VAL3[Carry in target stage = ", safe_mermaid_text(if (nzchar(target_hss)) target_hss else selected_hss), "]")
            )
          }
          path <- c(path, "H_VAL3 --> I1{Decision 1: Is STCC < 10?}")

          if (target_hss == "1") {
            path <- c(path,
                      paste0("I1 -- ", yn(stcc_lt_10, if (!is.na(stcc_val)) paste0("STCC = ", fmt_num(stcc_val)) else "STCC missing"), " --> I2[Carry forward: STCC band = ", stcc_band, ", grpSizeClass = ", safe_mermaid_text(grp_size_class), "]"),
                      "I2 --> I3{Decision 2: Is grpSizeClass = N?}",
                      paste0("I3 -- ", yn(grp_upper == "N", paste0("grpSizeClass = ", grp_upper)), " --> FINAL[Final output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else if (target_hss == "2") {
            path <- c(path,
                      paste0("I1 -- ", yn(!stcc_lt_10, if (!is.na(stcc_val)) paste0("STCC = ", fmt_num(stcc_val)) else "STCC missing"), " --> I2[Carry forward: STCC band = ", stcc_band, " (>=10 required)]"),
                      "I2 --> I3{Decision 2: Is grpSizeClass = E?}",
                      paste0("I3 -- ", yn(grp_upper == "E", paste0("grpSizeClass = ", grp_upper)), " --> FINAL[Final output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else if (target_hss %in% c("3A", "3B", "3C")) {
            stage3_msg <- if (target_hss == "3A") "0 < STCC < 40" else if (target_hss == "3B") "40 <= STCC < 70" else "STCC >= 70"
            path <- c(path,
                      paste0("I1 -- ", yn(!stcc_lt_10, if (!is.na(stcc_val)) paste0("STCC = ", fmt_num(stcc_val)) else "STCC missing"), " --> I2[Carry forward: STCC band = ", stcc_band, " (>=10 required)]"),
                      paste0("I2 --> I3{Decision 2: Is grpSizeClass in S/M? actual = ", grp_upper, "}"),
                      paste0("I3 -- ", yn(grp_upper %in% c("S", "M"), paste0("grpSizeClass = ", grp_upper)), " --> I4[Carry forward: stage family = 3, STCC band = ", stcc_band, "]"),
                      paste0("I4 --> I5{Decision 3: Does STCC satisfy ", stage3_msg, "?}"),
                      paste0("I5 -- yes", if (!is.na(stcc_val)) paste0(", STCC = ", fmt_num(stcc_val)) else "", " --> FINAL[Final output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else if (target_hss %in% c("4A", "4B", "4C")) {
            stage4_msg <- if (target_hss == "4A") "0 < STCC < 40" else if (target_hss == "4B") "40 <= STCC < 70" else "STCC >= 70"
            path <- c(path,
                      paste0("I1 -- ", yn(!stcc_lt_10, if (!is.na(stcc_val)) paste0("STCC = ", fmt_num(stcc_val)) else "STCC missing"), " --> I2[Carry forward: STCC band = ", stcc_band, " (>=10 required)]"),
                      paste0("I2 --> I3{Decision 2: Is grpSizeClass in L/V? actual = ", grp_upper, "}"),
                      paste0("I3 -- ", yn(grp_upper %in% c("L", "V"), paste0("grpSizeClass = ", grp_upper)), " --> I4[Carry forward: stage family = 4, STCC band = ", stcc_band, "]"),
                      paste0("I4 --> I5{Decision 3: Does STCC satisfy ", stage4_msg, "?}"),
                      paste0("I5 -- yes", if (!is.na(stcc_val)) paste0(", STCC = ", fmt_num(stcc_val)) else "", " --> FINAL[Final output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else {
            path <- c(path,
                      "I1 --> I2[Carry forward failed: selected output does not align to valid HSS1_4C stage]",
                      paste0("I2 --> I3[Carry context: grpSizeClass = ", safe_mermaid_text(grp_size_class), ", STCC band = ", stcc_band, "]"),
                      paste0("I3 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          }

          return(mk(path))
        }

        # Show only the decision branch used to produce the selected HSS1_5 value.
        get_optional_num_hss5 <- function(candidates) {
          out <- get_row_num(candidates)
          if (is.na(out)) return(NA_real_)
          out
        }
        fmt_present_hss5 <- function(label, value) {
          if (is.na(value)) return(NULL)
          paste0(label, "=", fmt_num(value))
        }

        tpa_val <- get_row_num(c("TPA"))
        stcc_val <- get_row_num(c("CAN_COV", "CC", "STCC"))
        qmd_val <- get_row_num(c("QMD", "QMD_STM", "QMD_TOP20"))

        # Optional row-level intermediates; many outputs do not include these directly.
        ba5_val <- get_optional_num_hss5(c("BA5", "BA_5IN"))
        ba9_val <- get_optional_num_hss5(c("BA9", "BA_9IN"))
        ba16_val <- get_optional_num_hss5(c("BA16", "BA_16IN"))
        ba125_val <- get_optional_num_hss5(c("BA125", "BA_1TO5"))
        ba529_val <- get_optional_num_hss5(c("BA529", "BA_5TO9"))
        ogsc_val <- get_optional_num_hss5(c("OGSC"))

        barat_val <- if (!is.na(ba9_val) && ba9_val > 0 && !is.na(ba16_val)) ba16_val / ba9_val else NA_real_
        tpamax_val <- if (!is.na(qmd_val) && qmd_val > 0) 18641.0/(qmd_val^1.659925) else NA_real_
        bamax_val <- if (!is.na(qmd_val) && qmd_val > 0) 101.67*(qmd_val^0.34007) else NA_real_
        bamin_raw <- if (!is.na(bamax_val)) bamax_val * 0.10 else NA_real_

        bamin_adj <- bamin_raw
        ba5_adj <- ba5_val
        if (!is.na(bamin_adj) && bamin_adj < 20) {
          bamin_adj <- 20
          ba5_adj <- if (is.na(ba5_adj)) 20 else max(ba5_adj, 20)
        }
        if (!is.na(bamin_adj) && !is.na(ba5_adj) && bamin_adj >= 20 && bamin_adj >= ba5_adj && !is.na(stcc_val) && stcc_val > 10) {
          bamin_adj <- ba5_adj
        }

        cond_tsc6 <- !is.na(ba5_adj) && !is.na(bamin_adj) && !is.na(ba125_val) && !is.na(ba9_val) && !is.na(ba529_val) && !is.na(barat_val) && ba5_adj >= bamin_adj && ba5_adj >= ba125_val && ba9_val > 0 && ba9_val >= ba529_val && barat_val > 0.50
        cond_tsc5 <- !is.na(ba5_adj) && !is.na(bamin_adj) && !is.na(ba125_val) && !is.na(ba9_val) && !is.na(ba529_val) && !is.na(barat_val) && ba5_adj >= bamin_adj && ba5_adj >= ba125_val && ba9_val > 0 && ba9_val >= ba529_val && barat_val <= 0.50
        cond_tsc4 <- !is.na(ba5_adj) && !is.na(bamin_adj) && !is.na(ba125_val) && !is.na(ba9_val) && !is.na(ba529_val) && ba5_adj >= bamin_adj && ba5_adj >= ba125_val && ba9_val < ba529_val
        cond_tsc3 <- !is.na(ba5_adj) && !is.na(bamin_adj) && !is.na(ba125_val) && ba5_adj >= bamin_adj && ba5_adj < ba125_val
        cond_tsc2 <- !is.na(ba5_adj) && !is.na(bamin_adj) && !is.na(tpa_val) && ba5_adj < bamin_adj && tpa_val >= 300
        cond_tsc1 <- !is.na(stcc_val) && stcc_val < 10

        tsc_code <- if (cond_tsc6) {
          6
        } else if (cond_tsc5) {
          5
        } else if (cond_tsc4) {
          4
        } else if (cond_tsc3) {
          3
        } else if (cond_tsc2) {
          2
        } else if (cond_tsc1) {
          1
        } else {
          NA_real_
        }

        chess_base <- NA_real_
        if (!is.na(tsc_code) && tsc_code %in% c(5, 6)) {
          if (!is.na(stcc_val) && stcc_val < 40) chess_base <- 41
          if (!is.na(stcc_val) && stcc_val >= 40 && stcc_val < 70) chess_base <- 42
          if (!is.na(stcc_val) && stcc_val >= 70) chess_base <- 43
        } else if (!is.na(tsc_code) && tsc_code %in% c(3, 4)) {
          if (!is.na(stcc_val) && stcc_val < 40) chess_base <- 31
          if (!is.na(stcc_val) && stcc_val >= 40 && stcc_val < 70) chess_base <- 32
          if (!is.na(stcc_val) && stcc_val >= 70) chess_base <- 33
        } else if (!is.na(tsc_code) && tsc_code == 2) {
          chess_base <- 20
        } else if (!is.na(stcc_val) && stcc_val <= 10) {
          chess_base <- 10
        }

        chess_final <- chess_base
        if (!is.na(chess_base) && chess_base %in% c(41, 42, 43) && !is.na(ogsc_val) && ogsc_val >= 42) {
          chess_final <- 50
        }

        chess_to_hss <- c("10" = "1", "20" = "2", "31" = "3A", "32" = "3B", "33" = "3C", "41" = "4A", "42" = "4B", "43" = "4C", "50" = "5")
        derived_hss5 <- if (!is.na(chess_final)) as.character(chess_to_hss[as.character(as.integer(chess_final))]) else ""
        target_hss5 <- if (selected_hss %in% c("1", "2", "3A", "3B", "3C", "4A", "4B", "4C", "5")) selected_hss else derived_hss5
        chess_context <- function(target = NULL) {
          parts <- c(
            fmt_present_hss5("CHESS base", chess_base),
            fmt_present_hss5("CHESS final", chess_final),
            if (!is.null(target) && nzchar(target)) paste0("target=", target) else NULL
          )
          if (length(parts) == 0) {
            "Carry forward CHESS mapping context"
          } else {
            paste0("Carry forward: ", paste(parts, collapse = ", "))
          }
        }

        primary_parts <- c(
          fmt_present_hss5("STCC", stcc_val),
          fmt_present_hss5("TPA", tpa_val),
          fmt_present_hss5("QMD", qmd_val)
        )
        ba_parts <- c(
          fmt_present_hss5("BA5", ba5_val),
          fmt_present_hss5("BA9", ba9_val),
          fmt_present_hss5("BA16", ba16_val),
          fmt_present_hss5("BA125", ba125_val),
          fmt_present_hss5("BA529", ba529_val)
        )
        derived_parts <- c(
          fmt_present_hss5("BARAT", barat_val),
          fmt_present_hss5("BAMIN adj", bamin_adj),
          fmt_present_hss5("OGSC", ogsc_val),
          fmt_present_hss5("TSC", tsc_code)
        )

        path <- c(path, "B --> C[Compute HSS1_5 intermediates from plotAttr outputs]")
        carry_node <- "C"
        if (length(primary_parts) > 0) {
          path <- c(path, paste0(carry_node, " --> C1[Carry in primary values: ", paste(primary_parts, collapse = ", "), "]"))
          carry_node <- "C1"
        }
        if (length(ba_parts) > 0) {
          path <- c(path, paste0(carry_node, " --> C2[Carry in BA values: ", paste(ba_parts, collapse = ", "), "]"))
          carry_node <- "C2"
        }
        if (length(derived_parts) > 0) {
          path <- c(path, paste0(carry_node, " --> C3[Carry derived values: ", paste(derived_parts, collapse = ", "), "]"))
          carry_node <- "C3"
        }
        path <- c(path,
                  paste0(carry_node, " --> C4[Carry target path = ", safe_mermaid_text(if (nzchar(target_hss5)) target_hss5 else selected_hss), "]"),
                  "C4 --> D1{Apply ordered HSS1_5 rules: TSC then CHESS translation}"
        )

        if (target_hss5 == "1") {
          path <- c(path,
                    paste0("D1 -- ", yn(cond_tsc1, if (!is.na(stcc_val)) paste0("STCC<10 (", fmt_num(stcc_val), ")") else "STCC<10"), " --> FINAL[Final output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (target_hss5 == "2") {
          path <- c(path,
                    paste0("D1 -- ", yn(cond_tsc2, paste0("BA5<BAMIN and TPA>=300", if (!is.na(ba5_adj) && !is.na(bamin_adj) && !is.na(tpa_val)) paste0(" (", fmt_num(ba5_adj), "<", fmt_num(bamin_adj), "; TPA=", fmt_num(tpa_val), ")") else "")), " --> FINAL[Final output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (target_hss5 %in% c("3A", "3B", "3C")) {
          bin_txt <- if (target_hss5 == "3A") "STCC < 40" else if (target_hss5 == "3B") "40 <= STCC < 70" else "STCC >= 70"
          path <- c(path,
                    paste0("D1 -- ", yn(cond_tsc3 || cond_tsc4, "TSC in {3,4}"), " --> D2[TSC = 3 or 4]"),
                    paste0("D2 --> D3{Select stage-3 STCC bin: ", bin_txt, if (!is.na(stcc_val)) paste0(" (STCC=", fmt_num(stcc_val), ")") else "", "}"),
                    paste0("D3 --> FINAL[Final output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (target_hss5 %in% c("4A", "4B", "4C")) {
          bin_txt <- if (target_hss5 == "4A") "STCC < 40" else if (target_hss5 == "4B") "40 <= STCC < 70" else "STCC >= 70"
          path <- c(path,
                    paste0("D1 -- ", yn(cond_tsc5 || cond_tsc6, "TSC in {5,6}"), " --> D2[TSC = 5 or 6]"),
                    paste0("D2 --> D3{Select stage-4 STCC bin: ", bin_txt, if (!is.na(stcc_val)) paste0(" (STCC=", fmt_num(stcc_val), ")") else "", "}"),
                    paste0("D3 --> FINAL[Final output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (target_hss5 == "5") {
          path <- c(path,
                    paste0("D1 -- ", yn(cond_tsc5 || cond_tsc6, "TSC in {5,6}"), " --> D2[Base mature CHESS = 41/42/43 by STCC]"),
                    paste0("D2 --> D3{Apply OGSC override: OGSC >= 42 -> CHESS 50", if (!is.na(ogsc_val)) paste0(" ; OGSC=", fmt_num(ogsc_val)) else "", "}"),
                    paste0("D3 --> FINAL[Final output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else {
          path <- c(path,
                    "D1 --> D2[Unable to map selected output to a valid HSS1_5 translated stage from row decisions]",
                    paste0("D2 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        }

        return(mk(path))
      }

      if (region_key == "3" && attr_key %in% c("DOM_TYPE", "DCC1", "XDCC1", "DCC2", "XDCC2")) {
        dom_val <- toupper(trimws(as.character(get_row_text(c("DOM_TYPE", "DOMTYPE"), ""))))
        if (!nzchar(dom_val) && attr_key == "DOM_TYPE") dom_val <- toupper(trimws(as.character(selected_value %||% "")))

        selected_dom <- toupper(trimws(as.character(selected_value %||% "")))
        open_stand <- !is.na(stcc) && !is.na(tpa) && stcc < 10 && tpa < 100

        main_dom <- if (nzchar(dom_val)) dom_val else selected_dom
        dom_tokens <- trimws(strsplit(main_dom, "_", fixed = TRUE)[[1]])
        dom_tokens <- dom_tokens[nzchar(dom_tokens)]

        is_species_token <- function(x) grepl("^[A-Z][A-Z0-9]{3,6}$", x)
        is_genus_token <- function(x) grepl("^[A-Z]{3}$", x)
        fallback_codes <- c("TDMX", "TEDX", "TETX", "TEIX")

        dom_path <- "fallback"
        if (open_stand || identical(main_dom, "NVG") || identical(selected_dom, "NVG")) {
          dom_path <- "open"
        } else if (main_dom %in% fallback_codes || selected_dom %in% fallback_codes) {
          dom_path <- "fallback"
        } else if (length(dom_tokens) == 1) {
          if (is_species_token(dom_tokens[[1]])) {
            dom_path <- "lead11"
          } else if (is_genus_token(dom_tokens[[1]])) {
            dom_path <- "lead13"
          } else {
            dom_path <- "fallback"
          }
        } else if (length(dom_tokens) >= 2) {
          t1_species <- is_species_token(dom_tokens[[1]])
          t2_species <- is_species_token(dom_tokens[[2]])
          t1_genus <- is_genus_token(dom_tokens[[1]])
          t2_genus <- is_genus_token(dom_tokens[[2]])

          if (t1_species && t2_species) {
            dom_path <- "lead12"
          } else if (t1_genus && t2_genus) {
            dom_path <- "lead15"
          } else {
            dom_path <- "lead14"
          }
        }

        path <- c(
          "A[Call Region 3 dominant-type helper domTypeR3] --> B[Get corrected stand canopy cover and trees per acre]",
          paste0("B --> B1[Carried value: STCC = ", fmt_num(stcc), ", TPA = ", fmt_num(tpa), "]"),
          paste0("B1 --> B2[Carried value: DOM_TYPE context = ", safe_mermaid_text(if (nzchar(main_dom)) main_dom else "unknown"), "]"),
          paste0("B2 --> C{Open-stand gate: STCC < 10 and TPA < 100?}"),
          paste0("C -- ", yn(dom_path == "open", paste0("STCC = ", fmt_num(stcc), ", TPA = ", fmt_num(tpa))), " --> D")
        )

        if (dom_path == "open") {
          path <- c(path,
                    "D[Open stand path: DOMTYPE is NVG]",
                    paste0("D --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
          return(mk(path))
        }

        path <- c(path,
                  "D[Enter LEAD 11-18 sequence] --> E{LEAD 11: single species canopy >= 60%?}")

        if (dom_path == "lead11") {
          path <- c(path,
                    "E -- yes --> F[LEAD 11 selected: dominant type from top species]",
                    paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else {
          path <- c(path,
                    "E -- no --> F{LEAD 12: top two species each >= 20% and sum >= 80%?}")

          if (dom_path == "lead12") {
            path <- c(path,
                      "F -- yes --> G[LEAD 12 selected: two-species dominant type]",
                      paste0("G --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else {
            path <- c(path,
                      "F -- no --> G{LEAD 13: single genus canopy >= 60%?}")

            if (dom_path == "lead13") {
              path <- c(path,
                        "G -- yes --> H[LEAD 13 selected: single-genus dominant type]",
                        paste0("H --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
              )
            } else {
              path <- c(path,
                        "G -- no --> H{LEAD 14: top species + mutually exclusive genus >= 80% and each >= 20%?}")

              if (dom_path == "lead14") {
                path <- c(path,
                          "H -- yes --> I[LEAD 14 selected: species-genus dominant type]",
                          paste0("I --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
                )
              } else {
                path <- c(path,
                          "H -- no --> I{LEAD 15: top two genera each >= 20% and sum >= 80%?}")

                if (dom_path == "lead15") {
                  path <- c(path,
                            "I -- yes --> J[LEAD 15 selected: two-genus dominant type]",
                            paste0("J --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
                  )
                } else {
                  path <- c(path,
                            "I -- no --> J[Fallback path: apply evergreen-deciduous and shade-tolerance rules (LEAD 6-7 and 16-18)]",
                            paste0("J --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
                  )
                }
              }
            }
          }
        }

        return(mk(path))
      }

      if (region_key == "3" && attr_key %in% c("CAN_SIZCL", "CAN_SZTMB", "CAN_SZWDL")) {
        mode_type <- if (attr_key == "CAN_SZTMB") 2L else if (attr_key == "CAN_SZWDL") 3L else 1L
        mode_label <- if (mode_type == 2L) "timberland" else if (mode_type == 3L) "woodland" else "general"

        out_num <- suppressWarnings(as.integer(trimws(as.character(selected_value %||% ""))))
        open_gate <- !is.na(stcc) && !is.na(tpa) && stcc < 10 && tpa < 100
        sparse_gate <- !is.na(stcc) && !is.na(tpa) && stcc < 10 && tpa >= 100

        path <- c(
          "A[Call Region 3 canopy-size helper canSizCl] --> B[Determine canopy-size mode from selected attribute]",
          paste0("B --> B1[Carried value: mode = ", mode_label, " ; STCC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa), "]"),
          "B1 --> C{Is STCC < 10 and TPA < 100?}",
          paste0("C -- ", yn(open_gate, paste0("STCC = ", fmt_num(stcc), ", TPA = ", fmt_num(tpa))), " --> D")
        )

        if (out_num == 0L || open_gate) {
          path <- c(path,
                    "D[Open-stand branch returns canopy size class 0]",
                    paste0("D --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
          return(mk(path))
        }

        path <- c(path,
                  "D --> E{Is STCC < 10 and TPA >= 100?}",
                  paste0("E -- ", yn(sparse_gate, paste0("STCC = ", fmt_num(stcc), ", TPA = ", fmt_num(tpa))), " --> F"))

        if (out_num == 1L && sparse_gate) {
          path <- c(path,
                    "F[Sparse-stocked branch returns canopy size class 1]",
                    paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
          return(mk(path))
        }

        path <- c(path,
                  "F[Density gates not triggered; aggregate TREECC by getCanSizeDC diameter bins] --> G[Pick initial class from highest canopy bin]",
                  "G --> H{Apply mode-specific promotion adjustments?}")

        if (mode_type == 2L) {
          path <- c(path,
                    "H -- yes --> I[Timberland mode: if initial class is 1 or 2 and larger bins carry enough canopy, promote class]",
                    paste0("I --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (mode_type == 3L) {
          path <- c(path,
                    "H -- yes --> I[Woodland mode: if initial class is 1 and bins 2-5 dominate, promote class]",
                    paste0("I --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else {
          path <- c(path,
                    "H -- no --> I[General mode uses initial class with no promotion]",
                    paste0("I --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        }

        return(mk(path))
      }

      if (region_key == "3" && attr_key == "BA_STORY") {
        out_num <- suppressWarnings(as.integer(trimws(as.character(selected_value %||% ""))))
        tpa_nonpos <- !is.na(tpa) && tpa <= 0
        open_gate <- !is.na(stcc) && !is.na(tpa) && stcc < 10 && tpa < 100
        sparse_gate <- !is.na(stcc) && !is.na(tpa) && stcc < 10 && tpa >= 100

        path <- c(
          "A[Call Region 3 BA_STORY helper baStory] --> B[Read stand context values]",
          paste0("B --> B1[Carried value: STCC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa), " ; BA = ", fmt_num(ba), "]"),
          "B1 --> C{Is TPA <= 0?}",
          paste0("C -- ", yn(tpa_nonpos, paste0("TPA = ", fmt_num(tpa))), " --> D[No-stock shortcut returns story class 0]")
        )

        if (tpa_nonpos) {
          path <- c(path,
                    paste0("D --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
          return(mk(path))
        }

        path <- c(path,
                  "D --> E{Is STCC < 10 and TPA < 100?}",
                  paste0("E -- ", yn(open_gate, paste0("STCC = ", fmt_num(stcc), ", TPA = ", fmt_num(tpa))), " --> F[Open-stand branch returns story class 0]"))

        if (out_num == 0L && open_gate) {
          path <- c(path,
                    paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
          return(mk(path))
        }

        path <- c(path,
                  "F --> G{Is STCC < 10 and TPA >= 100?}",
                  paste0("G -- ", yn(sparse_gate, paste0("STCC = ", fmt_num(stcc), ", TPA = ", fmt_num(tpa))), " --> H[Sparse-stocked branch returns story class 1]"))

        if (out_num == 1L && sparse_gate) {
          path <- c(path,
                    paste0("H --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
          return(mk(path))
        }

        path <- c(path,
                  "H[Else branch sets provisional story class 3] --> I{Is BA from DBH >= 24 inches at least 70% of total BA?}")

        if (out_num == 1L) {
          path <- c(path,
                    "I -- yes --> J[Single-story branch selected (story class 1)]",
                    paste0("J --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (out_num == 2L) {
          path <- c(path,
                    "I -- no --> J[Slide 8-inch DBH window from 0 to 24 and recompute BA share each step]",
                    "J --> K{Any window BA share >= 60% and < 70%?}",
                    "K -- yes --> L[Two-story branch selected (story class 2)]",
                    paste0("L --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (out_num == 3L) {
          path <- c(path,
                    "I -- no --> J[Slide 8-inch DBH window from 0 to 24 and recompute BA share each step]",
                    "J --> K{Any window BA share >= 70%?}",
                    "K -- no --> L{Any window BA share >= 60% and < 70%?}",
                    "L -- no --> M[Keep provisional multistory class 3]",
                    paste0("M --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else {
          path <- c(path,
                    "I --> J[Apply sliding-window BA checks to finalize BA_STORY class]",
                    paste0("J --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        }

        return(mk(path))
      }

      if (region_key == "1" && attr_key == "DOM6040") {
        rule_tpa <- !is.na(ba) && !is.na(tpa) && ba < 20 && tpa > 100
        rule_ba <- !is.na(ba) && ba > 20
        is_none <- selected_value == "NONE"
        is_mix_pair <- grepl("-", selected_value, fixed = TRUE)
        is_mix_only <- selected_value %in% c("TMIX", "IMIX", "HMIX")

        path <- c(
          "A[Call Region 1 DOM6040 logic] --> B[Get stand basal area and trees per acre]",
          paste0("B --> B_VAL1[Carried value: Basal Area = ", fmt_num(ba), "]"),
          paste0("B_VAL1 --> B_VAL2[Carried value: Trees Per Acre = ", fmt_num(tpa), "]"),
          "B_VAL2 --> C{Is BA < 20 and TPA > 100?}",
          paste0("C -- ", yn(rule_tpa), " --> D")
        )

        if (rule_tpa) {
          path <- c(path,
                    "D[Result: Use TPA-weighted dominance path]",
                    "D --> E[Call computeDominance6040 with TPA proportions]",
                    "E --> F[Sort species by proportion, break ties with height/diameter]",
                    "F --> G{Is top species proportion at least 60 percent?}",
                    paste0("G -- ", yn(!is_mix_pair && !is_mix_only && !is_none, "top species path"), " --> H[Result: map top species code]"),
                    paste0("G -- ", yn(is_mix_pair || is_mix_only, "mixed subclass path"), " --> I[Result: compute TMIX IMIX HMIX subclass from grouped proportions]"),
                    paste0("I --> I1[If top species proportion at least 40 percent prepend species code to subclass else keep subclass only]"),
                    paste0("H --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"),
                    paste0("I1 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else {
          path <- c(path,
                    "D{Is BA > 20?}",
                    paste0("D -- ", yn(rule_ba), " --> E")
          )
          if (rule_ba) {
            path <- c(path,
                      "E[Result: Use BA-weighted dominance path]",
                      "E --> F[Call computeDominance6040 with BA proportions]",
                      "F --> G[Sort species by proportion, break ties with height/diameter]",
                      "G --> H{Is top species proportion at least 60 percent?}",
                      paste0("H -- ", yn(!is_mix_pair && !is_mix_only && !is_none, "top species path"), " --> I[Result: map top species code]"),
                      paste0("H -- ", yn(is_mix_pair || is_mix_only, "mixed subclass path"), " --> J[Result: compute TMIX IMIX HMIX subclass from grouped proportions]"),
                      paste0("J --> J1[If top species proportion at least 40 percent prepend species code to subclass else keep subclass only]"),
                      paste0("I --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"),
                      paste0("J1 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else {
            path <- c(path,
                      "E[Result: Dominance is NONE]",
                      "E --> FINAL[Set output value = NONE]"
            )
          }
        }
        return(mk(path))
      }

      if (region_key == "1" && attr_key == "VERTICAL_STRUCTURE") {
        low_low <- !is.na(ba) && !is.na(tpa) && ba < 20 && tpa < 100
        low_high <- !is.na(ba) && !is.na(tpa) && ba < 20 && tpa >= 100

        path <- c(
          "A[Call Region 1 vertical structure logic] --> B[Get stand basal area and trees per acre]",
          paste0("B --> B_VAL1[Carried value: Basal Area = ", fmt_num(ba), "]"),
          paste0("B_VAL1 --> B_VAL2[Carried value: Trees Per Acre = ", fmt_num(tpa), "]"),
          "B_VAL2 --> C{Is BA < 20 and TPA < 100?}",
          paste0("C -- ", yn(low_low), " --> D")
        )

        if (low_low) {
          path <- c(path,
                    "D[Result: Vertical structure is NONE]",
                    "D --> FINAL[Set output value = NONE]"
          )
        } else {
          path <- c(path,
                    "D{Is BA < 20 and TPA >= 100?}",
                    paste0("D -- ", yn(low_high), " --> E")
          )
          if (low_high) {
            path <- c(path,
                      "E[Result: Single-layer structure (class 1)]",
                      "E --> FINAL[Set output value = 1]"
            )
          } else {
            path <- c(path,
                      "E[Result: Compute multi-layer structure from diameter distribution]",
                      "E --> F[Call computeVerticalStructure helper]",
                      "F --> G[Test for separated peaks in BA by diameter class to count layers]",
                      paste0("G --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          }
        }
        return(mk(path))
      }

      if (region_key == "1" && attr_key == "SIZECLASS_NTG") {
        path <- c(
          "A[Call Region 1 size class logic] --> B[Get stand basal-area-weighted diameter]",
          paste0("B --> B_VAL[Carried value: BA-weighted diameter = ", fmt_num(dbh_wt), "]"),
          "B_VAL --> C[Apply fixed diameter bins to assign size class]",
          paste0("C --> D[Result: Value falls into bin '", safe_mermaid_text(selected_value), "']"),
          paste0("D --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )
        return(mk(path))
      }

      if (region_key == "8" && attr_key == "VEGCLASS") {
        low_ba <- !is.na(ba) && ba < 10
        nmba <- get_row_num(c("NMBA"))
        pwba <- get_row_num(c("PWBA"))
        stba <- get_row_num(c("STBA"))
        size_pool_values <- c(
          NMBA = if (is.na(nmba)) -Inf else nmba,
          PWBA = if (is.na(pwba)) -Inf else pwba,
          STBA = if (is.na(stba)) -Inf else stba
        )
        largest_size_pool <- if (all(is.infinite(size_pool_values))) {
          NA_character_
        } else {
          names(size_pool_values)[which.max(size_pool_values)]
        }
        largest_size_ba <- if (is.na(largest_size_pool)) {
          NA_real_
        } else if (largest_size_pool == "NMBA") {
          nmba
        } else if (largest_size_pool == "PWBA") {
          pwba
        } else {
          stba
        }
        size_code <- if (nzchar(selected_value)) substr(selected_value, 1, 1) else "unknown"
        den_code <- if (nchar(selected_value) >= 2) substr(selected_value, nchar(selected_value), nchar(selected_value)) else "unknown"

        path <- c(
          "A[Call Region 8 size-density logic] --> B[Get stand basal area and trees per acre]",
          paste0("B --> B_VAL1[Carried value: Basal Area = ", fmt_num(ba), "]"),
          paste0("B_VAL1 --> B_VAL2[Carried value: Trees Per Acre = ", fmt_num(tpa), "]"),
          "B_VAL2 --> C{Is stand basal area < 10?}",
          paste0("C -- ", yn(low_ba, paste0("BA = ", fmt_num(ba), " vs threshold 10")), " --> D")
        )

        if (low_ba) {
          path <- c(path,
                    "D[Result: Use advanced regeneration size class pathway]",
                    paste0("D --> D_VAL[Carried value: size class fixed to 1 because BA < 10]"),
                    "D_VAL --> E[Assign density class from TPA thresholds A B C D using cutoffs 200 400 600]",
                    paste0("E --> E_VAL[Carried value: density class = ", safe_mermaid_text(den_code), "]"),
                    paste0("E_VAL --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else {
          path <- c(path,
                    "D[Result: Use non-merchantable, pulpwood, or sawtimber size pathway]",
                    paste0("D --> E{Which size pool has the largest BA among NMBA=", fmt_num(nmba), ", PWBA=", fmt_num(pwba), ", STBA=", fmt_num(stba), "?}"),
                    paste0("E -- ", safe_mermaid_text(paste0("largest pool = ", largest_size_pool, ", BA = ", fmt_num(largest_size_ba))), " --> E_PICK[Choose size class from largest BA group: non-merchantable (NMBA)=2, pulpwood (PWBA)=3, sawtimber (STBA)=4]"),
                    paste0("E_PICK -- ", safe_mermaid_text(paste0("determined size class = ", size_code)), " --> G[Assign density class from BA ranges using stand BA: A if BA < 40, B if 40 <= BA < 80, C if 80 <= BA < 120, D if BA >= 120]"),
                    paste0("G -- ", safe_mermaid_text(paste0("stand BA = ", fmt_num(ba), ", assigned density class = ", den_code)), " --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        }
        return(mk(path))
      }

      if (region_key == "8" && attr_key == "SSDOMSPP") {
        sstpa_total <- get_row_num(c("SSTPA"))
        is_none <- selected_value == "NONE"
        dom_parts <- trimws(strsplit(selected_value, "-", fixed = TRUE)[[1]])
        dom_parts <- dom_parts[nzchar(dom_parts)]
        n_parts <- length(dom_parts)
        looks_single <- n_parts == 1 && !is_none
        looks_pair <- n_parts == 2
        no_pool <- !is.na(sstpa_total) && sstpa_total <= 0

        path <- c(
          "A[Call Region 8 dominance helper domTypeR8] --> B[Read species-level SSTPA values and sort descending]",
          paste0("B --> B_VAL[Carried value: SSTPA = ", fmt_num(sstpa_total), "]"),
          "B_VAL --> C{Is total SSTPA less than or equal to 0?}",
          paste0("C -- ", yn(no_pool), " --> D")
        )

        if (no_pool || is_none) {
          path <- c(path,
                    "D[No trees in the advanced-regeneration group, so SSDOMSPP is NONE]",
                    "D --> FINAL[Set output value = NONE]")
          return(mk(path))
        }

        path <- c(path,
                  "D[Trees are present in the advanced-regeneration group] --> E{Does the top species contribute at least 70% of total SSTPA?}",
                  paste0("E -- ", yn(looks_single, "single-species branch"), " --> F"))

        if (looks_single) {
          path <- c(path,
                    "F[Single-species dominance selected]",
                    paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"))
        } else {
          path <- c(path,
                    "F[Top species is less than 70% of total SSTPA] --> G{Do the top two species together contribute at least 70% of total SSTPA?}",
                    paste0("G -- ", yn(looks_pair, "two-species branch"), " --> H"))
          if (looks_pair) {
            path <- c(path,
                      "H[Two-species dominance selected]",
                      paste0("H --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"))
          } else {
            path <- c(path,
                      "H[Neither 70% test passed, so use the top three species in rank order]",
                      paste0("H --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"))
          }
        }

        return(mk(path))
      }

      if (region_key == "8" && attr_key %in% c("NMDOMSPP", "PWDOMSPP", "STDOMSPP", "DOMTYPE")) {
        pool_total <- if (attr_key == "NMDOMSPP") {
          get_row_num(c("NMBA"))
        } else if (attr_key == "PWDOMSPP") {
          get_row_num(c("PWBA"))
        } else if (attr_key == "STDOMSPP") {
          get_row_num(c("STBA"))
        } else {
          get_row_num(c("BA"))
        }

        total_label <- if (attr_key == "NMDOMSPP") {
          "NMBA"
        } else if (attr_key == "PWDOMSPP") {
          "PWBA"
        } else if (attr_key == "STDOMSPP") {
          "STBA"
        } else {
          "BA"
        }

        is_none <- selected_value == "NONE"
        dom_parts <- trimws(strsplit(selected_value, "-", fixed = TRUE)[[1]])
        dom_parts <- dom_parts[nzchar(dom_parts)]
        n_parts <- length(dom_parts)
        looks_single <- n_parts == 1 && !is_none
        looks_pair <- n_parts == 2
        no_pool <- !is.na(pool_total) && pool_total <= 0

        path <- c(
          "A[Call Region 8 dominant-type helper domTypeR8] --> B[Build species vectors from attrList by group and sort descending]",
          paste0("B --> B_VAL[Carried value: ", total_label, " = ", fmt_num(pool_total), "]"),
          paste0("B_VAL --> C{Is ", total_label, " less than or equal to 0?}")
        )

        if (attr_key != "DOMTYPE") {
          if (no_pool || is_none) {
            path <- c(path,
                      paste0("C -- yes --> D[No trees in the ", total_label, " group, so the output is NONE]"),
                      "D --> FINAL[Set output value = NONE]")
            return(mk(path))
          }

          path <- c(path,
                    paste0("C -- no --> D{Does the top species contribute at least 70% of total ", total_label, "?}"),
                    paste0("D -- ", yn(looks_single, "single-species branch"), " --> E"))

          if (looks_single) {
            path <- c(path,
                      "E[Single-species dominance selected]",
                      paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"))
          } else {
            path <- c(path,
                      paste0("E[Top species is less than 70% of total ", total_label, "] --> F{Do the top two species together contribute at least 70% of total ", total_label, "?}"),
                      paste0("F -- ", yn(looks_pair, "two-species branch"), " --> G"))
            if (looks_pair) {
              path <- c(path,
                        "G[Two-species dominance selected]",
                        paste0("G --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"))
            } else {
              path <- c(path,
                        "G[Neither 70% test passed, so use the three-species fallback]",
                        paste0("G --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"))
            }
          }

          return(mk(path))
        }

        path <- c(path,
                  "C -- no --> D{Does the top species contribute at least 70% of stand BA?}",
                  paste0("D -- ", yn(looks_single, "single-species branch"), " --> E"))

        if (looks_single) {
          path <- c(path,
                    "E[Single-species stand dominance selected]",
                    paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"))
        } else {
          path <- c(path,
                    "E[Top species is less than 70% of stand BA] --> F{Do the top two species together contribute at least 70% of stand BA?}",
                    paste0("F -- ", yn(looks_pair, "two-species branch"), " --> G"))
          if (looks_pair) {
            path <- c(path,
                      "G[Two-species stand dominance selected]",
                      paste0("G --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"))
          } else {
            path <- c(path,
                      "G[Three-species stand dominance fallback selected]",
                      paste0("G --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"))
          }
        }

        return(mk(path))
      }

      if (region_key == "8" && attr_key %in% c("SSSIZE", "SSTPA", "NMSIZE", "NMBA", "PWSIZE", "PWBA", "STSIZE", "STBA")) {
        sstpa <- get_row_num(c("SSTPA"))
        nmba <- get_row_num(c("NMBA"))
        pwba <- get_row_num(c("PWBA"))
        stba <- get_row_num(c("STBA"))

        if (attr_key == "SSTPA") {
          return(mk(c(
            "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
            "B --> C[Keep only trees in the advanced-regeneration group with DBH less than 1.5 inches]",
            "C --> D[Add TEXPF to the SSTPA accumulator for each matching tree]",
            "D --> E[Continue loop until all trees are processed]",
            paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )))
        }

        if (attr_key == "SSSIZE") {
          if (!is.na(sstpa) && sstpa > 0) {
            return(mk(c(
              "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
              "B --> C[Keep only trees in the advanced-regeneration group with DBH less than 1.5 inches]",
              "C --> D[Accumulate weighted height as sum(HT * TEXPF) across matching trees]",
              "D --> E[Accumulate SSTPA as sum(TEXPF) across matching trees]",
              paste0("E --> E_VAL[Carried value: SSTPA = ", fmt_num(sstpa), "]"),
              "E_VAL --> F[Compute SSSIZE = sum(HT * TEXPF) / SSTPA]",
              paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )))
          }

          return(mk(c(
            "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
            "B --> C[Keep only trees in the advanced-regeneration group with DBH less than 1.5 inches]",
            "C --> D[Accumulate SSTPA as sum(TEXPF) across matching trees]",
            paste0("D --> D_VAL[Carried value: SSTPA = ", fmt_num(sstpa), "]"),
            "D_VAL --> E[Because SSTPA is zero, the weighted-mean equation cannot be applied and SSSIZE stays 0]",
            paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )))
        }

        if (attr_key == "NMBA") {
          return(mk(c(
            "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
            "B --> C{DBH < 1.5 inches?}",
            "C -- no --> D{DBH >= 1.5 and vol1 <= 0?}",
            "D -- yes --> E[Add TREEBA to NMBA accumulator]",
            "E --> F[Continue loop until all trees are processed]",
            paste0("F --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )))
        }

        if (attr_key == "NMSIZE") {
          if (!is.na(nmba) && nmba > 0) {
            return(mk(c(
              "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
              "B --> C[Keep only non-merchantable trees where DBH >= 1.5 and vol1 <= 0]",
              "C --> D[Accumulate BAWTD and NMBA across kept trees]",
              paste0("D --> D_VAL[Carried value: NMBA = ", fmt_num(nmba), "]"),
              "D_VAL --> E[Compute NMSIZE = accumulated BAWTD / NMBA]",
              paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )))
          }

          return(mk(c(
            "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
            "B --> C[Keep only non-merchantable trees where DBH >= 1.5 and vol1 <= 0]",
            "C --> D[Accumulate NMBA across kept trees]",
            paste0("D --> D_VAL[Carried value: NMBA = ", fmt_num(nmba), "]"),
            "D_VAL --> E[Because NMBA is zero, the ratio is not computed and NMSIZE stays 0]",
            paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )))
        }

        if (attr_key == "PWBA") {
          return(mk(c(
            "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
            "B --> C{DBH < 1.5 inches?}",
            "C -- no --> D{DBH >= 1.5 and vol1 <= 0?}",
            "D -- no --> E{vol1 > 0 and vol2 <= 0?}",
            "E -- yes --> F[Add TREEBA to PWBA accumulator]",
            "F --> G[Continue loop until all trees are processed]",
            paste0("G --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )))
        }

        if (attr_key == "PWSIZE") {
          if (!is.na(pwba) && pwba > 0) {
            return(mk(c(
              "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
              "B --> C{DBH < 1.5 inches?}",
              "C -- no --> D{DBH >= 1.5 and vol1 <= 0?}",
              "D -- no --> E{vol1 > 0 and vol2 <= 0?}",
              "E -- yes --> F[Add BAWTD to PWSIZE numerator and TREEBA to PWBA]",
              "F --> G[Continue loop until all trees are processed]",
              paste0("G --> G_VAL[Carried value: PWBA = ", fmt_num(pwba), "]"),
              "G_VAL --> H[PWBA is positive, so finalize PWSIZE = accumulated BAWTD / PWBA]",
              paste0("H --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )))
          }

          return(mk(c(
            "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
            "B --> C{DBH < 1.5 inches?}",
            "C -- no --> D{DBH >= 1.5 and vol1 <= 0?}",
            "D -- no --> E{vol1 > 0 and vol2 <= 0?}",
            "E -- yes --> F[Add BAWTD to PWSIZE numerator and TREEBA to PWBA]",
            "F --> G[Continue loop until all trees are processed]",
            paste0("G --> G_VAL[Carried value: PWBA = ", fmt_num(pwba), "]"),
            "G_VAL --> H[PWBA is zero, so PWSIZE stays 0]",
            paste0("H --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )))
        }

        if (attr_key == "STBA") {
          return(mk(c(
            "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
            "B --> C{DBH < 1.5 inches?}",
            "C -- no --> D{DBH >= 1.5 and vol1 <= 0?}",
            "D -- no --> E{vol1 > 0 and vol2 <= 0?}",
            "E -- no --> F{vol3 > 0?}",
            "F -- yes --> G[Add TREEBA to STBA accumulator]",
            "G --> H[Continue loop until all trees are processed]",
            paste0("H --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )))
        }

        if (attr_key == "STSIZE") {
          if (!is.na(stba) && stba > 0) {
            return(mk(c(
              "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
              "B --> C{DBH < 1.5 inches?}",
              "C -- no --> D{DBH >= 1.5 and vol1 <= 0?}",
              "D -- no --> E{vol1 > 0 and vol2 <= 0?}",
              "E -- no --> F{vol3 > 0?}",
              "F -- yes --> G[Add TREEBA to STBA and BAWTD to the STSIZE numerator]",
              "G --> H[Continue loop until all trees are processed]",
              paste0("H --> H_VAL[Carried value: STBA = ", fmt_num(stba), "]"),
              "H_VAL --> I[Compute STSIZE = accumulated BAWTD / STBA]",
              paste0("I --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )))
          }

          return(mk(c(
            "A[Call Region 8 plotAttr helper] --> B[Loop through each included tree and apply Region 8 group tests]",
            "B --> C{DBH < 1.5 inches?}",
            "C -- no --> D{DBH >= 1.5 and vol1 <= 0?}",
            "D -- no --> E{vol1 > 0 and vol2 <= 0?}",
            "E -- no --> F{vol3 > 0?}",
            "F -- yes --> G[Add TREEBA to STBA and BAWTD to the STSIZE numerator]",
            "G --> H[Continue loop until all trees are processed]",
            paste0("H --> H_VAL[Carried value: STBA = ", fmt_num(stba), "]"),
            "H_VAL --> I[Because STBA is zero, the ratio is not computed and STSIZE stays 0]",
            paste0("I --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )))
        }

        return(NULL)
      }

      if (region_key == "MPSG" && attr_key == "TREE_SIZE_CLASS") {
        path <- c(
          "A[Call MPSG tree size class logic] --> B[Get stand basal-area-weighted diameter]",
          paste0("B --> B_VAL[Carried value: BA-weighted diameter = ", fmt_num(dbh_wt), "]"),
          "B_VAL --> C[Apply MPSG diameter bins to assign size class]",
          "C --> D[Bins: <5=s, 5-10=p, 10-15=m, 15-20=l, >=20=v]",
          paste0("D --> E[Result: Value falls into bin '", safe_mermaid_text(selected_value), "']"),
          paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )
        return(mk(path))
      }

      if (region_key == "MPSG" && attr_key == "CROWN_CLASS") {
        path <- c(
          "A[Call MPSG crown class logic] --> B[Get total stand canopy cover]",
          paste0("B --> B_VAL[Carried value: total stand canopy cover = ", fmt_num(stcc), "]"),
          "B_VAL --> C[Apply MPSG canopy cover bins to assign crown class]",
          "C --> D[Bins: <10=n, 10-40=o, 40-60=m, >=60=c]",
          paste0("D --> E[Result: Value falls into bin '", safe_mermaid_text(selected_value), "']"),
          paste0("E --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )
        return(mk(path))
      }

      if (region_key == "MPSG" && attr_key == "VERTICAL_STRUCTURE") {
        vsel <- toupper(trimws(as.character(selected_value %||% "")))
        tpa_nonpos <- !is.na(tpa) && tpa <= 0
        cc_lt10_tpa_lt100 <- !is.na(stcc) && !is.na(tpa) && stcc < 10 && tpa < 100
        cc_lt10_tpa_ge100 <- !is.na(stcc) && !is.na(tpa) && stcc < 10 && tpa >= 100
        if (vsel == "0") {
          if (tpa_nonpos) {
            path <- c(
              "A[Call MPSG vertical structure logic] --> B[Call baStory helper]",
              paste0("B --> B1[Carried value: CC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa), " ; BA = ", fmt_num(ba), "]"),
              "B1 --> C{baStory rule: Is TPA less than or equal to 0?}",
              paste0("C -- ", yn(TRUE, paste0("TPA = ", fmt_num(tpa))), " --> D[baStory returns 0]"),
              "D --> E{MPSG normalization: Is baStory result NA?}",
              paste0("E -- ", yn(FALSE), " --> FINAL[Set output value = 0]")
            )
          } else if (cc_lt10_tpa_lt100) {
            path <- c(
              "A[Call MPSG vertical structure logic] --> B[Call baStory helper]",
              paste0("B --> B1[Carried value: CC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa), "]"),
              "B1 --> C{baStory rule: Is TPA less than or equal to 0?}",
              paste0("C -- ", yn(FALSE, paste0("TPA = ", fmt_num(tpa))), " --> D{baStory rule: Is CC less than 10 and TPA less than 100?}"),
              paste0("D -- ", yn(TRUE, paste0("CC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa))), " --> E[baStory returns 0]"),
              "E --> F{MPSG normalization: Is baStory result NA?}",
              paste0("F -- ", yn(FALSE), " --> FINAL[Set output value = 0]")
            )
          } else {
            path <- c(
              "A[Call MPSG vertical structure logic] --> B[Call baStory helper]",
              paste0("B --> B1[Carried value: CC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa), " ; BA = ", fmt_num(ba), "]"),
              "B1 --> C{baStory rule checks for 0 or 1 bins all evaluate false on this selected path}",
              "C --> D{MPSG normalization: Is baStory result NA?}",
              paste0("D -- ", yn(TRUE, "selected path implies NA to 0 normalization"), " --> FINAL[Set output value = 0]")
            )
          }
        } else if (vsel == "1") {
          if (cc_lt10_tpa_ge100) {
            path <- c(
              "A[Call MPSG vertical structure logic] --> B[Call baStory helper]",
              paste0("B --> B1[Carried value: CC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa), "]"),
              "B1 --> C{baStory rule: Is TPA less than or equal to 0?}",
              paste0("C -- ", yn(FALSE, paste0("TPA = ", fmt_num(tpa))), " --> D{baStory rule: Is CC less than 10 and TPA less than 100?}"),
              paste0("D -- ", yn(FALSE, paste0("CC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa))), " --> E{baStory rule: Is CC less than 10 and TPA at least 100?}"),
              paste0("E -- ", yn(TRUE, paste0("CC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa))), " --> F[baStory returns 1]"),
              "F --> G{MPSG normalization: Is baStory result equal to 3?}",
              paste0("G -- ", yn(FALSE), " --> FINAL[Set output value = 1]")
            )
          } else {
            path <- c(
              "A[Call MPSG vertical structure logic] --> B[Call baStory helper]",
              paste0("B --> B1[Carried value: CC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa), " ; BA = ", fmt_num(ba), "]"),
              "B1 --> C{baStory rule: Is TPA less than or equal to 0?}",
              paste0("C -- ", yn(FALSE), " --> D{baStory rule: Is CC less than 10 and TPA less than 100?}"),
              paste0("D -- ", yn(FALSE), " --> E{baStory rule: Is CC less than 10 and TPA at least 100?}"),
              paste0("E -- ", yn(FALSE), " --> F{baStory rule: Is DBH at least 24 BA share at least 0.7?}"),
              paste0("F -- ", yn(TRUE, "selected path to output 1"), " --> G[baStory returns 1]"),
              "G --> H{MPSG normalization: Is baStory result equal to 3?}",
              paste0("H -- ", yn(FALSE), " --> FINAL[Set output value = 1]")
            )
          }
        } else if (vsel == "2") {
          path <- c(
            "A[Call MPSG vertical structure logic] --> B[Call baStory helper]",
            paste0("B --> B1[Carried value: CC = ", fmt_num(stcc), " ; TPA = ", fmt_num(tpa), " ; BA = ", fmt_num(ba), "]"),
            "B1 --> C{baStory rule: Is TPA less than or equal to 0?}",
            paste0("C -- ", yn(FALSE), " --> D{baStory rule: Is CC less than 10 and TPA less than 100?}"),
            paste0("D -- ", yn(FALSE), " --> E{baStory rule: Is CC less than 10 and TPA at least 100?}"),
            paste0("E -- ", yn(FALSE), " --> F{baStory rule: Is DBH at least 24 BA share at least 0.7?}"),
            paste0("F -- ", yn(FALSE), " --> G{baStory rule: Is any sliding 8-inch BA share at least 0.7?}"),
            paste0("G -- ", yn(FALSE), " --> H{baStory rule: Is any sliding 8-inch BA share at least 0.6 and less than 0.7?}"),
            paste0("H -- ", yn(TRUE, "selected path to output 2"), " --> I[baStory returns 2]"),
            "I --> J{MPSG normalization: Is baStory result equal to 3?}",
            paste0("J -- ", yn(FALSE), " --> FINAL[Set output value = 2]")
          )
        } else {
          path <- c(
            "A[Call MPSG vertical structure logic] --> B[Call baStory helper and apply MPSG normalization rules]",
            paste0("B --> C[Selected output path for value ", safe_mermaid_text(selected_value), "]"),
            paste0("C --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        }
        return(mk(path))
      }

      if (region_key == "MPSG" && attr_key == "COVERTYPE") {
        ruleset_input <- trimws(as.character(input$MPSGcovTyp %||% ""))
        ruleset_row <- get_row_text(c("MPSGcovTyp"), "")
        ruleset_clean <- if (nzchar(ruleset_input)) ruleset_input else ruleset_row
        ruleset_num <- suppressWarnings(as.integer(ruleset_clean))

        # Accept common text forms entered in the MPSG Cover Type input.
        if (is.na(ruleset_num) && grepl("(^|[^0-9])1([^0-9]|$)|REGION\\s*1|\\bR1\\b", toupper(ruleset_clean), perl = TRUE)) ruleset_num <- 1L
        if (is.na(ruleset_num) && grepl("(^|[^0-9])2([^0-9]|$)|REGION\\s*2|\\bR2\\b", toupper(ruleset_clean), perl = TRUE)) ruleset_num <- 2L
        if (is.na(ruleset_num) && grepl("(^|[^0-9])3([^0-9]|$)|REGION\\s*3|\\bR3\\b", toupper(ruleset_clean), perl = TRUE)) ruleset_num <- 3L

        path <- c(
          "A[Call MPSG cover type logic] --> B[Read total stand canopy cover and MPSG Cover Type input]",
          paste0("B --> B_VAL[Carried value: total stand canopy cover = ", fmt_num(stcc), "]"),
          paste0("B_VAL --> B_VAL2[Carried value: MPSG Cover Type input = ", safe_mermaid_text(ruleset_clean), "]"),
          "B_VAL2 --> C{Is total stand canopy cover < 10%?}",
          paste0("C -- ", yn(!is.na(stcc) && stcc < 10), " --> D")
        )
        if (!is.na(stcc) && stcc < 10) {
          path <- c(
            path,
            "D[Open-canopy shortcut from MPSG.r: COVERTYPE_MPSG is forced to NONE]",
            "D --> FINAL[Set output value = NONE]"
          )
        } else {
          chosen_1 <- !is.na(ruleset_num) && ruleset_num == 1L
          chosen_2 <- !is.na(ruleset_num) && ruleset_num == 2L
          chosen_3 <- !is.na(ruleset_num) && ruleset_num == 3L

          path <- c(
            path,
            "D[Evaluate all MPSG cover type rulesets] --> E{Which ruleset is selected?}",
            paste0("E -- ", yn(chosen_1, "ruleset 1 selected"), " --> F1[Ruleset 1 path starts with R1 helper]"),
            paste0("E -- ", yn(chosen_2, "ruleset 2 selected"), " --> F2[Ruleset 2 path starts with R2 helper]"),
            paste0("E -- ", yn(chosen_3, "ruleset 3 selected"), " --> F3[Ruleset 3 path starts with domTypeR3 helper]"),
            "E -- no --> F4[Unrecognized ruleset input]",
            "F1 --> G1[Call R1 helper to compute DOM6040 and related Region 1 stand outputs]",
            "G1 --> H1[R1 helper calls computeDominance6040 using TPA or BA path based on BA and TPA thresholds]",
            "H1 --> I1[R1 helper maps DOM6040 to COVERTYPE_R1 and applies mixedmesiccon to dryDouglasfir switch by habitat lookup when applicable]",
            "I1 --> J1[Set COVERTYPE_MPSG equal to Region 1 COVERTYPE_R1]",
            "F2 --> G2[Call R2 helper to compute DOM_TYPE_R2 canopy shares and COVERTYPE_R2]",
            "G2 --> H2[R2 helper builds top species ranking then applies white-fir and grouped-species override checks to final family code]",
            "H2 --> I2[R2 helper crosswalks final family code through saf2R2 to get COVERTYPE_R2]",
            "I2 --> J2[Crosswalk COVERTYPE_R2 through R2toMPSG dictionary in MPSG helper]",
            "F3 --> G3[Call domTypeR3 helper to run LEAD sequence for Region 3 dominant type]",
            "G3 --> H3[domTypeR3 helper evaluates sparse stand then species dominance genus dominance and fallback rules]",
            "H3 --> I3[domTypeR3 helper returns DOMTYPE value]",
            "I3 --> J3[Set COVERTYPE_MPSG equal to Region 3 DOMTYPE]",
            "J2 --> K2[Set COVERTYPE_MPSG equal to R2toMPSG crosswalk result]",
            paste0("J1 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"),
            paste0("K2 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"),
            paste0("J3 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"),
            paste0("F4 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        }
        return(mk(path))
      }

      if (region_key == "MPSG" && attr_key %in% c("HSS1_4C", "HSS1_5")) {
        is_14c_mpsg <- attr_key == "HSS1_4C"
        selected_hss_mpsg <- toupper(trimws(as.character(selected_value %||% "")))
        stcc_mpsg <- get_row_num(c("CAN_COV", "CC", "STCC"))
        tpa_mpsg <- get_row_num(c("TPA"))
        qmd_mpsg <- get_row_num(c("QMD", "QMD_STM", "QMD_TOP20"))

        target_hss_mpsg <- if (is_14c_mpsg) {
          if (selected_hss_mpsg %in% c("1", "2", "3A", "3B", "3C", "4A", "4B", "4C")) selected_hss_mpsg else ""
        } else {
          if (selected_hss_mpsg %in% c("1", "2", "3A", "3B", "3C", "4A", "4B", "4C", "5")) selected_hss_mpsg else ""
        }

        path <- c(
          "A[Call HSS helper function] --> B[Get stand attributes]",
          paste0("B --> B1[Carried values: STCC=", if (!is.na(stcc_mpsg)) fmt_num(stcc_mpsg) else "unknown", ", TPA=", if (!is.na(tpa_mpsg)) fmt_num(tpa_mpsg) else "unknown", ", QMD=", if (!is.na(qmd_mpsg)) fmt_num(qmd_mpsg) else "unknown", "]")
        )

        if (is_14c_mpsg) {
          inferred_grp <- if (target_hss_mpsg == "1") {
            "N"
          } else if (target_hss_mpsg == "2") {
            "E"
          } else if (target_hss_mpsg %in% c("3A", "3B", "3C")) {
            "S/M"
          } else if (target_hss_mpsg %in% c("4A", "4B", "4C")) {
            "L/V"
          } else {
            "unknown"
          }

          path <- c(path,
                    "B1 --> C[Call HSS with HSStype 1 for HSS1_4C]",
                    paste0("C --> C1[Selected path = ", safe_mermaid_text(if (nzchar(target_hss_mpsg)) target_hss_mpsg else selected_hss_mpsg), " ; grpSizeClass family = ", inferred_grp, "]"),
                    "C1 --> D1{Decision 1: Is STCC < 10?}"
          )

          if (target_hss_mpsg == "1") {
            path <- c(path,
                      paste0("D1 -- ", yn(!is.na(stcc_mpsg) && stcc_mpsg < 10, if (!is.na(stcc_mpsg)) paste0("STCC = ", fmt_num(stcc_mpsg)) else "STCC from row"), " --> D2[HSS1_4C rule path: grpSizeClass = N]"),
                      paste0("D2 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else if (target_hss_mpsg == "2") {
            path <- c(path,
                      paste0("D1 -- ", yn(!is.na(stcc_mpsg) && stcc_mpsg >= 10, if (!is.na(stcc_mpsg)) paste0("STCC = ", fmt_num(stcc_mpsg)) else "STCC from row"), " --> D2[HSS1_4C rule path: grpSizeClass = E]"),
                      paste0("D2 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else if (target_hss_mpsg %in% c("3A", "3B", "3C")) {
            stcc_bin_txt <- if (target_hss_mpsg == "3A") "0 < STCC < 40" else if (target_hss_mpsg == "3B") "40 <= STCC < 70" else "STCC >= 70"
            path <- c(path,
                      paste0("D1 -- ", yn(!is.na(stcc_mpsg) && stcc_mpsg >= 10, if (!is.na(stcc_mpsg)) paste0("STCC = ", fmt_num(stcc_mpsg)) else "STCC from row"), " --> D2[HSS1_4C rule path: grpSizeClass in S/M]"),
                      paste0("D2 --> D3{Decision 2: STCC bin for stage 3 family = ", stcc_bin_txt, "}"),
                      paste0("D3 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else if (target_hss_mpsg %in% c("4A", "4B", "4C")) {
            stcc_bin_txt <- if (target_hss_mpsg == "4A") "0 < STCC < 40" else if (target_hss_mpsg == "4B") "40 <= STCC < 70" else "STCC >= 70"
            path <- c(path,
                      paste0("D1 -- ", yn(!is.na(stcc_mpsg) && stcc_mpsg >= 10, if (!is.na(stcc_mpsg)) paste0("STCC = ", fmt_num(stcc_mpsg)) else "STCC from row"), " --> D2[HSS1_4C rule path: grpSizeClass in L/V]"),
                      paste0("D2 --> D3{Decision 2: STCC bin for stage 4 family = ", stcc_bin_txt, "}"),
                      paste0("D3 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          } else {
            path <- c(path,
                      "D1 --> D2[Unable to map selected value to a valid MPSG HSS1_4C stage]",
                      paste0("D2 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
            )
          }

          return(mk(path))
        }

        path <- c(path,
                  "B1 --> C[Call HSS with HSStype 2 for HSS1_5]",
                  paste0("C --> C1[Selected path = ", safe_mermaid_text(if (nzchar(target_hss_mpsg)) target_hss_mpsg else selected_hss_mpsg), "]"),
                  "C1 --> D1{Apply ordered HSS1_5 rules: TSC then CHESS translation}"
        )

        if (target_hss_mpsg == "1") {
          path <- c(path,
                    paste0("D1 -- ", yn(!is.na(stcc_mpsg) && stcc_mpsg < 10, if (!is.na(stcc_mpsg)) paste0("STCC<10 (", fmt_num(stcc_mpsg), ")") else "STCC from row"), " --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (target_hss_mpsg == "2") {
          path <- c(path,
                    "D1 -- yes --> D2[TSC 2 path: BA5 < BAMIN and TPA >= 300 in HSS helper]",
                    paste0("D2 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (target_hss_mpsg %in% c("3A", "3B", "3C")) {
          stcc_bin_txt <- if (target_hss_mpsg == "3A") "STCC < 40" else if (target_hss_mpsg == "3B") "40 <= STCC < 70" else "STCC >= 70"
          path <- c(path,
                    "D1 -- yes --> D2[TSC in {3,4} path in HSS helper]",
                    paste0("D2 --> D3{Decision 2: stage-3 STCC bin = ", stcc_bin_txt, "}"),
                    paste0("D3 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (target_hss_mpsg %in% c("4A", "4B", "4C")) {
          stcc_bin_txt <- if (target_hss_mpsg == "4A") "STCC < 40" else if (target_hss_mpsg == "4B") "40 <= STCC < 70" else "STCC >= 70"
          path <- c(path,
                    "D1 -- yes --> D2[TSC in {5,6} mature path in HSS helper]",
                    paste0("D2 --> D3{Decision 2: stage-4 STCC bin = ", stcc_bin_txt, "}"),
                    paste0("D3 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else if (target_hss_mpsg == "5") {
          path <- c(path,
                    "D1 -- yes --> D2[TSC in {5,6} mature path]",
                    "D2 --> D3{Decision 2: Apply OGSC override: OGSC >= 42 sets CHESS 50}",
                    paste0("D3 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        } else {
          path <- c(path,
                    "D1 --> D2[Unable to map selected value to a valid MPSG HSS1_5 stage]",
                    paste0("D2 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
          )
        }

        return(mk(path))
      }

      # Generic detailed fallback for any other output column.
      return(mk(c(
        "A[Read selected output row context] --> B[Carry region attribute and selected value]",
        paste0("B --> C{Does selected value exist for this row?}"),
        paste0("C -- ", yn(nzchar(selected_value), paste0("selected value = ", safe_mermaid_text(selected_value))), " --> D[Carry selected value through attribute specific rule set]"),
        paste0("D --> E[Set output value = ", safe_mermaid_text(selected_value), "]")
      )))
    }

    use_value_specific_fallback <- nzchar(value_norm) && attr != "COVERTYPE_R2"

    if (use_value_specific_fallback) {
      value_specific_other <- get_non_covertype_value_flow(region_norm, attr, value_norm)
      if (!is.null(value_specific_other)) return(value_specific_other)
    }

    if (region_norm == "2") {
      if (attr == "DOM_TYPE_R2") return(mk(c(
        "A[Compute STCC from plotvals ALL CC R/R2.r:186]",
        "A --> B{Is total stand canopy cover less than 10 percent? R/R2.r:187}",
        "B -- yes --> B1[Region 2 dominant species label equals NONE]",
        "B -- no --> C[Build SpeciesCC for each species]",
        "C --> D[Sort SpeciesCC descending ]",
        "D --> E{Are at least three species present with nonzero canopy cover?}",
        "E -- yes --> F[Set dominant label to first species : second species : third species]",
        "E -- no --> G{Are exactly two species present with nonzero canopy cover?}",
        "G -- yes --> H[Set dominant label to first species : second species and set third-species canopy share to zero]",
        "G -- no --> I[Set dominant label to first species only and set second- and third-species canopy shares to zero]"
      )))

      if (attr == "DOM_TYPE_R2_CC1") return(mk(c(
        "A[Call R2] --> B[Compute STCC and SpeciesCC]",
        "B --> C{Is total stand canopy cover less than 10 percent?}",
        "C -- yes --> C1[Region 2 top-species canopy share equals NONE]",
        "C -- no --> D[Sort SpeciesCC descending]",
        "D --> E[Set first canopy-share value from the highest-ranked species]"
      )))

      if (attr == "DOM_TYPE_R2_CC2") return(mk(c(
        "A[Call R2] --> B[Compute STCC and SpeciesCC]",
        "B --> C{Is total stand canopy cover less than 10 percent?}",
        "C -- yes --> C1[Region 2 second-species canopy share equals NONE]",
        "C -- no --> D{Are at least two species present in sorted canopy-share values?}",
        "D -- yes --> E[Set second canopy-share value from the second-ranked species]",
        "D -- no --> F[Set second canopy-share value to zero]"
      )))

      if (attr == "DOM_TYPE_R2_CC3") return(mk(c(
        "A[Call R2] --> B[Compute STCC and SpeciesCC]",
        "B --> C{Is total stand canopy cover less than 10 percent?}",
        "C -- yes --> C1[Region 2 third-species canopy share equals NONE]",
        "C -- no --> D{Are at least three species present in sorted canopy-share values?}",
        "D -- yes --> E[Set third canopy-share value from the third-ranked species]",
        "D -- no --> F[Set third canopy-share value to zero]"
      )))

      if (attr == "COVERTYPE_R2" && nzchar(value_norm)) {
        value_specific <- get_r2_covertype_value_flow(value_norm)
        if (!is.null(value_specific)) return(value_specific)
      }

      if (attr == "COVERTYPE_R2") return(mk(c(
        "A[Call R2] --> B[Resolve first ranked species second ranked species third ranked species and their canopy shares]",
        "B --> C[Assign initial SAF or SRM family code from top-species lookup table]",
        "C --> D{Is top species white fir?}",
        "D -- no --> E[Keep current family code from top-species lookup]",
        "D -- yes --> F{Is second or third species Douglas-fir?}",
        "F -- yes --> G[Set family code to Douglas-fir group T210]",
        "F -- no --> H[Set family code to white-fir group T211]",
        "E --> I[Compute spruce-fir grouped canopy total from top three species]",
        "G --> I",
        "H --> I",
        "I --> I1[Spruce-fir species set used for grouping: PIEN ABLA ABLAA ABAR2 ABBI2]",
        "I1 --> J{Is spruce-fir grouped total greater than each individual top-three canopy share and greater than zero?}",
        "J -- yes --> K[Override family code to spruce-fir group T206]",
        "J -- no --> L[No spruce-fir override]",
        "K --> M[Compute pinyon-juniper grouped canopy total from top three species]",
        "L --> M",
        "M --> M1[Pinyon-juniper species set used for grouping: PIED JUSC2 SAUT3 JUNIP JUOS JUMO]",
        "M1 --> N{Is pinyon-juniper grouped total greater than each individual top-three canopy share and greater than zero?}",
        "N -- yes --> O[Override family code to pinyon-juniper group T239]",
        "N -- no --> P[No pinyon-juniper override]",
        "O --> Q[Compute Douglas-fir grouped canopy total from top three species including white fir]",
        "P --> Q",
        "Q --> Q1[Douglas-fir grouping species set: PSME PSMEG ABCO]",
        "Q1 --> R{Is Douglas-fir grouped total greater than each individual top-three canopy share and greater than zero?}",
        "R -- yes --> S[Override family code to Douglas-fir group T210]",
        "R -- no --> T[No Douglas-fir override]",
        "S --> U{Is top species coded as 2TB or 2TN?}",
        "T --> U{Is top species coded as 2TB or 2TN?}",
        "U -- yes --> V[Force fallback family code T999 for other]",
        "U -- no --> W[Keep family code after exception and grouping decisions]",
        "V --> X[Crosswalk final family code through saf2R2 to Region 2 cover type code]",
        "W --> X"
      )))

      if (attr == "TREE_SIZE_CLASS_R2") return(mk(c(
        "A[Call R2] --> B[Build CCByDiameterClass bins R/R2.r:338-347]",
        "B --> C[Aggregate canopy into seedling-plus-small medium and large-plus-very-large groups]",
        "C --> D[Pick preliminary group from largest canopy share among grouped classes]",
        "D --> E[Break grouped ties by choosing the largest member class such as very-large versus large and small versus seedling]",
        "E --> F{grpSizeClass is NA and STCC LT 10}",
        "F -- yes --> G[TREE_SIZE_CLASS_R2 = n]",
        "F -- no --> H[TREE_SIZE_CLASS_R2 = grpSizeClass]"
      )))

      if (attr == "CROWN_CLASS_R2") return(mk(c(
        "A[Call R2] --> B[Compute STCC]",
        "B --> C{STCC GE 10 and LT 40}",
        "C -- yes --> D[CROWN_CLASS_R2 = 1]",
        "C -- no --> E{STCC GE 40 and LT 70}",
        "E -- yes --> F[CROWN_CLASS_R2 = 2]",
        "E -- no --> G{STCC GE 70}",
        "G -- yes --> H[CROWN_CLASS_R2 = 3]"
      )))

      if (attr %in% c("HSS1_4C", "HSS1_5")) return(mk(c(
        "A[Call R2] --> B[Call HSS helper with HSStype selected by attribute]",
        "B --> C{HSStype equals 1 for one-through-four-C path?}",
        "C -- yes --> D{HSS1_4C decision path}",
        "D --> E{Is STCC less than 10?}",
        "E -- yes --> F[Set grpSizeClass N then HSS = 1]",
        "E -- no --> G[Build canopy bins E,S,M,L,V from DBH and TREECC then compute ES and LV grouped totals]",
        "G --> H[Pick grpSizeClass with LV and ES tie-break rules]",
        "H --> I{grpSizeClass equals E?}",
        "I -- yes --> J[HSS = 2]",
        "I -- no --> K{grpSizeClass is S or M?}",
        "K -- yes --> L[Use STCC bins: less than 40 = 3A, 40 to less than 70 = 3B, at least 70 = 3C]",
        "K -- no --> M[Use STCC bins: less than 40 = 4A, 40 to less than 70 = 4B, at least 70 = 4C]",
        "C -- no --> N{HSS1_5 decision path}",
        "N --> O[Compute BA1 BA5 BA9 BA16 BA125 BA529 and QMD then derive BARAT BAMIN and related thresholds]",
        "O --> P[Assign TSC in code order: 6 then 5 then 4 then 3 then 2 then 1]",
        "P --> Q{TSC in 5 or 6?}",
        "Q -- yes --> R[Map to CHESS 41 42 43 by STCC bins and evaluate OGSC]",
        "R --> S{OGSC at least 42?}",
        "S -- yes --> T[Force CHESS 50 stage 5]",
        "S -- no --> U[Keep CHESS 41 42 or 43]",
        "Q -- no --> V{TSC in 3 or 4?}",
        "V -- yes --> W[Map to CHESS 31 32 33 by STCC bins]",
        "V -- no --> X{TSC equals 2?}",
        "X -- yes --> Y[Map to CHESS 20 stage 2]",
        "X -- no --> Z[If STCC <= 10 map to CHESS 10 stage 1]",
        "T --> AA[Translate CHESS: 10=1,20=2,31=3A,32=3B,33=3C,41=4A,42=4B,43=4C,50=5]",
        "U --> AA",
        "W --> AA",
        "Y --> AA",
        "Z --> AA",
        "F --> AB[Return HSS code from helper]",
        "J --> AB",
        "L --> AB",
        "M --> AB",
        "AA --> AB"
      )))
    }

    if (region_norm == "1") {
      if (attr == "DOM6040") return(mk(c(
        "A[vegOut region 1 gate] --> B[Call R1]",
        "B --> C[Build species vectors for trees per acre proportions basal area proportions basal-area-weighted diameters and heights]",
        "C --> D{BA LT 20 and TPA GT 100 R/R1.r:320}",
        "D -- yes --> E[Call computeDominance6040 using trees-per-acre proportions]",
        "D -- no --> F{BA GT 20 R/R1.r:323}",
        "F -- yes --> G[Call computeDominance6040 using basal-area proportions]",
        "F -- no --> H[DOM6040 = NONE]",
        "E --> E1[Inside computeDominance6040 sort species by proportion then break ties with height and diameter]",
        "E1 --> E2[If top species is at least sixty percent return mapped species group]",
        "E2 --> E3[Otherwise combine species into hardwood mixed mesic or conifer subclasses and apply forty-percent dominant check]",
        "G --> G1[Inside computeDominance6040 repeat same sorting and subclass logic with basal-area inputs]"
      )))

      if (attr == "COVERTYPE_R1") return(mk(c(
        "A[Call R1] --> B[Build species dominance inputs for Region 1 cover type]",
        "B --> C[Compute DOM6040 from trees per acre or basal area dominance rules]",
        "C --> D[Map DOM6040 to the Region 1 cover type dictionary]",
        "D --> E{Is mapped cover type mixedmesiccon?}",
        "E -- no --> F[Use mapped COVERTYPE_R1 as final output]",
        "E -- yes --> G[Check HABTYPE against Hot Dry and Warm Dry habitat sets]",
        "G --> H{Does HABTYPE match the dry Douglas fir override rules?}",
        "H -- yes --> I[Set COVERTYPE_R1 to dryDouglasfir]",
        "H -- no --> F"
      )))

      if (attr == "VERTICAL_STRUCTURE") return(mk(c(
        "A[Call R1] --> B[Compute BA and TPA]",
        "B --> C{BA LT 20 and TPA LT 100 R/R1.r:404}",
        "C -- yes --> D[VERTICAL_STRUCTURE = NONE]",
        "C -- no --> E{BA LT 20 and TPA GE 100 R/R1.r:407}",
        "E -- yes --> F[VERTICAL_STRUCTURE = 1]",
        "E -- no --> G[Call computeVerticalStructure with basal-area-by-diameter proportions]",
        "G --> G1[Inside computeVerticalStructure test separated peaks across diameter classes to count canopy layers]",
        "G1 --> G2[If no clear peaks test adjacent-class blends to find two-layer patterns]",
        "G2 --> G3[If still unresolved classify as continuous layering when many classes exceed threshold; otherwise return one-layer code]"
      )))

      if (attr == "VEGTYPE") return(mk(c(
        "A[Call R1] --> B{HABTYPE available and non-empty R/R1.r:469}",
        "B -- no --> C[VEGTYPE remains NA]",
        "B -- yes --> D[Map HABTYPE to PVT via MapADPtoStSimPVT]",
        "D --> E[Abbreviate COVERTYPE_R1 via dictionary]",
        "E --> F[VEGTYPE = PVT hyphen COVABBR]"
      )))

      if (attr == "SIZECLASS_NTG") return(mk(c(
        "A[Call R2] --> B[Resolve first second and third ranked species and their canopy shares]",
        "B --> C[Assign initial family code from the dominant species lookup table]",
        "C --> D{Is top species white fir?}",
        "D -- no --> E[Keep current family code from the top-species lookup]",
        "D -- yes --> F{Is second or third species Douglas-fir?}",
        "F -- yes --> G[Set family code to Douglas-fir group T210]",
        "F -- no --> H[Set family code to white-fir group T211]",
        "E --> I[Compute grouped canopy totals for spruce fir pinyon juniper and Douglas fir]",
        "G --> I",
        "H --> I",
        "I --> I1[Spruce-fir species set PIEN ABLA ABLAA ABAR2 ABBI2]",
        "I1 --> J{Does spruce-fir grouped total exceed each of the top three canopy shares and stay above zero?}",
        "J -- yes --> K[Override family code to spruce-fir group T206]",
        "J -- no --> L[Keep current family code]",
        "K --> M[Check pinyon-juniper grouped canopy total]",
        "L --> M",
        "M --> M1[Pinyon-juniper species set PIED JUSC2 SAUT3 JUNIP JUOS JUMO]",
        "M1 --> N{Does pinyon-juniper grouped total exceed each of the top three canopy shares and stay above zero?}",
        "N -- yes --> O[Override family code to pinyon-juniper group T239]",
        "N -- no --> P[Keep current family code]",
        "O --> Q[Check Douglas-fir grouped canopy total including white fir]",
        "P --> Q",
        "Q --> Q1[Douglas-fir grouping species set PSME PSMEG ABCO]",
        "Q1 --> R{Does Douglas-fir grouped total exceed each of the top three canopy shares and stay above zero?}",
        "R -- yes --> S[Override family code to Douglas-fir group T210]",
        "R -- no --> T[Keep current family code]",
        "S --> U{Is top species coded as 2TB or 2TN?}",
        "T --> U{Is top species coded as 2TB or 2TN?}",
        "U -- yes --> V[Force fallback family code T999 for other]",
        "U -- no --> W[Keep final family code after all overrides]",
        "V --> X[Crosswalk final family code through saf2R2 to Region 2 cover type code]",
        "W --> X"
      )))

    }

    if (region_norm == "3") {
      if (attr %in% c("DOM_TYPE", "DCC1", "XDCC1", "DCC2", "XDCC2")) return(mk(c(
        "A[Call Region 3 dominant-type helper domTypeR3] --> B[Ensure TREECC exists per tree; if missing compute from crown width and trees per acre]",
        "B --> C[Aggregate canopy to species, genus, shade-tolerance, and leaf-retention groups]",
        paste0("C --> C_VAL1[Carried value: stand canopy cover = ", fmt_num(stcc), "]"),
        paste0("C_VAL1 --> C_VAL2[Carried value: trees per acre = ", fmt_num(tpa), "]"),
        "C_VAL2 --> D{Open-stand gate: corrected canopy cover < 10 and trees per acre < 100?}",
        "D -- yes --> E[Set DOMTYPE to NVG and assign open-stand dominant components]",
        "D -- no --> F[Enter LEAD dominance sequence]",
        "F --> G{LEAD11: is top species canopy share >= 60%?}",
        "G -- yes --> H[Set single-species DOMTYPE and DCC1]",
        "G -- no --> I{LEAD12: are top two species each >= 20% and sum >= 80%?}",
        "I -- yes --> J[Set two-species DOMTYPE with DCC1 and DCC2]",
        "I -- no --> K{LEAD13: is top genus canopy share >= 60%?}",
        "K -- yes --> L[Set single-genus DOMTYPE and dominant components]",
        "K -- no --> M{LEAD14: top species plus first non-matching genus sum >= 80%?}",
        "M -- yes --> N[Call excGenusSp and set species-plus-genus DOMTYPE]",
        "M -- no --> O{LEAD15: do top two genera each >= 20% and sum >= 80%?}",
        "O -- yes --> P[Set two-genus DOMTYPE with DCC1 and DCC2]",
        "O -- no --> Q[Apply LEAD fallback classes using evergreen-deciduous and shade-tolerance rules]",
        "E --> R[Apply correctCC to dominant-component canopy values for XDCC fields]",
        "H --> R",
        "J --> R",
        "L --> R",
        "N --> R",
        "P --> R",
        "Q --> R",
        paste0("R --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
      )))

      if (attr %in% c("CAN_SIZCL", "CAN_SZTMB", "CAN_SZWDL")) return(mk(c(
        "A[Call Region 3 canopy-size helper canSizCl] --> B[Determine mode from attribute: general, timberland, or woodland]",
        paste0("B --> B_VAL1[Carried value: stand canopy cover = ", fmt_num(stcc), "]"),
        paste0("B_VAL1 --> B_VAL2[Carried value: trees per acre = ", fmt_num(tpa), "]"),
        "B_VAL2 --> C{Open-stand rule: canopy cover < 10 and trees per acre < 100?}",
        "C -- yes --> D[Assign canopy size class 0]",
        "C -- no --> E{Sparse-stocked rule: canopy cover < 10 and trees per acre >= 100?}",
        "E -- yes --> F[Assign canopy size class 1]",
        "E -- no --> G[For each tree call getCanSizeDC to map diameter into mode-specific size bins]",
        "G --> H[Accumulate canopy cover by size bin and select highest-cover bin as initial class]",
        "H --> I{Mode is timberland CAN_SZTMB?}",
        "I -- yes --> J[Apply timberland promotion rule when larger-size grouped canopy dominates]",
        "I -- no --> K[Skip timberland promotion]",
        "J --> L{Mode is woodland CAN_SZWDL?}",
        "K --> L",
        "L -- yes --> M[Apply woodland promotion rule when classes 2-5 together exceed class 1]",
        "L -- no --> N[Keep current class]",
        "M --> O[Finalize canopy size class for selected mode]",
        "N --> O",
        paste0("O --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
      )))

    }

    if (region_norm == "8") {
      if (attr == "SSDOMSPP") return(mk(c(
        "A[vegOut Region 8 gate] --> B[Call domTypeR8]",
        "B --> C[Read advanced-regeneration totals from attrList: species SSTPA values and ALL SSTPA]",
        "C --> D[Sort species by SSTPA from highest to lowest]",
        "D --> E{Is ALL SSTPA <= 0?}",
        "E -- yes --> F[No advanced-regeneration trees, so SSDOMSPP = NONE]",
        "E -- no --> G{Is top-species SSTPA >= 70% of ALL SSTPA?}",
        "G -- yes --> H[Set SSDOMSPP to the top species code]",
        "G -- no --> I{Do top two species SSTPA values sum to >= 70% of ALL SSTPA?}",
        "I -- yes --> J[Set SSDOMSPP to species1-species2]",
        "I -- no --> K[Set SSDOMSPP to species1-species2-species3]",
        "F --> L[Store SSDOMSPP in results list]",
        "H --> L",
        "J --> L",
        "K --> L"
      )))

      if (attr %in% c("NMDOMSPP", "PWDOMSPP", "STDOMSPP", "DOMTYPE")) return(mk(c(
        "A[vegOut region 8 gate] --> B[Call domTypeR8]",
        "B --> C[Read ALL totals from attrList: SSTPA NMBA PWBA STBA BA and TPA]",
        "C --> D[Build species vectors excluding ALL using source totals: advanced regeneration from SSTPA, non-merchantable from NMBA, pulpwood from PWBA, sawtimber from STBA, and whole stand from BA]",
        "D --> E[Sort each vector in descending order]",
        "E --> F{For SSDOMSPP is ALL SSTPA <= 0?}",
        "F -- yes --> G[Set SSDOMSPP to NONE]",
        "F -- no --> H{Is top SSTPA species at least 70% of ALL SSTPA?}",
        "H -- yes --> I[Set SSDOMSPP to one species code]",
        "H -- no --> J{Do top two SSTPA species sum to at least 70% of ALL SSTPA?}",
        "J -- yes --> K[Set SSDOMSPP to species1-species2]",
        "J -- no --> L[Set SSDOMSPP to species1-species2-species3]",
        "E --> M{For NMDOMSPP is ALL NMBA <= 0?}",
        "M -- yes --> N[Set NMDOMSPP to NONE]",
        "M -- no --> O[Apply same 70% single then top-two else top-three logic using NMBA vector]",
        "E --> P{For PWDOMSPP is ALL PWBA <= 0?}",
        "P -- yes --> Q[Set PWDOMSPP to NONE]",
        "P -- no --> R[Apply same 70% single then top-two else top-three logic using PWBA vector]",
        "E --> S{For STDOMSPP is ALL STBA <= 0?}",
        "S -- yes --> T[Set STDOMSPP to NONE]",
        "S -- no --> U[Apply same 70% single then top-two else top-three logic using STBA vector]",
        "E --> V[For DOMTYPE use stand BA vector only: 70% single else top-two >=70% else top-three]",
        "G --> W[Store outputs in results list]",
        "I --> W",
        "K --> W",
        "L --> W",
        "N --> W",
        "O --> W",
        "Q --> W",
        "R --> W",
        "T --> W",
        "U --> W",
        "V --> W"
      )))

      if (attr == "VEGCLASS") return(mk(c(
        "A[vegOut region 8 gate] --> B[Call denSizeR8]",
        "B --> C[Read stand TPA and BA from attrList ALL]",
        "C --> D{Is stand BA < 10?}",
        "D -- yes (BA < 10) --> E[Set sizeClass to 1 for advanced regeneration]",
        "E --> F[Set densityClass from TPA bins: A 0-<200, B 200-<400, C 400-<600, D >=600]",
        "D -- no (BA >= 10) --> G[Compute size index as which.max of ALL NMBA PWBA STBA]",
        "G --> H[Map size index to sizeClass: NMBA->2, PWBA->3, STBA->4]",
        "H --> I[Set densityClass from stand BA ranges: A if BA < 40, B if 40 <= BA < 80, C if 80 <= BA < 120, D if BA >= 120]",
        "F --> J[VEGCLASS = paste0 sizeClass and densityClass]",
        "I --> J"
      )))

      if (attr %in% c("SSSIZE", "SSTPA", "NMSIZE", "NMBA", "PWSIZE", "PWBA", "STSIZE", "STBA")) return(mk(c(
        "A[vegOut region 8 gate] --> B[Compute attrList via plotAttr using Region 8 group rules]",
        "B --> C[For each included tree compute TEXPF TREEBA and BAWTD]",
        "C --> D{DBH < 1.5?}",
        "D -- yes --> E[Advanced regeneration: add TEXPF to SSTPA and HT*TEXPF to SSSIZE numerator]",
        "D -- no --> F{DBH >= 1.5 and vol1 <= 0?}",
        "F -- yes --> G[Non-merchantable: add TREEBA to NMBA and BAWTD to NMSIZE numerator]",
        "F -- no --> H{vol1 > 0 and vol2 <= 0?}",
        "H -- yes --> I[Pulpwood: add TREEBA to PWBA and BAWTD to PWSIZE numerator]",
        "H -- no --> J{vol3 > 0?}",
        "J -- yes --> K[Sawtimber: add TREEBA to STBA and BAWTD to STSIZE numerator]",
        "E --> L[Finalize group metrics per species and ALL]",
        "G --> L",
        "I --> L",
        "K --> L",
        "L --> M[Derived means: SSSIZE = sumHTxTPA/SSTPA, NMSIZE = sumBAWTD/NMBA, PWSIZE = sumBAWTD/PWBA, STSIZE = sumBAWTD/STBA when denominators > 0]",
        "M --> N[Return requested Region 8 group attribute from ALL row]"
      )))
    }

    if (region_norm == "MPSG") {
      if (attr == "COVERTYPE") {
        ruleset_txt <- trimws(as.character(input$MPSGcovTyp %||% ""))
        ruleset_num <- suppressWarnings(as.integer(ruleset_txt))
        if (is.na(ruleset_num) && grepl("(^|[^0-9])1([^0-9]|$)|REGION\\s*1|\\bR1\\b", toupper(ruleset_txt), perl = TRUE)) ruleset_num <- 1L
        if (is.na(ruleset_num) && grepl("(^|[^0-9])2([^0-9]|$)|REGION\\s*2|\\bR2\\b", toupper(ruleset_txt), perl = TRUE)) ruleset_num <- 2L
        if (is.na(ruleset_num) && grepl("(^|[^0-9])3([^0-9]|$)|REGION\\s*3|\\bR3\\b", toupper(ruleset_txt), perl = TRUE)) ruleset_num <- 3L

        chosen_1 <- !is.na(ruleset_num) && ruleset_num == 1L
        chosen_2 <- !is.na(ruleset_num) && ruleset_num == 2L
        chosen_3 <- !is.na(ruleset_num) && ruleset_num == 3L

        return(mk(c(
          "A[vegOut MPSG gate] --> B[Read MPSG Cover Type input and total stand canopy cover]",
          paste0("B --> B1[Carried value: MPSG Cover Type input = ", safe_mermaid_text(ruleset_txt), "]"),
          "B1 --> C{Is total stand canopy cover less than 10 percent?}",
          "C -- yes --> D[Final value forced to NONE]",
          "C -- no --> E{Which ruleset is selected?}",
          paste0("E -- ", yn(chosen_1, "ruleset 1 selected"), " --> F1[Region 1 path: compute DOM6040 from dominance inputs]"),
          paste0("E -- ", yn(chosen_2, "ruleset 2 selected"), " --> F2[Region 2 path: resolve top species canopy shares and family code overrides]"),
          paste0("E -- ", yn(chosen_3, "ruleset 3 selected"), " --> F3[Region 3 path: resolve DOMTYPE from genus and shade-tolerance rules]"),
          "E -- no --> F4[Unrecognized ruleset input]",
          "F1 --> G1[Call Region 1 helper and compute DOM6040]",
          "G1 --> H1[Map DOM6040 to the Region 1 cover type dictionary]",
          "H1 --> I1{Is mapped cover type mixedmesiccon?}",
          "I1 -- no --> J1[Use mapped COVERTYPE_R1 as final Region 1 cover type]",
          "I1 -- yes --> K1[Check habitat type against Hot Dry and Warm Dry sets]",
          "K1 --> L1{Does habitat type match the dry Douglas-fir override rules?}",
          "L1 -- yes --> M1[Set Region 1 cover type to dryDouglasfir]",
          "L1 -- no --> J1",
          "J1 --> G1_OUT[Set COVERTYPE_MPSG equal to the Region 1 cover type result]",
          "M1 --> G1_OUT",
          "F2 --> G2[Call Region 2 helper and resolve first second and third ranked species]",
          "G2 --> H2[Assign initial family code from the dominant species lookup]",
          "H2 --> I2{Is top species white fir?}",
          "I2 -- yes --> J2{Is second or third species Douglas-fir?}",
          "I2 -- no --> K2[Keep current family code from the top-species lookup]",
          "J2 -- yes --> L2[Set family code to Douglas-fir group T210]",
          "J2 -- no --> M2[Set family code to white-fir group T211]",
          "K2 --> N2[Compute grouped canopy totals for spruce fir pinyon juniper and Douglas fir]",
          "L2 --> N2",
          "M2 --> N2",
          "N2 --> O2{Does spruce-fir grouped total exceed each top-three canopy share and stay above zero?}",
          "O2 -- yes --> P2[Override family code to spruce-fir group T206]",
          "O2 -- no --> Q2[Keep current family code]",
          "P2 --> R2{Does pinyon-juniper grouped total exceed each top-three canopy share and stay above zero?}",
          "Q2 --> R2",
          "R2 -- yes --> S2[Override family code to pinyon-juniper group T239]",
          "R2 -- no --> T2[Keep current family code]",
          "S2 --> U2{Does Douglas-fir grouped total exceed each top-three canopy share and stay above zero?}",
          "T2 --> U2",
          "U2 -- yes --> V2[Override family code to Douglas-fir group T210]",
          "U2 -- no --> W2[Keep final family code after overrides]",
          "V2 --> X2{Is top species coded as 2TB or 2TN?}",
          "W2 --> X2",
          "X2 -- yes --> Y2[Force fallback family code T999 for other]",
          "X2 -- no --> Z2[Crosswalk final family code through saf2R2 to Region 2 cover type code]",
          "Y2 --> Z2",
          "Z2 --> G2_OUT[Set COVERTYPE_MPSG equal to the Region 2 crosswalk result]",
          "F3 --> G3[Call Region 3 helper and compute DOMTYPE]",
          "G3 --> H3[Create TREECC if needed and build species genus shade-tolerance and leaf-retention vectors]",
          "H3 --> I3{Is corrected canopy cover less than 10 and trees per acre less than 100?}",
          "I3 -- yes --> J3[Use the LEAD1-5 sparse stand DOMTYPE path]",
          "I3 -- no --> K3[Compute species canopy cover genus canopy cover shade-tolerance canopy cover and leaf-retention canopy cover]",
          "K3 --> L3{Does one species reach the single-species dominance threshold?}",
          "L3 -- yes --> M3[Set DOMTYPE from the top species]",
          "L3 -- no --> N3{Do the top two species each reach the two-species threshold?}",
          "N3 -- yes --> O3[Set two-species DOMTYPE and DCC fields]",
          "N3 -- no --> P3{Does a genus reach the genus dominance threshold?}",
          "P3 -- yes --> Q3[Set genus DOMTYPE or mixed species genus DOMTYPE]",
          "P3 -- no --> R3[Use evergreen deciduous and shade-tolerance fallback rules]",
          "M3 --> S3[Set COVERTYPE_MPSG equal to DOMTYPE]",
          "O3 --> S3",
          "Q3 --> S3",
          "R3 --> S3",
          paste0("D --> FINAL[Set output value = NONE]"),
          paste0("G1_OUT --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"),
          paste0("G2_OUT --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"),
          paste0("S3 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]"),
          paste0("F4 --> FINAL[Set output value = ", safe_mermaid_text(selected_value), "]")
        )))
      }

      if (attr == "TREE_SIZE_CLASS") return(mk(c(
        "A[Call MPSG] --> B[Compute StBAWTDBH]",
        "B --> C[Apply tree-size bins: less than 5 maps to s, 5 to less than 10 maps to p, 10 to less than 15 maps to m, 15 to less than 20 maps to l, and 20 or greater maps to v]"
      )))

      if (attr == "CROWN_CLASS") return(mk(c(
        "A[Call MPSG] --> B[Compute STCC]",
        "B --> C[Apply crown-cover bins: less than 10 maps to n, 10 to less than 40 maps to o, 40 to less than 60 maps to m, and 60 or greater maps to c]"
      )))

      if (attr == "VERTICAL_STRUCTURE") return(mk(c(
        "A[Call MPSG] --> B[Call baStory helper]",
        "B --> C{In baStory, is TPA less than or equal to 0 or tree list empty?}",
        "C -- yes --> D[baStory result is 0]",
        "C -- no --> E{Is CC less than 10 and TPA less than 100?}",
        "E -- yes --> F[baStory result is 0]",
        "E -- no --> G{Is CC less than 10 and TPA at least 100?}",
        "G -- yes --> H[baStory result is 1]",
        "G -- no --> I[Initialize baStory result to 3]",
        "I --> J{Is BA from DBH at least 24 inches at least 70 percent of stand BA?}",
        "J -- yes --> K[baStory result is 1]",
        "J -- no --> L[Slide an 8-inch DBH window from 0 to less than 24 in 1-inch steps and compute baSlide divided by BA]",
        "L --> M{Any window share at least 0.7?}",
        "M -- yes --> N[baStory result is 1]",
        "M -- no --> O{Any window share at least 0.6 and less than 0.7?}",
        "O -- yes --> P[baStory result is 2]",
        "O -- no --> Q[baStory result stays 3]",
        "D --> R[Apply MPSG normalization]",
        "F --> R",
        "H --> R",
        "K --> R",
        "N --> R",
        "P --> R",
        "Q --> R",
        "R --> S{Is baStory result NA?}",
        "S -- yes --> T[Set VERTICAL_STRUCTURE to 0]",
        "S -- no --> U{Is baStory result equal to 3?}",
        "U -- yes --> V[Normalize 3 to 2]",
        "U -- no --> W[Keep baStory result]"
      )))

      if (attr %in% c("HSS1_4C", "HSS1_5")) return(mk(c(
        "A[Call MPSG] --> B{addHSS enabled R/MPSG.r:307-318}",
        "B -- no --> C[HSS not produced]",
        "B -- yes --> D[Call HSS with HSStype 1 for HSS1_4C or 2 for HSS1_5]",
        "D --> E{HSStype equals 1?}",
        "E -- yes --> F[HSS1_4C uses grpSizeClass decisions: N=1, E=2, S/M with STCC bins => 3A/3B/3C, L/V with STCC bins => 4A/4B/4C]",
        "E -- no --> G[HSS1_5 uses BA and QMD thresholds to assign TSC then CHESS; OGSC can force CHESS 50]",
        "G --> H[Translate CHESS map: 10=1,20=2,31=3A,32=3B,33=3C,41=4A,42=4B,43=4C,50=5]",
        "F --> I[Return requested HSS output]",
        "H --> I[Return requested HSS output]"
      )))
    }

    paste0("flowchart TD\n  A[No attribute-specific flowchart mapping for region ", region_norm, " attr ", attr, "]\n")
  }

  output$output_preview <- renderDT({
    df <- preview_df()
    datatable(
      df,
      class = "display nowrap",
      rownames = FALSE,
      selection = list(mode = "single", target = "cell"),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = TRUE
      )
    )
  })

  output$plot_status <- renderText({
    path <- resolved_output_file()
    if (is.null(path)) {
      return("No output CSV is available yet. Run processing first, then build a scatter, bar, per-run time-series, or composition plot here.")
    }

    df_all <- preview_df()
    df_plot <- plot_df()
    run_col <- plot_runtitle_col()
    run_msg <- ""
    if (!is.null(run_col) && nzchar(run_col)) {
      selected_runs <- selected_plot_runtitles()
      if (length(selected_runs) > 0) {
        run_msg <- paste0(" | RunTitles: ", length(selected_runs), " selected")
      }
    }

    sprintf(
      "Plot source: %s%s | Filtered rows: %d of %d | Columns: %d",
      basename(path),
      run_msg,
      nrow(df_plot),
      nrow(df_all),
      ncol(df_plot)
    )
  })

  output$output_plot <- renderPlotly({
    df <- plot_df()
    req(nrow(df) > 0, ncol(df) > 0)

    plot_type <- input$plot_type %||% "scatter"

    if (identical(plot_type, "timeseries")) {
      time_col <- find_col_ignore_case(df, c("YEAR", "CY", "CYCLE"))
      validate(
        need(!is.null(time_col), "Time series requires a YEAR (or CY/CYCLE) column in the output.")
      )

      run_col <- plot_runtitle_col()
      if (!is.null(run_col) && run_col %in% colnames(df)) {
        run_vals_raw <- trimws(as.character(df[[run_col]]))
        run_vals_raw[!nzchar(run_vals_raw)] <- "(missing)"
      } else {
        run_vals_raw <- rep("All Runs", nrow(df))
      }

      time_vals <- coerce_numeric(df[[time_col]])
      keep_time <- !is.na(time_vals)
      validate(
        need(sum(keep_time) > 0, "No numeric time values were found in the YEAR/CY column.")
      )

      value_col <- input$plot_ts_value %||% "__count__"
      agg_fun <- input$plot_ts_fun %||% "mean"

      tmp <- data.frame(
        time = time_vals[keep_time],
        run = run_vals_raw[keep_time],
        stringsAsFactors = FALSE
      )

      if (identical(value_col, "__count__") || identical(agg_fun, "count")) {
        agg_df <- as.data.frame(table(time = tmp$time, run = tmp$run), stringsAsFactors = FALSE)
        agg_df <- agg_df[agg_df$Freq > 0, , drop = FALSE]
        names(agg_df)[names(agg_df) == "Freq"] <- "val"
        agg_df$time <- suppressWarnings(as.numeric(as.character(agg_df$time)))
        y_label <- "Count"
        title_y <- "Count"
      } else {
        req(value_col %in% colnames(df))
        values <- coerce_numeric(df[[value_col]])

        tmp$value <- values[keep_time]

        fun_to_use <- switch(
          agg_fun,
          "sum" = function(v) sum(v, na.rm = TRUE),
          "mean" = function(v) mean(v, na.rm = TRUE),
          "median" = function(v) median(v, na.rm = TRUE),
          function(v) sum(!is.na(v))
        )

        agg_df <- aggregate(value ~ time + run, data = tmp, FUN = fun_to_use)
        names(agg_df)[names(agg_df) == "value"] <- "val"
        agg_df <- agg_df[!is.na(agg_df$val), , drop = FALSE]
        y_label <- paste0(tools::toTitleCase(agg_fun), "(", value_col, ")")
        title_y <- y_label
      }

      validate(
        need(nrow(agg_df) > 0, "No time-series values are available for the selected options.")
      )

      agg_df <- agg_df[order(agg_df$run, agg_df$time), , drop = FALSE]
      agg_df$rows <- vapply(seq_len(nrow(agg_df)), function(i) {
        sum(tmp$time == agg_df$time[i] & tmp$run == agg_df$run[i], na.rm = TRUE)
      }, numeric(1))
      hover_text <- paste0(
        time_col, ": ", agg_df$time, "<br>",
        "RunTitle: ", agg_df$run, "<br>",
        "Value: ", agg_df$val, "<br>",
        "Rows: ", agg_df$rows
      )

      click_keys <- paste(as.character(agg_df$time), as.character(agg_df$run), sep = "|||")
      use_run_color <- length(unique(agg_df$run)) > 1

      if (use_run_color) {
        p <- plot_ly(
          data = agg_df,
          x = ~time,
          y = ~val,
          type = "scatter",
          mode = if (isTRUE(input$plot_ts_markers)) "lines+markers" else "lines",
          source = "output_plot_src",
          key = click_keys,
          color = ~run,
          colors = "Set2",
          marker = list(size = 8),
          text = hover_text,
          hoverinfo = "text"
        )
      } else {
        p <- plot_ly(
          data = agg_df,
          x = ~time,
          y = ~val,
          type = "scatter",
          mode = if (isTRUE(input$plot_ts_markers)) "lines+markers" else "lines",
          source = "output_plot_src",
          key = click_keys,
          line = list(color = "#1f77b4", width = 2),
          marker = list(size = 8),
          text = hover_text,
          hoverinfo = "text",
          name = "Per-run time series"
        )
      }

      p <- event_register(p, "plotly_click")

      return(layout(
        p,
        title = paste("Time Series (Per Run):", title_y),
        xaxis = list(title = time_col),
        yaxis = list(title = y_label, tickmode = "auto", nticks = 10, tickformat = ".4~g", automargin = TRUE),
        showlegend = use_run_color,
        legend = list(title = list(text = if (use_run_color) "RunTitle" else ""))
      ))
    }

    if (identical(plot_type, "composition")) {
      x_col <- input$plot_comp_x
      group_col <- input$plot_comp_group
      req(!is.null(x_col), nzchar(x_col), x_col %in% colnames(df))
      req(!is.null(group_col), nzchar(group_col), group_col %in% colnames(df))

      run_col <- plot_runtitle_col()
      split_by_run <- isTRUE(input$plot_comp_split_run) && !is.null(run_col) && run_col %in% colnames(df)

      x_vals <- as.character(df[[x_col]])
      grp_vals <- as.character(df[[group_col]])
      x_vals[is.na(x_vals) | !nzchar(x_vals)] <- "(missing)"
      grp_vals[is.na(grp_vals) | !nzchar(grp_vals)] <- "(missing)"

      run_vals <- if (split_by_run) as.character(df[[run_col]]) else rep("All Runs", nrow(df))
      run_vals[is.na(run_vals) | !nzchar(run_vals)] <- "(missing)"

      comp_df <- as.data.frame(table(x = x_vals, run = run_vals, grp = grp_vals), stringsAsFactors = FALSE)
      comp_df <- comp_df[comp_df$Freq > 0, , drop = FALSE]
      validate(need(nrow(comp_df) > 0, "No composition values available for selected columns."))

      names(comp_df)[names(comp_df) == "Freq"] <- "count"
      comp_df$x_run <- paste(comp_df$x, comp_df$run, sep = "|||")
      totals <- tapply(comp_df$count, comp_df$x_run, sum, na.rm = TRUE)
      comp_df$pct <- 100 * comp_df$count / as.numeric(totals[as.character(comp_df$x_run)])

      x_totals <- tapply(comp_df$count, comp_df$x, sum, na.rm = TRUE)
      x_order <- names(sort(x_totals, decreasing = TRUE))
      run_order <- selected_plot_runtitles()
      if (!split_by_run || length(run_order) == 0) run_order <- unique(as.character(comp_df$run))

      ordered_levels <- unlist(lapply(x_order, function(xv) {
        runs_here <- unique(as.character(comp_df$run[as.character(comp_df$x) == xv]))
        runs_here <- c(intersect(run_order, runs_here), setdiff(runs_here, run_order))
        paste(xv, runs_here, sep = "|||")
      }), use.names = FALSE)

      comp_df$x_plot <- if (split_by_run) {
        paste0(comp_df$x, " | ", comp_df$run)
      } else {
        as.character(comp_df$x)
      }

      if (split_by_run) {
        x_plot_levels <- paste0(sub("\\|\\|\\|.*$", "", ordered_levels), " | ", sub("^.*\\|\\|\\|", "", ordered_levels))
      } else {
        x_plot_levels <- x_order
      }

      comp_df$x_plot <- factor(comp_df$x_plot, levels = unique(x_plot_levels))
      comp_df <- comp_df[order(comp_df$x_plot, comp_df$grp), , drop = FALSE]

      hover_comp <- paste0(
        x_col, ": ", comp_df$x, "<br>",
        if (split_by_run) paste0("RunTitle: ", comp_df$run, "<br>") else "",
        group_col, ": ", comp_df$grp, "<br>",
        "Count: ", comp_df$count, "<br>",
        "Percent: ", sprintf("%.1f", comp_df$pct), "%"
      )

      click_keys <- paste(as.character(comp_df$x), as.character(comp_df$run), as.character(comp_df$grp), sep = "|||")

      p <- plot_ly(
        data = comp_df,
        x = ~x_plot,
        y = ~pct,
        type = "bar",
        color = ~grp,
        colors = "Set2",
        source = "output_plot_src",
        key = click_keys,
        text = hover_comp,
        hoverinfo = "text"
      )

      p <- event_register(p, "plotly_click")

      return(layout(
        p,
        title = paste(
          "Categorical Composition (%):",
          group_col,
          "within",
          x_col,
          if (split_by_run) "(separated by RunTitle)" else ""
        ),
        xaxis = list(title = if (split_by_run) paste(x_col, "and RunTitle") else x_col, tickangle = -45),
        yaxis = list(title = "Percent", range = c(0, 100), ticksuffix = "%"),
        barmode = "stack",
        legend = list(title = list(text = group_col))
      ))
    }

    x_col <- input$plot_x
    req(!is.null(x_col), nzchar(x_col), x_col %in% colnames(df))

    x_vals_raw <- df[[x_col]]
    color_col <- input$plot_color_by %||% "__none__"
    use_color <- !identical(color_col, "__none__") && color_col %in% colnames(df)

    if (identical(plot_type, "scatter")) {
      y_col <- input$plot_y
      req(!is.null(y_col), nzchar(y_col), y_col %in% colnames(df))

      x_vals <- coerce_numeric(x_vals_raw)
      y_vals <- coerce_numeric(df[[y_col]])
      keep <- !is.na(x_vals) & !is.na(y_vals)

      validate(
        need(sum(keep) > 1, "Scatter plot requires numeric X and Y columns with at least two valid rows.")
      )

      point_rows <- which(keep)
      stand_vals <- if ("STANDID" %in% colnames(df)) as.character(df$STANDID[keep]) else rep("N/A", sum(keep))
      stand_cn_vals <- if ("STAND_CN" %in% colnames(df)) as.character(df$STAND_CN[keep]) else rep("N/A", sum(keep))
      color_vals <- if (use_color) as.character(df[[color_col]][keep]) else rep("All", sum(keep))
      color_vals[is.na(color_vals) | !nzchar(color_vals)] <- "(missing)"

      plot_df <- data.frame(
        x = x_vals[keep],
        y = y_vals[keep],
        row_key = point_rows,
        standid = stand_vals,
        stand_cn = stand_cn_vals,
        color_group = color_vals,
        stringsAsFactors = FALSE
      )

      hover_text <- if (use_color) {
        paste0(
          x_col, ": ", plot_df$x, "<br>",
          y_col, ": ", plot_df$y, "<br>",
          color_col, ": ", plot_df$color_group, "<br>",
          "STANDID: ", plot_df$standid, "<br>",
          "STAND_CN: ", plot_df$stand_cn
        )
      } else {
        paste0(
          x_col, ": ", plot_df$x, "<br>",
          y_col, ": ", plot_df$y, "<br>",
          "STANDID: ", plot_df$standid, "<br>",
          "STAND_CN: ", plot_df$stand_cn
        )
      }

      if (use_color) {
        p <- plot_ly(
          data = plot_df,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "markers",
          source = "output_plot_src",
          key = ~row_key,
          color = ~color_group,
          colors = "Set2",
          marker = list(size = 8),
          text = hover_text,
          hoverinfo = "text"
        )
      } else {
        p <- plot_ly(
          data = plot_df,
          x = ~x,
          y = ~y,
          type = "scatter",
          mode = "markers",
          source = "output_plot_src",
          key = ~row_key,
          marker = list(color = "#1f77b4", size = 8),
          text = hover_text,
          hoverinfo = "text"
        )
      }

      if (isTRUE(input$plot_add_trendline)) {
        fit_df <- data.frame(x = x_vals[keep], y = y_vals[keep])
        fit <- lm(y ~ x, data = fit_df)
        x_seq <- seq(min(x_vals[keep]), max(x_vals[keep]), length.out = 200)
        y_seq <- predict(fit, newdata = data.frame(x = x_seq))

        p <- add_lines(
          p,
          x = x_seq,
          y = y_seq,
          inherit = FALSE,
          line = list(color = "#d62728", width = 2),
          name = "Trend Line",
          hoverinfo = "skip",
          showlegend = FALSE
        )
      }

      p <- event_register(p, "plotly_click")
      return(layout(
        p,
        title = paste("Scatter Plot:", y_col, "vs", x_col),
        xaxis = list(title = x_col),
        yaxis = list(title = y_col, tickmode = "auto", nticks = 10, tickformat = ".4~g", automargin = TRUE),
        legend = list(title = list(text = if (use_color) color_col else ""))
      ))
    }

    x_levels <- unique(as.character(x_vals_raw))
    validate(
      need(length(x_levels) <= 80, "Selected X axis has more than 80 categories. Choose a column with fewer unique values.")
    )

    x_group <- as.factor(as.character(x_vals_raw))
    color_group <- if (use_color) as.character(df[[color_col]]) else rep("All", nrow(df))
    color_group[is.na(color_group) | !nzchar(color_group)] <- "(missing)"

    value_col <- input$plot_bar_value %||% "__count__"
    agg_fun <- input$plot_bar_fun %||% "count"

    if (identical(value_col, "__count__") || identical(agg_fun, "count")) {
      agg_df <- as.data.frame(table(x = as.character(x_group), color = color_group), stringsAsFactors = FALSE)
      agg_df <- agg_df[agg_df$Freq > 0, , drop = FALSE]
      names(agg_df)[names(agg_df) == "Freq"] <- "val"
      y_label <- "Count"
      title_suffix <- "Count by"
      value_col <- "__count__"
    } else {
      req(value_col %in% colnames(df))
      values <- coerce_numeric(df[[value_col]])

      fun_to_use <- switch(
        agg_fun,
        "sum" = function(v) sum(v, na.rm = TRUE),
        "mean" = function(v) mean(v, na.rm = TRUE),
        "median" = function(v) median(v, na.rm = TRUE),
        function(v) sum(!is.na(v))
      )

      tmp <- data.frame(
        x = as.character(x_group),
        color = color_group,
        value = values,
        stringsAsFactors = FALSE
      )
      agg_df <- aggregate(value ~ x + color, data = tmp, FUN = fun_to_use)
      names(agg_df)[names(agg_df) == "value"] <- "val"
      agg_df <- agg_df[!is.na(agg_df$val), , drop = FALSE]
      validate(
        need(nrow(agg_df) > 0, "No valid values found for the selected bar aggregation.")
      )
      y_label <- paste0(tools::toTitleCase(agg_fun), "(", value_col, ")")
      title_suffix <- paste(tools::toTitleCase(agg_fun), value_col, "by")
    }

    validate(need(nrow(agg_df) > 0, "No bars available for the selected plot options."))

    x_order <- names(sort(tapply(agg_df$val, agg_df$x, sum, na.rm = TRUE), decreasing = TRUE))
    agg_df$x <- factor(agg_df$x, levels = x_order)
    agg_df <- agg_df[order(agg_df$x, agg_df$color), , drop = FALSE]

    stand_by_group <- lapply(seq_len(nrow(agg_df)), function(i) {
      gx <- as.character(agg_df$x[i])
      gc <- as.character(agg_df$color[i])
      rows <- which(as.character(x_group) == gx & color_group == gc)
      stand_vals <- if ("STANDID" %in% colnames(df)) unique(as.character(df$STANDID[rows])) else "N/A"
      stand_cn_vals <- if ("STAND_CN" %in% colnames(df)) unique(as.character(df$STAND_CN[rows])) else "N/A"
      list(
        standid = stand_vals,
        stand_cn = stand_cn_vals,
        n_rows = length(rows)
      )
    })

    hover_bar <- vapply(seq_len(nrow(agg_df)), function(i) {
      gx <- as.character(agg_df$x[i])
      gc <- as.character(agg_df$color[i])
      s <- stand_by_group[[i]]
      sid <- paste(utils::head(s$standid, 5), collapse = ", ")
      scn <- paste(utils::head(s$stand_cn, 5), collapse = ", ")
      if (use_color) {
        paste0(
          x_col, ": ", gx, "<br>",
          color_col, ": ", gc, "<br>",
          "Value: ", agg_df$val[i], "<br>",
          "Rows: ", s$n_rows, "<br>",
          "STANDID sample: ", sid, "<br>",
          "STAND_CN sample: ", scn
        )
      } else {
        paste0(
          x_col, ": ", gx, "<br>",
          "Value: ", agg_df$val[i], "<br>",
          "Rows: ", s$n_rows, "<br>",
          "STANDID sample: ", sid, "<br>",
          "STAND_CN sample: ", scn
        )
      }
    }, character(1))

    click_keys <- paste(as.character(agg_df$x), as.character(agg_df$color), sep = "|||")

    if (use_color) {
      p <- plot_ly(
        data = agg_df,
        x = ~x,
        y = ~val,
        type = "bar",
        source = "output_plot_src",
        key = click_keys,
        color = ~color,
        colors = "Set2",
        hovertext = hover_bar,
        textposition = "none",
        hoverinfo = "text"
      )
    } else {
      p <- plot_ly(
        data = agg_df,
        x = ~x,
        y = ~val,
        type = "bar",
        source = "output_plot_src",
        key = click_keys,
        marker = list(color = "#2ca02c"),
        hovertext = hover_bar,
        textposition = "none",
        hoverinfo = "text"
      )
    }

    p <- event_register(p, "plotly_click")

    layout(
      p,
      title = NULL,
      xaxis = list(title = x_col, tickangle = -45, showticklabels = TRUE),
      yaxis = list(title = y_label, showticklabels = TRUE),
      barmode = if (use_color) (input$plot_bar_mode %||% "group") else "group",
      showlegend = use_color,
      legend = list(title = list(text = if (use_color) color_col else ""))
    )
  })

  observeEvent(suppressWarnings(event_data("plotly_click", source = "output_plot_src")), {
    click <- suppressWarnings(event_data("plotly_click", source = "output_plot_src"))
    req(!is.null(click), nrow(click) >= 1)

    df <- plot_df()
    plot_type <- input$plot_type %||% "scatter"

    if (identical(plot_type, "timeseries")) {
      time_col <- find_col_ignore_case(df, c("YEAR", "CY", "CYCLE"))
      req(!is.null(time_col))

      key_text <- as.character(click$key[[1]])
      req(!is.null(key_text), nzchar(key_text))

      key_parts <- strsplit(key_text, "\\|\\|\\|", perl = TRUE)[[1]]
      clicked_time <- suppressWarnings(as.numeric(key_parts[[1]]))
      clicked_run <- if (length(key_parts) >= 2) key_parts[[2]] else "All Runs"
      req(!is.na(clicked_time))

      run_col <- plot_runtitle_col()
      run_vals <- if (!is.null(run_col) && run_col %in% colnames(df)) {
        trimws(as.character(df[[run_col]]))
      } else {
        rep("All Runs", nrow(df))
      }
      run_vals[!nzchar(run_vals)] <- "(missing)"

      time_vals <- coerce_numeric(df[[time_col]])
      rows <- which(!is.na(time_vals) & time_vals == clicked_time & run_vals == clicked_run)
      req(length(rows) > 0)

      showModal(modalDialog(
        title = "Selected Time-Series Point",
        p(tags$b(time_col), ": ", clicked_time),
        p(tags$b("Rows at this time step"), ": ", length(rows)),
        p(tags$b("RunTitle"), ": ", clicked_run),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    if (identical(plot_type, "composition")) {
      key_text <- as.character(click$key[[1]])
      req(!is.null(key_text), nzchar(key_text))

      key_parts <- strsplit(key_text, "\\|\\|\\|", perl = TRUE)[[1]]
      x_group <- if (length(key_parts) >= 1) key_parts[[1]] else "(missing)"
      run_group <- if (length(key_parts) >= 2) key_parts[[2]] else "All Runs"
      cat_group <- if (length(key_parts) >= 3) key_parts[[3]] else "(missing)"

      x_col <- input$plot_comp_x
      group_col <- input$plot_comp_group
      req(!is.null(x_col), x_col %in% colnames(df))
      req(!is.null(group_col), group_col %in% colnames(df))

      run_col <- plot_runtitle_col()
      split_by_run <- isTRUE(input$plot_comp_split_run) && !is.null(run_col) && run_col %in% colnames(df)

      x_vals <- as.character(df[[x_col]])
      grp_vals <- as.character(df[[group_col]])
      run_vals <- if (split_by_run) as.character(df[[run_col]]) else rep("All Runs", nrow(df))
      x_vals[is.na(x_vals) | !nzchar(x_vals)] <- "(missing)"
      grp_vals[is.na(grp_vals) | !nzchar(grp_vals)] <- "(missing)"
      run_vals[is.na(run_vals) | !nzchar(run_vals)] <- "(missing)"

      rows <- which(x_vals == x_group & run_vals == run_group & grp_vals == cat_group)
      total_x <- sum(x_vals == x_group & run_vals == run_group)
      req(length(rows) > 0, total_x > 0)
      pct <- 100 * length(rows) / total_x

      showModal(modalDialog(
        title = "Selected Composition Segment",
        p(tags$b(x_col), ": ", x_group),
        p(tags$b("RunTitle"), ": ", run_group),
        p(tags$b(group_col), ": ", cat_group),
        p(tags$b("Count"), ": ", length(rows)),
        p(tags$b("Percent within group"), ": ", sprintf("%.1f%%", pct)),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    if (identical(plot_type, "scatter")) {
      row_key <- suppressWarnings(as.integer(click$key[[1]]))
      req(!is.na(row_key), row_key >= 1, row_key <= nrow(df))

      x_col <- input$plot_x
      y_col <- input$plot_y
      x_val <- as.character(df[[x_col]][row_key])
      y_val <- as.character(df[[y_col]][row_key])
      stand_value <- if ("STANDID" %in% colnames(df)) as.character(df$STANDID[row_key]) else "N/A"
      stand_cn_value <- if ("STAND_CN" %in% colnames(df)) as.character(df$STAND_CN[row_key]) else "N/A"

      showModal(modalDialog(
        title = "Selected Point Details",
        p(tags$b(x_col), ": ", x_val),
        p(tags$b(y_col), ": ", y_val),
        p(tags$b("STANDID"), ": ", stand_value),
        p(tags$b("STAND_CN"), ": ", stand_cn_value),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    x_col <- input$plot_x
    color_col <- input$plot_color_by %||% "__none__"
    use_color <- !identical(color_col, "__none__") && color_col %in% colnames(df)

    key_text <- as.character(click$key[[1]])
    req(!is.null(key_text), nzchar(key_text))

    key_parts <- strsplit(key_text, "\\|\\|\\|", perl = TRUE)[[1]]
    x_group <- if (length(key_parts) >= 1) key_parts[[1]] else key_text
    color_group <- if (length(key_parts) >= 2) key_parts[[2]] else "All"

    row_colors <- if (use_color) as.character(df[[color_col]]) else rep("All", nrow(df))
    row_colors[is.na(row_colors) | !nzchar(row_colors)] <- "(missing)"

    rows <- which(as.character(df[[x_col]]) == x_group & row_colors == color_group)
    req(length(rows) > 0)

    stand_ids <- if ("STANDID" %in% colnames(df)) unique(as.character(df$STANDID[rows])) else "N/A"
    stand_cns <- if ("STAND_CN" %in% colnames(df)) unique(as.character(df$STAND_CN[rows])) else "N/A"

    showModal(modalDialog(
      title = "Selected Bar Details",
      p(tags$b(x_col), ": ", x_group),
      if (use_color) p(tags$b(color_col), ": ", color_group),
      p(tags$b("Aggregated Value"), ": ", as.character(click$y[[1]])),
      p(tags$b("Rows in Group"), ": ", as.character(length(rows))),
      p(tags$b("STANDID values"), ": ", paste(utils::head(stand_ids, 20), collapse = ", ")),
      p(tags$b("STAND_CN values"), ": ", paste(utils::head(stand_cns, 20), collapse = ", ")),
      p(class = "text-muted", "Showing up to the first 20 unique STANDID/STAND_CN values for the selected bar."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$output_preview_cell_clicked, {
    cell <- input$output_preview_cell_clicked
    req(!is.null(cell$row), !is.null(cell$col))

    df <- preview_df()
    # DT reports clicked column as zero-based; convert to R's one-based index.
    col_index <- as.integer(cell$col) + 1L
    req(nrow(df) >= cell$row, ncol(df) >= col_index)

    attr_name <- colnames(df)[col_index]
    attr_value <- as.character(df[cell$row, col_index, drop = TRUE])
    region_value <- if ("REGION" %in% colnames(df)) as.character(df[cell$row, "REGION", drop = TRUE]) else "Unknown"

    stand_value <- if ("STANDID" %in% colnames(df)) as.character(df[cell$row, "STANDID", drop = TRUE]) else "N/A"
    stand_cn_value <- if ("STAND_CN" %in% colnames(df)) as.character(df[cell$row, "STAND_CN", drop = TRUE]) else "N/A"
    year_value <- if ("YEAR" %in% colnames(df)) as.character(df[cell$row, "YEAR", drop = TRUE]) else "N/A"
    case_value <- if ("CASEID" %in% colnames(df)) as.character(df[cell$row, "CASEID", drop = TRUE]) else "N/A"
    run_title_value <- if ("RUNTITLE" %in% colnames(df)) as.character(df[cell$row, "RUNTITLE", drop = TRUE]) else "N/A"

    explanation <- get_attribute_explanation(region_value, attr_name, attr_value)
    row_data <- df[cell$row, , drop = FALSE]
    flowchart_text <- get_attribute_flowchart_mermaid(region_value, attr_name, attr_value, row_data = row_data)
    flowchart_id <- "attr_flowchart_mermaid"
    no_flowchart_attrs <- c("RUNTITLE", "CASEID", "STAND_CN", "STANDID", "VARIANT", "REGION", "YEAR", "CY")
    show_flowchart <- !(toupper(attr_name) %in% no_flowchart_attrs)
    if (show_flowchart) {
      # Apply green node styling only to attribute-output flowcharts.
      flowchart_text <- paste(
        "%%{init: {'theme':'base','flowchart':{'htmlLabels':true},'themeVariables':{'primaryColor':'#CDEFD8','secondaryColor':'#CDEFD8','tertiaryColor':'#CDEFD8','primaryBorderColor':'#2E7D32','lineColor':'#8A8F98','edgeLabelBackground':'#F5F6F7','primaryTextColor':'#1F2933'}}}%%",
        flowchart_text,
        sep = "\n"
      )
    }
    region_key <- toupper(trimws(as.character(region_value %||% "")))
    if (region_key %in% c("2", "2.0")) region_key <- "2"
    if (region_key %in% c("8", "8.0")) region_key <- "8"
    if (region_key %in% c("MPS", "MPSG")) region_key <- "MPSG"
    show_hss14c_legend <- show_flowchart && region_key == "2" && toupper(attr_name) == "HSS1_4C"
    show_hss15_legend <- show_flowchart && region_key == "2" && toupper(attr_name) == "HSS1_5"
    show_mpsg_hss_legend <- show_flowchart && region_key == "MPSG" && toupper(attr_name) %in% c("HSS1_4C", "HSS1_5")
    show_hss_legend <- show_hss14c_legend || show_hss15_legend || show_mpsg_hss_legend
    show_r8_legend <- show_flowchart && region_key == "8" && toupper(attr_name) %in% c(
      "SSDOMSPP", "SSSIZE", "SSTPA", "NMDOMSPP", "NMSIZE", "NMBA",
      "PWDOMSPP", "PWSIZE", "PWBA", "STDOMSPP", "STSIZE", "STBA",
      "DOMTYPE", "VEGCLASS"
    )
    show_side_legend <- show_hss_legend || show_r8_legend

    hss14c_legend_ui <- tagList(
      tags$div(class = "flowchart-legend",
               tags$h6("HSS1_4C Bin Legend"),
               tags$p(tags$b("Purpose:"), "Long-form bin and threshold notes for the selected Region 2 HSS1_4C output path."),
               tags$p(tags$b("DBH canopy bins using TREECC contribution:")),
               tags$ul(
                 tags$li(tags$b("E"), ": 0 <= DBH < 1 inch (seedlings)."),
                 tags$li(tags$b("S"), ": 1 <= DBH < 5 inches (saplings)."),
                 tags$li(tags$b("M"), ": 5 <= DBH < 9 inches (medium trees)."),
                 tags$li(tags$b("L"), ": 9 <= DBH < 16 inches (large trees)."),
                 tags$li(tags$b("V"), ": DBH >= 16 inches (very large trees).")
               ),
               tags$p(tags$b("Grouped bins used for precedence:")),
               tags$ul(
                 tags$li(tags$b("ES"), ": E + S (small-diameter cohort)."),
                 tags$li(tags$b("LV"), ": L + V (large-diameter cohort).")
               ),
               tags$p(tags$b("grpSizeClass precedence from HSS helper:")),
               tags$ul(
                 tags$li("If STCC < 10 then class N (non-stocked)."),
                 tags$li("Else initialize class to LV when LV > 0."),
                 tags$li("Override to M when M > LV."),
                 tags$li("Override to ES when ES > M and ES > LV."),
                 tags$li("If LV selected, split to V when V >= L; otherwise L."),
                 tags$li("If ES selected, split to S when S >= E; otherwise E.")
               ),
               tags$p(tags$b("Final stage bins for selected output:")),
               tags$ul(
                 tags$li("N -> stage 1."),
                 tags$li("E -> stage 2."),
                 tags$li("S or M -> stage 3A when 0 < STCC < 40, stage 3B when 40 <= STCC < 70, stage 3C when STCC >= 70."),
                 tags$li("L or V -> stage 4A when 0 < STCC < 40, stage 4B when 40 <= STCC < 70, stage 4C when STCC >= 70.")
               )
      )
    )

    hss15_legend_ui <- tagList(
      tags$div(class = "flowchart-legend",
               tags$h6("HSS1_5 Logic Legend"),
               tags$p(tags$b("Purpose:"), "Long-form rule notes for the selected Region 2 HSS1_5 output path."),
               tags$p(tags$b("Input families used by HSS helper:")),
               tags$ul(
                 tags$li(tags$b("Primary"), ": STCC canopy cover, TPA trees per acre, QMD quadratic mean diameter."),
                 tags$li(tags$b("BA partitions"), ": BA5, BA9, BA16, BA125 (1 to less than 5 inches), BA529 (5 to less than 9 inches)."),
                 tags$li(tags$b("Derived"), ": BARAT = BA16/BA9, BAMIN adjustment, optional OGSC old-growth score override.")
               ),
               tags$p(tags$b("Ordered TSC assignment (first true wins):")),
               tags$ul(
                 tags$li(tags$b("TSC 6"), ": BA5 >= BAMIN, BA5 >= BA125, BA9 >= BA529, BARAT > 0.50."),
                 tags$li(tags$b("TSC 5"), ": same as TSC 6 but BARAT <= 0.50."),
                 tags$li(tags$b("TSC 4"), ": BA5 >= BAMIN, BA5 >= BA125, BA9 < BA529."),
                 tags$li(tags$b("TSC 3"), ": BA5 >= BAMIN and BA5 < BA125."),
                 tags$li(tags$b("TSC 2"), ": BA5 < BAMIN and TPA >= 300."),
                 tags$li(tags$b("TSC 1"), ": STCC < 10.")
               ),
               tags$p(tags$b("CHESS to HSS mapping:")),
               tags$ul(
                 tags$li("10 -> 1, 20 -> 2."),
                 tags$li("31/32/33 -> 3A/3B/3C by STCC bin (<40, 40 to <70, >=70)."),
                 tags$li("41/42/43 -> 4A/4B/4C by STCC bin (<40, 40 to <70, >=70)."),
                 tags$li("OGSC override: mature branch with OGSC >= 42 sets CHESS = 50 -> HSS 5.")
               )
      )
    )

    mpsg_hss_legend_ui <- tagList(
      tags$div(class = "flowchart-legend",
               tags$h6("MPSG HSS Legend"),
               tags$p(tags$b("Purpose:"), "Long-form notes for MPSG HSS outputs. MPSG computes cover/size/crown/vertical structure, then calls the Region 2 HSS helper when addHSS is enabled."),
               tags$p(tags$b("MPSG wrapper behavior from MPSG.r:")),
               tags$ul(
                 tags$li("MPSG passes stand tree list, TPA, CC, and plotvals into HSS."),
                 tags$li("HSS1_4C uses HSS(HSStype = 1)."),
                 tags$li("HSS1_5 uses HSS(HSStype = 2).")
               ),
               tags$p(tags$b("HSS1_4C rule family (via HSS helper):")),
               tags$ul(
                 tags$li("If STCC < 10 then non-stocked class N -> stage 1."),
                 tags$li("Otherwise diameter-bin canopy groups determine class E, S/M, or L/V."),
                 tags$li("E -> stage 2."),
                 tags$li("S/M -> 3A, 3B, 3C by STCC bins (<40, 40 to <70, >=70)."),
                 tags$li("L/V -> 4A, 4B, 4C by STCC bins (<40, 40 to <70, >=70).")
               ),
               tags$p(tags$b("HSS1_5 rule family (via HSS helper):")),
               tags$ul(
                 tags$li("Ordered TSC assignment from BA and TPA thresholds (first true rule wins)."),
                 tags$li("TSC 3/4 maps to CHESS 31/32/33 and stages 3A/3B/3C by STCC bins."),
                 tags$li("TSC 5/6 maps to CHESS 41/42/43 and stages 4A/4B/4C by STCC bins."),
                 tags$li("OGSC override can set CHESS 50 -> stage 5.")
               )
      )
    )

    r8_legend_ui <- tagList(
      tags$div(class = "flowchart-legend",
               tags$h6("Region 8 Flowchart Legend"),
               tags$p(tags$b("Purpose:"), "Abbreviation meanings used in the selected Region 8 flowchart path."),
               tags$p(tags$b("Tree pool gates and computed terms:")),
               tags$ul(
                 tags$li(tags$b("DBH"), ": diameter at breast height."),
                 tags$li(tags$b("vol1"), ": first volume field used to route trees into the non-merchantable or pulpwood pools."),
                 tags$li(tags$b("vol2"), ": second volume field used to separate pulpwood from sawtimber."),
                 tags$li(tags$b("vol3"), ": third volume field used to identify the sawtimber pool."),
                 tags$li(tags$b("TEXPF"), ": tree expansion factor."),
                 tags$li(tags$b("TREEBA"), ": tree basal area."),
                 tags$li(tags$b("BAWTD"), ": basal-area-weighted diameter."),
                 tags$li(tags$b("SSTPA"), ": advanced-regeneration trees per acre."),
                 tags$li(tags$b("SSSIZE"), ": advanced-regeneration mean height."),
                 tags$li(tags$b("NMBA"), ": non-merchantable basal area."),
                 tags$li(tags$b("NMSIZE"), ": non-merchantable basal-area-weighted diameter."),
                 tags$li(tags$b("PWBA"), ": pulpwood basal area."),
                 tags$li(tags$b("PWSIZE"), ": pulpwood basal-area-weighted diameter."),
                 tags$li(tags$b("STBA"), ": sawtimber basal area."),
                 tags$li(tags$b("STSIZE"), ": sawtimber basal-area-weighted diameter.")
               ),
               tags$p(tags$b("Dominant-species codes used in Region 8 output paths:")),
               tags$ul(
                 tags$li(tags$b("SSDOMSPP"), ": advanced-regeneration dominant species grouping."),
                 tags$li(tags$b("NMDOMSPP"), ": non-merchantable dominant species grouping."),
                 tags$li(tags$b("PWDOMSPP"), ": pulpwood dominant species grouping."),
                 tags$li(tags$b("STDOMSPP"), ": sawtimber dominant species grouping."),
                 tags$li(tags$b("DOMTYPE"), ": stand-level dominant species grouping."),
                 tags$li(tags$b("VEGCLASS"), ": Region 8 size-density vegetation class.")
               ),
               tags$p(tags$b("Common equations:"), " SSSIZE = sum(HT * TEXPF) / SSTPA; NMSIZE = sum(BAWTD) / NMBA; PWSIZE = sum(BAWTD) / PWBA; STSIZE = sum(BAWTD) / STBA")
      )
    )

    meta_desc <- switch(
      toupper(attr_name),
      "RUNTITLE" = "Run title: the named simulation or scenario group in the FVS database. It identifies the run that produced this row.",
      "CASEID" = "Case ID: the unique identifier for this FVS case or stand record used to join related tables.",
      "STAND_CN" = "Stand CN: the inventory stand control number or stand-level identifier used to match records across the database.",
      "STANDID" = "Stand ID: the stand identifier stored in the FVS output database.",
      "VARIANT" = "Variant: the FVS model variant code used for this run, which controls the variant-specific logic and table structure.",
      "REGION" = "Region: the USFS region setting used by the app to choose the appropriate vegetation-classification rules.",
      "YEAR" = "Year: the simulation year for the selected record.",
      "CY" = "Cycle: the processing cycle number for the selected record.",
      "No descriptive text is available for this field."
    )

    showModal(modalDialog(
      title = paste("Attribute Logic:", attr_name),
      p(tags$b("Region:"), region_value),
      p(tags$b("CASEID:"), case_value, " | ", tags$b("RUNTITLE:"), run_title_value, " | ", tags$b("STAND_CN:"), stand_cn_value),
      p(tags$b("STANDID:"), stand_value, " | ", tags$b("YEAR:"), year_value),
      p(tags$b("Selected Value:"), attr_value),
      hr(),
      tags$div(style = "white-space: pre-line;", explanation),
      if (show_flowchart) {
        tagList(
          h5("Decision Flowchart"),
            div(class = if (show_side_legend) "flowchart-layout" else NULL,
              div(class = "flowchart-wrap",
                  tags$div(id = flowchart_id, class = "mermaid", flowchart_text)
              ),
              if (show_hss14c_legend) hss14c_legend_ui,
              if (show_hss15_legend) hss15_legend_ui,
                if (show_mpsg_hss_legend) mpsg_hss_legend_ui,
                if (show_r8_legend) r8_legend_ui
          ),
          tags$div(class = "flowchart-resize-handle", title = "Drag to resize")
        )
      } else {
        tags$div(
          class = "status-block",
          tags$h5("Field Description"),
          tags$p(meta_desc)
        )
      },
      easyClose = TRUE,
      size = "l",
      footer = tagList(
        actionButton("toggle_flowchart_size", "Expand/Collapse", class = "btn btn-outline-secondary"),
        modalButton("Close")
      )
    ))

    if (show_flowchart) {
      session$sendCustomMessage("setFlowchartModalClass", list())
      session$sendCustomMessage("enableFlowchartResize", list(id = flowchart_id))
      session$sendCustomMessage("renderMermaid", list(id = flowchart_id))
    }
  })

  observeEvent(input$toggle_flowchart_size, {
    if (!(toupper(input$output_preview_cell_clicked$col) %in% character(0))) {
      session$sendCustomMessage("toggleFlowchartModal", list(id = "attr_flowchart_mermaid"))
    }
  })

  observeEvent(input$runMain, {
    log_val("")
    terminal_log_val("")

    input_db <- selected_db()
    output_parent_dir <- selected_output_dir()

    if (is.null(input_db) || !file.exists(input_db) ||
        is.null(output_parent_dir) || !nzchar(output_parent_dir)) {
      log_val("Please select a valid input database and output directory.")
      return()
    }

    run_titles <- input$runTitles %||% character(0)
    run_titles <- trimws(as.character(run_titles))
    run_titles <- gsub('^"|"$', "", run_titles)
    run_titles <- run_titles[nzchar(run_titles)]

    if (!isTRUE(input$allRuns) && length(run_titles) == 0) {
      log_val("No run titles were provided while 'Process All Runs' is unchecked.")
      return()
    }

    append_log <- function(msg) {
      current_log <- isolate(log_val())
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      log_val(paste0(current_log, "[", timestamp, "] ", msg, "\n"))
    }

    format_duration_display <- function(seconds_val) {
      seconds_num <- suppressWarnings(as.numeric(seconds_val))
      if (is.na(seconds_num) || seconds_num < 0) {
        return("unknown duration")
      }

      if (seconds_num >= 3600) {
        return(sprintf("%.2f hours", seconds_num / 3600))
      }
      if (seconds_num >= 60) {
        return(sprintf("%.2f minutes", seconds_num / 60))
      }
      sprintf("%.2f seconds", seconds_num)
    }

    append_log(sprintf("Processing input database: %s", input_db))

    capture_main_console <- function(expr) {
      log_file <- tempfile(pattern = "vegclass_terminal_", fileext = ".log")
      con <- file(log_file, open = "wt")
      output_sinks_before <- sink.number(type = "output")
      message_sinks_before <- sink.number(type = "message")

      sink(con, type = "output")
      sink(con, type = "message")

      result <- NULL
      captured_error <- NULL

      tryCatch(
        {
          result <- force(expr)
        },
        error = function(e) {
          captured_error <<- e
        }
      )

      while (sink.number(type = "message") > message_sinks_before) sink(type = "message")
      while (sink.number(type = "output") > output_sinks_before) sink(type = "output")
      close(con)

      captured_lines <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character(0))
      unlink(log_file)

      list(
        result = result,
        error = captured_error,
        text = paste(captured_lines, collapse = "\n")
      )
    }

    run_in_progress(TRUE)
    on.exit(run_in_progress(FALSE), add = TRUE)

    withProgress(message = "Running vegClass processing", value = 0, {
      num_cores <- suppressWarnings(as.integer(input$num_cores))
      if (is.na(num_cores) || num_cores < 1) {
        num_cores <- 1L
      }

      out_base <- tools::file_path_sans_ext(basename(input_db))
      output_csv_name <- trimws(input$outputCsvName)
      if (!nzchar(output_csv_name)) {
        output_csv_name <- paste0(out_base, ".csv")
      } else if (!grepl("\\.csv$", output_csv_name, ignore.case = TRUE)) {
        output_csv_name <- paste0(output_csv_name, ".csv")
      }
      output_file <- file.path(output_parent_dir, output_csv_name)

      if (!dir.exists(output_parent_dir)) {
        dir.create(output_parent_dir, recursive = TRUE, showWarnings = FALSE)
      }

      append_log(sprintf("Processing: %s", input_db))
      append_log(sprintf("Run mode: allRuns=%s | runTitles=%s | startYear=%s | endYear=%s | startCycle=%s | endCycle=%s | num_cores=%d",
                         as.character(input$allRuns),
                         if (length(run_titles) == 0) "<none>" else paste(run_titles, collapse = ", "),
                         as.character(input$startYear),
                         as.character(input$endYear),
                         as.character(input$startCycle),
                         as.character(input$endCycle),
                         num_cores))
      incProgress(0.3, detail = "Running main()")

      last_progress <- 0.3
      progress_callback <- function(info) {
        global_pct <- as.numeric(info$global_percent %||% 0)
        global_done <- as.integer(info$global_completed %||% 0)
        global_total <- as.integer(info$global_total %||% 0)

        target <- 0.3 + (max(0, min(100, global_pct)) / 100) * 0.69
        delta <- target - last_progress
        if (is.finite(delta) && delta > 0) {
          incProgress(delta, detail = sprintf("Processed %d/%d stands", global_done, global_total))
          last_progress <<- target
        }
      }

      # selectInput returns strings; convert numeric region choices to numbers
      # so main() validation accepts 1/2/3/8, while preserving MPSG/CUSTOM.
      region_value_raw <- trimws(as.character(input$region %||% ""))
      region_value_num <- suppressWarnings(as.numeric(region_value_raw))
      region_arg <- if (!is.na(region_value_num) && region_value_num %in% c(1, 2, 3, 8)) {
        region_value_num
      } else {
        toupper(region_value_raw)
      }

      tryCatch({
        captured <- capture_main_console(
          main(
            input = input_db,
            output = output_file,
            num_cores = num_cores,
            runTitles = run_titles,
            allRuns = input$allRuns,
            region = region_arg,
            MPSGcovTyp = if (identical(region_arg, "MPSG")) input$MPSGcovTyp else NULL,
            addHSS = input$addHSS,
            addCompute = input$addCompute,
            addPotFire = input$addPotFire,
            addFuels = input$addFuels,
            addCarbon = input$addCarbon,
            addVolume = input$addVolume,
            overwriteOut = input$overwriteOut,
            vol1DBH = input$vol1DBH,
            vol2DBH = input$vol2DBH,
            vol3DBH = input$vol3DBH,
            startYear = input$startYear,
            endYear = input$endYear,
            startCycle = input$startCycle,
            endCycle = input$endCycle,
            InvDB = inv_db_path(),
            InvStandTbl = input$invStandTbl,
            customVars = resolved_custom_vars_path(),
            customOutputScripts = if (isTRUE(input$enableCustomOutputScripts)) resolved_custom_output_scripts() else character(0),
            excludeAttributes = excluded_attributes(),
            removeCaseIndices = input$removeCaseIndices,
            show_progress = TRUE,
            progress_callback = progress_callback
          )
        )

        terminal_text <- gsub("\r", "\n", captured$text, fixed = TRUE)
        terminal_lines <- unlist(strsplit(terminal_text, "\n", fixed = TRUE), use.names = FALSE)

        # Drop progress-bar redraw lines like: [====      ]  42%
        is_progress_line <- grepl("^\\[[= ]+\\]\\s*\\d{1,3}%\\s*$", trimws(terminal_lines))
        terminal_lines <- terminal_lines[!is_progress_line]

        terminal_text <- paste(terminal_lines, collapse = "\n")
        terminal_text <- gsub("\n{3,}", "\n\n", terminal_text)
        terminal_log_val(if (nzchar(trimws(terminal_text))) terminal_text else "No terminal output was captured for this run.")

        if (!is.null(captured$error)) {
          stop(captured$error$message)
        }

        main_result <- captured$result

        incProgress(1, detail = "Completed")

        if (file.exists(output_file) && file.info(output_file)$size > 0) {
          last_output_file(output_file)
          append_log(sprintf("Output written to: %s", output_file))

          if (is.list(main_result) && !is.null(main_result$run_summaries)) {
            for (run_name in names(main_result$run_summaries)) {
              rs <- main_result$run_summaries[[run_name]]
              not_processed <- as.integer(rs$not_processed_stands %||% 0)
              processed <- as.integer(rs$processed_stands %||% 0)
              total <- as.integer(rs$total_stands %||% 0)
              run_minutes <- as.numeric(rs$duration_minutes %||% NA_real_)

              append_log(sprintf("Run %s summary: %d/%d stands processed.", run_name, processed, total))
              if (!is.na(run_minutes)) {
                append_log(sprintf("Run %s time: %s.", run_name, format_duration_display(run_minutes * 60)))
              }

              if (not_processed > 0) {
                append_log(sprintf("Run %s had %d stand(s) not processed.", run_name, not_processed))

                status_counts <- rs$status_counts %||% list()
                if (length(status_counts) > 0) {
                  status_txt <- paste(
                    paste0(names(status_counts), "=", unlist(status_counts)),
                    collapse = ", "
                  )
                  append_log(sprintf("Run %s skipped status counts: %s", run_name, status_txt))
                }

                skipped_ids <- as.character(rs$skipped_standids %||% character(0))
                skipped_ids <- skipped_ids[nzchar(skipped_ids)]
                if (length(skipped_ids) > 0) {
                  show_n <- min(20, length(skipped_ids))
                  append_log(sprintf(
                    "Run %s skipped stand IDs (first %d of %d): %s",
                    run_name,
                    show_n,
                    length(skipped_ids),
                    paste(skipped_ids[seq_len(show_n)], collapse = ", ")
                  ))
                }
              }
            }

            total_seconds <- as.numeric(main_result$duration_seconds %||% NA_real_)
            if (!is.na(total_seconds)) {
              append_log(sprintf("Total processing time: %s.", format_duration_display(total_seconds)))
            }
          }
        } else {
          append_log(sprintf("main() completed but no output rows were written to: %s", output_file))
          append_log("Check runTitles/allRuns filters and startYear, or try allRuns=TRUE to validate records exist.")
        }
      }, error = function(e) {
        append_log(sprintf("Error processing %s: %s", input_db, e$message))
      })
    })

    append_log("Processing finished.")
  })

  output$log <- renderText({
    log_val()
  })

  output$terminal_log <- renderText({
    terminal_log_val()
  })
}

app <- shinyApp(ui = ui, server = server)

if (interactive()) {
  runApp(app, launch.browser = TRUE)
} else {
  app
}
