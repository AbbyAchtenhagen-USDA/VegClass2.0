#' Run the vegClass Shiny App
#'
#' Launches the packaged vegClass Shiny application.
#'
#' @param launch.browser Logical. Passed to [shiny::runApp()].
#' @param ... Additional arguments passed to [shiny::runApp()] when
#'   `launch = TRUE`.
#' @param launch Logical. If `TRUE`, run the app. If `FALSE`, return the app
#'   object.
#'
#' @return A Shiny app object when `launch = FALSE`; otherwise starts the app.
#' @export
runVegClassApp <- function(launch.browser = TRUE, ..., launch = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the app. Please install it.")
  }

  app_dir <- system.file("shiny-app", package = "vegClass2.0")
  if (!nzchar(app_dir)) {
    fallback_dir <- normalizePath(
      file.path(getwd(), "inst", "shiny-app"),
      winslash = "/",
      mustWork = FALSE
    )
    if (dir.exists(fallback_dir)) {
      app_dir <- fallback_dir
    } else {
      stop("Could not locate packaged app assets under inst/shiny-app.")
    }
  }

  app_file <- file.path(app_dir, "app.R")
  if (!file.exists(app_file)) {
    stop(sprintf("App entry point was not found: %s", app_file))
  }

  app_env <- new.env(parent = globalenv())
  app_env$app_root_path <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  app_env$vegclass_app_autorun <- FALSE

  sys.source(app_file, envir = app_env)

  if (!exists("app", envir = app_env, inherits = FALSE)) {
    stop("The packaged app did not create an 'app' object.")
  }

  app_obj <- get("app", envir = app_env, inherits = FALSE)
  if (!isTRUE(launch)) {
    return(app_obj)
  }

  shiny::runApp(app_obj, launch.browser = launch.browser, ...)
}
