#' Launch the Shiny App
#' @export
launch_shiny_app <- function() {
  appDir <- system.file("Shinyapp", package = "immoswiss")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
