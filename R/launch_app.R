#' @export
launch_app <- function() {
  appDir <- system.file("vietnam_covid", package = "vietnamcovid19")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vietnamcovid19`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}


