#' Launch the dashboard for Vietnam Covid-19
#' 
#' @description this function will launch the shiny app dashboard embedded in the package
#' 
#' @export

launch_app <- function() {
  appDir <- system.file("vietnam_covid", package = "vietnamcovid19")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `vietnamcovid19`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = "normal")
}


