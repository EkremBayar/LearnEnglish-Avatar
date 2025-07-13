#' Run My Shiny Application
#'
#' This function runs the Shiny application located in the inst/shiny/my_app/ directory
#' within the package.
#'
#' @export
#' @importFrom shiny runApp
#' @importFrom fs path_package
#'
#' @examples
#' \dontrun{
#'   run_transcript_app()
#' }
run_transcript_app <- function() {
  app_dir <- fs::path_package("LearnEnglishWithAvatar", "shiny")

  if (!dir.exists(app_dir)) {
    stop("Shiny app directory not found. Please ensure the package is installed correctly.")
  }

  # Shiny uygulamasını çalıştır
  shiny::runApp(app_dir, display.mode = "normal")
}
