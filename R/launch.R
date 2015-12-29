#' Run the interactive graphical interface to the game
#' @export
launch <- function() {
  shiny::runApp(system.file("shiny", package = "lightsout"),
                display.mode = "normal",
                launch.browser = TRUE)
}
