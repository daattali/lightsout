fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$script(src = "lightsout.js"),
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  selectInput("boardSize", "Board size",
              c("3x3" = "3", "5x5"= "5", "7x7" = "7", "9x9" = "9")),
  checkboxInput("fullRow", "Light the entire row", FALSE),
  actionButton("new", "New"),
  actionButton("solve", "Show solution"),
  uiOutput("board")
)
