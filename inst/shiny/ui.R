fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  selectInput("boardSize", "Board size",
              unlist(lapply(allowed_sizes,
                            function(n) setNames(n, paste0(n, "x", n))))),
  selectInput("mode", "Game mode (what lights get flipped)",
              c("Classic (adjacent lights only)" = "classic",
                "Variant (entire row and column)" = "variant")),
  helpText("sdfds"),
  actionButton("new", "New game"),
  actionButton("solve", "Show solution"),
  br(),
  uiOutput("board")
)
