source("global.R")

fluidPage(
  title = "Lights Out / Dean Attali",
  shinyjs::useShinyjs(),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet"),

    # Favicon
    tags$link(rel = "shortcut icon", type="image/x-icon", href="favicon.ico"),

    # Facebook OpenGraph tags
    tags$meta(property = "og:title", content = "Lights Out"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = "http://daattali.com/shiny/lightsout/"),
    tags$meta(property = "og:image", content = "http://daattali.com/shiny/img/lightsout.png"),

    # Twitter summary cards
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:site", content = "@daattali"),
    tags$meta(name = "twitter:creator", content = "@daattali"),
    tags$meta(name = "twitter:title", content = "Lights Out"),
    tags$meta(name = "twitter:description", content = "Play the classic Lights Out puzzle game in R"),
    tags$meta(name = "twitter:image", content = "http://daattali.com/shiny/img/lightsout.png")
  ),
  div(id = "header",
      div(id = "title",
          "Lights Out"
      ),
      div(id = "subsubtitle",
          "Created by",
          tags$a(href = "http://deanattali.com/", "Dean Attali"),
          HTML("&bull;"),
          "Package available",
          tags$a(href = "https://github.com/daattali/lightsout", "on GitHub"),
          HTML("&bull;"),
          tags$a(href = "http://daattali.com/shiny/", "More apps"), "by Dean"
      )
  ),
  fluidRow(
    column(4, wellPanel(
      id = "leftPanel",
      selectInput("boardSize", "Board size",
                  unlist(lapply(allowed_sizes,
                                function(n) setNames(n, paste0(n, "x", n))))),
      selectInput("mode", "Game mode",
                  c("Classic" = "classic",
                    "Variant" = "variant")),
      actionButton("newgame", "New Game", class = "btn-lg"),
      hr(),
      p("Lights Out is a puzzle game consisting of a grid of lights that",
      "are either", em("on"), "(light green) or", em("off"), "(dark green). In", em("classic"), "mode, pressing any light will toggle",
      "it and its adjacent lights. In", em("variant"), "mode, pressing a light will toggle all the lights in its",
      "row and column. The goal of the game is to", strong("switch all the lights off."))
    )),
    column(8,
      hidden(div(id = "congrats", "Good job, you won!")),
      uiOutput("board"),
      br(),
      actionButton("solve", "Show solution")
    )
  )
)
