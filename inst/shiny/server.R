library(lightsout)
library(shinyjs)

server <- function(input, output, session) {
  values <- reactiveValues(
    size = NULL,
    board = NULL,
    showHint = NULL,
    solution = NULL
  )

  observeEvent(input$new, ignoreNULL = FALSE, {
    values$size <- as.numeric(input$boardSize)
    values$board <- random_board(values$size, !input$fullRow)
    values$showHint <- FALSE
    values$solution <- NULL
  })

  output$board <- renderUI({
    values$board
    values$showHint

    isolate({
      size <- values$size

      lapply(seq(size), function(row) {
        div(
          class = "board-row",
          lapply(seq(size), function(col) {
            value <- board_entries(values$board)[row, col]
            visClass <- ifelse(value == 0, "off", "on")
            showSolution <-
              values$showHint &&
              (values$solution[row, col] == 1)
            div(
              class = paste("board-cell", visClass),
              `data-row` = row,
              `data-col` = col,
              `data-value` = value,
              `data-solution` = ifelse(showSolution, "1", "0")
            )
          })
        )
      })
    })
  })

  observeEvent(input$click, {
    values$board <- play(values$board,
                         row = input$click$row,
                         col = input$click$col)
    if (values$showHint) {
      values$solution <- solve_board(values$board)
    }

    if (board_solved(values$board)) {
      delay(200, info("Good job!"))
    }
  })

  observeEvent(input$solve, {
    values$showHint <- TRUE
    values$solution <- solve_board(values$board)
  })
}
