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
    values$board <- random_board(values$size, input$mode == "classic")
    values$showHint <- FALSE
    values$solution <- NULL
  })

  output$board <- renderUI({
    values$board
    values$showHint

    isolate({
      size <- values$size

      div(
        id = "board-inner",
        lapply(seq(size), function(row) {
          tagList(
            div(
              class = "board-row",
              lapply(seq(size), function(col) {
                value <- board_entries(values$board)[row, col]
                visClass <- ifelse(value == 0, "off", "on")
                solutionClass <-
                  ifelse(
                    values$showHint && (values$solution[row, col] == 1),
                    "solution",
                    ""
                  )
                id <- sprintf("cell-%s-%s", row, col)

                actionLink(
                  id, NULL,
                  class = paste("board-cell", visClass, solutionClass),
                  `data-row` = row,
                  `data-col` = col
                )
              })
            ),
            div()
          )
        })
      )
    })
  })

  lapply(seq(max(allowed_sizes)), function(row) {
    lapply(seq(max(allowed_sizes)), function(col) {
      id <- sprintf("cell-%s-%s", row, col)
      observeEvent(input[[id]], {
        values$board <- play(values$board, row = row, col = col)
        if (values$showHint) {
          values$solution <- solve_board(values$board)
        }

        if (board_solved(values$board)) {
          delay(200, info("Good job!"))
        }
      })
    })
  })

  observeEvent(input$solve, {
    values$showHint <- !values$showHint
    if (values$showHint) {
      values$solution <- solve_board(values$board)
    }
  })

  observe({
    toggleClass(id = "solve", class = "btn-primary", condition = values$showHint)
  })
}
