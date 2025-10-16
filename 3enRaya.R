library(shiny)

# --- Funciones auxiliares ---
antidiag <- function(m) {
  m[row(m) + col(m) == nrow(m) + 1]
}

ui <- fluidPage(
  titlePanel("Tres en Raya"),
  checkboxInput("vs_ai", "Jugar contra la máquina (O)", value = TRUE),
  fluidRow(
    column(12,
           lapply(1:3, function(i) {
             fluidRow(
               lapply(1:3, function(j) {
                 actionButton(inputId = paste0("cell_", i, "_", j),
                              label = "",
                              width = "80px",
                              style = "font-size: 24px; height: 80px;")
               })
             )
           }),
           br(),
           textOutput("status"),
           actionButton("reset", "Reiniciar juego")
    )
  )
)

server <- function(input, output, session) {
  board <- reactiveVal(matrix("", nrow = 3, ncol = 3))
  turn <- reactiveVal("X")
  game_over <- reactiveVal(FALSE)
  
  # Crear observadores para cada celda del tablero
  lapply(1:3, function(i) {
    lapply(1:3, function(j) {
      observeEvent(input[[paste0("cell_", i, "_", j)]], {
        # El jugador (X) hace un movimiento
        if (board()[i, j] == "" && !game_over() && turn() == "X") {
          b <- board()
          b[i, j] <- "X"
          board(b)
          
          if (check_game(b)) return() # Termina si el juego acabó
          
          turn("O") # Cambia el turno a la IA
          
          # La IA (O) hace un movimiento
          if (input$vs_ai && !game_over()) {
            # Usamos isolate para que ai_move no se vuelva a ejecutar si board() cambia
            isolate({
              ai_move()
            })
          }
        }
      })
    })
  })
  
  ai_move <- function() {
    b <- board()
    empty_cells <- which(b == "", arr.ind = TRUE)
    if (nrow(empty_cells) == 0) return()

    # Estrategia de la IA:
    # 1. Buscar una jugada para ganar.
    # 2. Si no, buscar una jugada para bloquear al oponente.
    # 3. Si no, hacer un movimiento aleatorio.

    # Función para encontrar un movimiento ganador o de bloqueo
    find_strategic_move <- function(player_symbol) {
      # Asegurarse de que empty_cells es una matriz
      if (is.vector(empty_cells)) {
        empty_cells <- t(as.matrix(empty_cells))
      }
      for (i in 1:nrow(empty_cells)) {
        temp_board <- b
        move <- empty_cells[i, , drop = FALSE]
        temp_board[move] <- player_symbol
        if (!is.null(check_winner(temp_board))) {
          return(move)
        }
      }
      return(NULL)
    }

    # 1. Buscar jugada ganadora para 'O'
    winning_move <- find_strategic_move("O")
    if (!is.null(winning_move)) {
      b[winning_move] <- "O"
      board(b)
      if (!check_game(b)) {
        turn("X")
      }
      return()
    }

    # 2. Buscar jugada para bloquear a 'X'
    blocking_move <- find_strategic_move("X")
    if (!is.null(blocking_move)) {
      b[blocking_move] <- "O"
      board(b)
      if (!check_game(b)) {
        turn("X")
      }
      return()
    }

    # 3. Movimiento aleatorio
    if (is.vector(empty_cells)) {
        empty_cells <- t(as.matrix(empty_cells))
    }
    random_move <- empty_cells[sample(nrow(empty_cells), 1), , drop = FALSE]
    b[random_move] <- "O"
    board(b)
    if (!check_game(b)) {
      turn("X")
    }
  }
  
  # Comprueba el estado del juego (ganador, empate o sigue)
  # Devuelve TRUE si el juego ha terminado, FALSE en caso contrario.
  check_game <- function(b) {
    winner <- check_winner(b)
    if (!is.null(winner)) {
      output$status <- renderText(paste("Ganador:", winner))
      game_over(TRUE)
      return(TRUE)
    } else if (all(b != "")) {
      output$status <- renderText("¡Empate!")
      game_over(TRUE)
      return(TRUE)
    }
    return(FALSE)
  }
  
  
  observe({
    b <- board()
    lapply(1:3, function(i) {
      lapply(1:3, function(j) {
        updateActionButton(session, paste0("cell_", i, "_", j),
                           label = b[i, j])
      })
    })
  })
  
  # Observador para el botón de reinicio
  observeEvent(input$reset, {
    board(matrix("", nrow = 3, ncol = 3))
    turn("X")
    game_over(FALSE)
    output$status <- renderText("")
  })
  
  check_winner <- function(b) {
    lines <- c(
      apply(b, 1, paste, collapse = ""),
      apply(b, 2, paste, collapse = ""),
      paste(diag(b), collapse = ""),
      paste(antidiag(b), collapse = "")
    )
    if ("XXX" %in% lines) return("X")
    if ("OOO" %in% lines) return("O")
    return(NULL)
  }
}

shinyApp(ui, server)