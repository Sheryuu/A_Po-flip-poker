library(shiny)

# UI部分
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .card-button {
        width: 100px;
        height: 100px;
        font-size: 24px;
        margin: 5px;
        display: inline-block;
        vertical-align: top;
      }
      .card-row {
        display: flex;
        justify-content: center;
        flex-wrap: wrap;
        width: 100%;
      }
      .center-content {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
        min-height: 100vh;
      }
      .center-panel {
        text-align: center;
      }
      .title-panel {
        text-align: center;
        margin-bottom: 20px;
      }
      .exit-button {
        margin-top: 20px;
      }
      @media (max-width: 600px) {
        .card-button {
          width: 80px;
          height: 80px;
          font-size: 20px;
        }
        .card-row > div {
          flex: 1 1 50%;
          max-width: 50%;
        }
      }
      @media (min-width: 601px) and (max-width: 1024px) {
        .card-button {
          width: 100px;
          height: 100px;
          font-size: 24px;
        }
        .card-row > div {
          flex: 1 1 25%;
          max-width: 25%;
        }
      }
      @media (min-width: 1025px) {
        .card-button {
          width: 120px;
          height: 120px;
          font-size: 28px;
        }
        .card-row > div {
          flex: 1 1 16.66%;
          max-width: 16.66%;
        }
      }
    "))
  ),
  div(class = "center-content",
      div(class = "title-panel",
          h2("翻牌游戏")
      ),
      div(class = "center-panel",
          selectInput("num_pairs", "选择数字对数:", choices = 1:10, selected = 4)
      ),
      div(class = "center-panel",
          uiOutput("game_ui"),
          textOutput("message"),
          textOutput("time_taken")
      ),
      div(class = "exit-button",
          actionButton("exit", "退出游戏")
      )
  )
)

# 服务器部分
server <- function(input, output, session) {
  # 生成随机数字对
  generate_pairs <- reactive({
    num_pairs <- as.numeric(input$num_pairs)
    pairs <- rep(1:num_pairs, each = 2)
    sample(pairs)
  })
  
  # 初始化游戏状态
  game_state <- reactiveValues(
    cards = NULL,
    revealed = NULL,
    first_card = NULL,
    second_card = NULL,
    matched = NULL,
    message = "",
    start_time = NULL,
    end_time = NULL,
    lock = FALSE
  )
  
  observeEvent(input$num_pairs, {
    num_pairs <- as.numeric(input$num_pairs)
    game_state$cards <- generate_pairs()
    game_state$revealed <- rep(FALSE, 2 * num_pairs)
    game_state$matched <- rep(FALSE, 2 * num_pairs)
    game_state$first_card <- NULL
    game_state$second_card <- NULL
    game_state$message <- ""
    game_state$start_time <- Sys.time()
    game_state$end_time <- NULL
    game_state$lock <- FALSE
  })
  
  observeEvent(input$card, {
    card_index <- as.numeric(input$card)
    
    if (game_state$lock || game_state$matched[card_index]) {
      return()
    }
    
    if (game_state$revealed[card_index]) {
      game_state$revealed[card_index] <- FALSE
      if (game_state$first_card == card_index) {
        game_state$first_card <- NULL
      } else if (game_state$second_card == card_index) {
        game_state$second_card <- NULL
      }
    } else {
      if (!is.null(game_state$first_card) && !is.null(game_state$second_card)) {
        showModal(modalDialog(
          title = "提示",
          "只能同时展示两个数字",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      
      game_state$revealed[card_index] <- TRUE
      
      if (is.null(game_state$first_card)) {
        game_state$first_card <- card_index
      } else if (is.null(game_state$second_card)) {
        game_state$second_card <- card_index
        
        if (game_state$cards[game_state$first_card] == game_state$cards[game_state$second_card]) {
          game_state$matched[game_state$first_card] <- TRUE
          game_state$matched[game_state$second_card] <- TRUE
          game_state$first_card <- NULL
          game_state$second_card <- NULL
          
          if (all(game_state$matched)) {
            game_state$message <- "获胜"
            game_state$end_time <- Sys.time()
          }
        } else {
          game_state$lock <- TRUE
          invalidateLater(1000, session)
          observe({
            if (game_state$lock) {
              game_state$revealed[game_state$first_card] <- FALSE
              game_state$revealed[game_state$second_card] <- FALSE
              game_state$first_card <- NULL
              game_state$second_card <- NULL
              game_state$lock <- FALSE
            }
          })
        }
      }
    }
  })
  
  output$game_ui <- renderUI({
    num_pairs <- as.numeric(input$num_pairs)
    cards <- game_state$cards
    revealed <- game_state$revealed
    matched <- game_state$matched
    
    card_buttons <- lapply(1:(2 * num_pairs), function(i) {
      div(
        actionButton(
          inputId = paste0("card_", i),
          label = if (revealed[i]) cards[i] else "",
          class = "card-button",
          onclick = sprintf("Shiny.setInputValue('card', %d, {priority: 'event'})", i),
          style = if (matched[i]) "visibility: hidden;" else ""
        )
      )
    })
    
    div(class = "card-row", do.call(tagList, card_buttons))
  })
  
  output$message <- renderText({
    game_state$message
  })
  
  output$time_taken <- renderText({
    if (!is.null(game_state$end_time)) {
      time_taken <- difftime(game_state$end_time, game_state$start_time, units = "secs")
      paste("游戏时间:", round(time_taken, 2), "秒")
    }
  })
  
  observeEvent(input$exit, {
    stopApp()
  })
}

# 运行应用
shinyApp(ui = ui, server = server)
