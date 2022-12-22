library(shiny)
library(DT)
shinyApp(
  ui = fluidPage(
    DTOutput('x1'),
    textOutput("text"),
    verbatimTextOutput("print")
  ),
  server = function(input, output, session) {
    x = reactiveValues(df = NULL)
    
    observe({
      df <- iris
      #df$Date = Sys.time() + seq_len(nrow(df))
      x$df <- df
    })
    
    output$x1 = renderDT(x$df, selection = 'none', editable = TRUE)
    
    proxy = dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      
      # problem starts here
      x$df[i, j] <- isolate(DT::coerceValue(v, x$df[i, j]))
    
    })
    
    n <- reactive({
      
      x$df[2,2]
    }  )
    
    output$text <- renderText({ 
      paste("There are ", n(), "players to select from") 
    })
      
  
    
    output$print <- renderPrint({
      x$df
    })
  }
)
