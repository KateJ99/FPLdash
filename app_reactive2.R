library(shiny)
library(DT)

options(shiny.reactlog = TRUE)

shinyApp(
  ui = fluidPage(
    actionButton(
      inputId = "go",
      label = "Update"
    ),
    textOutput('test'),
    DTOutput('x1')
  ),
  server = function(input, output, session) {
    
    my_event <- eventReactive(input$go, {
      x = iris
      x$Date = Sys.time() + seq_len(nrow(x))
      x <- as.data.frame(x)
    })
    
    rec_val <- reactiveValues(df = NULL)
    
    observe({
      rec_val$df <- my_event()
    })
    
    output$x1 <- renderDT(my_event(), selection = 'none', filter = "top", rownames = F, editable = T)
    
    proxy <- dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col + 1  # column index offset by 1
      v = info$value
      
      rec_val$df[i, j] <<- DT::coerceValue(v, rec_val$df[i, j])
      replaceData(proxy, rec_val$df, resetPaging = FALSE, rownames = FALSE)
      

    })
    
    n <- reactive(
      rec_val$df[1,1]
    )
    
    output$test <- renderText({ 
      paste("There are ", n, "players to select from") 
    })
  }
)