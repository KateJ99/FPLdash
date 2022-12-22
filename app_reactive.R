library(shiny)
library(DT)

shinyApp(
  ui = fluidPage(
    actionButton(
      inputId = "go",
      label = "Update"
    ),
    DTOutput('x1'),
    textOutput("test")
  ),
  server = function(input, output, session) {
    
    ### This is new ###
    
    evt_go_iris <- eventReactive(input$go, {
      x = iris
      x$Date = Sys.time() + seq_len(nrow(x))
      x <- as.data.frame(x)
    })
    
    rec_val = reactiveValues(df = NULL)
    
    observe({
      rec_val$iris   <- evt_go_iris()
    })
    
    ### This is new ###
    
    output$x1 = renderDT(rec_val$iris, selection = 'none', filter = "top", rownames = F, editable = T)
    
    proxy = dataTableProxy('x1')
    
    observeEvent(input$x1_cell_edit, {
      info = input$x1_cell_edit
      str(info)
      i = info$row
      j = info$col + 1  # column index offset by 1
      v = info$value
      rec_val$iris[i, j] <<- DT::coerceValue(v, rec_val$iris[i, j])
      replaceData(proxy, rec_val$iris, resetPaging = FALSE, rownames = FALSE)
      
      value <- rec_val[i,j]
    })
    
    output$test <- renderText({ 
      paste("There are ", value, "players to select from") 
    })
  }
)

