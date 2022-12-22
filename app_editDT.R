#remotes::install_github("jbryer/DTedit")
library(DTedit)


server <- function(input, output) {
  
  mydata <- reactiveVal({
    data.frame(
      Buy = c('Tea', 'Biscuits', 'Apples'),
      Quantity = c(7, 2, 5),
      stringsAsFactors = FALSE
    )
  })
  
  Grocery_List_Results <- dtedit(
    input, output,
    name = 'Grocery_List',
    thedata = mydata,
    edit.cols = c('Quantity'),
    view.cols = c('Buy', 'Quantity'),
    show.delete = FALSE,
    show.insert = FALSE,
    show.copy = FALSE
  )
  
  n <- observe(
    
  n <- Grocery_List_Results$thedata[2,2] )
  
  output$text <- renderText({ 
    paste("There are ", n$n , "players to select from") 
  })
}

ui <- fluidPage(
  h3('Grocery List'),
  uiOutput('Grocery_List'),
  textOutput("text")
)

shinyApp(ui = ui, server = server)