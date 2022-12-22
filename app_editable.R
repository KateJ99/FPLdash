library(shiny)
library(DT)



ui<-function(id){
  ns <- NS(id)
  DT::dataTableOutput(ns("mod_table"))
}

server<-function(id,data,disable_col){
  moduleServer(id,
               function(input,output,session){
                 print('hi')
                 v<-reactiveValues(data = data)
                 print('here we got v')
                 
                 
                 output$mod_table <- renderDT(v$data,
                                              editable = list(target = 'cell',
                                                              disable = list(columns=c(disable_col))))
                 proxy = dataTableProxy('mod_table')
                 observeEvent(input$mod_table_cell_edit, {
                   info = input$mod_table_cell_edit
                   v$data <<- editData(v$data, info)
                   replaceData(proxy, v$data, resetPaging = FALSE)
                 })
                 
                 
                 return(v)
               }
  )}

shinyApp(ui = ui, server = server)