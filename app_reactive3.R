library(shiny)
library(DT)
library(tidyverse)
library(fplr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

attribute <- function(item, ...) {
  attribute <- as.list(...)
  as.numeric(attribute[item])
}

#points_per_game <- players$`Points per game`

teams <- fpl_get_teams() %>%
  select(code, name)

players <- fplr::fpl_get_player_all() %>%
  select(team_code, now_cost, ict_index, points_per_game,total_points, element_type, web_name,
         chance_of_playing_next_round, chance_of_playing_this_round, form) %>%
  distinct() %>%
  left_join(teams, by = c("team_code" = "code")) %>%
  mutate(Position = case_when(
    element_type == 1 ~ "Goalkeeper",
    element_type == 2 ~ "Defender",
    element_type == 3 ~ "Midfielder",
    element_type == 4 ~ "Striker"
  )) %>%
  ## need separate binary cols for each criteria
  mutate(dummy = 1) %>%
  pivot_wider(names_from = team_code,
              names_prefix = "team_",
              values_from = dummy) %>%
  mutate(dummy = 1) %>%
  pivot_wider(names_from = element_type,
              names_prefix = "position_",
              values_from = dummy) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  rename(Player = web_name, `Cost (£mil)` = now_cost,
         `ICT index` = ict_index,
         `Points per game` = points_per_game,
         `Total points` = total_points,
         Team = name) %>%
  mutate(Selected = 0) %>%
  relocate(Selected,Player, Team, Position) 



shinyApp(
  ui = fluidPage(
    
    # App title ----
    titlePanel("FPL Dashboard"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        #actionButton(
        #  inputId = "show",
        #  label = "Show players"
        #),
        actionButton("solver", "Solve"),
        actionButton("reset", "Reset")
        # Input: Slider for the number of bins ----
        # sliderInput(inputId = "bins",
        #            label = "Number of bins:",
        #           min = 1,
        #          max = 50,
        #         value = 30)
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
    
    DTOutput('x1'),
    textOutput("text"),
    textOutput("text2"),
    #verbatimTextOutput("print")
    DTOutput('solution')
  ))),
  server = function(input, output, session) {
    
    session$onSessionEnded(function() { stopApp() })
    
    x = reactiveValues(df = NULL)
    
    observe({
      df <- as.data.frame(players)
      #df$Date = Sys.time() + seq_len(nrow(df))
      x$df <- df
    })
    
    output$x1 = renderDT(x$df, selection = 'none', 
                         editable = TRUE,
                         options=list(columnDefs = list(list(visible=FALSE, targets=c(0,12,13,
                                                                                      14,15,16,17,18,19,20,
                                                                                      21,22,23,24,25,26,27,28,29,30,
                                                                                      31,32,33,34,35)))))
    
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
    
   

      
    data <- reactive(
      
      x$df %>%
        filter(Selected == 0)
    )
    
    prepicked <- reactive(
      x$df %>%
        filter(Selected == 1)
    )
    
    selected <- reactive({
      
      #x$df[2,2] 
      nrow(prepicked())
      
    }  )
    
    output$text <- renderText({ 
      paste("There are ", selected(), "players selected") 
    })
    
    

    
    n <- reactive(
      
      nrow(data())
    )

    gk_selected <- reactive(
      x$df %>%
        filter(position_1 == 1, Selected == 1) %>%
        nrow()

    )
    
    gk_to_select <- reactive(
      2 - gk_selected()
      
    )
    
    df_selected <- reactive(
      x$df %>%
        filter(position_2 == 1, Selected == 1) %>%
        nrow()
      
    )
    
    df_to_select <- reactive(
      5 - df_selected()
      
    )
    
    mf_selected <- reactive(
      x$df %>%
        filter(position_3 == 1, Selected == 1) %>%
        nrow()
      
    )
    
    mf_to_select <- reactive(
      5 - mf_selected()
      
    )
    
    
    str_selected <- reactive(
      x$df %>%
        filter(position_4 == 1, Selected == 1) %>%
        nrow()
      
    )
    
    str_to_select <- reactive(
      3 - str_selected()
      
    )
    
    cost_selected <- reactive(
      x$df %>%
        filter(Selected == 1) %>%
        summarise(sum(`Cost (£mil)`)) %>%
        pull()
      
    )
    
    cost_to_select <- reactive(
      100 - cost_selected()
    )
    
    str_to_select <- reactive(
      3 - str_selected()
      
    )
    
    to_select <- reactive(
      as.numeric(15 - selected()) 
    )
    
    points_per_game <- reactive(
      data()$`Points per game`
      
    )
    
    ict <- reactive(
      data()$`ICT index`
      
    )
    
    form <- reactive(
      data()$form
      
    )
    
    output$text2 <- renderText({ 
      paste("There are ", n(), "players remaining for selection") 
    })
    
    solution <- eventReactive(input$solver, {
      
      #players <- playertablereactive()
      
      model <- MIPModel() %>%
        
        ###### SET UP DECISION AND OBJECTIVE  VARIABLES
        ##create decision variable to indicate inclusion in test
        ## here was are creating a variable u for every item i which is either 1 (selected)
        ## or 0 (not selected)
        add_variable(u[i], i = 1:n(), type = "binary") %>% 
        ##set objective var to maximise the sum information
        
        set_objective(sum_expr(ict()[i] * u[i], i = 1:n()),"max") %>%
        ##add constraint on exp score at threshold 
        #add_constraint(sum_expr(exp(i) * u[i], i = 1:n) <= 24) %>%
        
        ###### TOTAL ITEM CONSTRAINTS 
        add_constraint(sum_expr(u[i], i = 1:n()) == to_select()) %>%
        
        ###### Total spend constraint
        add_constraint(sum_over(u[i] * attribute(i,data()$`Cost (£mil)`), i = 1:n()) <= cost_to_select()) %>%
        ###### TEAM CONSTRAINTS
        
        ## team_3
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_3), i = 1:n()) <= 3) %>%
        ## team_7
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_7), i = 1:n()) <= 3) %>%
        ## team_91
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_91), i = 1:n()) <= 3) %>%
        ## team_94
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_94), i = 1:n()) <= 3) %>%
        ## team_36
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_36), i = 1:n()) <= 3) %>%
        ## team_8
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_8), i = 1:n()) <= 3) %>%
        ## team_31
        add_constraint(sum_expr(u[i] * attribute(i, data()$team_31), i = 1:n()) <= 3) %>%
        ## team_11
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_11), i = 1:n()) <= 3) %>%
        ## team_54
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_54), i = 1:n()) <= 3) %>%
        ## team_13
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_13), i = 1:n()) <= 3) %>%
        ## team_2
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_2), i = 1:n()) <= 3) %>%
        ## team_14
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_14), i = 1:n()) <= 3) %>%
        ## team_43
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_43), i = 1:n()) <= 3) %>%
        ## team_1
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_1), i = 1:n()) <= 3) %>%
        ## team_4
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_4), i = 1:n()) <= 3) %>%
        ## team_17
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_17), i = 1:n()) <= 3) %>%
        ## team_20
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_20), i = 1:n()) <= 3) %>%
        ## team_6
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_6), i = 1:n()) <= 3) %>%
        ## team_21
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_21), i = 1:n()) <= 3) %>%
        ## team_39
        add_constraint(sum_expr(u[i] * attribute(i,data()$team_39), i = 1:n()) <= 3) %>%
        ### constraints for positions
        ## goalkeepers
        add_constraint(sum_expr(u[i] * attribute(i,data()$position_1), i = 1:n()) == gk_to_select()) %>%
        ## defender
        add_constraint(sum_expr(u[i] * attribute(i,data()$position_2), i = 1:n()) == df_to_select()) %>%
        ## midfielders
        add_constraint(sum_expr(u[i] * attribute(i,data()$position_3), i = 1:n()) == mf_to_select()) %>%
        ## strikers
        add_constraint(sum_expr(u[i] * attribute(i,data()$position_4), i = 1:n()) == str_to_select()) #%>%
      
    })
    
    output$solution <- renderDT({
      validate(
        need(input$solver, 'Press solve to see solution'),
        need(solution(), "Solver not run yet"))
      
      result <- solve_model(solution(), with_ROI(solver = "glpk", verbose = TRUE))
      matching <- result %>% 
        get_solution(u[i])
      
      players <- data()
      players$Selected <- matching$value
      players <- players %>%
        filter(Selected == 1) %>%
        bind_rows(prepicked())
      
      # tmp <- playertablereactive()
      #  tmp$Selected <- matching$value
      # playertablereactive(tmp)
      
      
      DT::datatable(players, rownames = FALSE,
                    extensions = 'Buttons',
                    options=list(columnDefs = list(list(visible=FALSE, targets=c(11,12,13,
                                                                                 14,15,16,17,18,19,20,
                                                                                 21,22,23,24,25,26,27,28,29,30,
                                                                                 31,32,33,34))),
                                 pageLength = 15,
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv')))
    })
    
    
   # output$print <- renderPrint({
    #  x$df
    #})
  }
)
