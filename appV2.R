library(shiny)
library(fplscrapR)
library(DT)
library(tidyverse)
library(fplr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

## function
attribute <- function(item, ...) {
  attribute <- as.list(...)
  as.numeric(attribute[item])
}

teams <- fpl_get_teams() %>%
  select(code, name)

players <- fplr::fpl_get_player_all() %>%
  select(team_code, now_cost, ict_index, points_per_game,total_points, element_type, web_name) %>%
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

n <- nrow(players)
points_per_game <- players$`Points per game`

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("FPL Dashboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      actionButton(
        inputId = "show",
        label = "Show players"
      ),
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
      tabsetPanel(
        tabPanel("Selections",
                 DTOutput('playertable'),
                 textOutput("text"),
                 DTOutput('solution')
        ),
        tabPanel("Goal predictor",
            # Output: Games table ----
            dataTableOutput(outputId = "Fixtures")
          
        )
        
        
      )
      
   
      
    )
  )
)

server <- function(input, output) {
  
  evt_go_players <- eventReactive(input$show, {
    x = players
    x <- as.data.frame(x)
  })
  
  #playertablereactive <- as.data.frame(players)
  
 
  
  rec_val = reactiveValues(df = NULL)
  
  observe({
    rec_val$players   <- evt_go_players()
  })
  
  
  output$playertable <- renderDT({
    
    
    DT::datatable(rec_val$players, rownames = FALSE,
                  options=list(columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11,12,13,
                                                                               14,15,16,17,18,19,20,
                                                                               21,22,23,24,25,26,27,28,29,30,
                                                                               31)))),
                  editable = list(target = "row", disable = list(columns = c(1,2,3, 4, 5,6,7,
                                                                             8,9,10,11,12,13,
                                                                             14,15,16,17,18,19,20,
                                                                             21,22,23,24,25,26,27,28,29,30,
                                                                             31))))
    
  })
  
  proxy = dataTableProxy('playertable')
 # observeEvent(input$playertable_cell_edit, {
  #  playertablereactive <<- editData(playertablereactive, input$playertable_cell_edit, 'playertable')

   # player_n <<- playertablereactive %>%
    #  filter(Selected == 0) %>%
     # nrow()
#  })
  
  observeEvent(input$playertable_cell_edit, {
    info = input$playertable_cell_edit
    str(info)
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    rec_val$players[i, j] <<- DT::coerceValue(v, rec_val$players[i, j])
    replaceData(proxy, rec_val$players, resetPaging = FALSE, rownames = FALSE)
    
  #  player_n <- rec_val$players %>%
   #     filter(Selected == 0) %>%
    #   nrow()
    
  })
  
  #observeEvent(input$playertable_cell_edit, {

    
#  })
  
  n <- reactive({
    
    x$df[1,1]
  }  )
  
  output$text <- renderText({ 
    paste("There are ", player_n, "players to select from") 
    })
  
  solution <- eventReactive(input$solver, {
    
    #players <- playertablereactive()
    
    model <- MIPModel() %>%
      
      ###### SET UP DECISION AND OBJECTIVE  VARIABLES
      ##create decision variable to indicate inclusion in test
      ## here was are creating a variable u for every item i which is either 1 (selected)
      ## or 0 (not selected)
      add_variable(u[i], i = 1:n, type = "binary") %>% 
      ##set objective var to maximise the sum information
      
      set_objective(sum_expr(points_per_game[i] * u[i], i = 1:n),"max") %>%
      ##add constraint on exp score at threshold 
      #add_constraint(sum_expr(exp(i) * u[i], i = 1:n) <= 24) %>%
      
      ###### TOTAL ITEM CONSTRAINTS 
      add_constraint(sum_expr(u[i], i = 1:n) == 15) %>%
      
      ###### Total spend constraint
      add_constraint(sum_over(u[i] * attribute(i,playertablereactive()$`Cost (£mil)`), i = 1:n) <= 100) %>%
      ###### TEAM CONSTRAINTS
      ## team_3
      add_constraint(sum_expr(u[i] * attribute(i,players$team_3), i = 1:n) <= 3) %>%
      ## team_7
      add_constraint(sum_expr(u[i] * attribute(i,players$team_7), i = 1:n) <= 3) %>%
      ## team_91
      add_constraint(sum_expr(u[i] * attribute(i,players$team_91), i = 1:n) <= 3) %>%
      ## team_94
      add_constraint(sum_expr(u[i] * attribute(i,players$team_94), i = 1:n) <= 3) %>%
      ## team_36
      add_constraint(sum_expr(u[i] * attribute(i,players$team_36), i = 1:n) <= 3) %>%
      ## team_8
      add_constraint(sum_expr(u[i] * attribute(i,players$team_8), i = 1:n) <= 3) %>%
      ## team_31
      add_constraint(sum_expr(u[i] * attribute(i,players$team_31), i = 1:n) <= 3) %>%
      ## team_11
      add_constraint(sum_expr(u[i] * attribute(i,players$team_11), i = 1:n) <= 3) %>%
      ## team_54
      add_constraint(sum_expr(u[i] * attribute(i,players$team_54), i = 1:n) <= 3) %>%
      ## team_13
      add_constraint(sum_expr(u[i] * attribute(i,players$team_13), i = 1:n) <= 3) %>%
      ## team_2
      add_constraint(sum_expr(u[i] * attribute(i,players$team_2), i = 1:n) <= 3) %>%
      ## team_14
      add_constraint(sum_expr(u[i] * attribute(i,players$team_14), i = 1:n) <= 3) %>%
      ## team_43
      add_constraint(sum_expr(u[i] * attribute(i,players$team_43), i = 1:n) <= 3) %>%
      ## team_1
      add_constraint(sum_expr(u[i] * attribute(i,players$team_1), i = 1:n) <= 3) %>%
      ## team_4
      add_constraint(sum_expr(u[i] * attribute(i,players$team_4), i = 1:n) <= 3) %>%
      ## team_17
      add_constraint(sum_expr(u[i] * attribute(i,players$team_17), i = 1:n) <= 3) %>%
      ## team_20
      add_constraint(sum_expr(u[i] * attribute(i,players$team_20), i = 1:n) <= 3) %>%
      ## team_6
      add_constraint(sum_expr(u[i] * attribute(i,players$team_6), i = 1:n) <= 3) %>%
      ## team_21
      add_constraint(sum_expr(u[i] * attribute(i,players$team_21), i = 1:n) <= 3) %>%
      ## team_39
      add_constraint(sum_expr(u[i] * attribute(i,players$team_39), i = 1:n) <= 3) %>%
      ### constraints for positions
      ## goalkeepers
      add_constraint(sum_expr(u[i] * attribute(i,players$position_1), i = 1:n) == 2) %>%
      ## defender
      add_constraint(sum_expr(u[i] * attribute(i,players$position_2), i = 1:n) == 5) %>%
      ## midfielders
      add_constraint(sum_expr(u[i] * attribute(i,players$position_3), i = 1:n) == 5) %>%
      ## strikers
      add_constraint(sum_expr(u[i] * attribute(i,players$position_4), i = 1:n) == 3) #%>%
    
  })
  
  output$solution <- renderDT({
    validate(
      need(input$solver, 'Press solve to see solution'),
      need(solution(), "Solver not run yet"))
    
    result <- solve_model(solution(), with_ROI(solver = "glpk", verbose = TRUE))
    matching <- result %>% 
      get_solution(u[i])
    
    players <- playertablereactive()
    players$Selected <- matching$value
      players <- players %>%
      filter(Selected == 1)

   # tmp <- playertablereactive()
  #  tmp$Selected <- matching$value
   # playertablereactive(tmp)
  
  
    DT::datatable(players, rownames = FALSE,
                  options=list(columnDefs = list(list(visible=FALSE, targets=c(8,9,10,11,12,13,
                                                                               14,15,16,17,18,19,20,
                                                                               21,22,23,24,25,26,27,28,29,30,
                                                                               31)))))
  })
  
  output$Fixtures <- renderDataTable({ 
    
    #scores <- get_game_list() %>%
      ## home goals for 
     # group_by(home) %>%
    #  mutate(goals_h = sum(team_h_score),
     #        against_h = sum(team_a_score)) %>%
    #  ungroup() %>%
    #  group_by(away) %>%
    #  mutate(goals_a = sum(team_a_score),
     #        against_a = sum(team_h_score)) %>%
    #  ungroup()
    
     # home <- scores %>%
      #  select(home, goals_h, against_h) %>%
       # distinct()
      
      #away <- scores %>%
       # select(away, goals_a, against_a) %>%
        #distinct()
      
    #  home_away <- home %>%
     #   full_join(away, by = c("home" = "away")) %>%
      #  rename(team = home) %>%
       # mutate(goals = goals_h + goals_a,
        #       against = against_h, against_a,
         #      attack = goals/mean(goals),
          #     defence = against/mean(against)) %>%
        #select(team, attack, defence)
    
    #games <- get_game_list() %>%
     # select(home, away, kickoff, team_h_score, team_a_score) %>%
    #  left_join(home_away, by = c("home" = "team")) %>%
     # rename(home_attack = attack, home_defence = defence) %>%
      #left_join(home_away, by = c("away" = "team")) %>%
      #rename(away_attack = attack, away_defence = defence) %>%
      #mutate(home_score_exp = home_attack * away_defence * 1.36,
      #       away_score_exp = away_attack * home_defence * 1.06)
    # helpful to parse out date and time separately  
    #mutate(kickoff = as.POSIXct(kickoff))
    
    #DT::datatable(head(games), options=list(columnDefs = list(list(visible=FALSE, targets=c(4,5,6,7,8,9)))))
  })

}

shinyApp(ui = ui, server = server)