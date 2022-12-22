
###### SET UP

library(pacman)

p_load(readr)
p_load(ompr)
p_load(ompr.roi)
p_load(ROI.plugin.glpk)
p_load(dplyr)
p_load(tidyverse)
library(fplr)

rm(list = ls())

## reading in item metadata. This file has been preformatted so that there's a var
## for each item attribute with 0s & 1s. Enemies need to be entered in a similar way
## with a col for each enemy item

players <- fplr::fpl_get_player_all() %>%
  select(team_code, now_cost, ict_index, points_per_game,total_points, element_type, web_name) %>%
  distinct() %>%
  ## need separate binary cols for each criteria
  mutate(dummy = 1) %>%
  pivot_wider(names_from = team_code,
              names_prefix = "team_",
              values_from = dummy) %>%
  mutate(dummy = 1) %>%
  pivot_wider(names_from = element_type,
              names_prefix = "position_",
              values_from = dummy) %>%
  mutate_all(~replace(., is.na(.), 0))

teams <- fpl_get_teams()

#items - setting up a var with total number of items
n <- nrow(players)


## this function pulls a list of a particular attribute for item and then select the 
## value for a given item in the list - used to sum item attributes in the model constraints

attribute <- function(item, ...) {
  attribute <- as.list(...)
  as.numeric(attribute[item])
}

objective <- function(item, ...) {
  attribute <- as.list(...)
  as.double(attribute[item])
}

## checking function works ok - this should return the matching col value for the first item
objective(1,players$points_per_game)

points_per_game <- players$points_per_game

##### SOLVER MODEL
## This uses the MIP (mixed integer linear optimisation problem) model from OMPR package 
## https://dirkschumacher.github.io/ompr/articles/modelling.html
## I used this model because it works for this type of problem set up - other types 
## may also work well for these types of problems

## specify model type
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
  add_constraint(sum_over(u[i] * attribute(i,players$now_cost), i = 1:n) <= 100) %>%
  
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
  
  
  ###### ITEM TYPE CONSTRAINTS
  ## underlining
  add_constraint(sum_expr(u[i] * attribute(i,itemdata$Underlining), i = 1:n) >= 1) %>%
  ## matching
  add_constraint(sum_expr(u[i] * attribute(i,itemdata$Matching), i = 1:n) <= 1) %>%
  
  ###### ENEMIES - need to add these individually
  ## Enemy expression = if item i and enemy item are in test (1+1 = 2), and i is an enemy (1) of enemy item 
  ## then result will be 1+1*1 = 2 but if i & enemy item are in test and not enemies then 
  ## 1 + 1 * 0 = 0. If enemy item in test and i not in test but i is an enemy then 1 + 0 * 1 = 1
  ## & conversely if i in test and not enemy, and i is an enemy then 0 + 1 * 1 = 1. 
  ##add constraint on enemy3
  add_constraint(sum_expr(u[3] + u[i] * enemy(i,itemdata$Enemy3), i = 1:n) <= 1) %>%
  ##add constraint on enemy4
  add_constraint(sum_expr(u[4] + u[i] * enemy(i,itemdata$Enemy4), i = 1:n) <= 1) %>%
  ##add constraint on enemy5
  add_constraint(sum_expr(u[5] + u[i] * enemy(i,itemdata$Enemy5), i = 1:n) <= 1) %>%
  ##add constraint on enemy7
  add_constraint(sum_expr(u[7] + u[i] * enemy(i,itemdata$Enemy7), i = 1:n) <= 1) %>%
  ##add constraint on enemy8
  add_constraint(sum_expr(u[8] + u[i] * enemy(i,itemdata$Enemy8), i = 1:n) <= 1) %>%
  ##add constraint on enemy9
  add_constraint(sum_expr(u[9] + u[i]  * enemy(i,itemdata$Enemy9), i = 1:n) <= 1) %>%
  ##add constraint on enemy10
  add_constraint(sum_expr(u[10] + u[i] * enemy(i,itemdata$Enemy10), i = 1:n) <= 1) 


## summarises model
model

###### SOLVE MODEL

## using GLPK which is a linear solving engine - https://www.gnu.org/software/glpk/
## Shouldn't need to install anything additional for this as it will come with the 
## OMPR packages. Again, using this because it works but other engines could also be
## used
result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

## creating a list decision variable outcomes
matching <- result %>% 
  get_solution(u[i]) 


## adding solutions back onto dataset 
playerssolved <- players %>%
  add_column(Decision = matching$value) %>%
  filter(Decision == 1)
sum(playerssolved$position_4)
