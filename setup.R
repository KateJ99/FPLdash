
library(rsconnect)
install.packages("FPLdata")

remotes::install_github("wiscostret/fplscrapR")
library(fplscrapR)

remotes::install_github("ewenme/fplr", force = TRUE)
library(fplr)

players <- fpl_get_player_all()

rsconnect::deployApp()

??get_game_list
games <- get_game_list()
install.packages("DT")

players <- get_player_info(season = 22)

games <- get_game_list()
DT::datatable(head(games))
