library(tidyverse)

#Baseball Game Logs Dataset
suppressWarnings(data <- read_csv(here::here("dataset-ignore", "game_logs.csv"), 
                                  col_types = cols(forefeit = col_character(), 
                                                   completion = col_character())))
baseball <- as_tibble(data)

years<-c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)

baseball <- baseball %>% separate(date,into=c("year","month_day"),sep=4) %>% 
                         separate(month_day,into=c("month","day"),sep=2) %>% 
                         filter(year %in% years) %>% 
                         mutate(score_diff=h_score-v_score,year=as.numeric(year))

#replace MON and FLO with WAS and MIA (franchises that moved)
baseball$v_name <- str_replace(baseball$v_name, "MON", "WAS")
baseball$h_name <- str_replace(baseball$h_name, "MON", "WAS")
baseball$v_name <- str_replace(baseball$v_name, "FLO", "MIA")
baseball$h_name <- str_replace(baseball$h_name, "FLO", "MIA")

save(baseball, file = "dataset/baseball.RData")


#Winning Percentage RData file

load(here::here("dataset", "baseball.RData"))
options(dplyr.summarise.inform = FALSE)
#win/loss column (1 = win, 0 = loss or tie)
h_win_loss <- function(h, v) {
  ifelse(h > v, 1, 0)
}
(baseball <- baseball %>% mutate(h_win_loss = h_win_loss(h_score, v_score)))

#total games won
home_wins <- baseball %>% group_by(h_name, year) %>% summarize(h_wins = sum(h_win_loss == 1))
away_wins <- baseball %>% group_by(v_name, year) %>% summarize(a_wins = sum(h_win_loss == 0))
team_year_wins <- home_wins %>% add_column(a_wins = away_wins$a_wins) %>% 
  mutate(games_won = h_wins + a_wins)
#total games played
home_games <- baseball %>% group_by(h_name, year) %>% summarize(home_games = n())
away_games <- baseball %>% group_by(v_name, year) %>% summarize(away_games = n())
win_perc <- home_games %>% add_column(away_games = away_games$away_games) %>% 
  mutate(games_played = home_games + away_games) %>% 
  add_column(games_won = team_year_wins$games_won) %>% 
  mutate(win_perc = games_won/games_played)

save(win_perc, file = "dataset/win_perc.RData")


#MLB Team Payrolls Dataset
mlb_payroll <- as_tibble(read_csv(here::here("dataset", "mlb_payroll.csv"), show_col_types = FALSE))

save(mlb_payroll, file = "dataset/mlb_payroll.RData")


#Stadium Locations Dataset
save(MLBstadiums, file = "dataset/MLBstadiums.RData")
     