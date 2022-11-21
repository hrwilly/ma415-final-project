library(tidyverse)

loan_data <- read_csv(here::here("dataset", "loan_refusal.csv"))

## CLEAN the data
loan_data_clean <- loan_data

write_csv(loan_data_clean, file = here::here("dataset", "loan_refusal_clean.csv"))

save(loan_data_clean, file = here::here("dataset/loan_refusal.RData"))


#Baseball Game Logs Dataset
suppressWarnings(data <- read_csv(here::here("dataset-ignore", "game_logs.csv"), 
                                  col_types = cols(forefeit = col_character(), 
                                                   completion = col_character())))
baseball <- as_tibble(data)

baseball <- baseball %>% separate(date,into=c("year","month_day"),sep=4) %>% 
    separate(month_day,into=c("month","day"),sep=2)

years<-c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
baseball <- baseball%>%filter(year %in% years)

baseball <- baseball %>% mutate(score_diff=h_score-v_score)

save(baseball, file = "dataset/baseball.RData")


#Stadium Locations Dataset
save(MLBstadiums, file = "dataset/MLBstadiums.RData")
     