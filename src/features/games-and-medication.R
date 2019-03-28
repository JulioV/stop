library(tibble)
library(tidyverse)
library(anytime)
library(lubridate)
library(ggplot2)
source("data/common_source.R")

participants_levodopa = c("p02", "p07", "p08", "p09", "p10", "p13")

# Function to get the index specifying closest or after
Ind_closest_or_after <- function(d1, d2){
  which.min(abs(d1 - d2))
}


for(p_id in participants_levodopa){
  
  tz = get_tz(p_id)
  
  # Read self-reported medication log
  actual_meds <-  as.tibble(readr::read_tsv(get_file_path(p_id, "medication", parent_folder = "../data/processed"), quote = "", col_types = cols())) %>% 
    # Convert timestamp taking into accoun timezone
    mutate(intake_time = anytime(double_medication / 1000, tz = tz),
           ind = 1:nrow(.)) %>% 
    select(intake_time, ind)
  
  # Read accelerometer time stamps
  games <- as.tibble(readr::read_tsv(get_file_path(p_id, "ball_game_acc", parent_folder = "../data/processed"), quote = "", col_types = cols())) %>% 
    select(game_id, timestamp, sensitivity, score) %>% 
    group_by(game_id) %>% 
    mutate(date_time = anytime(timestamp / 1000, tz = tz), start = first(date_time)) %>% 
    summarise(start = first(date_time), n = n()) %>% 
    ungroup()
  
  # Searches for the closest time in the self-reported medication log for each game
  games <- games %>% mutate(closest_to = map_int(.x = games$start, .f = Ind_closest_or_after, d2 = actual_meds$intake_time))
  
  # Adds the closest medication intake_time to the game logs
  games_meds = left_join(games, actual_meds, by = c('closest_to' = 'ind')) %>% 
    mutate(time_since_meds = parse_number(start - intake_time))
  
  before_condition <-  quo(time_since_meds >= -3600 & time_since_meds <= 900) # 1 hour before and up to 15 min after medication
  after_condition <-  quo(time_since_meds >= 1800 & time_since_meds <= 3600 * 1.5) # 30 min after and up to 1.5hr after medication
  before <-  nrow(games_meds %>% filter(!!before_condition))
  after <-  nrow(games_meds %>% filter(!!after_condition))
  print(c(p_id, before, after, nrow(games_meds) - before - after, nrow(games)))
  
  # Label acc game dataset
  games_labelled <- games_meds %>% 
    mutate(label = ifelse(!!before_condition, "before",ifelse(!!after_condition,"after","discarded")),
           medication_intake_time = intake_time) %>% 
    select(game_id, start, medication_intake_time, time_since_meds, label)
  
  csv_path = get_file_path(p_id, "ball_game_acc_medication", parent_folder = "../data/processed")
  dir.create(dirname(csv_path), recursive=TRUE)
  write.csv(games_labelled, csv_path, row.names = F, quote = F)
  
}





