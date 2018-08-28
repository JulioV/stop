library(tibble)
library(anytime)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(jsonlite)
source("data/common_source.R")

clean_medication <- function(p_id){
  medication = as.tibble(read.csv(get_file_path(p_id, "medication", parent_folder = "../data/raw"), sep="\t", stringsAsFactors = F))
  medication = medication %>% filter(!duplicated(.[["timestamp"]])) %>% mutate(intake_time = anytime(double_medication / 1000))

  medication = add_column(medication, date_time = "", .after = 1)
  medication$date_time = anytime(medication$timestamp / 1000)
  
  csv_path = get_file_path(p_id, "medication", parent_folder = "../data/processed")
  dir.create(dirname(csv_path), recursive=TRUE)
  write.table(medication, csv_path, row.names = F, quote = F, sep = "\t")
}

filter_out_buggy_notifications <- function(data){
  data = data %>% arrange(timestamp)
  
  # Daily symptoms is a different notification, keep separately
  data_symptoms = data %>% filter(period == "dailysymptoms")
  # Label all notifications pairs that are the same period but have events = show, opened
  data_symptoms = data_symptoms %>%  mutate(open_pair = ifelse(period == lag(period) & event != lag(event) & event == "opened", 1, 0))  %>%
    mutate(open_pair = ifelse(!is.na(lead(open_pair)) & lead(open_pair) == 1, 1,open_pair))
  
  data = data %>% filter(period != "dailysymptoms")
  
  # Label all notifications pairs that are the same period but have events = show, opened
  data = data %>%  mutate(open_pair = ifelse(period == lag(period) & event != lag(event)  & event == "opened", 1, 0)) %>% 
    mutate(open_pair = ifelse(!is.na(lead(open_pair)) & lead(open_pair) == 1, 1,open_pair))
  
  # Keep separately all couples
  data_couples = data %>% filter(open_pair == 1)
  
  data = data %>% filter(open_pair == 0)
  # Get rid of all pairs (evening, opened), this seems to be the buggy notification
  data = data %>% filter(!(period == "evening" & event == "opened"))
  
  # Repeate the process of labelling pairs that are the same period but have events = show, opened
  data = data %>%  mutate(open_pair = ifelse(period == lag(period) & event != lag(event)  & event == "opened", 1, 0)) %>% 
    mutate(open_pair = ifelse(!is.na(lead(open_pair)) & lead(open_pair) == 1, 1,open_pair))
  
  # Get all shown notifications and pairs
  data_shown = data %>% filter(event == "shown" | open_pair == 1)
          # data_shown = data %>% filter(event == "shown")
  
  filtered_data = bind_rows(data_shown, data_couples, data_symptoms) %>% arrange(timestamp)
  return(filtered_data)
}

clean_notifications <- function(p_id){
  notifications = as.tibble(read.csv(get_file_path(p_id, "notification_data", parent_folder = "../data/raw"), sep="\t", stringsAsFactors = F))
  notifications = notifications %>% filter(!duplicated(.[["timestamp"]]))
  notifications$event <- sapply(notifications$event, 
                          function(x){ifelse(str_count(x, pattern = "_") == 2, str_replace (x, pattern = "_",
                                           replacement = ""), x)})
  notifications = notifications %>% separate(event, into=c("period", "event"), sep = "_")
  notifications$period = str_replace(notifications$period, "iltapÃ¤ivÃ¤", "afternoon")
  notifications$period = str_replace(notifications$period, "pÃ¤ivÃ¤", "noon")
  notifications$period = str_replace(notifications$period, "ilta", "evening")
  notifications$period = str_replace(notifications$period, "aamu", "morning")
  notifications$period = str_replace(notifications$period, "pÃ¤ivittÃ¤inenkysely", "dailysymptoms")
  notifications$period = str_replace(notifications$period, "pÃ¤ivittÃ¤inen kysely", "dailysymptoms")
  notifications$period = str_replace(notifications$period, "daily survey", "dailysymptoms")
  notifications$period = str_replace(notifications$period, "dailysurvey", "dailysymptoms")
  notifications = add_column(notifications, date_time = "", .after = 1)
  notifications$date_time = anytime(notifications$timestamp / 1000)
  
  notifications = filter_out_buggy_notifications(notifications)
  
  csv_path = get_file_path(p_id, "notification_data", parent_folder = "../data/processed")
  dir.create(dirname(csv_path), recursive=TRUE)
  write.table(notifications, csv_path, row.names = F, quote = F, sep = "\t")
}

clean_diary <- function(p_id){
  diary = as.tibble(read.csv(get_file_path(p_id, "health", parent_folder = "../data/raw"), sep="\t", stringsAsFactors = F))
  diary = diary %>% filter(!duplicated(.[["timestamp"]]))
  diary = add_column(diary, date_time = "", .after = 1)
  diary$date_time = anytime(diary$timestamp / 1000)
  
  csv_path = get_file_path(p_id, "health", parent_folder = "../data/processed")
  dir.create(dirname(csv_path), recursive=TRUE)
  write.table(diary, csv_path, row.names = F, quote = F, sep = "\t")
}

clean_game <- function(p_id){
  game = as.tibble(readr::read_tsv(get_file_path(p_id, "ball_game", parent_folder = "../data/raw"), quote = ""))
  game = game %>% filter(!duplicated(.[["timestamp"]]))
  game = add_column(game, date_time = "", .after = 1)
  game$date_time = anytime(game$timestamp / 1000)
  
  if(p_id == "p04")
    game = game %>% filter(`_id` != 477)
  else if(p_id == "p11")
    game = game %>% filter(`_id` != 1345 & `_id` != 1316 & `_id` != 5113)
  
  # game = tail(game,5)
  # print(game)
  
  
  acc = game %>%
    mutate(json_data = map(data, ~ fromJSON(.))) %>% 
    mutate(gamedata = map(json_data, ~ .$gamedata %>% select(-samples))) %>% unnest(gamedata) %>% 
    mutate(accelerometer = map(json_data, ~ if(class(.$accelerometer) == "character") list() else .$accelerometer)) %>%
    mutate(n_accelerometer = map_dbl(accelerometer, length)) %>% filter(n_accelerometer > 0) %>% 
    mutate(game_id = row_number(timestamp)) %>% 
    unnest(accelerometer, .drop = F)  %>% 
    select(-data, -json_data)
  
  gyr = game %>%
    mutate(json_data = map(data, ~ fromJSON(.))) %>% 
    mutate(gamedata = map(json_data, ~ .$gamedata %>% select(-samples))) %>% unnest(gamedata) %>% 
    mutate(gyroscope = map(json_data, ~ if(class(.$gyroscope) == "character") list() else .$gyroscope)) %>% 
    mutate(n_gyroscope = map_dbl(gyroscope, length)) %>% filter(n_gyroscope > 0) %>% 
    mutate(game_id = row_number(timestamp)) %>% 
    unnest(gyroscope, .drop = F) %>% 
    select(-data, -json_data)
  
  rota = game %>%
    mutate(json_data = map(data, ~ fromJSON(.))) %>% 
    mutate(gamedata = map(json_data, ~ .$gamedata %>% select(-samples))) %>% unnest(gamedata) %>% 
    mutate(rotation = map(json_data, ~ if(class(.$rotation) == "character") list() else .$rotation)) %>% 
    mutate(n_rotation = map_dbl(rotation, length)) %>% filter(n_rotation > 0) %>% 
    mutate(game_id = row_number(timestamp)) %>% 
    unnest(rotation, .drop = F) %>% 
    select(-data, -json_data)
  
  lacc = game %>%
    mutate(json_data = map(data, ~ fromJSON(.))) %>% 
    mutate(gamedata = map(json_data, ~ .$gamedata %>% select(-samples))) %>% unnest(gamedata) %>% 
    mutate(linearaccelerometer = map(json_data, ~ if(class(.$linearaccelerometer) == "character") list() else .$linearaccelerometer)) %>% 
    mutate(n_lacc = map_dbl(linearaccelerometer, length)) %>% filter(n_lacc > 0) %>% 
    mutate(game_id = row_number(timestamp)) %>% 
    unnest(linearaccelerometer, .drop = F) %>% 
    select(-data, -json_data)
  
  csv_path = get_file_path(p_id, "ball_game_acc", parent_folder = "../data/processed")
  dir.create(dirname(csv_path), recursive=TRUE)
  write.table(acc, csv_path, row.names = F, quote = F, sep = "\t")
  
  csv_path = get_file_path(p_id, "ball_game_gyr", parent_folder = "../data/processed")
  dir.create(dirname(csv_path), recursive=TRUE)
  write.table(gyr, csv_path, row.names = F, quote = F, sep = "\t")
  
  csv_path = get_file_path(p_id, "ball_game_rota", parent_folder = "../data/processed")
  dir.create(dirname(csv_path), recursive=TRUE)
  write.table(rota, csv_path, row.names = F, quote = F, sep = "\t")
  
  csv_path = get_file_path(p_id, "ball_game_lacc", parent_folder = "../data/processed")
  dir.create(dirname(csv_path), recursive=TRUE)
  write.table(lacc, csv_path, row.names = F, quote = F, sep = "\t")
}

# If you want to clean the data of only certain participants uncomment line below
# participants = c("p07" = "", "p08" = "","p09" = "","p10" = "","p11" = "")
# participants = c("p11" = "")

clean_data <- function(){
  for(p_id in names(participants)){
    print(p_id)
    clean_medication(p_id)
    clean_notifications(p_id)
    clean_diary(p_id)
    clean_game(p_id)
  }
}

clean_data()