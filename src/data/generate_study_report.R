library(tibble)
library(anytime)
library(dplyr)
source("data/common_source.R")



main <- function(args){
  tables_names = c("aware_device","ball_game", "health", "medication", "notification_data")
  
  label = vector("character", length(participants))
  device_id = vector("character", length(participants))
  device_brand = vector("character", length(participants))
  games_count = vector("character", length(participants))
  medication_count = vector("character", length(participants))
  survey_count = vector("character", length(participants))
  notifications_count = vector("character", length(participants))
  start_date = vector("character", length(participants))
  
  row_id = 1
  for(p_id in names(participants)){

    # Device
    device = as.tibble(read.csv(get_file_path(p_id, "aware_device", parent_folder = "../data/raw"), sep="\t", stringsAsFactors = F))
    device_id[row_id] = device$device_id
    device_brand[row_id] = paste(device$brand, device$model)
    start_date[row_id] = as.character.Date(anytime(device$timestamp / 1000), format = "%Y-%m-%d")
    label[row_id] = device$label
      
    # Games
    games = as.tibble(readr::read_tsv(get_file_path(p_id, "ball_game", parent_folder = "../data/raw"), quote = ""))
    duplicted_games = games %>% filter(duplicated(.[["timestamp"]]))
    games_count[row_id] = paste(nrow(games) - nrow(duplicted_games),"(", nrow(games), ")", sep = "")
    
    # Medications
    medication = as.tibble(read.csv(get_file_path(p_id, "medication", parent_folder = "../data/raw"), sep="\t", stringsAsFactors = F))
    duplicted_medication = medication %>% filter(duplicated(.[["timestamp"]]))
    medication_count[row_id] = paste(nrow(medication) - nrow(duplicted_medication),"(", nrow(medication), ")", sep = "")
    
    # Survey
    survey = as.tibble(read.csv(get_file_path(p_id, "health", parent_folder = "../data/raw"), sep="\t", stringsAsFactors = F))
    duplicted_survey = survey %>% filter(duplicated(.[["timestamp"]]))
    survey_count[row_id] = paste(nrow(survey) - nrow(duplicted_survey),"(", nrow(survey), ")", sep = "")
    
    # Notifications
    notifications = as.tibble(read.csv(get_file_path(p_id, "notification_data", parent_folder = "../data/raw"), sep="\t", stringsAsFactors = F))
    duplicted_notifications = notifications %>% filter(duplicated(.[["timestamp"]]))
    notifications_count[row_id] = paste(nrow(notifications) - nrow(duplicted_notifications),"(", nrow(notifications), ")", sep = "")
    
    row_id = row_id + 1
    
  }
  report = data.frame(label = label, device_id = device_id, device_brand = device_brand, start_date = start_date, 
                      games_count = games_count, medication_count = medication_count,survey_count = survey_count,
                      notifications_count = notifications_count)
  print(report)
  write.csv(report, "../reports/study_report.csv",  row.names = F, quote = F)
}

main()