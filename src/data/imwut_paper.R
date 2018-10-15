library(tibble)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
source("data/common_source.R")

get_tz <- function(p_id){
  if(p_id %in% c("p01","p02","p03","p04","p05","p06","p07"))
    return("Europe/Helsinki")
  else
    return("Europe/London")
}

acceleration_magnitude <- function(acc){
  magnitude = acc %>% 
                  # filter(game_id < 10) %>%
                  group_by(game_id) %>% 
                  filter(row_number() >= 0.5 * n()) %>% # only the last half of the test for now
                  mutate(mag = sqrt(double_values_0^2 + double_values_1^2 + double_values_2^2)) %>% 
                  summarise(game_date = first(date), mean_magnitude = mean(mag))
  return(magnitude)
}

diff_actual_reference <- function(intake_date_time, reference_meds, tz){
  intake_date_time = as_datetime(intake_date_time, tz = tz)
  reference_meds = as.list(reference_meds)
  actual_time = hms::as.hms(intake_date_time, tz = tz)
  diffs = purrr::map_dbl(as.list(reference_meds), ~ abs( int_length(interval(hms::as.hms(., tz = tz), actual_time))))
  time_of_day = reference_meds[names(diffs[which.min(diffs)])][[1]]

  return(paste0(int_length(interval(hms::as.hms(time_of_day, tz = tz), actual_time)) / 60, "#", time_of_day))
}

accelerometer_vs_medication <- function(){
  # participants = c("p06" = "p06")
  Diff_actual_reference <- Vectorize(diff_actual_reference, vectorize.args = c('intake_date_time'))
  
  for(p_id in names(participants)){
    
    tz = get_tz(p_id)
    actual_meds = as.tibble(readr::read_tsv(get_file_path(p_id, "medication", parent_folder = "../data/processed"), quote = "", col_types = cols()))
    actual_meds = actual_meds %>% mutate(intake_time = as_datetime(double_medication/1000, tz = tz)) %>% select(intake_time)
    
    acc = as.tibble(readr::read_tsv(get_file_path(p_id, "ball_game_acc", parent_folder = "../data/processed"), quote = "", col_types = cols()))
    acc = acc %>% select(game_id, timestamp1, double_values_0, double_values_1, double_values_2) %>% 
      mutate(date = as_datetime(timestamp1/1000, tz = tz))
    
    reference_meds = read_csv(get_file_path("reference_medication", "reference_medication", parent_folders = "../data/raw"))
    time_reference_meds = reference_meds %>% filter(participant == p_id) %>% select(-participant) %>% select_if(~sum(!is.na(.)) > 0)
    time_reference_meds = time_reference_meds[,1:ncol(time_reference_meds)]
    
 
    if(p_id == "p01") # Test entries at the end of p01
      actual_meds = head(actual_meds, -2)
    
    # if(p_id == "p06" || p_id == "p09")
    intake_times = unique(actual_meds$intake_time)

    acc_metric = acceleration_magnitude(acc) %>%
      rowwise() %>% 
      mutate(mins_since_reported_meds = intake_times[which(abs(intake_times - game_date) == min(abs(intake_times - game_date)))],
             mins_since_reported_meds = as.double(difftime(mins_since_reported_meds, game_date, units = "mins")),
             mins_since_theoretical_meds=Diff_actual_reference(game_date, time_reference_meds, tz)) %>%
             separate(mins_since_theoretical_meds, into = c("mins_since_theoretical_meds", "theoretical_time"), sep = "#", convert = TRUE)


    print(ggplot(acc_metric) + geom_point(aes(x = mins_since_reported_meds, y = mean_magnitude)) +
            ggtitle(paste(p_id, " reported medication")))

    print(ggplot(acc_metric) + geom_point(aes(x = mins_since_theoretical_meds, y = mean_magnitude)) +
            ggtitle(paste(p_id, " theoretical medication")))

    print(acc_metric)
  }
}

accelerometer_vs_medication()