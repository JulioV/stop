library(anytime)
library(zoo)
library(lubridate)
library(tibble)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)
library(purrr)
source("../data/common_source.R")

# There's a bug when this script is run after the others, restart R session

plot_ts_date_time <- function(df, time, variable, start_date, title) {
  plot = ggplot(df) + 
    geom_jitter(aes_string(x = time, y = variable), size = 1) +
    scale_x_datetime(breaks = date_breaks("1 days"), 
                     labels = date_format("%Y-%m-%d"), 
                     limits = c(start_date,  (start_date + days(30)))) + 
    scale_y_continuous(limits = c(0,2)) +
    theme_minimal() +
    theme(text = element_text(size=10), axis.text.x = element_text(angle = 45, hjust = 1))+
    ggtitle(title)
  return(plot)
}

plot_ts_date <- function(df, time, variable, start_date, title){

  plot = ggplot(df) + 
    geom_bar(aes_string(x = time, y = variable), stat = "identity", size = 1) +
    scale_x_date(breaks = date_breaks("1 days"), 
                   labels = date_format("%Y-%m-%d"), 
                   limits = c(start_date,  (start_date + days(30)))) + 
    
    theme_minimal() +
    scale_y_continuous(breaks = seq(0, 15)) +
    ylim(0, 15) +
    theme(text = element_text(size=10), axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggtitle(title)
  return(plot)
}

visualise_medication <- function(){
  # pdf(file="../reports/medication_intake.pdf", paper = "a4")
  # for(p_id in names(participants)){
  plots = map(.x = names(participants), .f = function(p_id){
    medication = as.tibble(read.csv(get_file_path(p_id, "medication", parent_folder = "../../data/processed"), sep="\t", stringsAsFactors = F))
    medication$intake_time = as.POSIXct(medication$intake_time)
    medication$intake_quantity = 1
    medication$date = as.Date(medication$intake_time)
    start_date = medication$intake_time[1]
    
    plot1 = plot_ts_date_time(medication, "intake_time", "intake_quantity", start_date, paste("All meds for", p_id))
    # print(plot)
    
    medication_per_day = medication %>% group_by(date) %>% summarise(meds_day = n())
    plot2 = plot_ts_date(medication_per_day, "date", "meds_day", as.Date(start_date), paste("Daily meds for", p_id))
    # print(plot)
    # grid.arrange(plot1, plot2)
    # return(plot1)
    return(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))
  })
  
  grid.arrange(grobs = plots, ncol=3,top="Medication per day")
  # dev.off()
}

visualise_notifications <- function(){
  # pdf(file="../reports/notifications.pdf")
  
  # Notifications count
  plots = map(.x = names(participants), .f = function(p_id){
    notifications = as.tibble(read.csv(get_file_path(p_id, "notification_data", parent_folder = "../../data/processed"), sep="\t", stringsAsFactors = F))
    notifications_count = notifications %>% group_by(event) %>% summarise(event_count = n())
    plot1 = ggplot(notifications_count) + geom_bar(aes(x = event, y = event_count), stat = "identity") +
      ylab("count") + 
      ggtitle(paste("Notificantions count ", p_id))
    return(ggplotGrob(plot1))
  })
  grid.arrange(grobs = plots, ncol=4,top="Notification_count")

  
  # Notifications rate
  rates = map(.x = names(participants), .f = function(p_id){
      notifications = as.tibble(read.csv(get_file_path(p_id, "notification_data", parent_folder = "../../data/processed"), sep="\t", stringsAsFactors = F))
      notifications_count = notifications %>% group_by(event) %>% summarise(event_count = n())
      rate = notifications_count[1,2] /notifications_count[2,2]
      return(data.frame(participant = c(p_id), rate = c(rate)))
    }) 
  rates = bind_rows(rates) %>% replace(is.na(.), 0)
  rates_plot = ggplot(rates) + geom_bar(aes(x = participant, y = event_count), stat = "identity") +  
      ylab("Notifications  opened/shown") +
      ggtitle("Notificantions rates (opened/showed) all participants (periods combined)")
  grid.arrange(rates_plot)
  
  # Notifications rate per time of day
  rates_per_event = map(.x = names(participants), .f = function(p_id){
    notifications = as.tibble(read.csv(get_file_path(p_id, "notification_data", parent_folder = "../../data/processed"), sep="\t", stringsAsFactors = F))
    if(nrow(notifications) == 0)
      return(data.frame(period = c("afternoon", "dailysymptoms","evening", "morning", "noon"), 
                        rate = c(0,0,0,0,0), 
                        participant = c(p_id,p_id,p_id,p_id,p_id)))
    notifications_count = notifications %>% group_by(period, event) %>% summarise(event_count = n()) %>%
                                            group_by(period) %>% do(data.frame(rate= rate_count(group = .)))
    notifications_count$participant = p_id
    return(notifications_count)
  })
  rates_per_event = bind_rows(rates_per_event) %>% ungroup()
  # print(rates_per_event)
  rates_per_event_plot = ggplot(rates_per_event) +
                facet_wrap(~participant, ncol = 3) +
                geom_bar(aes(x = period, y = rate, fill=period), stat = "identity") + 
                theme(axis.text.x = element_text(angle = 45, hjust = 1))+
                scale_x_discrete(limits = c("morning", "noon", "afternoon", "evening", "dailysymptoms")) +
                ylab("Open notifications/Sown notifications")+
                ggtitle("Rate of  shown and opened notifications per period per participant")
  grid.arrange(arrangeGrob(rates_per_event_plot))
  
  # Time to open notifications
  open_time = map(.x = names(participants), .f = function(p_id){
    notifications = as.tibble(read.csv(get_file_path(p_id, "notification_data", parent_folder = "../../data/processed"), sep="\t", stringsAsFactors = F))
    if(nrow(notifications) == 0)
      return(data.frame(period = c("afternoon", "dailysymptoms","evening", "morning", "noon"), 
                        rate = c(0,0,0,0,0), 
                        participant = c(p_id,p_id,p_id,p_id,p_id)))

      notifications_time = notifications %>% filter(open_pair == 1) %>% mutate(difference = ifelse(period == lag(period) & event == "opened",
                                                                          (timestamp - lag(timestamp)) / (1000 * 60), NA)) %>%
      filter(!is.na(difference) & difference > 3) %>% group_by(period) %>% summarise(mean = mean(difference),
                                                                                     sd = sd(difference))
    notifications_time$participant = p_id
    return(notifications_time)
  })
  open_time = bind_rows(open_time) %>% ungroup() %>% filter(period != "dailysymptoms")
  # print(open_time)
  open_time_plot = ggplot(open_time) +
    facet_wrap(~participant, ncol = 3) +
    geom_bar(aes(x = period, y = mean, fill=period), stat = "identity") + 
    geom_errorbar(aes(x=period, ymin = mean -sd, ymax = mean + sd ), 
                  position = position_dodge(width = 0.9), 
                  width=0.5, colour="gray40") + #, linetype="longdash")
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_discrete(limits = c("morning", "noon", "afternoon", "evening", "dailysymptoms")) +
    ylab("Mean time (minutes)") +
    ggtitle("Mean time between shown and opened notifications per period per participant")
  # print(open_time_plot)
  grid.arrange(arrangeGrob(open_time_plot))
  # dev.off()
  
}

rate_count <- function(group){
  # print(group)
  if(group$event %in% c("opened", "showed")){
    open_count <- (group %>% filter(event == "opened"))$event_count[1]
    shown_count <- (group %>% filter(event == "shown"))$event_count[1]
    return(open_count/shown_count)
  }
  return(0)
}

visualise_diary <- function(){
  # Diary score
  plots = map(.x = names(participants), .f = function(p_id){
    diary = as.tibble(read.csv(get_file_path(p_id, "health", parent_folder = "../../data/processed"), sep="\t", stringsAsFactors = F))
    diary$date_time = anytime(diary$date_time)
    start_date = diary$date_time[1]
    plot1 = ggplot(diary) + 
      geom_point(aes(x = date_time, y = pd_value),size = 2) +
      geom_line(aes(x = date_time, y = pd_value)) + 
      scale_x_datetime(breaks = date_breaks("1 days"),
                   labels = date_format("%Y-%m-%d"),
                   limits = c(start_date,  (start_date + days(30)))) +
      theme_minimal() +
      ylim(0, 5) +
      theme(text = element_text(size=10), axis.text.x = element_text(angle = 45, hjust = 1)) + 
      ggtitle(paste("Symptom diary", p_id))
    return(ggplotGrob(plot1))
  })
  grid.arrange(grobs = plots, ncol=4,top="Symptoms diary")
}

visualise_meds_games <- function(){
  plots = map(.x = names(participants), .f = function(p_id){
    game = as.tibble(readr::read_tsv(get_file_path(p_id, "ball_game_acc", parent_folder = "../../data/processed"), quote = ""))
    medication = as.tibble(readr::read_tsv(get_file_path(p_id, "medication", parent_folder = "../../data/processed"), quote = ""))
    
    game = game %>% group_by(date_time, timestamp, ball_radius, sensitivity, device_x_res, device_y_res, score) %>% 
      summarise() %>% 
      mutate(score = if(score < 0) 0 else score, is_game = 1) %>% ungroup
    
    medication = medication %>% mutate(is_med = 1)
    
    game_med_30 = bind_rows(game, medication) %>% arrange(timestamp) %>% select(date_time, timestamp, score, is_game, is_med) %>% 
      mutate(min_since_meds = ifelse((is_game == 1) & (lag(is_med) == 1), (timestamp - lag(timestamp)) / (1000 * 60) , NA)) %>% 
      filter(!is.na(min_since_meds)) %>% 
      mutate(bin = min_since_meds %/% 30) %>% 
      group_by(bin) %>% summarize(n_games = n(), mean_score = mean(score), sd = sd(score)) %>% 
      mutate(bin = (bin * 30) / 60)
    
    game_meds_60 = bind_rows(game, medication) %>% arrange(timestamp) %>% select(date_time, timestamp, score, is_game, is_med) %>% 
      mutate(min_since_meds = ifelse((is_game == 1) & (lag(is_med) == 1), (timestamp - lag(timestamp)) / (1000 * 60) , NA)) %>% 
      filter(!is.na(min_since_meds)) %>% 
      mutate(bin = min_since_meds %/% 60) %>% 
      group_by(bin) %>% summarize(n_games = n(), mean_score = mean(score), sd = sd(score)) %>% 
      mutate(bin = (bin * 60)/60)
    
    plot1 = ggplot(data=game_med_30) + 
      # geom_bar(aes(x=bin, y = mean_score, fill = n_games), stat = "identity") +
      geom_point(aes(x=bin, y = mean_score, colour = n_games), size=3) +
      geom_text(aes(x = bin, y = mean_score, label=n_games), vjust=-0.5, hjust=1) +
      geom_errorbar(aes(x=bin, ymin = mean_score -sd, ymax = mean_score + sd ), 
                    position = position_dodge(width = 0.9), 
                    width=0.3, colour="gray40") +
      scale_x_continuous(breaks = seq(0,24, by = 0.5)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste("Mean scores for",p_id,"games bucketed into 30 min bins since the latest logged medication"))
    # print(plot)
    
    plot2 = ggplot(data=game_meds_60) + 
      geom_point(aes(x=bin, y = mean_score, colour = n_games), size=3) +
      geom_text(aes(x = bin, y = mean_score, label=n_games), vjust=-0.5, hjust=1) +
      geom_errorbar(aes(x=bin, ymin = mean_score -sd, ymax = mean_score + sd ), 
                    position = position_dodge(width = 0.9), 
                    width=0.3, colour="gray40") +
      scale_x_continuous(breaks = seq(0,24, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ggtitle(paste("Mean scores for",p_id,"games bucketed into 60 min bins since the latest logged medication"))
    
    return(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))
  })
  
  grid.arrange(grobs = plots, ncol=3,top="Game scores binned per 30/60 minutes since last medication")
}

visualise_sd_sensor_games <- function(sensor_file, sensor_string){
  plots = map(.x = names(participants), .f = function(p_id){
    game = as.tibble(readr::read_tsv(get_file_path(p_id, sensor_file, parent_folder = "../../data/processed"), quote = ""))
    medication = as.tibble(readr::read_tsv(get_file_path(p_id, "medication", parent_folder = "../../data/processed"), quote = ""))

    if(nrow(game) < 1)
      return(ggplot())
    
    game2 = game %>% group_by(timestamp, game_id) %>% 
      mutate(magnitude = sqrt(double_values_0*double_values_0 + double_values_1 * double_values_1 + double_values_2 * double_values_2)) %>% 
      summarise(sd_magnitude = sd(magnitude)) %>%
      mutate(is_game = 1) %>%
      ungroup()
    medication = medication %>% mutate(is_med = 1)
    
    
    
    game_med_30 = bind_rows(game2, medication) %>% arrange(timestamp) %>% select(date_time, timestamp, is_game, is_med, sd_magnitude) %>% 
      mutate(min_since_meds = ifelse((is_game == 1) & (lag(is_med) == 1), (timestamp - lag(timestamp)) / (1000 * 60) , NA)) %>% 
      filter(!is.na(min_since_meds)) %>% 
      mutate(bin = min_since_meds %/% 30) %>% 
      group_by(bin) %>% summarize(n_games = n(), mean_sd_magnitude = mean(sd_magnitude), sd_sd_magnitude = sd(sd_magnitude)) %>%
      mutate(bin = (bin * 30) / 60)
    
    game_med_60 = bind_rows(game2, medication) %>% arrange(timestamp) %>% select(date_time, timestamp, is_game, is_med, sd_magnitude) %>% 
      mutate(min_since_meds = ifelse((is_game == 1) & (lag(is_med) == 1), (timestamp - lag(timestamp)) / (1000 * 60) , NA)) %>% 
      filter(!is.na(min_since_meds)) %>% 
      mutate(bin = min_since_meds %/% 60) %>% 
      group_by(bin) %>% summarize(n_games = n(), mean_sd_magnitude = mean(sd_magnitude), sd_sd_magnitude = sd(sd_magnitude)) %>%
      mutate(bin = (bin * 60) / 60)
    
    plot1 = ggplot(data=game_med_30) + 
      geom_point(aes(x=bin, y = mean_sd_magnitude, colour = n_games), size=3) +
      geom_text(aes(x = bin, y = mean_sd_magnitude, label=n_games), vjust=-0.5, hjust=1) +
      geom_errorbar(aes(x=bin, ymin = mean_sd_magnitude -sd_sd_magnitude, ymax = mean_sd_magnitude + sd_sd_magnitude ), 
                    position = position_dodge(width = 0.9), 
                    width=0.3, colour="gray40") +
      scale_x_continuous(breaks = seq(0,24, by = 0.5)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size=10)) +
      ggtitle(paste("Mean of sd of",sensor_string,"magnitude for",p_id,"games\n bucketed into 30 min bins since the latest logged medication"))
    

    
    plot2 = ggplot(data=game_med_60) + 
      geom_point(aes(x=bin, y = mean_sd_magnitude, colour = n_games), size=3) +
      geom_text(aes(x = bin, y = mean_sd_magnitude, label=n_games), vjust=-0.5, hjust=1) +
      geom_errorbar(aes(x=bin, ymin = mean_sd_magnitude -sd_sd_magnitude, ymax = mean_sd_magnitude + sd_sd_magnitude ), 
                    position = position_dodge(width = 0.9), 
                    width=0.3, colour="gray40") +
      scale_x_continuous(breaks = seq(0,24, by = 1)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size=10)) +
      ggtitle(paste("Mean of sd of",sensor_string,"magnitude for",p_id," \n games bucketed into 60 min bins since the latest logged medication"))
    
    return(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))
  })
  
  grid.arrange(grobs = plots, ncol=3,top="Game sd",sensor_string,"binned per 30/60 minutes since last medication")
}

visualise_movement_sensor <- function(file, sensor){
  
  plots = map(.x = names(participants), .f = function(p_id){
    game = as.tibble(readr::read_tsv(get_file_path(p_id, file, parent_folder = "../../data/processed"), quote = ""))
    medication = as.tibble(readr::read_tsv(get_file_path(p_id, "medication", parent_folder = "../../data/processed"), quote = ""))
    if(nrow(game) < 1)
      return(ggplot())
    game2 = game %>% group_by(date_time, timestamp, ball_radius, sensitivity, device_x_res, device_y_res, score) %>%
      summarise() %>%
      mutate(score = if(score < 0) 0 else score, 
             is_game = 1) %>% ungroup
    
    medication = medication %>% mutate(is_med = 1)
    
    game_meds = bind_rows(game2, medication) %>% arrange(timestamp) %>% select(date_time, timestamp, score, is_game, is_med) %>%
      mutate(min_since_meds = ifelse((is_game == 1) & (lag(is_med) == 1), (timestamp - lag(timestamp)) / (1000 * 60) , NA)) %>%
      filter(!is.na(min_since_meds))
    
    options(digits.secs=6)
    games_shifted = game %>% mutate( 
      magnitude = sqrt(double_values_0*double_values_0 + double_values_1 * double_values_1 + double_values_2 * double_values_2)) %>% 
      group_by(game_id) %>% 
      mutate(min_timestamp = min(timestamp1)) %>% ungroup()%>%  
      mutate(dtimestamp = timestamp1 - min_timestamp) 
    
    joinned_game_meds <- inner_join(games_shifted, game_meds, by=c("date_time")) %>%  
      mutate(bin = (min_since_meds %/% 60) + 1,
             lab_bin = paste("<",as.character( (min_since_meds %/% 60) + 1)), "hr" )
    # print(joinned_game_meds)
    # labeller_bins <-  function(var){
    #   return(paste("<", var, "hrs"))
    # } 
    plot = joinned_game_meds %>% ggplot() + geom_point( aes(x = dtimestamp, y = magnitude, colour = score.x), size =0.1) +
      scale_colour_gradientn(colours = rev(terrain.colors(4))) +
      facet_wrap(~bin, ncol = 2) + #), labeller = labeller_bins)
      ggtitle(paste(sensor, "date binned for ", p_id))
    return(ggplotGrob(plot))
  })
  
  grid.arrange(grobs = plots, ncol=1,top=paste(sensor, "data binned by 60 minutes bins since latest medication"))
}

# visualise_medication()
# visualise_notifications()
# visualise_diary()
# visualise_movement_sensor("ball_game_acc", "accelerometer")
# visualise_movement_sensor("ball_game_rota", "rotation")