library(fs)
library(purrr)
library(readr)
library(dplyr)
library(ggplot2)
library(tibble)
library(anytime)
library(lubridate)
library(xts)
library(trend)
source("data/common_source.R")

diary_entries <- function(){
  
  symptoms <- path("../data/processed/health") %>%
          dir_ls(regexp = "p[0-9]{2}.csv") %>% 
          map_df(read_tsv, .id = "filename") %>% 
          mutate(day = as.Date(date_time))
  symptoms_metrics = symptoms %>% group_by(filename) %>% summarise(mean_diary = mean(pd_value), n = n(), unique_days = n_distinct(day))
  print(symptoms_metrics)
}

clean_consent <- function(){
  for(p_id in names(participants)){
      
    consent = as.tibble(readr::read_tsv(get_file_path(p_id, "consent", parent_folder = "../data/raw"), quote = "", col_types = cols() ))
    consent = consent %>% filter(!duplicated(.[["timestamp"]]))
    consent = add_column(consent, date_time = "", .after = 1)
    consent$date_time = anytime(consent$timestamp / 1000)
    
    if(p_id == "p01")
      consent <- consent %>% filter(`_id` == "24")
    
    demographics = consent %>%
      mutate(json_data = map(user_data, ~possibly(fromJSON, otherwise = list())(.))) %>% 
      filter(lengths(json_data) != 0) %>%
      mutate(medication = map(json_data, ~ .$medications),
             symptoms = map(json_data, ~ .$symptoms),
             diagnosis = map(json_data, ~ .$diagnosed_time)) 
    
    medication = demographics$medication[[1]]
    symptoms = demographics$symptoms[[1]]
    diagnosis = 2018 - year(as.POSIXct(anytime(demographics$diagnosis[[1]] / 1000)))
    updrs =   do.call(sum,symptoms)
    
    # print(demographics)
    print(anytime(demographics$diagnosis[[1]] / 1000))
    print("Years since diagnosis")
    print((diagnosis))
    print("Medication schedule")
    print(medication)
    print("UPDRS II")
    print(updrs)
  }
}

medications <- function(){
  for(p_id in names(participants)){
    
    reference_meds = read_csv(get_file_path("reference_medication", "reference_medication", parent_folders = "../data/raw"))
    
    p_meds = reference_meds %>% filter(participant == p_id) %>% select(-participant) %>% select_if(~sum(!is.na(.)) > 0)
    p_meds = p_meds[,1:ncol(p_meds)]
  }
  
  print(p_meds)
  
  symptoms <- path("../data/raw/consent") %>%
    dir_ls(regexp = "p[0-9]{2}.csv") %>% 
    map_df(read_tsv, .id = "filename") %>% 
    group_by(filename) %>% 
    summarise(date_time = anytime(timestamp/1000), user_data = user_data)
  print(symptoms)
}

game_speed <- function(){
  games <- path("../data/processed/ball_game_acc") %>%
    dir_ls(regexp = "p[0-9]{2}.csv") %>%
    # dir_ls(regexp = "p01.csv") %>% 
    map_df(read_tsv, .id = "filename") %>% 
    select(filename, game_id, date_time, sensitivity, score) %>% 
    group_by(filename, game_id) %>% 
    slice(1) %>% 
    group_by(filename, sensitivity) %>% 
    summarise(mean_score = round(mean(score),3), sd_score = round(sd(score),3), games = n()) %>% 
    ungroup() %>% 
    mutate(filename = stringr::str_match(filename, "(.*)(p[0-9]{2})(.*)")[, 3], participant = filename) %>% 
    select(-filename)
  
  print(games)
  write_csv(games, "../reports/game_sensitivity.csv")
}

meds_table <-  function(){
  all_actual_meds = tibble()
  for(p_id in names(participants)){
    print(p_id)  
    tz = get_tz(p_id)
    reference_meds = read_csv(get_file_path("reference_medication", "reference_medication", parent_folders = "../data/raw"))
    
    p_meds = reference_meds %>% filter(participant == p_id) %>% select(-participant) %>% select_if(~sum(!is.na(.)) > 0)
    p_meds = p_meds[,1:ncol(p_meds)]
    
    diff_actual_reference <- function(intake_date_time, reference_meds, tz){
      intake_date_time = as_datetime(intake_date_time, tz = tz)
      reference_meds = as.list(reference_meds)
      actual_time = hms::as.hms(intake_date_time, tz = tz)
      diffs = purrr::map_dbl(as.list(reference_meds), ~ abs( int_length(interval(hms::as.hms(., tz = tz), actual_time))))
      time_of_day = reference_meds[names(diffs[which.min(diffs)])][[1]]
      
      return(paste0(min(diffs)[[1]] / 60, "#", time_of_day))
    }
    
    Diff_actual_reference <- Vectorize(diff_actual_reference, vectorize.args = c('intake_date_time'))
    
    
    actual_meds = as.tibble(readr::read_tsv(get_file_path(p_id, "medication", parent_folder = "../data/processed"), quote = "", col_types = cols()))
    
    if(p_id == "p01") # Get rid off test entries at the end
      actual_meds = head(actual_meds, -2)
    
    
    # Get difference in minutes between logged and theoretical
    actual_meds = actual_meds %>% 
      select(intake_time) %>% mutate(intake_time = anytime(intake_time, tz = tz), 
                                     diff_min=Diff_actual_reference(intake_time, p_meds, tz)) %>% 
      separate(diff_min, into = c("diff_min", "time_of_day"), sep = "#", convert = TRUE) %>% 
      mutate(date = as.Date(intake_time))
    
    # Grouped and summarise per week
    first_day = first(actual_meds$date)
    actual_meds = actual_meds %>%  
                  mutate(days_since = as.integer(date - first_day), week = days_since %/% 7) %>% 
                  group_by(week) %>% 
                  summarise(participant = p_id, mean = mean(diff_min), sd = sd(diff_min), n = n()) #%>% 
                  # filter(!((participant == "p12" | participant == "p13") & week >= 2)) # participants have not finish yet
    
    
    all_actual_meds = bind_rows(all_actual_meds, actual_meds)
    
    
    
    
  }
  
  write_csv(all_actual_meds, "../reports/medication_compliance.csv")
  
  
}

meds_plots <- function(){
  all_actual_meds = read_csv("../reports/medication_compliance.csv")%>% 
                      filter(week < 4) %>% mutate(week = paste("Week", week + 1))
  plot = ggplot(all_actual_meds)  +
    facet_wrap(~participant, ncol = 3) +
    geom_point(aes(x = week, y = mean), stat = "identity") + 
    geom_errorbar(aes(x=week, ymin = mean - sd, ymax = mean + sd ), 
                  position = position_dodge(width = 0.9), 
                  width=0.5, colour="gray40") + #, linetype="longdash")
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_continuous(breaks = seq(0, max(all_actual_meds$mean) + max(all_actual_meds$sd), by = 50)) +
    ylab("Difference between logged and theoretical medication intake (minutes)")
  print(plot)
  ggsave(paste0("../reports/figures/medication_intake/all_differences.png"), plot, height = 5, width = 5)
}

game_speed_plots <- function(){
  games = read_csv("../reports/game_sensitivity.csv")%>% 
    mutate(sensitivity = paste("Sen.", sensitivity))
  plot = ggplot(games)  +
    facet_wrap(~participant, ncol = 3) +
    geom_point(aes(x = sensitivity, y = mean_score), stat = "identity") + 
    geom_errorbar(aes(x=sensitivity, ymin = mean_score - sd_score, ymax = mean_score + sd_score ), 
                  position = position_dodge(width = 0.9), 
                  width=0.5, colour="gray40") + #, linetype="longdash")
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_y_continuous(breaks = seq(0, max(games$mean_score) + max(games$sd_score), by = 20)) +
    ylab("Game score")
  print(plot)
  ggsave(paste0("../reports/figures/all_game_speed.png"), plot, height = 5, width = 5)
}

correlation_score_sensitivity <- function(){
  
  games <- path("../data/processed/ball_game_acc") %>%
    dir_ls(regexp = "p[0-9]{2}.csv") %>%
    # dir_ls(regexp = "p01.csv") %>% 
    map_df(read_tsv, .id = "filename", col_types = cols()) %>% 
    select(filename, game_id, date_time, sensitivity, score) %>% 
    mutate(filename = stringr::str_match(filename, "(.*)(p[0-9]{2})(.*)")[, 3], participant = filename) %>% 
    select(-filename) %>% 
    group_by(participant, game_id) %>% 
    slice(1) %>% # select first row of each game group
    group_by(participant) %>% 
    summarise(cor = list(cor.test(x = score, sensitivity, method = "kendall")), n = n()) %>% 
    rowwise() %>% 
    mutate(stat = cor$statistic, p.value = cor$p.value, estimate = cor$estimate[[1]])# %>% 
    
  games$p.value.adjusted = p.adjust(games$p.value, method = "fdr")
  games <- games %>% 
    mutate(p.value_str = ifelse(p.value <= 0.05, "<=0.05", as.character(round(p.value,3))),
           p.value.adjusted_str = ifelse(p.value.adjusted <= 0.05, "<=0.05", as.character(round(p.value.adjusted,3))),
           estimate = round(estimate, 2),
           stat = round(stat, 2)) %>%
    select(-cor, -p.value, -p.value.adjusted)
  print(games, digits = 8)
  write_csv(games, "../reports/correlation_score_speed.csv")
  
  
}

correlation_test <- function(participant){
  game = read_tsv(path(paste0("../data/processed/ball_game_acc/", participant, ".csv")), col_types = cols()) %>% 
    select(game_id, date_time, sensitivity, score) %>% 
    group_by(game_id) %>% 
    slice(1) 
  print(game)
  res = cor.test(x = game$sensitivity, y = game$score, method = "kendall")
  print(res$estimate[[1]])
  print(res$p.value)
  print(res$method)
}


save_medication_adherence <- function(){
  all_actual_meds = tibble()
  for(p_id in names(participants)){
    print(p_id)  
    tz = get_tz(p_id)
    reference_meds = read_csv(get_file_path("reference_medication", "reference_medication", parent_folders = "../data/raw"))
    
    p_meds = reference_meds %>% filter(participant == p_id) %>% select(-participant) %>% select_if(~sum(!is.na(.)) > 0)
    p_meds = p_meds[,1:ncol(p_meds)]
    
    diff_actual_reference <- function(intake_date_time, reference_meds, tz){
      intake_date_time = as_datetime(intake_date_time, tz = tz)
      reference_meds = as.list(reference_meds)
      actual_time = hms::as.hms(intake_date_time, tz = tz)
      diffs = purrr::map_dbl(as.list(reference_meds), ~ abs( int_length(interval(hms::as.hms(., tz = tz), actual_time))))
      time_of_day = reference_meds[names(diffs[which.min(diffs)])][[1]]
      
      return(paste0(min(diffs)[[1]] / 60, "#", time_of_day))
    }
    
    Diff_actual_reference <- Vectorize(diff_actual_reference, vectorize.args = c('intake_date_time'))
    
    
    actual_meds = as.tibble(readr::read_tsv(get_file_path(p_id, "medication", parent_folder = "../data/processed"), quote = "", col_types = cols()))
    
    if(p_id == "p01") # Test entries at the end
      actual_meds = head(actual_meds, -2)
    
    
    # Get difference in minutes between logged and theoretical
    actual_meds = actual_meds %>% 
      select(intake_time) %>% mutate(intake_time = anytime(intake_time, tz = tz), 
                                     diff_min=Diff_actual_reference(intake_time, p_meds, tz)) %>% 
      separate(diff_min, into = c("diff_min", "time_of_day"), sep = "#", convert = TRUE) %>% 
      mutate(date = as.Date(intake_time))
    
    print(actual_meds)
    write_tsv(actual_meds, paste0("../data/processed/medication_adherence/", p_id, ".csv"))
    
  }
}


adherence_trend_test <- function(){
  res_test = as.tibble()
  for(p_id in names(participants)){
    adherence <- read_tsv(path("../data/processed/medication_adherence", p_id, ext = "csv"), col_types = cols()) 
    ts = ts(zoo(adherence$diff_min, order.by = adherence$intake_time))
    res = mk.test(as.ts(ts))
    res_test = bind_rows(res_test, 
                         tibble(p_id = p_id, n = nrow(adherence), tau = res$estimates[[3]], p.value = res$p.value))
  }
  res_test$p.value.adjusted = p.adjust(res_test$p.value, method = "fdr")
  res_test = res_test %>% mutate(p.value_str = ifelse(p.value <= 0.05, "<=0.05", as.character(round(p.value,3))),
                                 p.value_adjusted_str = ifelse(p.value.adjusted <= 0.05, "<=0.05", as.character(round(p.value.adjusted,3))),
                                 tau = round(tau, 3)) %>%
                          select(-p.value, -p.value.adjusted)
  print(res_test)
  write_csv(res_test, "../reports/adherence_test.csv")
  
}

mann_whitney_u_test <- function(){
  library(coin)
  
  
  sus <- read_csv("../data/processed/sus/sus_scores.csv", col_types = cols()) %>% 
    t() %>% 
    as.tibble() %>%
    tail(-1) %>%
    rownames_to_column()
  
  colnames(sus) = c("p", "country", "sus_1", "sus_2")
  sus <- as.data.frame(sus) %>% mutate(country = as.integer(country), 
                                       sus_1 = as.numeric(sus_1),
                                       sus_2 = as.numeric(sus_2))
  print(sus)
  
  
  country_1 = (sus %>% filter(country == 1))$sus_2
  country_2 = (sus %>% filter(country == 2))$sus_2
  
  # MannWhitney test
  print(wilcox.test(country_1, country_2))
  # Efect size
  country = factor(sus$country)
  print(wilcox_test(sus$sus_2 ~ country, distribution="exact"))
  print("Effect size")
  print(1.7262/sqrt(13))
  # print("Pearson r")
  # print(cor.test(country_1, country_2, method = "pearson"))
  
  # Values for Country 1 were not significantly different from those for Country 2 (U = 9, Z = -1.7262, p = 0.09207, r = 0.47).
}

# adherence_trend_test()
# correlation_score_sensitivity()
# meds_table()
# meds_plots()
# game_speed()
# game_speed_plots()
# correlation_test()
mann_whitney_u_test()