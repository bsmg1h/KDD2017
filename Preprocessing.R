# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("chron")
library(dplyr)
library(tidyr)
library(ggplot2)
# library(chron)
setwd("/Users/LL/Documents/HKUST/KDD/R code")

table3 <- read.csv("links (table 3).csv")
# train data
train_trajectories <- read.csv("trajectories(table 5)_training.csv")
train_weather <- read.csv("weather (table 7)_training_update.csv")
# test data
test_trajectories = read.csv("trajectories(table 5)_test1.csv")
test_weather <- read.csv("weather (table 7)_test1.csv")

preprocess <- function(trajectories) {
  
  # Part 1 : create data.frame centered on datas of each link
  # Create matrix with link_id, starting_time from intersection, enter link time, link travel time, vehicle id
  
  # B_3
  trajectories_B_3 = select(filter(trajectories, intersection_id == 'B', tollgate_id == 3), -travel_time)
  trajectories_B_3 = separate(trajectories_B_3, travel_seq, 
                              into = c("sequence_1", "sequence_2", "sequence_3", "sequence_4", "sequence_5"),
                              sep = ";")
  B_3_subset_1 = select(separate(trajectories_B_3, sequence_1, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_2, -sequence_3, -sequence_4, -sequence_5)
  B_3_subset_2 = select(separate(trajectories_B_3, sequence_2, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_3, -sequence_4, -sequence_5)
  B_3_subset_3 = select(separate(trajectories_B_3, sequence_3, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_4, -sequence_5)
  B_3_subset_4 = select(separate(trajectories_B_3, sequence_4, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_5)
  B_3_subset_5 = select(separate(trajectories_B_3, sequence_5, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4)
  B_3 = rbind(B_3_subset_1, B_3_subset_2, B_3_subset_3, B_3_subset_4, B_3_subset_5)
  # rm(B_3_subset_1, B_3_subset_2, B_3_subset_3, B_3_subset_4, B_3_subset_5, trajectories_B_3)
  
  # B_1
  trajectories_B_1 = select(filter(trajectories, intersection_id == 'B', tollgate_id == 1), -travel_time)
  trajectories_B_1 = separate(trajectories_B_1, travel_seq, 
                              into = c("sequence_1", "sequence_2", "sequence_3", "sequence_4", "sequence_5",
                                       "sequence_6", "sequence_7", "sequence_8", "sequence_9"),
                              sep = ";")
  B_1_subset_1 = select(separate(trajectories_B_1, sequence_1, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_6,
                        -sequence_7, -sequence_8, -sequence_9)
  B_1_subset_2 = select(separate(trajectories_B_1, sequence_2, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_3, -sequence_4, -sequence_5, -sequence_6,
                        -sequence_7, -sequence_8, -sequence_9)
  B_1_subset_3 = select(separate(trajectories_B_1, sequence_3, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_4, -sequence_5, -sequence_6,
                        -sequence_7, -sequence_8, -sequence_9)
  B_1_subset_4 = select(separate(trajectories_B_1, sequence_4, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_5, -sequence_6,
                        -sequence_7, -sequence_8, -sequence_9)
  B_1_subset_5 = select(separate(trajectories_B_1, sequence_5, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_6,
                        -sequence_7, -sequence_8, -sequence_9)
  B_1_subset_6 = select(separate(trajectories_B_1, sequence_6,
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5,
                        -sequence_7, -sequence_8, -sequence_9)
  B_1_subset_7 = select(separate(trajectories_B_1, sequence_7, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5,
                        -sequence_6, -sequence_8, -sequence_9)
  B_1_subset_8 = select(separate(trajectories_B_1, sequence_8,
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5,
                        -sequence_6, -sequence_7, -sequence_9)
  B_1_subset_9 = select(separate(trajectories_B_1, sequence_9, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5,
                        -sequence_6, -sequence_7, -sequence_8)
  B_1 = rbind(B_1_subset_1, B_1_subset_2, B_1_subset_3, B_1_subset_4, B_1_subset_5, 
              B_1_subset_6, B_1_subset_7, B_1_subset_8, B_1_subset_9)
  # # rm(B_1_subset_1, B_1_subset_2, B_1_subset_3, B_1_subset_4, B_1_subset_5, 
  #    B_1_subset_6, B_1_subset_7, B_1_subset_8, B_1_subset_9, trajectories_B_1)
  # filter(B_1, vehicle_id == 1086390) # to verify
  
  # A_2
  trajectories_A_2 = select(filter(trajectories, intersection_id == 'A', tollgate_id == 2), -travel_time)
  trajectories_A_2 = separate(trajectories_A_2, travel_seq, 
                              into = c("sequence_1", "sequence_2", "sequence_3", "sequence_4", "sequence_5",
                                       "sequence_6"),
                              sep = ";")
  A_2_subset_1 = select(separate(trajectories_A_2, sequence_1, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_6)
  A_2_subset_2 = select(separate(trajectories_A_2, sequence_2, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_3, -sequence_4, -sequence_5, -sequence_6)
  A_2_subset_3 = select(separate(trajectories_A_2, sequence_3, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_4, -sequence_5, -sequence_6)
  A_2_subset_4 = select(separate(trajectories_A_2, sequence_4, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_5, -sequence_6)
  A_2_subset_5 = select(separate(trajectories_A_2, sequence_5, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_6)
  A_2_subset_6 = select(separate(trajectories_A_2, sequence_6, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5)
  A_2 = rbind(A_2_subset_1, A_2_subset_2, A_2_subset_3, A_2_subset_4, A_2_subset_5, A_2_subset_6)
  # rm(A_2_subset_1, A_2_subset_2, A_2_subset_3, A_2_subset_4, A_2_subset_5, A_2_subset_6, trajectories_A_2)
  # filter(A_2, vehicle_id == 1071181) # to verify
  
  # A_3
  trajectories_A_3 = select(filter(trajectories, intersection_id == 'A', tollgate_id == 3), -travel_time)
  trajectories_A_3 = separate(trajectories_A_3, travel_seq, 
                              into = c("sequence_1", "sequence_2", "sequence_3", "sequence_4", "sequence_5",
                                       "sequence_6", "sequence_7", "sequence_8"),
                              sep = ";")
  A_3_subset_1 = select(separate(trajectories_A_3, sequence_1, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_6, -sequence_7, -sequence_8)
  A_3_subset_2 = select(separate(trajectories_A_3, sequence_2, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_3, -sequence_4, -sequence_5, -sequence_6, -sequence_7, -sequence_8)
  A_3_subset_3 = select(separate(trajectories_A_3, sequence_3, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_4, -sequence_5, -sequence_6, -sequence_7, -sequence_8)
  A_3_subset_4 = select(separate(trajectories_A_3, sequence_4, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_5, -sequence_6, -sequence_7, -sequence_8)
  A_3_subset_5 = select(separate(trajectories_A_3, sequence_5, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_6, -sequence_7, -sequence_8)
  A_3_subset_6 = select(separate(trajectories_A_3, sequence_6, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_7, -sequence_8)
  A_3_subset_7 = select(separate(trajectories_A_3, sequence_7, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_6, -sequence_8)
  A_3_subset_8 = select(separate(trajectories_A_3, sequence_8, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_6, -sequence_7)
  
  A_3 = rbind(A_3_subset_1, A_3_subset_2, A_3_subset_3, A_3_subset_4, A_3_subset_5, A_3_subset_6, A_3_subset_7, A_3_subset_8)
  # rm(A_3_subset_1, A_3_subset_2, A_3_subset_3, A_3_subset_4, A_3_subset_5, A_3_subset_6, A_3_subset_7, A_3_subset_8, trajectories_A_3)
  # filter(A_3, vehicle_id == 1064408) # to verify
  
  # C_3
  trajectories_C_3 = select(filter(trajectories, intersection_id == 'C', tollgate_id == 3), -travel_time)
  trajectories_C_3 = separate(trajectories_C_3, travel_seq, 
                              into = c("sequence_1", "sequence_2", "sequence_3", "sequence_4", "sequence_5",
                                       "sequence_6", "sequence_7", "sequence_8"),
                              sep = ";")
  C_3_subset_1 = select(separate(trajectories_C_3, sequence_1, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_6, -sequence_7, -sequence_8)
  C_3_subset_2 = select(separate(trajectories_C_3, sequence_2, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_3, -sequence_4, -sequence_5, -sequence_6, -sequence_7, -sequence_8)
  C_3_subset_3 = select(separate(trajectories_C_3, sequence_3, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_4, -sequence_5, -sequence_6, -sequence_7, -sequence_8)
  C_3_subset_4 = select(separate(trajectories_C_3, sequence_4, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_5, -sequence_6, -sequence_7, -sequence_8)
  C_3_subset_5 = select(separate(trajectories_C_3, sequence_5, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_6, -sequence_7, -sequence_8)
  C_3_subset_6 = select(separate(trajectories_C_3, sequence_6, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_7, -sequence_8)
  C_3_subset_7 = select(separate(trajectories_C_3, sequence_7, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_6, -sequence_8)
  C_3_subset_8 = select(separate(trajectories_C_3, sequence_8, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_6, -sequence_7)
  
  C_3 = rbind(C_3_subset_1, C_3_subset_2, C_3_subset_3, C_3_subset_4, C_3_subset_5, C_3_subset_6, C_3_subset_7, C_3_subset_8)
  # rm(C_3_subset_1, C_3_subset_2, C_3_subset_3, C_3_subset_4, C_3_subset_5, C_3_subset_6, C_3_subset_7, C_3_subset_8, trajectories_C_3)
  # filter(C_3, vehicle_id == 1072812) # to verify
  
  # C_1
  trajectories_C_1 = select(filter(trajectories, intersection_id == 'C', tollgate_id == 1), -travel_time)
  trajectories_C_1 = separate(trajectories_C_1, travel_seq, 
                              into = c("sequence_1", "sequence_2", "sequence_3", "sequence_4", "sequence_5",
                                       "sequence_6", "sequence_7", "sequence_8", "sequence_9", "sequence_10",
                                       "sequence_11", "sequence_12"),
                              sep = ";")
  C_1_subset_1 = select(separate(trajectories_C_1, sequence_1, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_2, -sequence_3, -sequence_4, -sequence_5, -sequence_6, 
                        -sequence_7, -sequence_8, -sequence_9, -sequence_10, -sequence_11, -sequence_12)
  C_1_subset_2 = select(separate(trajectories_C_1, sequence_2, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_3, -sequence_4, -sequence_5, -sequence_6, 
                        -sequence_7, -sequence_8, -sequence_9, -sequence_10, -sequence_11, -sequence_12)
  C_1_subset_3 = select(separate(trajectories_C_1, sequence_3, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_4, -sequence_5, -sequence_6, 
                        -sequence_7, -sequence_8, -sequence_9, -sequence_10, -sequence_11, -sequence_12)
  C_1_subset_4 = select(separate(trajectories_C_1, sequence_4, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_5, -sequence_6, 
                        -sequence_7, -sequence_8, -sequence_9, -sequence_10, -sequence_11, -sequence_12)
  C_1_subset_5 = select(separate(trajectories_C_1, sequence_5, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_6, 
                        -sequence_7, -sequence_8, -sequence_9, -sequence_10, -sequence_11, -sequence_12)
  C_1_subset_6 = select(separate(trajectories_C_1, sequence_6, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, 
                        -sequence_7, -sequence_8, -sequence_9, -sequence_10, -sequence_11, -sequence_12)
  C_1_subset_7 = select(separate(trajectories_C_1, sequence_7, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, 
                        -sequence_6, -sequence_8, -sequence_9, -sequence_10, -sequence_11, -sequence_12)
  C_1_subset_8 = select(separate(trajectories_C_1, sequence_8, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, 
                        -sequence_6, -sequence_7, -sequence_9, -sequence_10, -sequence_11, -sequence_12)
  C_1_subset_9 = select(separate(trajectories_C_1, sequence_9, 
                                 into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                        -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, 
                        -sequence_6, -sequence_7, -sequence_8, -sequence_10, -sequence_11, -sequence_12)
  C_1_subset_10 = select(separate(trajectories_C_1, sequence_10, 
                                  into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                         -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, 
                         -sequence_6, -sequence_7, -sequence_8, -sequence_9, -sequence_11, -sequence_12)
  C_1_subset_11 = select(separate(trajectories_C_1, sequence_11, 
                                  into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                         -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, 
                         -sequence_6, -sequence_7, -sequence_8, -sequence_9, -sequence_10, -sequence_12)
  C_1_subset_12 = select(separate(trajectories_C_1, sequence_12, 
                                  into = c("link_id", "link_enter_time", "link_travel_time"),sep = "#"),
                         -sequence_1, -sequence_2, -sequence_3, -sequence_4, -sequence_5, 
                         -sequence_6, -sequence_7, -sequence_8, -sequence_9, -sequence_10, -sequence_11)
  
  C_1 = rbind(C_1_subset_1, C_1_subset_2, C_1_subset_3, C_1_subset_4, C_1_subset_5, C_1_subset_6, 
              C_1_subset_7, C_1_subset_8, C_1_subset_9, C_1_subset_10, C_1_subset_11, C_1_subset_12)
  # rm(C_1_subset_1, C_1_subset_2, C_1_subset_3, C_1_subset_4, C_1_subset_5, C_1_subset_6, 
  #    C_1_subset_7, C_1_subset_8, C_1_subset_9, C_1_subset_10, C_1_subset_11, C_1_subset_12, trajectories_C_1)
  # filter(C_1, vehicle_id == 1056529) # to verify
  
  # final combining part
  links = rbind(B_3, B_1, A_2, A_3, C_3, C_1) # compile all previous results
  links = links[complete.cases(links),] # remove NAs : where a vehicle isn't being followed at the end the route
  
  # rm(B_3, B_1, A_2, A_3, C_3, C_1) # remove unused variables
  
  
  
  # Part 2 : Create new variables
  # link_travel_time as numeric
  # month, hour, minute, seconds as numeric
  # weekdays as character
  # date as Date
  
  links = mutate(links, link_id = as.numeric(link_id))
  links = mutate(links, link_travel_time = as.numeric(link_travel_time))
  # month, hour, weekday, working day for starting datas
  links = separate(links, starting_time, into = c("start_date", "start_time"), sep = " ")
  links = mutate(links, start_date = as.Date(start_date))
  links = mutate(links, start_weekdays = weekdays(start_date))
  links = mutate(links, weekday = sapply(start_weekdays, convert_weekday))
  links = separate(links, start_time, into = c("start_hour", "start_minute", "start_second"), sep = ":")
  links = mutate(links, start_hour = as.numeric(start_hour))
  links = mutate(links, start_minute = as.numeric(start_minute))
  links = mutate(links, start_second = as.numeric(start_second))
  # create time bin
  links = mutate(links, day_time_group = 1 + start_hour*3 + start_minute%/%20 )
  
  # month, hour, weekday, working day for link entering datas
  # links = separate(links, link_enter_time, into = c("link_date", "link_time"), sep = " ")
  # links = mutate(links, link_date = as.Date(link_date))
  # links = mutate(links, link_weekdays = weekdays(link_date))
  # links = separate(links, link_time, into = c("link_hour", "link_minute", "link_second"), sep = ":")
  # links = mutate(links, link_hour = as.numeric(link_hour))
  # links = mutate(links, link_minute = as.numeric(link_minute))
  # links = mutate(links, link_second = as.numeric(link_second))
  
  links = select(links, -intersection_id, -tollgate_id, -start_weekdays, -link_enter_time)
  return(links)
}

# Monday is 1, Sunday is 7
convert_weekday <- function(char_weekday) {
  if (char_weekday == "Monday") {
    return(1)
  } else if (char_weekday == "Tuesday") {
    return(2)
  } else if (char_weekday == "Wednesday") {
    return(3)
  } else if (char_weekday == "Thursday") {
    return(4)
  } else if (char_weekday == "Friday") {
    return(5)
  } else if (char_weekday == "Saturday") {
    return(6)
  } else {
    return(7)
  }
}

join_weather <- function(links, weather) {
  links = mutate(links, weather_group = 3*(start_hour%/%3))
  weather = mutate(weather, date = as.Date(date))
  links = left_join(links, weather, by = c("start_date" = "date", "weather_group" = "hour"))
  links = select(links, -weather_group)
  return(links)
}

join_link_information <- function(links, link_information) {
  link_information = link_information %>% select(link_id, length, width)
  links = left_join(links, link_information, by = c("link_id" = "link_id"))
  return(links)
}

# complete information about :
# 'trajectories(table 5)_training.csv' x 'weather (table 7)_training_update.csv' x 'links (table 3).csv'
train_links = preprocess(train_trajectories) %>% 
  join_weather(train_weather) %>% join_link_information(table3)
# complete information about : 
# 'trajectories(table 5)_test1.csv' x 'weather (table 7)_test1.csv' x 'links (table 3).csv'
test_links = preprocess(test_trajectories) %>% 
  join_weather(test_weather) %>% join_link_information(table3)

ml_model <- function(trajectories, link_information, weather) {
  # build matrix ready to be used
  return(preprocess(trajectories) %>% 
           group_by(link_id, weekday, start_date, day_time_group) %>% 
           summarise(Mean = mean(link_travel_time), start_hour = mean(start_hour)) %>%
           join_link_information(link_information) %>%
           join_weather(weather))
  # start_hour and start_date needs to be deleted later, doesn't work here
}

train_model = ml_model(train_trajectories, table3, train_weather)
test_model = ml_model(test_trajectories, table3, test_weather)

# output file
write.csv(train_links, file = "links_with_weather.csv", row.names = FALSE)
write.csv(test_links, file = "test_links_with_weather.csv", row.names = FALSE)
write.csv(train_model, file = "train_model.csv", row.names = FALSE)
write.csv(test_model, file = "test_model.csv", row.names = FALSE)