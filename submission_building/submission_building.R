if(!isTRUE(require(dplyr))){
	install.packages("dplyr")
	require(dplyr)
}

if(!isTRUE(require(dtplyr))){
	install.packages("dtplyr")
	require(dtplyr)
}

if(!isTRUE(require(tidyr))){
	install.packages("tidyr")
	require(tidyr)
}

if(!isTRUE(require(data.table))){
	install.packages("data.table")
	require(data.table)
}

if(!isTRUE(require(ggplot2))){
	install.packages("ggplot2")
	require(ggplot2)
}


links = fread("links.csv")
original_d = fread("trajectories(table 5)_training.csv")
str(links)



a = links %>% group_by(link_id, weekday, day_time_group) %>% 
	count()

history_average_by_all = links %>% 
	group_by(link_id, weekday, day_time_group) %>% 
	summarise(history_average = mean(link_travel_time))

fwrite(history_average_by_all,"history_average_by_all.csv")

history_average_by_weekday = links %>% 
	group_by(link_id, weekday) %>% 
	summarise(history_average = mean(link_travel_time))

fwrite(history_average_by_weekday,"history_average_by_weekday.csv")


history_average_by_group = links %>% 
	group_by(link_id, day_time_group) %>% 
	summarise(history_average = mean(link_travel_time))

fwrite(history_average_by_group,"history_average_by_group.csv")


b = links %>% 
	left_join(history_average,by = c("link_id" = "link_id", 
									 "weekday" = "weekday",
									 "day_time_group" = "day_time_group"))

##############################################################################

# Submission1 -------------------------------------------------------------

links = fread("links.csv")
routes_join = fread("routes_join.csv")
history_average_by_all = fread("history_average_by_all.csv")
submission_sample_travelTime_join = fread("submission_sample_travelTime_join.csv")

avg_travel_time=c()
for(i in 1:nrow(submission_sample_travelTime_join)){
	intersection = submission_sample_travelTime_join$intersection_id[i]
	tollgate = submission_sample_travelTime_join$tollgate_id[i]
	d = submission_sample_travelTime_join$day_time_group[i]
	w = submission_sample_travelTime_join$weekday[i]
	link_set = routes_join %>% 
		filter(intersection_id == intersection & 
									 tollgate_id == tollgate) %>% 
		select(link_seq) %>% 
		as.matrix() %>% as.numeric()
	a = history_average_by_all %>% 
		filter(link_id %in% link_set & 
			   day_time_group == d &
			   weekday == w) %>% 
		select(history_average) %>% 
		sum()
	avg_travel_time = c(avg_travel_time,a)
}
submission1 = submission_sample_travelTime_join %>% 
	select(intersection_id,tollgate_id,time_window,avg_travel_time)
submission1$avg_travel_time = avg_travel_time
fwrite(submission1,"submissions/submission1.csv")


