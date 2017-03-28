# install.packages("dplyr")
# install.packages("tidyr")
require(dplyr)
require(tidyr)


# link_id, starting_time from intersection, enter link time, link travel time, vehicle id


trajectories <- read.csv("trajectories(table 5)_training.csv")

distinct(trajectories, intersection_id, tollgate_id)
# summarise(trajectories, mean_travel_time = mean(travel_time))
# by_tollgate = group_by(trajectories, tollgate_id)
# summarize(by_tollgate, mean_travel_time = mean(travel_time))
# by_intersection_tollgate = group_by(trajectories, intersection_id, tollgate_id)
# summarize(by_intersection_tollgate, mean_travel_time = mean(travel_time))


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
rm(B_3_subset_1, B_3_subset_2, B_3_subset_3, B_3_subset_4, B_3_subset_5)
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
rm(B_1_subset_1, B_1_subset_2, B_1_subset_3, B_1_subset_4, B_1_subset_5, 
   B_1_subset_6, B_1_subset_7, B_1_subset_8, B_1_subset_9)
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
rm(A_2_subset_1, A_2_subset_2, A_2_subset_3, A_2_subset_4, A_2_subset_5, A_2_subset_6)
# filter(A_2, vehicle_id == 1071181) # to verify

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
rm(A_2_subset_1, A_2_subset_2, A_2_subset_3, A_2_subset_4, A_2_subset_5, A_2_subset_6)
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
rm(A_3_subset_1, A_3_subset_2, A_3_subset_3, A_3_subset_4, A_3_subset_5, A_3_subset_6, A_3_subset_7, A_3_subset_8)
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
rm(C_3_subset_1, C_3_subset_2, C_3_subset_3, C_3_subset_4, C_3_subset_5, C_3_subset_6, C_3_subset_7, C_3_subset_8)
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
rm(C_1_subset_1, C_1_subset_2, C_1_subset_3, C_1_subset_4, C_1_subset_5, C_1_subset_6, 
  C_1_subset_7, C_1_subset_8, C_1_subset_9, C_1_subset_10, C_1_subset_11, C_1_subset_12)
# filter(C_1, vehicle_id == 1056529) # to verify


# final part
new_trajectories = rbind(B_3, B_1, A_2, A_3, C_3, C_1)
rm(B_3, B_1, A_2, A_3, C_3, C_1)
