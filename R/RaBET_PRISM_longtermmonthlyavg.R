library(tidyverse)
setwd("C:/Users/BatCave/Documents/RaBET")
MLRA65_P30R30<- read.csv("C:/Users/BatCave/Desktop/MLRA65_ppt/MLRA65_P30R30.csv")
MLRA65_P30R31<- read.csv("C:/Users/BatCave/Desktop/MLRA65_ppt/MLRA65_P30R31.csv")
MLRA65_P31R30<- read.csv("C:/Users/BatCave/Desktop/MLRA65_ppt/MLRA65_P31R30.csv")
MLRA65_P31R31<- read.csv("C:/Users/BatCave/Desktop/MLRA65_ppt/MLRA65_P31R31.csv")
MLRA65_P32R30<- read.csv("C:/Users/BatCave/Desktop/MLRA65_ppt/MLRA65_P32R30.csv")
MLRA65_P32R31<- read.csv("C:/Users/BatCave/Desktop/MLRA65_ppt/MLRA65_P32R31.csv")



MLRA65_P30R30_longterm_monthly_avg <- MLRA65_P30R30 %>% 
  separate(system.time_start, sep = " ", c("month", "day", "year")) %>% 
  group_by(month) %>%
  summarise_at(vars(ppt), list(name = mean)) 

write.csv(MLRA65_P30R30_longterm_monthly_avg,file = "MLRA65_P30R30_longterm_monthly_avg.csv")




MLRA65_P30R31_longterm_monthly_avg <- MLRA65_P30R31 %>% 
  separate(system.time_start, sep = " ", c("month", "day", "year")) %>% 
  group_by(month) %>%
  summarise_at(vars(ppt), list(name = mean)) 

write.csv(MLRA65_P30R31_longterm_monthly_avg,file = "MLRA65_P30R31_longterm_monthly_avg.csv")




MLRA65_P31R30_longterm_monthly_avg <- MLRA65_P31R30 %>% 
  separate(system.time_start, sep = " ", c("month", "day", "year")) %>% 
  group_by(month) %>%
  summarise_at(vars(ppt), list(name = mean)) 

write.csv(MLRA65_P31R30_longterm_monthly_avg,file = "MLRA65_P31R30_longterm_monthly_avg.csv")




MLRA65_P31R31_longterm_monthly_avg <- MLRA65_P31R31 %>% 
  separate(system.time_start, sep = " ", c("month", "day", "year")) %>% 
  group_by(month) %>%
  summarise_at(vars(ppt), list(name = mean)) 

write.csv(MLRA65_P31R31_longterm_monthly_avg,file = "MLRA65_P31R31_longterm_monthly_avg.csv")




MLRA65_P32R30_longterm_monthly_avg <- MLRA65_P32R30 %>% 
  separate(system.time_start, sep = " ", c("month", "day", "year")) %>% 
  group_by(month) %>%
  summarise_at(vars(ppt), list(name = mean)) 

write.csv(MLRA65_P32R30_longterm_monthly_avg,file = "MLRA65_P32R30_longterm_monthly_avg.csv")




MLRA65_P32R31_longterm_monthly_avg <- MLRA65_P32R31 %>% 
  separate(system.time_start, sep = " ", c("month", "day", "year")) %>% 
  group_by(month) %>%
  summarise_at(vars(ppt), list(name = mean)) 

write.csv(MLRA65_P32R31_longterm_monthly_avg,file = "MLRA65_P32R31_longterm_monthly_avg.csv")
