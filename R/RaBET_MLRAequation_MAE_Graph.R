#Code written by Cynthia L. Norton - University of Arizona
#Automation for production of mean MAE and Graphs

library(tidyverse)
library(car)

#import MLRA file in easier readable format
MLRA_aggregate <- read.csv('C:/Users/BatCave/Documents/RaBET/MLRA_81C_aggregated_edSMS.csv') %>% select(-c(1:6)) %>% 
  separate(col = 1, into = c("MLRA", "Column2","Class", "Column4","Column5", "Column6","Column7", "Column8"), sep = "_") %>% #seperate ID class for filtering
  select(c(MLRA,Class,NAIP_WC,MSAVI,NDI5,WVI)) #select important columns

#class sample selection
select_Class <- c("12", "20") #change or add numbers for selected class

#Subset data
equation_Subset <- MLRA_aggregate %>% filter(Class %in% select_Class) #subset classes for equation creation
prediction_Subset <- MLRA_aggregate %>% filter(! Class %in% select_Class) #subset classes for figure and MAE production

#calculate linear model
equation_Subset_lm_equation <- lm(NAIP_WC ~ MSAVI + NDI5 + WVI, data=equation_Subset) #linear model using subsetted data

#extract coefficients 
equation_Subset_coefficients <- coefficients(equation_Subset_lm_equation) %>% 
  as.data.frame() %>% #set as a dataframe
  setNames("value") %>% #change column name
  mutate(coef = c("intercet", "MSAVI", "NDI5", "WVI"))%>% #add column with new column names
  tidyr::spread(key = coef, value = value) #changes format of coefficients to one row and 4 columns for ease of value extraction
# model coefficients

intercept_coeff <- equation_Subset_coefficients$intercet #intercept value for equation
MSAVI_coeff <- equation_Subset_coefficients$MSAVI #MSAVI value for equation
NDI5_coeff <- equation_Subset_coefficients$NDI5 #NDI5 value for equation
WVI_coeff <- equation_Subset_coefficients$WVI #WVI value for equation

#Calculate prediction for all pixels without class samples used in linear model
MLRA_aggregate_processed <- prediction_Subset %>% #Prediction calculation
  transform(Prediction = (MSAVI_coeff*MSAVI) +
                            (NDI5_coeff*NDI5) +
                            (WVI_coeff*WVI) + 
                            intercept_coeff) %>%
  group_by(Class) %>% #group by class
  summarise(round(across(everything(), list(mean))), digits = 0) %>% #gather mean values by group rounded up
  select(c(1,3,7)) %>% #select columns wanted
  rename(Class = 1, NAIP_WC_mean = 2, Predicted_WC_mean = 3) %>% #rename columns
  transform(Difference = abs((NAIP_WC_mean-Predicted_WC_mean))) #abosulute value difference

final_mean_mae <- mean(MLRA_aggregate_processed$Difference) #FINAL MAE value

plot(MLRA_aggregate_processed$NAIP_WC_mean, MLRA_aggregate_processed$Predicted_WC_mean, 
     xlim = c(0,100), ylim = c(0,100), pch = 16, cex = 1.5, col = "red", 
     ylab = "Predicted Mean WC", xlab = "NAIP Mean WC",
     cex.lab = 1.5 , cex.axis = 1.5) #plotting
  abline(a = 0, b = 1, lwd = 3)   # 1 to 1 line
  mylabel = bquote(MAE == .(format(final_mean_mae, digits = 3))) #add MAE on figure
  text(x = 10, y = 90, labels = mylabel , cex = 2.5)
