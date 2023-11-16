
library(regclass)
SAL = read.csv('C:/Users/jackg/OneDrive - University of Tennessee/UTK Fall 23/BAS 320/Homework Files/Assignment 8/ds_salaries.csv', stringsAsFactors = TRUE)


SAL <- SAL %>%
  mutate(new_experience_level = case_when(
    experience_level %in% c("EN", "MI") ~ "Low_to_Mid",
    experience_level %in% c("SE", "EX") ~ "Senior_to_Exec",
  ))
SAL$new_experience_level = as.factor(SAL$new_experience_level)

#summary(SAL$work_year)
summary(lm(salary_in_usd~work_year, data=SAL))




M1 = lm(salary_in_usd~work_year+new_experience_level,data=SAL) #no interaction
  visualize_model(M1)
M2 = lm(salary_in_usd~remote_ratio*new_experience_level,data=SAL) #with interaction

############
M1 = lm(Writing~Math+Success, data=TESTSCORES) #Fit a regression predicting y from x1 (numeric) and x2 (categorical) that EXCLUDES the interaction
summary(M1)

visualize_model(M1)

M2 = lm(Writing~Math*Lunch, data=TESTSCORES)  #Fit a regression predicting y from x1 (numeric) and x2 (categorical) that INCLUDES the interaction
summary(M2)
visualize_model(M2)