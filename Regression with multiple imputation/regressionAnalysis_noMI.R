

library(haven)
library(dplyr)
library(nnet)
library(mice)

# Regression analysis to see effect of exposure (adulthood SEP) and outcome ()

# All variables considering:
# Gender, ethnicity
# Region at 16, School type at 16, parental social class at 16
# Malaise at 16, Health at 16, Health behaviors at 16
# children at 26

# loading and filtering trajectory class data (outcome) and SEP at 30 (exposure)

load("../Data/data_for_regression.Rdata")



####################### Running models  #######################

## NULL MODEL ##
null_model <- multinom(class ~ 1, data = data_for_regression)

# n = 12914, gives Residual Deviance: 25903.5, AIC: 25909.5 
nm_summary = summary(null_model)

nullm_data <- data_for_regression %>%
  select(class)
nullm_complete_cases <- sum(complete.cases(nullm_data))


## SIMPLE MODEL ##
simple_model <- multinom(class ~ SEP + ethnicity_collapsed + gender + 
                           parental_class_10 + region_10, 
                         data = data_for_regression)

# n = 9360, gives Residual Deviance: 18973.62, AIC: 19009.62 
summary(simple_model)

simplem_data <- data_for_regression %>%
  select(class, SEP, gender, ethnicity_collapsed, parental_class_10, region_10)
simplem_complete_cases <- sum(complete.cases(simplem_data))



## SIXTEN FOCUSED MODEL V1 ##
# n = 2258, gives Residual Deviance: 4424.155 , AIC: 4478.155, 
sixteen_focused_model <- multinom(class ~ 
                                    SEP + gender +  ethnicity_collapsed + 
                                    region_16 + malaise_16 +
                                    school_type_16 + parental_sc_16 +
                                    children_26, 
                                  data = data_for_regression)

summary(sixten_focused_model)

sfm_data <- data_for_regression %>%
  select(class, SEP, gender, ethnicity_collapsed, 
         region_16, malaise_16, school_type_16, parental_sc_16, children_26)
sfm_complete_cases <- sum(complete.cases(sfm_data))



## SIXTEN FOCUSED MODEL V2 ##
sixteen_focused_v2_model <- multinom(class ~ 
                                    SEP + gender +  ethnicity_collapsed + 
                                    region_16 +
                                    school_type_16 + parental_class_10 +
                                    children_26, 
                                  data = data_for_regression)

summary(sixteen_focused_v2_model)

sfmv2_data <- data_for_regression %>%
  select(class, SEP, gender, ethnicity_collapsed, 
         region_16, school_type_16, parental_class_10, children_26)
sfmv2_complete_cases <- sum(complete.cases(sfmv2_data))















