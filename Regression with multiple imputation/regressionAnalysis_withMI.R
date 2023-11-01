

library(dplyr)
library(nnet)
library(mice)

source("convergencePlotFunctions.R")

load("../Data/data_for_regression.Rdata")

load(file = "Imputed data/imputed_data_all_variables_rfr_m5iter100.Rdata")






#### Null model (no imputed data required) ##########

null_model <- multinom(class ~ 1, data = data_for_regression)

# n = 12914, gives Residual Deviance: 25903.5, AIC: 25909.5 
nm_summary = summary(null_model)





#### Simple model (no imputed data required) ##########

simple_imputed_model <- with(imputed_data, 
                             multinom(class ~ 
                                        SEP + ethnicity_collapsed + gender + 
                                                      parental_class_10 + region_10))

simple_fit = averageAICdivergence(simple_imputed_model)

simple_model_pool = pool(simple_imputed_model)

# includes Z-statistics as 'statistics'
simple_model_summary = summary(simple_model_pool)




########## Sixteen focused model #############


sixteen_focused_model = with(imputed_data,
     multinom(class ~ 
           SEP + gender +  ethnicity_collapsed + 
           region_16 + malaise_16 +
           school_type_16 + parental_sc_16 +
           children_26))

sf_fit = averageAICdivergence(sixteen_focused_model)

sixteen_focused_pool = pool(sixteen_focused_model)

sixteen_focused_summary = summary(sixteen_focused_pool)




#### SVF2 model (no imputed data required) ##########


sixten_focused_v2_model = with(imputed_data,
                            multinom(class ~ 
                                       SEP + gender +  ethnicity_collapsed + 
                                       region_16 + school_type_16 + parental_class_10 +
                                       children_26))

sfv2_fit = averageAICdivergence(sixten_focused_v2_model)

sixteen_focused_v2_pool = pool(sixten_focused_v2_model )

sixteen_focused_v2_summary = summary(sixteen_focused_v2_pool)










