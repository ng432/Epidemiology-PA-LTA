



library(dplyr)
library(nnet)
library(mice)

source("convergencePlotFunctions.R")



load("../Data/data_for_regression.Rdata")


####################### Running imputation #######################



######### Trying to impute all variables ########

# Removing BCSID, as would mess up imputation
data_for_regression %>% 
  select(-BCSID) ->
  data_for_mi_regression

imputed_data <- mice(data_for_mi_regression, m = 5, maxit = 100)

# Plots traces 
plot(imputed_data)

conv_imputed_data = convergence(imputed_data)

plotConvergenceGrid(conv_imputed_data, start=1, end=6)








#### Trying to impute Sixteen focused V2 (sf) model variables ####

# sixteen focused model ignoring variables with a number high missing values


variables_for_sfv2_model = c('class',  'SEP', 'gender', 'ethnicity_collapsed', 
                             'parental_class_10', 'children_26', 'region_16',
                             'school_type_16')

data_for_mi_regression %>%
  select(variables_for_sfv2_model) ->
  data_for_sfv2_model



sfv2_imputed_data <- mice(data_for_sf_model, m = 5, maxit = 100)



load(file = "Imputed data/sfv2_reduced_variables_imputed_data_m5_maxit100.Rdata")




# Plots traces 
plot(sfv2_imputed_data, layout = c(2,4))

load("sfv2_imputed_data_m5_maxit100.Rdata")

sfv2_conv_data = convergence(sfv2_imputed_data)

plotConvergenceGrid(sfv2_conv_data, ignoreVars = c("class", "SEP"))











