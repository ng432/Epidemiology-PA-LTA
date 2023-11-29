

library(dplyr)
library(nnet)
library(mice)

source("convergencePlotFunctions.R")

load(file = "Imputed data/imputed_data_all_variables_rfr_m5iter100.Rdata")


##### Renaming PA trajectories and SEP trajectories to sensible names ########

# SEP from: https://jech.bmj.com/content/75/12/1172.abstract

# converting imputed data to table format to rename levels
imputed_data = complete(imputed_data, action='long', include=TRUE)

levels(imputed_data$SEP) <- c(
  "Continued Education",
  "Managerial Employment",
  "Skilled Non-manual Employment",
  "Skilled Manual Employment",
  "Partly Skilled Employment",
  "Economically Inactive"
)


levels(imputed_data$class) <- c(
  "Highly active",
  "Mid active",
  "Inactive",
  "Decl. activity"
)

colnames(imputed_data)[colnames(imputed_data) == "class"] <- "PA"


# Making 'Mid active' the reference class
# mid active is the most populous PA trajectory
imputed_data$class <- relevel(imputed_data$PA, ref = "Mid active")

# returing imputed data for mids structure
imputed_data <- as.mids(imputed_data)



############### Running sixteen focused model      #############

sixteen_focused_model = with(imputed_data,
                             multinom(class ~ 
                                        SEP + gender +  ethnicity_collapsed + 
                                        region_16 + malaise_16 +
                                        school_type_16 + parental_class_10 +
                                        children_26))

sf_fit = averageAICdivergence(sixteen_focused_model)

sixteen_focused_pool = pool(sixteen_focused_model)

sixteen_focused_summary = summary(sixteen_focused_pool)













