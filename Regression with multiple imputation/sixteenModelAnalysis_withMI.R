

library(dplyr)
library(nnet)
library(mice)
library(ggplot2)

source("convergencePlotFunctions.R")

load(file = "Imputed data/imputed_data_all_variables_rfr_m5iter100.Rdata")

load("../Data/data_for_regression.Rdata")


##### Renaming PA trajectories and SEP trajectories to sensible names ########

# SEP from: https://jech.bmj.com/content/75/12/1172.abstract

# converting imputed data to table format to rename levels
imputed_data = complete(imputed_data, action='long', include=TRUE)


SEP_names = c(
  "Continued Education",
  "Managerial Employment",
  "Skilled Non-manual Employment",
  "Skilled Manual Employment",
  "Partly Skilled Employment",
  "Economically Inactive"
)

levels(imputed_data$SEP) <- SEP_names


levels(imputed_data$class) <- c(
  "Highly active",
  "Mid active",
  "Inactive",
  "Decl. activity"
)

colnames(imputed_data)[colnames(imputed_data) == "class"] <- "PA"


# Making most populous factors the reference classes
# mid active is the most populous PA trajectory
imputed_data$PA <- relevel(imputed_data$PA, ref = "Mid active")

# returning imputed data to mids structure
imputed_data <- as.mids(imputed_data)



############### Running sixteen focused model      #############

sixteen_focused_model = with(imputed_data,
                             multinom(PA ~ 
                                        SEP + gender +  ethnicity_collapsed + 
                                        region_16 + malaise_16 +
                                        school_type_16 + parental_class_10 +
                                        children_26))

sf_fit = averageAICdivergence(sixteen_focused_model)

sixteen_focused_pool = pool(sixteen_focused_model)

sixteen_focused_summary = summary(sixteen_focused_pool)




############ Extracting predicted probabilites given SEP ###########

# want to find predicted probability, assuming the average for continuous variables, and 
# the mode for categorical 

# For categorical / factors, mode are asfollowing:
# Gender, ethnicity_collapsed, region_16 , school_type_16, parental_class_10
# Female, white              , South east, Comprehensive , III manual



# SEP names
SEP_names <- c(
  "SEPContinued Education",
  "SEPManagerial Employment",
  "SEPSkilled Non-manual Employment",
  "SEPSkilled Manual Employment",
  "SEPPartly Skilled Employment",
  "SEPEconomically Inactive"
)

calculate_pred_prob <- function(activity_level) {
  # Filter the coefficients for the given activity level
  ha_coeff <- sixteen_focused_summary %>%
    filter(y.level == activity_level)
  
  # Calculate means
  mean_malaise_16 <- mean(data_for_regression$malaise_16, na.rm = TRUE)
  mean_children_26 <- mean(data_for_regression$children_26, na.rm = TRUE)
  
  # Coefficients names
  gender_coeff_name <- 'genderFemale'
  region_16_coeff_name <- 'region_16South East'
  pc_10_coeff_name <- 'parental_class_10III non-manual'
  
  # Initialize pred_prob_list
  pred_prob_list <- c()
  
  for (SEP in SEP_names) {
    # initiating with intercept
    log_odds <- ha_coeff[ha_coeff$term == "(Intercept)", 'estimate']
    
    if (SEP != "SEPContinued Education") {
      log_odds <- log_odds + ha_coeff[ha_coeff$term == SEP, 'estimate']
    }
    
    # adding odds for categorical variables which aren't the reference
    log_odds <- log_odds + ha_coeff[ha_coeff$term == gender_coeff_name, 'estimate']
    log_odds <- log_odds + ha_coeff[ha_coeff$term == region_16_coeff_name, 'estimate']
    log_odds <- log_odds + ha_coeff[ha_coeff$term == pc_10_coeff_name, 'estimate']
    
    # adding odds for mean values for continuous variables
    log_odds <- log_odds + mean_malaise_16 * ha_coeff[ha_coeff$term == "malaise_16", 'estimate']
    log_odds <- log_odds + mean_children_26 * ha_coeff[ha_coeff$term == "children_26", 'estimate']
    
    # Calculate predicted probability
    pred_prob <- exp(log_odds) / (1 + exp(log_odds))
    pred_prob_list <- c(pred_prob_list, pred_prob)
  }
  
  SEP_values <- factor(c(1:6), levels = 1:6, labels = SEP_names)
  
  pred_prob_df <- data.frame(PredProb = pred_prob_list, SEP = SEP_values)
  
  return(pred_prob_df)
}


activity_names = c("Highly active",
                   "Mid active",
                   "Inactive",
                   "Decl. activity")


# data frame to store predicted probabilites
pred_probalities <- data.frame(
  PredictiveProbability = numeric(),
  SEP = factor(levels = SEP_names),
  ActivityLevel = factor(levels = activity_names)
)

# calculating predicted probability for each of the non-reference classes 
for (activity in activity_names)
  {
  if (activity != 'Mid active')
  {
  activity_pp = calculate_pred_prob(activity)
  
  new_data <- data.frame(PredictiveProbability = activity_pp$PredProb,
                         SEP = factor(activity_pp$SEP,  levels = SEP_names),
                         ActivityLevel = factor(rep(activity, times = 6), levels = activity_names))
  
  pred_probalities <- rbind(pred_probalities, new_data)
  }
}

# calculating probability for reference class, for each SEP
for (SEP in SEP_names) 
{
  # probability for reference class (i.e. remaining probability)
  prob = 1 - sum(pred_probalities[pred_probalities$SEP == SEP, "PredictiveProbability"])
  
  new_row = data.frame(PredictiveProbability = prob,
                       SEP = factor(SEP,  levels = SEP_names),
                       ActivityLevel = factor('Mid active', levels = activity_names))
  
  pred_probalities <- rbind(pred_probalities, new_row)
  
}

SEP_names = c(
  "Cont. Educ.",
  "Managerial",
  "Skill. Non-manual",
  "Skill. Manual",
  "Partly Skill.",
  "Econom Inactive"
)

# renaming levels to make more sense
levels(pred_probalities$SEP) = SEP_names

ggplot(pred_probalities, aes(x = ActivityLevel, y = PredictiveProbability, color = SEP)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot", x = "SEP", y = "Probability", color = "Activity Level")







