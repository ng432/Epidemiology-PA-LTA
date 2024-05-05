
library(abind)
library(dplyr)
library(nnet)
library(mice)
library(ggplot2)

source("convergencePlotFunctions.R")
source("predictedProbFunctions.R")

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





############### No co-variate model  #############

ncv_model = with(imputed_data,
                             multinom(PA ~ 
                                        SEP))

ncv_fit = averageAICdivergence(ncv_model)

ncv_pool = pool(ncv_model)

ncv_summary = summary(ncv_pool)


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


# NOTE: 'Mid active' is the reference outcome for the log odds

activity_names = c("Highly active",
                   "Mid active",
                   "Inactive",
                   "Decl. activity")


# data frame to store predicted probabilities and log odds
pred_probalities <- data.frame(
  PredictiveProbability = numeric(),
  LogOdds = numeric(),
  SEP = factor(levels = SEP_names),
  ActivityLevel = factor(levels = activity_names)
)

# calculating log odds for each of the non-reference classes, for each SEP 
# adds placeholder value of 0 for predictive probabilities
for (activity in activity_names)
  {
  # if activity level is not reference activity level, calculate log odds
  if (activity != 'Mid active')
  {
    log_odds = calculate_log_odds(activity, sixteen_focused_summary, data_for_regression)
  }
  else # i.e. if activity level is reference, need to set log odds = 0
  {
    log_odds = rep(0, times = 6)
  }
  
  str(log_odds)
  
  new_data <- data.frame(PredictiveProbability = rep(0, times = 6),
                         LogOdds = log_odds,
                         SEP = factor(SEP_names, levels = SEP_names),
                         ActivityLevel = factor(rep(activity, times = 6), levels = activity_names))
  
  pred_probalities <- rbind(pred_probalities, new_data)
}



# calculating probability each PA / outcome, for each SEP / predictor
# this is assuming that predicted probabilities result from a softmax function of log odds
# note, log odds for reference PA is 1

for (SEP in SEP_names) 
{
  # denominator for softmax is sum of log odds
  denominator = sum(exp(pred_probalities[pred_probalities$SEP == SEP, "LogOdds"]))
  
  for (activity in activity_names)
  {
    numerator <- pred_probalities[pred_probalities$SEP == SEP & pred_probalities$ActivityLevel == activity, "LogOdds"]
    numerator = exp(numerator)
    
    pred_prob = numerator / denominator
    
    # setting pred_probability 
    pred_probalities$PredictiveProbability[
      pred_probalities$SEP == SEP 
      & pred_probalities$ActivityLevel == activity] <- pred_prob
    
  }
  
}






############ Bootstrapping for CI across multiple imputed dataset ###########

## Using method 1 for bootstrapping with MI from https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5986623/

# STEP 1: combining multiple imputed datasets into one single dataset for bootstrapping

# loop through each imputed data set (1 to 5)


variable_list = colnames(imputed_data$data)

# takes the dataset with it's missing values 
dataset_to_fill = imputed_data$data

filled_datasets = list()

for (i in 1:imputed_data$m) {
  # form each completed column
  j = 1
  for (variable in variable_list)
  {
    missing_elements = imputed_data$where[,j]
    imputed_values = imputed_data$imp[[variable_list[j]]][,i]
    dataset_to_fill[[variable]][missing_elements] = imputed_values
    j = j + 1
  }
  
  filled_datasets[[i]] = dataset_to_fill
}

# good news... modelling this gets similar results to pooling from separate instances
all_imputed_datasets = bind_rows(filled_datasets, .id = "iteration")

pred_probs_from_pool = predicted_prob_estimate(all_imputed_datasets)

###  all samples pooled
### statistic we are looking at it is predicted probability
### need to return a function for predicted probability


boot_attempt = boot(all_imputed_datasets, predicted_prob_estimate, 1)


## For each imputed data set, carrying out bootstrapping

pp_boot_list = list()

dataset_to_fill = imputed_data$data
for (i in 1:imputed_data$m) {
  # form each completed column
  j = 1
  for (variable in variable_list)
  {
    missing_elements = imputed_data$where[,j]
    imputed_values = imputed_data$imp[[variable_list[j]]][,i]
    dataset_to_fill[[variable]][missing_elements] = imputed_values
    j = j + 1
  }
  
  booted_pp = boot(dataset_to_fill, predicted_prob_estimate, 500)
  
  pp_boot_list[[i]] = booted_pp
}


all_t_matrices <- do.call(rbind, lapply(pp_boot_list, function(boot) boot$t))

ci_lower <- apply(all_t_matrices, 2, quantile, probs = 0.025)
ci_upper <- apply(all_t_matrices, 2, quantile, probs = 0.975)

pred_probalities$ci_lower = ci_lower
pred_probalities$ci_upper = ci_upper





################ Outputting pretty graph ##########################

# just renaming to be clear for graph
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

ggplot(pred_probalities, aes(x = SEP, y = PredictiveProbability, color = ActivityLevel)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot", x = "SEP", y = "Probability", color = "Activity Level")


ggplot(pred_probalities, aes(x = SEP, y = PredictiveProbability, color = ActivityLevel)) +
  geom_point() +  # Plot points
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  # Add error bars for CI
  theme_minimal() +
  labs(title = "Scatter Plot with Confidence Intervals", x = "SEP", y = "Predictive Probability", color = "Activity Level")





