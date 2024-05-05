
library(dplyr)
library(nnet)
library(mice)
library(ggplot2)
library(tidyr)

# necessary for bootstrapping method
predicted_prob_estimate <- function(filled_dataset, indices = NULL)
{

  if(is.null(indices)) {
    filled_datasample <- filled_dataset
  } else {
    filled_datasample <- filled_dataset[indices, ]
  }
  
  model = multinom(data = filled_datasample, PA ~ 
             SEP + gender +  ethnicity_collapsed + 
             region_16 + malaise_16 +
             school_type_16 + parental_class_10 +
             children_26)
  
  matrix_coef = coef(model)
  
  long_df <- as_tibble(matrix_coef, .name_repair = "unique")
  
  long_df$y.level <- rownames(matrix_coef)
  
  long_df <- long_df %>%
    pivot_longer(cols = -y.level, names_to = "term", values_to = "estimate")
  
  # NOTE: 'Mid active' is the reference outcome for the log odds
  
  activity_names = c("Highly active",
                     "Mid active",
                     "Inactive",
                     "Decl. activity")
  
  SEP_names <- c(
    "SEPContinued Education",
    "SEPManagerial Employment",
    "SEPSkilled Non-manual Employment",
    "SEPSkilled Manual Employment",
    "SEPPartly Skilled Employment",
    "SEPEconomically Inactive"
  )
  
  
  # data frame to store predicted probabilities and log odds
  pred_probalities <- data.frame(
    PredictiveProbability = numeric(),
    LogOdds = numeric(),
    SEP = factor(levels = SEP_names),
    ActivityLevel = factor(levels = activity_names)
  )
  
  # calculating log odds
  for (activity in activity_names)
  {
    # if activity level is not reference activity level, calculate log odds
    if (activity != 'Mid active')
    {
      log_odds = calculate_log_odds(activity, long_df, filled_datasample)
    }
    else # i.e. if activity level is reference, need to set log odds = 1 
    {
      log_odds = rep(0, times = 6)
    }
    
    
    new_data <- data.frame(PredictiveProbability = rep(0, times = 6),
                           LogOdds = log_odds,
                           SEP = factor(SEP_names,  levels = SEP_names),
                           ActivityLevel = factor(rep(activity, times = 6), levels = activity_names))
    
    str(new_data)
    str(pred_probalities)
    
    pred_probalities <- rbind(pred_probalities, new_data)
  }
  
  # calculating predicted probabilities
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
  
  pred_probalities$LogOdds <- NULL
  
  matrix_pp <- xtabs(PredictiveProbability ~ SEP + ActivityLevel, data = pred_probalities)
  
  return(matrix_pp)
  
}


calculate_log_odds <- function(activity_level, summary, data_for_regression) {
  
  # returns a dataframe of log odds for a given activity level / outcome (compared to reference activity level)
  # across all SEP levels / predictor.
  
  # Filter the coefficients for the given activity level
  ha_coeff <- summary %>%
    filter(y.level == activity_level)
  
  # Calculate means
  mean_malaise_16 <- mean(data_for_regression$malaise_16, na.rm = TRUE)
  mean_children_26 <- mean(data_for_regression$children_26, na.rm = TRUE)
  
  # Coefficients names
  gender_coeff_name <- 'genderFemale'
  region_16_coeff_name <- 'region_16South East'
  pc_10_coeff_name <- 'parental_class_10III non-manual'
  
  SEP_names <- c(
    "SEPContinued Education",
    "SEPManagerial Employment",
    "SEPSkilled Non-manual Employment",
    "SEPSkilled Manual Employment",
    "SEPPartly Skilled Employment",
    "SEPEconomically Inactive"
  )
  
  # Initialize log_odds_list
  log_odds_list <- c()
  
  # need to calculate (log odds / pred prob) FOR EACH SEP
  for (SEP in SEP_names) {
    
    # initiating with intercept specific to activity level
    log_odds <- ha_coeff[ha_coeff$term == "(Intercept)", 'estimate']
    
    # if SEP isn't the reference SEP (which the valued added would be 0), add it's value
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
    
    # adding to log_odds list
    if((is.list(log_odds) || is.data.frame(log_odds)) && "estimate" %in% names(log_odds)) {
      log_odds_list <- c(log_odds_list, log_odds$estimate)
    } else {
      log_odds_list <- c(log_odds_list, log_odds)
    }
  }
  
  return(log_odds_list)
}









