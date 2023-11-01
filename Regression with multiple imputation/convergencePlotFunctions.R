

plotConvergence <- function(data, variableName, convergenceType = "ac") {

  variableData <- data[data$vrb == variableName, ]
  

  x <- variableData$.it
  
  
  if (convergenceType == "ac") {
    y <- variableData$ac
    yLabel <- "Autocorrelation (AC)"
    threshold <- 0.1
  } else if (convergenceType == "psrf") {
    y <- variableData$psrf
    yLabel <- "Potential Scale Reduction Factor (PSRF)"
    threshold <- 1.1
  } else {
    stop("Invalid convergenceType. Use 'ac' or 'psrf'.")
  }
  

  plot(x, y, type = "l", xlab = "Iteration Number", ylab = yLabel, main = variableName)
  

  abline(h = threshold, col = "red", lty = 2)
}



plotConvergenceGrid <- function(data, acTitle = 'ac', psrfTitle = 'psrf', end = NULL, start = 1, ignoreVars = c("class")) {
  
  uniqueVarNames <- unique(data$vrb[!(data$vrb %in% ignoreVars)])
  
  if (is.null(end)) {
    end <- length(uniqueVarNames)
  } else {
    end <- min(end, length(uniqueVarNames))
  }
  
  par(mfrow = c(end - start + 1, 2))
  par(mar = c(2, 2, 2, 2))
  
  for (i in start:(start + end - 1)) {
    if (i > length(uniqueVarNames)) {
      break  # Prevent going beyond the available variables
    }
    
    var <- uniqueVarNames[i]
    
    plotConvergence(data, variableName = var, convergenceType = "ac")
    plotConvergence(data, variableName = var, convergenceType = "psrf")
  }
  
  par(mfrow = c(1, 1))
}


averageAICdivergence <- function(model) {
  if (!"analyses" %in% names(model)) {
    stop("The 'analyses' element is not present in the model.")
  }
  
  analyses <- model$analyses
  
  avg_AIC <- mean(sapply(analyses, function(x) x[["AIC"]]), na.rm = TRUE)
  avg_divergence <- mean(sapply(analyses, function(x) x[["deviance"]]), na.rm = TRUE)
  
  return(list(AIC = avg_AIC, deviance = avg_divergence))
}




getEmptyFactorLevels <- function(df) {
  empty_factor_levels <- list()
  
  
  for (col_name in names(df)) {
    
    if (is.factor(df[[col_name]])) {

      levels <- levels(df[[col_name]])
      
      
      for (level in levels) {
        if (sum(df[[col_name]] == level, na.rm = TRUE) == 0) {
          empty_factor_levels[[col_name]] <- c(empty_factor_levels[[col_name]], level)
        }
      }
    }
  }
  
  return(empty_factor_levels)
}



