# Function to pool multiple imputation results from mixed effects model using Rubin's rules
# This function extracts coefficients and R² from models fitted on imputed datasets
# and pools them according to Rubin's rules for multiple imputation inference

source("functions/extract_coefficient.R")

pool_mi_mixed_results <- function(model_list, exposure_var) {
  #' Pool Multiple Imputation Model Results Using Rubin's Rules
  #'
  #' @param model_list List of fitted lmer models from m imputed datasets
  #' @param exposure_var Character string specifying the exposure variable name
  #'
  #' @return A tibble with pooled estimates, standard errors, 95% CI, 
  #'         degrees of freedom, RIV, FMI, and R² values
  #'
  #' @details
  #' This function applies Rubin's rules to pool:
  #' - Point estimates (arithmetic mean across imputations)
  #' - Standard errors (accounting for within and between-imputation variance)
  #' - 95% confidence intervals (using adjusted degrees of freedom)
  #' - Marginal and conditional R² values (average across imputations)
  #'
  #' Diagnostics included:
  #' - RIV: Relative Increase in Variance (due to missing data)
  #' - FMI: Fraction of Missing Information (proportion of information lost)
  
  
  m <- length(model_list)  # number of imputations
  
  
  # Initialize storage for coefficients and R²
  coef_matrix <- matrix(NA, nrow = m, ncol = 4)
  colnames(coef_matrix) <- c("estimate", "se", "marginal_r2", "conditional_r2")
  
  # Extract coefficients and R² from each imputed model
  for (j in 1:m) {
    coef_tidy <- extract_coefficient(model_list[[j]], exposure_var)
    coef_matrix[j, "estimate"] <- coef_tidy$estimate
    coef_matrix[j, "se"] <- (coef_tidy$conf.high - coef_tidy$conf.low) / (2 * 1.96)
    coef_matrix[j, "marginal_r2"] <- coef_tidy$marginal_r2
    coef_matrix[j, "conditional_r2"] <- coef_tidy$conditional_r2
  }
  
  # Apply Rubin's rules for pooling
  # Pooled estimate (simple average across imputations)
  est_pooled <- mean(coef_matrix[, "estimate"])
  
  # Within-imputation variance (average of squared standard errors)
  var_within <- mean(coef_matrix[, "se"]^2)
  
  # Between-imputation variance
  var_between <- var(coef_matrix[, "estimate"])
  
  # Total variance (Rubin's rule combines within and between variance)
  # Includes inflation factor (1 + 1/m) to account for finite number of imputations
  var_total <- var_within + (1 + 1/m) * var_between
  se_pooled <- sqrt(var_total)
  
  # Adjusted degrees of freedom for confidence intervals
  # This accounts for variability in estimates across imputations
  df_pooled <- (m - 1) * (1 + var_within / ((1 + 1/m) * var_between))^2
  
  # 95% CI using t-distribution with adjusted degrees of freedom
  t_crit <- qt(0.975, df = df_pooled)
  ci_low <- est_pooled - t_crit * se_pooled
  ci_high <- est_pooled + t_crit * se_pooled
  
  # Relative Increase in Variance (RIV)
  # Measures how much variance increased due to missing data
  riv <- (1 + 1/m) * var_between / var_within
  
  # Fraction of Missing Information (FMI)
  # Proportion of information lost due to missing data
  fmi <- (riv + 2/(df_pooled + 3)) / (riv + 1)
  
  # Pool R² values (average across imputations)
  marginal_r2_pooled <- mean(coef_matrix[, "marginal_r2"], na.rm = TRUE)
  conditional_r2_pooled <- mean(coef_matrix[, "conditional_r2"], na.rm = TRUE)
  
  # Return results as a tibble
  tibble(
    exposure = exposure_var,
    n_imputed = m,
    estimate = est_pooled,
    se = se_pooled,
    conf.low = ci_low,
    conf.high = ci_high,
    df = df_pooled,
    riv = riv,
    fmi = fmi,
    marginal_r2 = marginal_r2_pooled,
    conditional_r2 = conditional_r2_pooled
  )
}
