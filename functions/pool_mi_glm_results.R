# Function to pool multiple imputation results using Rubin's rules for GLM models
# This function extracts coefficients from GLM models fitted on imputed datasets
# and pools them according to Rubin's rules for multiple imputation inference

pool_mi_glm_results <- function(model_list, terms_of_interest) {
  #' Pool Multiple Imputation GLM Results Using Rubin's Rules
  #'
  #' @param model_list List of fitted GLM models from m imputed datasets
  #' @param terms_of_interest Character vector specifying which terms to extract
  #'
  #' @return A tibble with pooled estimates (exponentiated), standard errors, 
  #'         95% CI, degrees of freedom, RIV, FMI for each term
  #'
  #' @details
  #' This function applies Rubin's rules to pool:
  #' - Point estimates on log scale, then exponentiate (for OR, HR, RR)
  #' - Standard errors (accounting for within and between-imputation variance)
  #' - 95% confidence intervals (using adjusted degrees of freedom)
  #' - Diagnostic statistics (RIV, FMI)
  

  m <- length(model_list)  # number of imputations
  
  # Extract coefficients from each imputed model
  results_list <- list()
  
  for (term in terms_of_interest) {
    coef_matrix <- matrix(NA, nrow = m, ncol = 2)
    colnames(coef_matrix) <- c("estimate", "se")
    
    for (j in 1:m) {
      coefs <- summary(model_list[[j]])$coefficients
      
      if (term %in% rownames(coefs)) {
        coef_matrix[j, "estimate"] <- coefs[term, "Estimate"]
        coef_matrix[j, "se"] <- coefs[term, "Std. Error"]
      }
    }
    
    # Check for missing values
    valid_idx <- !is.na(coef_matrix[, "estimate"])
    if (sum(valid_idx) == 0) {
      warning(paste("No valid estimates found for term:", term))
      next
    }
    
    m_valid <- sum(valid_idx)
    
    # Apply Rubin's rules for pooling (on log scale)
    est_pooled <- mean(coef_matrix[valid_idx, "estimate"])
    
    # Within-imputation variance
    var_within <- mean(coef_matrix[valid_idx, "se"]^2)
    
    # Between-imputation variance
    var_between <- var(coef_matrix[valid_idx, "estimate"])
    
    # Total variance (Rubin's rule)
    var_total <- var_within + (1 + 1/m_valid) * var_between
    se_pooled <- sqrt(var_total)
    
    # Adjusted degrees of freedom
    df_pooled <- (m_valid - 1) * (1 + var_within / ((1 + 1/m_valid) * var_between))^2
    
    # 95% CI on log scale, then exponentiate
    t_crit <- qt(0.975, df = df_pooled)
    ci_low_log <- est_pooled - t_crit * se_pooled
    ci_high_log <- est_pooled + t_crit * se_pooled
    
    # Exponentiate for reporting (OR, HR, RR, etc.)
    est_exp <- exp(est_pooled)
    ci_low_exp <- exp(ci_low_log)
    ci_high_exp <- exp(ci_high_log)
    
    # Relative Increase in Variance (RIV)
    if (var_between > 0) {
      riv <- (1 + 1/m_valid) * var_between / var_within
    } else {
      riv <- 0
    }
    
    # Fraction of Missing Information (FMI)
    fmi <- (riv + 2/(df_pooled + 3)) / (riv + 1)
    
    # Store results
    results_list[[term]] <- tibble(
      term = term,
      n_imputed = m_valid,
      estimate_log = est_pooled,
      se_log = se_pooled,
      estimate = est_exp,
      conf.low = ci_low_exp,
      conf.high = ci_high_exp,
      df = df_pooled,
      riv = riv,
      fmi = fmi
    )
  }
  
  # Combine all results into one dataframe
  bind_rows(results_list)
}
