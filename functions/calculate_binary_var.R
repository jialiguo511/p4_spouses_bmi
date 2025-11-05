# Function to calculate binary variable summaries with odds ratio pooling

calculate_binary_var <- function(datasets, var) {
  #' Calculate Binary Variable Summary with Odds Ratio Pooling
  #'
  #' @param datasets List of imputed baseline datasets
  #' @param var Character string specifying the variable name
  #'
  #' @return A list with pooled statistics and formatted strings for reporting
  #'
  #' @details
  #' For each imputed dataset:
  #' - Calculates proportions for females and males
  #' - Calculates odds ratio using Fisher's exact test on paired data
  #' - Calculates missing data percentage
  #'
  #' Results are pooled using:
  #' - Arithmetic mean for proportions and missing data
  #' - Rubin's rules on log-scale for odds ratio pooling
  
  # Calculate across imputations
  results <- lapply(datasets, function(df) {
    # Proportions
    f_prop <- mean(df[[var]][df$sex == "female"] == 1, na.rm = TRUE) * 100
    m_prop <- mean(df[[var]][df$sex == "male"] == 1, na.rm = TRUE) * 100
    
    f_n <- sum(df[[var]][df$sex == "female"] == 1, na.rm = TRUE)
    m_n <- sum(df[[var]][df$sex == "male"] == 1, na.rm = TRUE)
    
    # Missing data
    f_missing <- mean(is.na(df[[var]][df$sex == "female"])) * 100
    m_missing <- mean(is.na(df[[var]][df$sex == "male"])) * 100
    
    # Calculate OR for paired couples
    df_wide <- df %>%
      select(hhid, sex, !!sym(var)) %>%
      pivot_wider(
        id_cols = hhid,
        names_from = sex,
        values_from = !!sym(var),
        names_prefix = "sex_"
      ) %>%
      dplyr::filter(!is.na(sex_female) & !is.na(sex_male))
    
    or_result <- tryCatch({
      # Create contingency table
      tbl <- table(df_wide$sex_male, df_wide$sex_female)
      
      # Calculate OR using Fisher's exact test
      if(nrow(tbl) == 2 && ncol(tbl) == 2) {
        fisher_result <- fisher.test(tbl)
        list(
          or = fisher_result$estimate,
          lower = fisher_result$conf.int[1],
          upper = fisher_result$conf.int[2]
        )
      } else {
        list(or = NA_real_, lower = NA_real_, upper = NA_real_)
      }
    }, error = function(e) {
      list(or = NA_real_, lower = NA_real_, upper = NA_real_)
    })
    
    list(
      f_prop = f_prop, 
      m_prop = m_prop, 
      f_n = f_n,
      m_n = m_n,
      or = or_result$or,
      lower = or_result$lower,
      upper = or_result$upper,
      f_missing = f_missing,
      m_missing = m_missing
    )
  })
  
  # Pool proportions (simple average)
  f_prop <- mean(sapply(results, function(x) x$f_prop))
  m_prop <- mean(sapply(results, function(x) x$m_prop))
  f_missing <- mean(sapply(results, function(x) x$f_missing))
  m_missing <- mean(sapply(results, function(x) x$m_missing))
  
  # Pool OR and CIs using log-transform method (Rubin's rules)
  valid_indices <- which(sapply(results, function(x) 
    !is.na(x$or) && is.finite(x$or) && x$or > 0))
  
  or_ci_string <- "NA"
  
  if(length(valid_indices) > 0) {
    # Extract valid ORs and CIs
    valid_ors <- sapply(results[valid_indices], function(x) x$or)
    valid_lower <- sapply(results[valid_indices], function(x) x$lower)
    valid_upper <- sapply(results[valid_indices], function(x) x$upper)
    
    # Log transform for pooling
    log_ors <- log(valid_ors)
    log_lower <- log(valid_lower)
    log_upper <- log(valid_upper)
    
    # Pool point estimate
    pooled_log_or <- mean(log_ors)
    
    # Calculate variances for CIs (Rubin's rules)
    var_within <- mean((log_upper - log_lower)^2 / (2*1.96)^2)
    var_between <- var(log_ors)
    m <- length(valid_indices)
    total_var <- var_within + var_between * (1 + 1/m)
    
    # Back-transform for final results
    pooled_or <- exp(pooled_log_or)
    pooled_lower <- exp(pooled_log_or - 1.96 * sqrt(total_var))
    pooled_upper <- exp(pooled_log_or + 1.96 * sqrt(total_var))
    
    or_ci_string <- sprintf("%.2f (%.2f-%.2f)", pooled_or, pooled_lower, pooled_upper)
  }
  
  list(
    variable = var,
    female_fmt = sprintf("%.1f%%", f_prop),
    male_fmt = sprintf("%.1f%%", m_prop),
    compare_fmt = or_ci_string,
    missing_female = sprintf("%.1f%%", f_missing),
    missing_male = sprintf("%.1f%%", m_missing)
  )
}
