# Function to calculate categorical variable summaries with chi-square test

calculate_categorical_var <- function(datasets, var, var_levels = NULL) {
  #' Calculate Categorical Variable Summary with Chi-Square Test
  #'
  #' @param datasets List of imputed baseline datasets
  #' @param var Character string specifying the variable name
  #' @param var_levels Optional character vector specifying level order
  #'
  #' @return A list with pooled statistics and formatted strings for reporting
  #'
  #' @details
  #' For each imputed dataset:
  #' - Calculates proportions for each level and sex
  #' - Performs chi-square test for association
  #' - Calculates missing data percentage
  #'
  #' Results are pooled using:
  #' - Arithmetic mean for proportions and chi-square p-values
  #' - Arithmetic mean for missing data
  
  # Get all levels across imputations
  all_levels <- unique(unlist(lapply(datasets, function(df) unique(df[[var]]))))
  all_levels <- all_levels[!is.na(all_levels)]
  
  # Apply level ordering if specified
  if(!is.null(var_levels)) {
    # Keep only levels that exist
    var_levels <- var_levels[var_levels %in% all_levels]
    if(length(var_levels) > 0) {
      all_levels <- var_levels
    }
  }
  
  # Missing data percentages across imputations
  f_missing <- mean(sapply(datasets, function(df) mean(is.na(df[[var]][df$sex == "female"])) * 100))
  m_missing <- mean(sapply(datasets, function(df) mean(is.na(df[[var]][df$sex == "male"])) * 100))
  
  # Calculate chi-square p-value
  p_values <- sapply(datasets, function(df) {
    tab <- table(df[[var]], df$sex)
    tryCatch({
      chisq.test(tab)$p.value
    }, error = function(e) NA_real_)
  })
  pooled_p <- mean(p_values, na.rm = TRUE)
  
  # Format p-value
  if(is.na(pooled_p)) {
    p_value_fmt <- "NA"
  } else if(pooled_p < 0.001) {
    p_value_fmt <- "< 0.001"
  } else {
    p_value_fmt <- sprintf("%.3f", pooled_p)
  }
  
  # For each level, calculate proportions across imputations
  level_results_list <- list()
  
  for(level in all_levels) {
    level_results <- lapply(datasets, function(df) {
      f_prop <- mean(df[[var]][df$sex == "female"] == level, na.rm = TRUE) * 100
      m_prop <- mean(df[[var]][df$sex == "male"] == level, na.rm = TRUE) * 100
      list(f_prop = f_prop, m_prop = m_prop)
    })
    
    # Pool proportions for this level
    f_prop <- mean(sapply(level_results, function(x) x$f_prop))
    m_prop <- mean(sapply(level_results, function(x) x$m_prop))
    
    level_results_list[[level]] <- list(
      level = level,
      female_fmt = sprintf("%.1f%%", f_prop),
      male_fmt = sprintf("%.1f%%", m_prop)
    )
  }
  
  list(
    variable = var,
    all_levels = all_levels,
    p_value_fmt = p_value_fmt,
    missing_female = sprintf("%.1f%%", f_missing),
    missing_male = sprintf("%.1f%%", m_missing),
    level_results = level_results_list
  )
}
