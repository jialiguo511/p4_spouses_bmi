# Function to calculate continuous variable summaries with correlation pooling

calculate_continuous_var <- function(datasets, var) {
  #' Calculate Continuous Variable Summary with Correlation Pooling
  #'
  #' @param datasets List of imputed baseline datasets
  #' @param var Character string specifying the variable name
  #'
  #' @return A list with pooled statistics and formatted strings for reporting
  #'
  #' @details
  #' For each imputed dataset:
  #' - Calculates mean and SD for females and males
  #' - Calculates Pearson correlation of paired couples
  #' - Calculates missing data percentage
  #'
  #' Results are pooled using:
  #' - Arithmetic mean for mean, SD, and missing data
  #' - Fisher z-transform for correlation pooling
  
  # Calculate across imputations
  results <- lapply(datasets, function(df) {
    f_vals <- df[[var]][df$sex == "female"]
    m_vals <- df[[var]][df$sex == "male"]
    
    # Get paired data by household ID
    df_wide <- df %>%
      select(hhid, sex, !!sym(var)) %>%
      pivot_wider(
        id_cols = hhid, 
        names_from = sex, 
        values_from = !!sym(var),
        names_prefix = "sex_"
      ) %>%
      dplyr::filter(!is.na(sex_female) & !is.na(sex_male))
    
    # Calculate correlation for paired data
    r <- cor(df_wide$sex_female, df_wide$sex_male, use = "pairwise.complete.obs")
    
    list(
      f_mean = mean(f_vals, na.rm = TRUE),
      f_sd = sd(f_vals, na.rm = TRUE),
      m_mean = mean(m_vals, na.rm = TRUE),
      m_sd = sd(m_vals, na.rm = TRUE),
      corr = r,
      f_missing = mean(is.na(f_vals)) * 100,
      m_missing = mean(is.na(m_vals)) * 100
    )
  })
  
  # Pool results
  f_mean <- mean(sapply(results, function(x) x$f_mean))
  f_sd <- mean(sapply(results, function(x) x$f_sd))
  m_mean <- mean(sapply(results, function(x) x$m_mean))
  m_sd <- mean(sapply(results, function(x) x$m_sd))
  
  # Fisher z-transform for correlation pooling
  z_corrs <- atanh(sapply(results, function(x) x$corr))
  pooled_corr <- tanh(mean(z_corrs, na.rm = TRUE))
  
  # Missing data
  f_missing <- mean(sapply(results, function(x) x$f_missing))
  m_missing <- mean(sapply(results, function(x) x$m_missing))
  
  list(
    variable = var,
    female_fmt = sprintf("%.1f (%.1f)", f_mean, f_sd),
    male_fmt = sprintf("%.1f (%.1f)", m_mean, m_sd),
    compare_fmt = sprintf("%.2f", pooled_corr),
    missing_female = sprintf("%.1f%%", f_missing),
    missing_male = sprintf("%.1f%%", m_missing)
  )
}
