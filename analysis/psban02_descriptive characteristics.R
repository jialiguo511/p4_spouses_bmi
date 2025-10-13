rm(list=ls());gc();source(".Rprofile")

library(tableone)
library(mice)
library(purrr)

spouse_dfs <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cleaned/psban01_long spouse dfs.RDS"))

carrs_recoded <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbpre04_carrs recoded data.RDS"))


# Function to process a single imputed dataset
process_baseline_data <- function(df) {
  # Filter to only baseline data
  baseline_data <- df %>%
    dplyr::filter(fup == 0) %>%
    arrange(hhid, pid, carrs, fup) %>% 
    left_join(carrs_recoded %>% 
                select(pid, hhid, employ, occ),
              by = c("pid", "hhid", "employ"))
  
  # Apply variable transformations as specified
  baseline_data <- baseline_data %>%
    # Nielsen et al. 2023 (based on the mean of the second two of three blood pressure measurements)
    mutate(
      sbp = rowMeans(select(., sbp2, sbp3), na.rm = TRUE),
      dbp = rowMeans(select(., dbp2, dbp3), na.rm = TRUE)
    ) %>%
    # define disease indicators
    mutate(
      diabetes = case_when(
        fpg >= 126 | hba1c >= 6.5 | dm == 1 ~ 1,
        TRUE ~ 0
      ),
      overweight = case_when(
        is.na(bmi) ~ NA_real_,
        bmi >= 25 ~ 1,
        TRUE ~ 0
      ),
      hypertension = case_when(
        sbp > 140 | dbp > 90 | htn == 1 ~ 1,
        TRUE ~ 0
      ),
      high_tg = case_when(
        tg > 150 ~ 1,
        TRUE ~ 0
      )
    ) %>% 
    mutate(morbidity_number = rowSums(across(c(hypertension, diabetes, chd, cva, ckd), ~ .x == 1), na.rm = TRUE)) %>% 
    mutate(
      morbidity_category = case_when(
        morbidity_number == 0 ~ "None",
        morbidity_number == 1 ~ "Single morbidity",
        TRUE                  ~ "Multimorbidity"
      ),
      edu_category = case_when(
        educstat %in% c(1, 2)    ~ "College and above",
        educstat %in% c(3, 4)    ~ "High school to secondary",
        educstat %in% c(5, 6, 7) ~ "Up to primary schooling",
        TRUE                     ~ "Others"
      ),
      # Fixed employment category definition
      employ_category = case_when(
        employ %in% c(2, 3)                        ~ "Not in the labor force, student/housewives",
        employ == 4                                ~ "Not in the labor force, retired",
        employ == 5                                ~ "Unemployed",
        employ == 1 & occ %in% c(3, 4, 5)          ~ "Employed in a manual profession",
        employ == 1 & occ %in% c(1, 2)             ~ "Employed in a non-manual professional",
        TRUE                                       ~ NA_character_
      ),
      hhincome = case_when(
        hhincome %in% c(8,9) ~ NA_real_,
        TRUE ~ hhincome
      ),
      bmi_category = case_when(
        bmi >= 15 & bmi < 25 ~ "Underweight or normal weight",
        bmi >= 25 & bmi < 30 ~ "Overweight",
        bmi >= 30            ~ "Obese",
        TRUE                 ~ NA_character_
      )
    )
  
  return(baseline_data)
}

# Process all imputed datasets (extract baseline + add derived variables)
baseline_processed <- lapply(spouse_dfs, process_baseline_data)

# Define variable lists for Table 1
continuous_vars <- c("age", "bmi", "sbp", "dbp", "waist_cm", "fpg")  
proportion_vars <- c("smk_overall", "alc_overall", "famhx_htn", "famhx_dm", "famhx_cvd",
                     "chd", "cva", "ckd", 
                     "diabetes", "overweight", "hypertension", "high_tg")
grouped_vars <- c("edu_category", "employ_category", "bmi_category", "morbidity_category") 
all_vars <- c(continuous_vars, proportion_vars, grouped_vars)

# Create table with correlations and ORs
create_table1 <- function(datasets) {
  # Initialize results table
  table1 <- data.frame(
    variable = character(),
    level = character(),
    female = character(),
    male = character(),
    compare = character(),
    missing_female = character(),
    missing_male = character(),
    stringsAsFactors = FALSE
  )
  
  # Sample size row
  n_overall <- mean(sapply(datasets, nrow))
  n_female <- mean(sapply(datasets, function(df) sum(df$sex == "female")))
  n_male <- mean(sapply(datasets, function(df) sum(df$sex == "male")))
  
  table1 <- rbind(table1, data.frame(
    variable = "Sample size",
    level = NA_character_,
    female = sprintf("n = %d", round(n_female)),
    male = sprintf("n = %d", round(n_male)),
    compare = NA_character_,
    missing_female = "0.0%",
    missing_male = "0.0%"
  ))
  
  # 1. Process continuous variables with correlation
  for(var in continuous_vars) {
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
      
      # Calculate correlation
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
    
    # Pool results (using arithmetic mean for simplicity)
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
    
    table1 <- rbind(table1, data.frame(
      variable = var,
      level = NA_character_,
      female = sprintf("%.1f (%.1f)", f_mean, f_sd),
      male = sprintf("%.1f (%.1f)", m_mean, m_sd),
      compare = sprintf("%.2f", pooled_corr),
      missing_female = sprintf("%.1f%%", f_missing),
      missing_male = sprintf("%.1f%%", m_missing)
    ))
  }
  
  # 2. Process binary variables with odds ratio
  for(var in proportion_vars) {
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
      
      # Calculate OR
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
        
        # Calculate OR
        if(nrow(tbl) == 2 && ncol(tbl) == 2) {
          # Use fisher.test for more accurate CI
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
    
    # Pool results
    f_prop <- mean(sapply(results, function(x) x$f_prop))
    m_prop <- mean(sapply(results, function(x) x$m_prop))
    f_n <- mean(sapply(results, function(x) x$f_n))
    m_n <- mean(sapply(results, function(x) x$m_n))
    
    # Pool OR and CIs using log-transform method (Rubin's rules)
    valid_indices <- which(sapply(results, function(x) 
      !is.na(x$or) && is.finite(x$or) && x$or > 0))
    
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
      
      # Calculate variances for CIs
      var_within <- mean((log_upper - log_lower)^2 / (2*1.96)^2)
      var_between <- var(log_ors)
      m <- length(valid_indices)
      total_var <- var_within + var_between * (1 + 1/m)
      
      # Back-transform for final results
      pooled_or <- exp(pooled_log_or)
      pooled_lower <- exp(pooled_log_or - 1.96 * sqrt(total_var))
      pooled_upper <- exp(pooled_log_or + 1.96 * sqrt(total_var))
      
      or_ci_string <- sprintf("%.2f (%.2f-%.2f)", pooled_or, pooled_lower, pooled_upper)
    } else {
      or_ci_string <- "NA"
    }
    
    # Missing data
    f_missing <- mean(sapply(results, function(x) x$f_missing))
    m_missing <- mean(sapply(results, function(x) x$m_missing))
    
    table1 <- rbind(table1, data.frame(
      variable = var,
      level = NA_character_,
      female = sprintf("%.1f%%", f_prop),
      male = sprintf("%.1f%%", m_prop),
      compare = or_ci_string,
      missing_female = sprintf("%.1f%%", f_missing),
      missing_male = sprintf("%.1f%%", m_missing)
    ))
  }
  
  # 3. Process categorical variables with chi-square
  for(var in grouped_vars) {
    # Get all levels across imputations
    all_levels <- unique(unlist(lapply(datasets, function(df) unique(df[[var]]))))
    all_levels <- all_levels[!is.na(all_levels)]
    
    # Order levels appropriately if needed
    if(var == "edu_category") {
      level_order <- c("College and above", "High school to secondary", "Up to primary schooling")
      all_levels <- all_levels[order(match(all_levels, level_order))]
    } else if(var == "bmi_category") {
      level_order <- c("Underweight or normal weight", "Overweight", "Obese")
      all_levels <- all_levels[order(match(all_levels, level_order))]
    } else if(var == "morbidity_category") {
      level_order <- c("None", "Single morbidity", "Multimorbidity")
      all_levels <- all_levels[order(match(all_levels, level_order))]
    }
    
    # Missing data percentages
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
    p_value_fmt <- if(is.na(pooled_p)) "NA" else 
      if(pooled_p < 0.001) "< 0.001" else 
        sprintf("%.3f", pooled_p)
    
    # For each level, calculate percentages
    for(level in all_levels) {
      level_results <- lapply(datasets, function(df) {
        f_prop <- mean(df[[var]][df$sex == "female"] == level, na.rm = TRUE) * 100
        m_prop <- mean(df[[var]][df$sex == "male"] == level, na.rm = TRUE) * 100
        list(f_prop = f_prop, m_prop = m_prop)
      })
      
      # Pool proportions
      f_prop <- mean(sapply(level_results, function(x) x$f_prop))
      m_prop <- mean(sapply(level_results, function(x) x$m_prop))
      
      # Add row for this level
      table1 <- rbind(table1, data.frame(
        variable = if(level == all_levels[1]) var else "",
        level = level,
        female = sprintf("%.1f%%", f_prop),
        male = sprintf("%.1f%%", m_prop),
        compare = if(level == all_levels[1]) p_value_fmt else "",
        missing_female = if(level == all_levels[1]) sprintf("%.1f%%", f_missing) else "",
        missing_male = if(level == all_levels[1]) sprintf("%.1f%%", m_missing) else ""
      ))
    }
  }
  
  return(table1)
}

final_table1 <- create_table1(baseline_processed)

write.csv(final_table1, "analysis/psban02_descriptive characteristics.csv", row.names = FALSE)

