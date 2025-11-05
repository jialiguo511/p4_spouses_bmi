rm(list=ls());gc();source(".Rprofile")

library(tableone)
library(mice)
library(purrr)

source("functions/calculate_continuous_var.R")
source("functions/calculate_binary_var.R")
source("functions/calculate_categorical_var.R")

spouse_dfs <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cleaned/psban01_long spouse dfs.RDS"))


process_baseline_data <- function(df) {
  
  baseline_data <- df %>%
    dplyr::filter(fup == 0) %>%
    arrange(hhid, pid, carrs, fup) %>%
    # Blood pressure: mean of 2nd and 3rd measurements
    mutate(
      sbp = rowMeans(select(., sbp2, sbp3), na.rm = TRUE),
      dbp = rowMeans(select(., dbp2, dbp3), na.rm = TRUE)
    ) %>%
    # Define disease indicators
    mutate(
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
    # Morbidity indicators
    mutate(
      morbidity_number = rowSums(
        across(c(hypertension, diabetes, chd, cva, ckd), ~ .x == 1),
        na.rm = TRUE
      )
    ) %>%
    # Categorical variables
    mutate(
      morbidity_category = case_when(
        morbidity_number == 0 ~ "None",
        morbidity_number == 1 ~ "Single morbidity",
        TRUE ~ "Multimorbidity"
      ),
      bmi_category = case_when(
        bmi >= 15 & bmi < 25 ~ "Underweight or normal weight",
        bmi >= 25 & bmi < 30 ~ "Overweight",
        bmi >= 30 ~ "Obese",
        TRUE ~ NA_character_
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

# ============================================================================
# Build Table 1: Descriptive Characteristics
# ============================================================================

create_table1 <- function(datasets) {
  #' Create Descriptive Characteristics Table

  #' Table includes:
  #' - Continuous variables: Mean (SD), correlation between spouses
  #' - Binary variables: Prevalence (%), odds ratio between spouses
  #' - Categorical variables: Proportions (%), chi-square test
  #' - Missing data percentages for each variable
  
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
  
  # 1. Process continuous variables
  for(var in continuous_vars) {
    result <- calculate_continuous_var(datasets, var)
    table1 <- rbind(table1, data.frame(
      variable = result$variable,
      level = NA_character_,
      female = result$female_fmt,
      male = result$male_fmt,
      compare = result$compare_fmt,
      missing_female = result$missing_female,
      missing_male = result$missing_male
    ))
  }
  
  # 2. Process binary variables
  for(var in proportion_vars) {
    result <- calculate_binary_var(datasets, var)
    table1 <- rbind(table1, data.frame(
      variable = result$variable,
      level = NA_character_,
      female = result$female_fmt,
      male = result$male_fmt,
      compare = result$compare_fmt,
      missing_female = result$missing_female,
      missing_male = result$missing_male
    ))
  }
  
  # 3. Process categorical variables
  # Define level orderings for specific variables
  level_orderings <- list(
    edu_category = c("College and above", "High school to secondary", "Up to primary schooling"),
    bmi_category = c("Underweight or normal weight", "Overweight", "Obese"),
    morbidity_category = c("None", "Single morbidity", "Multimorbidity")
  )
  
  for(var in grouped_vars) {
    var_levels <- level_orderings[[var]]
    result <- calculate_categorical_var(datasets, var, var_levels = var_levels)
    
    # Add rows for each level
    for(i in seq_along(result$all_levels)) {
      level <- result$all_levels[i]
      level_res <- result$level_results[[level]]
      is_first_level <- (i == 1)
      
      table1 <- rbind(table1, data.frame(
        variable = if(is_first_level) result$variable else "",
        level = level_res$level,
        female = level_res$female_fmt,
        male = level_res$male_fmt,
        compare = if(is_first_level) result$p_value_fmt else "",
        missing_female = if(is_first_level) result$missing_female else "",
        missing_male = if(is_first_level) result$missing_male else ""
      ))
    }
  }
  
  return(table1)
}

# Create and save final table
final_table1 <- create_table1(baseline_processed)

write.csv(final_table1, "analysis/psban02_descriptive characteristics.csv", row.names = FALSE)

