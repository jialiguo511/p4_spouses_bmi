rm(list=ls());gc();source(".Rprofile")

library(dplyr)
library(tidyr)
library(tableone)
library(mice)
library(purrr)

# Load the imputed datasets
spouse_dfs <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cleaned/psban01_imputed spouse dfs.RDS"))

# Function to process a single imputed dataset
process_baseline_data <- function(df) {
  # Filter to only baseline data
  baseline_data <- df %>%
    dplyr::filter(fup == 0) %>%
    arrange(hhid, pid, carrs, fup)
  
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
        TRUE                     ~ NA_character_
      ),
      # Fixed employment category definition
      employ_category = case_when(
        employ %in% c(2, 3)                       ~ "Not in the labor force, student/housewives",
        employ == 4                               ~ "Not in the labor force, retired",
        employ == 5                               ~ "Unemployed",
        employ == 1          ~ "Employed in a manual profession",
        employ == 1             ~ "Employed in a non-manual professional",
        TRUE                                      ~ NA_character_
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

# Use direct calculation approach to avoid tableone issues
create_table1 <- function(datasets) {
  # Initialize result matrix
  result_matrix <- matrix(NA, nrow = length(all_vars), ncol = 3)
  rownames(result_matrix) <- all_vars
  colnames(result_matrix) <- c("Overall", "Female", "Male")
  
  # Number of imputations
  m <- length(datasets)
  
  # Process continuous variables
  for(var in continuous_vars) {
    # Calculate for each imputation
    overall_stats <- t(sapply(datasets, function(df) {
      c(mean = mean(df[[var]], na.rm = TRUE), 
        sd = sd(df[[var]], na.rm = TRUE))
    }))
    
    female_stats <- t(sapply(datasets, function(df) {
      subset_df <- df[df$sex == "female", ]
      c(mean = mean(subset_df[[var]], na.rm = TRUE), 
        sd = sd(subset_df[[var]], na.rm = TRUE))
    }))
    
    male_stats <- t(sapply(datasets, function(df) {
      subset_df <- df[df$sex == "male", ]
      c(mean = mean(subset_df[[var]], na.rm = TRUE), 
        sd = sd(subset_df[[var]], na.rm = TRUE))
    }))
    
    # Pool estimates using Rubin's rules
    overall_mean <- mean(overall_stats[, "mean"])
    overall_sd <- sqrt(mean(overall_stats[, "sd"]^2) + (1 + 1/m) * var(overall_stats[, "mean"]))
    
    female_mean <- mean(female_stats[, "mean"])
    female_sd <- sqrt(mean(female_stats[, "sd"]^2) + (1 + 1/m) * var(female_stats[, "mean"]))
    
    male_mean <- mean(male_stats[, "mean"])
    male_sd <- sqrt(mean(male_stats[, "sd"]^2) + (1 + 1/m) * var(male_stats[, "mean"]))
    
    # Format results
    result_matrix[var, "Overall"] <- sprintf("%.1f (%.1f)", overall_mean, overall_sd)
    result_matrix[var, "Female"] <- sprintf("%.1f (%.1f)", female_mean, female_sd)
    result_matrix[var, "Male"] <- sprintf("%.1f (%.1f)", male_mean, male_sd)
  }
  
  # Process binary variables
  for(var in proportion_vars) {
    # Calculate proportions for each imputation
    overall_props <- sapply(datasets, function(df) {
      mean(df[[var]] == 1, na.rm = TRUE) * 100
    })
    
    female_props <- sapply(datasets, function(df) {
      subset_df <- df[df$sex == "female", ]
      mean(subset_df[[var]] == 1, na.rm = TRUE) * 100
    })
    
    male_props <- sapply(datasets, function(df) {
      subset_df <- df[df$sex == "male", ]
      mean(subset_df[[var]] == 1, na.rm = TRUE) * 100
    })
    
    # Pool proportions across imputations
    overall_prop <- mean(overall_props)
    female_prop <- mean(female_props)
    male_prop <- mean(male_props)
    
    # Format results
    result_matrix[var, "Overall"] <- sprintf("%.1f%%", overall_prop)
    result_matrix[var, "Female"] <- sprintf("%.1f%%", female_prop)
    result_matrix[var, "Male"] <- sprintf("%.1f%%", male_prop)
  }
  
  # Process categorical variables
  for(var in grouped_vars) {
    # Get all possible levels
    all_levels <- unique(unlist(lapply(datasets, function(df) {
      unique(df[[var]])
    })))
    all_levels <- all_levels[!is.na(all_levels)]
    
    # For each level, calculate proportion across imputations
    level_results <- list()
    for(level in all_levels) {
      overall_props <- sapply(datasets, function(df) {
        mean(df[[var]] == level, na.rm = TRUE) * 100
      })
      
      female_props <- sapply(datasets, function(df) {
        subset_df <- df[df$sex == "female", ]
        mean(subset_df[[var]] == level, na.rm = TRUE) * 100
      })
      
      male_props <- sapply(datasets, function(df) {
        subset_df <- df[df$sex == "male", ]
        mean(subset_df[[var]] == level, na.rm = TRUE) * 100
      })
      
      # Pool proportions
      overall_prop <- mean(overall_props)
      female_prop <- mean(female_props)
      male_prop <- mean(male_props)
      
      level_results[[level]] <- c(
        sprintf("%.1f%%", overall_prop),
        sprintf("%.1f%%", female_prop),
        sprintf("%.1f%%", male_prop)
      )
    }
    
    # Combine all levels
    combined_result <- paste(
      paste(names(level_results), 
            sapply(level_results, function(x) x[1]), sep=": "),
      collapse="; "
    )
    combined_female <- paste(
      paste(names(level_results), 
            sapply(level_results, function(x) x[2]), sep=": "),
      collapse="; "
    )
    combined_male <- paste(
      paste(names(level_results), 
            sapply(level_results, function(x) x[3]), sep=": "),
      collapse="; "
    )
    
    result_matrix[var, "Overall"] <- combined_result
    result_matrix[var, "Female"] <- combined_female
    result_matrix[var, "Male"] <- combined_male
  }
  
  # Convert to data frame
  result_df <- as.data.frame(result_matrix)
  
  # Add sample size
  n_overall <- mean(sapply(datasets, nrow))
  n_female <- mean(sapply(datasets, function(df) sum(df$sex == "female")))
  n_male <- mean(sapply(datasets, function(df) sum(df$sex == "male")))
  
  result_df <- rbind(
    c(sprintf("n = %.0f", n_overall), 
      sprintf("n = %.0f", n_female), 
      sprintf("n = %.0f", n_male)),
    result_df
  )
  rownames(result_df)[1] <- "Sample size"
  
  return(result_df)
}

final_table1 <- create_table1(baseline_processed)

write.csv(final_table1, "analysis/psban02_descriptive characteristics.csv")



