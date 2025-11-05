rm(list=ls());gc();source(".Rprofile")

# ============================================================================
# Incident Obesity Analysis - Multiple Imputation with Rubin's Rules Pooling
# ============================================================================
# This script:
# 1. Loads 30 multiply-imputed long-format datasets
# 2. Prepares incident obesity data (spouse obesity change categories)
# 3. Fits complementary log-log regression models (unadjusted, adjusted)
# 4. Pools results across imputations using Rubin's rules
# ============================================================================

library(lme4)
library(broom)

spouse_long_list <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cleaned/psban01_long spouse dfs.RDS"))

# ============================================================================
# Prepare data for incident obesity analysis across all imputations
# ============================================================================

# Function to prepare data from long format
prepare_incident_obesity_data <- function(spouse_long) {
  
  wife_df <- spouse_long %>%
    dplyr::filter(sex == "female") %>%
    select(hhid, carrs, fup, pid, bmi) %>%
    arrange(pid, carrs, fup) %>%
    group_by(pid) %>%
    mutate(
      obese = case_when(bmi >= 30 ~ 1, TRUE ~ 0),
      obese_lag = dplyr::lag(obese)
    ) %>%
    ungroup() %>%
    rename_with(~ paste0("wife_", .), .cols = c(pid, obese, obese_lag))
  
  husband_df <- spouse_long %>%
    dplyr::filter(sex == "male") %>%
    select(hhid, carrs, fup, pid, bmi) %>%
    arrange(pid, carrs, fup) %>%
    group_by(pid) %>%
    mutate(
      obese = case_when(bmi >= 30 ~ 1, TRUE ~ 0),
      obese_lag = dplyr::lag(obese)
    ) %>%
    ungroup() %>%
    rename_with(~ paste0("husband_", .), .cols = c(pid, obese, obese_lag))
  
  couple_df <- full_join(wife_df, husband_df, by = c("hhid", "carrs", "fup"))
  
  wife_rows <- couple_df %>%
    transmute(
      hhid, carrs, fup, pid = wife_pid, sex = "female",
      obese = wife_obese,
      obese_lag = wife_obese_lag,
      became_obese = case_when(
        obese_lag == 0 & obese == 1 ~ 1,
        obese_lag == 0 & obese == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      spouse_obese = husband_obese,
      spouse_obese_lag = husband_obese_lag
    )
  
  husband_rows <- couple_df %>%
    transmute(
      hhid, carrs, fup, pid = husband_pid, sex = "male",
      obese = husband_obese,
      obese_lag = husband_obese_lag,
      became_obese = case_when(
        obese_lag == 0 & obese == 1 ~ 1,
        obese_lag == 0 & obese == 0 ~ 0,
        TRUE ~ NA_real_
      ),
      spouse_obese = wife_obese,
      spouse_obese_lag = wife_obese_lag
    )
  
  analytic_obese <- bind_rows(wife_rows, husband_rows) %>%
    mutate(
      spouse_obesity_change = case_when(
        spouse_obese_lag == 0 & spouse_obese == 0 ~ "Remained non-obese",
        spouse_obese_lag == 0 & spouse_obese == 1 ~ "Became obese",
        spouse_obese_lag == 1 & spouse_obese == 1 ~ "Remained obese",
        spouse_obese_lag == 1 & spouse_obese == 0 ~ "Became non-obese",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Remained non-obese", "Became obese", "Remained obese", "Became non-obese"))
    ) %>%
    left_join(
      spouse_long %>% select(-c("sex", "bmi")),  # drop duplicate columns
      by = c("pid", "hhid", "carrs", "fup")
    )
  
  return(analytic_obese)
}

# ============================================================================
# Fit models across all imputed datasets
# ============================================================================

# Create indicators for spouse obesity change (reference = Remained non-obese)
create_exposure_indicators <- function(analytic_obese) {
  df <- analytic_obese %>%
    dplyr::filter(!is.na(became_obese), !is.na(spouse_obesity_change)) %>%
    mutate(
      spouse_became_obese = as.integer(spouse_obesity_change == "Became obese"),
      spouse_remained_obese = as.integer(spouse_obesity_change == "Remained obese"),
      spouse_became_nonobese = as.integer(spouse_obesity_change == "Became non-obese")
    )
  return(df)
}

# Define model specifications
model_names <- c("unadj", "model1", "model2")
outcomes <- c("wife", "husband")

# Model formulas by outcome and model type
model_formulas <- list(
  wife = list(
    unadj = became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese,
    model1 = became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
             + bmi_baseline + age,
    model2 = became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
             + bmi_baseline + age + fup_duration + carrs
             + edu_category + employ_category + hhincome + site
             + diabetes_baseline + famhx_dm
  ),
  husband = list(
    unadj = became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese,
    model1 = became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
             + bmi_baseline + age,
    model2 = became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
             + bmi_baseline + age + fup_duration + carrs
             + smk_overall + alc_overall  
             + edu_category + employ_category + hhincome + site
             + diabetes_baseline + famhx_dm
  )
)

terms_of_interest <- c("spouse_became_obese", "spouse_remained_obese", "spouse_became_nonobese")

# Store models by outcome and type
models_by_outcome <- list()

for (outcome in outcomes) {
  models_by_outcome[[outcome]] <- list()
  for (model_name in model_names) {
    models_by_outcome[[outcome]][[model_name]] <- list()
  }
}

# Fit models across all imputations
for (i in 1:length(spouse_long_list)) {
  # Prepare data for this imputation
  analytic_obese <- prepare_incident_obesity_data(spouse_long_list[[i]])
  analytic_df <- create_exposure_indicators(analytic_obese)
  
  wife_df <- analytic_df %>% dplyr::filter(sex == "female")
  husband_df <- analytic_df %>% dplyr::filter(sex == "male")
  
  for (model_name in model_names) {
    formula <- model_formulas[["wife"]][[model_name]]
    models_by_outcome[["wife"]][[model_name]][[i]] <- glm(
      formula, 
      data = wife_df, 
      family = binomial(link = "cloglog")
    )
    
    formula <- model_formulas[["husband"]][[model_name]]
    models_by_outcome[["husband"]][[model_name]][[i]] <- glm(
      formula, 
      data = husband_df, 
      family = binomial(link = "cloglog")
    )
  }
}

# ============================================================================
# Pool results using Rubin's rules
# ============================================================================

source("functions/pool_mi_glm_results.R")

pooled_results <- list()

for (outcome in outcomes) {
  for (model_name in model_names) {
    model_list <- models_by_outcome[[outcome]][[model_name]]
    
    # Pool results for this outcome and model combination
    pooled_res <- pool_mi_glm_results(model_list, terms_of_interest)
    
    # Add outcome and model information
    pooled_res <- pooled_res %>%
      mutate(
        outcome = outcome, 
        model = model_name,
        .before = 1
      ) %>%
      mutate(
        term = dplyr::recode(term,
          "spouse_became_obese" = "Became Obese",
          "spouse_remained_obese" = "Remained Obese",
          "spouse_became_nonobese" = "Became Non-Obese"
        )
      )
    
    result_key <- paste0(outcome, "_", model_name)
    pooled_results[[result_key]] <- pooled_res
  }
}

# Combine all results into one dataframe
pooled_results_df <- bind_rows(pooled_results)


pooled_results_formatted <- pooled_results_df %>%
  mutate(
    HR_CI = sprintf("%.2f (%.2f, %.2f)", estimate, conf.low, conf.high)
  ) %>%
  select(outcome, model, term, estimate, HR_CI, riv, fmi)


write.csv(pooled_results_formatted, "analysis/psban04_incident obesity results.csv", row.names = FALSE)



