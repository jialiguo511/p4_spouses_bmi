rm(list=ls());gc();source(".Rprofile")

library(lme4)

# Load helper functions
source("functions/extract_coefficient.R")
source("functions/pool_mi_mixed_results.R")

spouse_wide <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cleaned/psban01_wide spouse dfs.RDS"))

# ============================================================================
# Fit models across all imputed datasets
# ============================================================================

# Define model specifications
model_names <- c("unadj", "model1", "model2")
outcomes <- c("wife", "husband")

# Model formulas by outcome and model type
model_formulas <- list(
  wife = list(
    unadj = female_bmi ~ male_bmi_lag + (1 | hhid),
    model1 = female_bmi ~ male_bmi_lag + female_age + female_bmi_baseline + (1 | hhid),
    model2 = female_bmi ~ male_bmi_lag + female_age + female_bmi_baseline + female_fup_duration 
           + female_edu_category + female_employ_category + female_hhincome + female_site + carrs
           + female_diabetes + female_famhx_dm + (1 | hhid)
  ),
  husband = list(
    unadj = male_bmi ~ female_bmi_lag + (1 | hhid),
    model1 = male_bmi ~ female_bmi_lag + male_age + male_bmi_baseline + (1 | hhid),
    model2 = male_bmi ~ female_bmi_lag + male_age + male_bmi_baseline + male_fup_duration 
           + male_edu_category + male_employ_category + male_hhincome + male_site + carrs
           + male_smk_overall + male_alc_overall
           + male_diabetes + male_famhx_dm + (1 | hhid)
  )
)

exposures <- list(wife = "male_bmi_lag", husband = "female_bmi_lag")

# Store models by outcome and type
models_by_outcome <- list()

for (outcome in outcomes) {
  models_by_outcome[[outcome]] <- list()
  for (model_name in model_names) {
    models_by_outcome[[outcome]][[model_name]] <- list()
  }
}

# Fit models across all imputations
for (i in 1:length(spouse_wide)) {
  analytic_df_wide <- spouse_wide[[i]] %>% 
    dplyr::filter(fup != 0)
  
  for (outcome in outcomes) {
    for (model_name in model_names) {
      formula <- model_formulas[[outcome]][[model_name]]
      models_by_outcome[[outcome]][[model_name]][[i]] <- lmer(formula, data = analytic_df_wide)
    }
  }
}

# ============================================================================
# Pool results using Rubin's rules
# ============================================================================

pooled_results <- list()

for (outcome in outcomes) {
  for (model_name in model_names) {
    model_list <- models_by_outcome[[outcome]][[model_name]]
    exposure_var <- exposures[[outcome]]
    
    # Pool results for this outcome and model combination
    pooled_res <- pool_mi_mixed_results(model_list, exposure_var)
    
    # Add outcome and model information
    pooled_res <- pooled_res %>%
      mutate(outcome = outcome, model = model_name, .before = 1)
    
    result_key <- paste0(outcome, "_", model_name)
    pooled_results[[result_key]] <- pooled_res
  }
}

# Combine all results into one dataframe
pooled_results_df <- bind_rows(pooled_results)

pooled_results_formatted <- pooled_results_df %>%
  mutate(
    est_CI = sprintf("%.3f (%.3f, %.3f)", estimate, conf.low, conf.high),
    marginal_r2 = sprintf("%.3f", marginal_r2)
  ) %>%
  select(outcome, model, exposure, est_CI, marginal_r2)


write.csv(pooled_results_formatted, "analysis/psban03_bmi mixed model results.csv", row.names = FALSE)


