rm(list=ls());gc();source(".Rprofile")

library(lme4)
library(performance)
library(dplyr)
library(tibble)

source("functions/extract_coefficient.R")

# ============================================================================
# Load and prepare data
# ============================================================================

analytic_df_long <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cca/psbcpre03_long spouse bmi complete cases.RDS")) %>%
  group_by(pid) %>%
  arrange(pid, carrs, fup) %>%
  mutate(
    bmi_lag = dplyr::lag(bmi),
    bmi_baseline = first(bmi[fup == 0])  # baseline BMI from first visit
  ) %>% 
  ungroup() %>% 
  dplyr::filter(fup != 0)

# Create spouse lag variables
female_data <- analytic_df_long %>%
  dplyr::filter(sex == "female") %>%
  # Join with male spouse's lagged BMI
  left_join(
    analytic_df_long %>%
      dplyr::filter(sex == "male") %>%
      select(hhid, carrs, fup, male_bmi_lag = bmi_lag),
    by = c("hhid", "carrs", "fup")
  ) 

male_data <- analytic_df_long %>%
  dplyr::filter(sex == "male") %>%
  # Join with female spouse's lagged BMI
  left_join(
    analytic_df_long %>%
      dplyr::filter(sex == "female") %>%
      select(hhid, carrs, fup, female_bmi_lag = bmi_lag),
    by = c("hhid", "carrs", "fup")
  ) 


# ============================================================================
# Fit FEMALE models
# ============================================================================

female_unadj <- lmer(bmi ~ male_bmi_lag + (1 | hhid), data = female_data)
female_model1 <- lmer(bmi ~ male_bmi_lag + age + bmi_baseline + (1 | hhid), data = female_data)
female_model2 <- lmer(bmi ~ male_bmi_lag + age + bmi_baseline + fup_duration 
                      + edu_category + employ_category + hhincome + site + carrs
                      + diabetes + famhx_dm + (1 | hhid), data = female_data)

# ============================================================================
# Fit MALE models  
# ============================================================================

male_unadj <- lmer(bmi ~ female_bmi_lag + (1 | hhid), data = male_data)
male_model1 <- lmer(bmi ~ female_bmi_lag + age + bmi_baseline + (1 | hhid), data = male_data)
male_model2 <- lmer(bmi ~ female_bmi_lag + age + bmi_baseline + fup_duration 
                    + edu_category + employ_category + hhincome + site + carrs
                    + smk_overall + alc_overall
                    + diabetes + famhx_dm + (1 | hhid), data = male_data)

# ============================================================================
# Extract results
# ============================================================================

# Extract coefficients for each model
female_unadj_coef <- extract_coefficient(female_unadj, "male_bmi_lag") %>% mutate(outcome = "female", model = "unadj")
female_model1_coef <- extract_coefficient(female_model1, "male_bmi_lag") %>% mutate(outcome = "female", model = "model1")
female_model2_coef <- extract_coefficient(female_model2, "male_bmi_lag") %>% mutate(outcome = "female", model = "model2")

male_unadj_coef <- extract_coefficient(male_unadj, "female_bmi_lag") %>% mutate(outcome = "male", model = "unadj")
male_model1_coef <- extract_coefficient(male_model1, "female_bmi_lag") %>% mutate(outcome = "male", model = "model1")
male_model2_coef <- extract_coefficient(male_model2, "female_bmi_lag") %>% mutate(outcome = "male", model = "model2")

# Combine results
results_df <- bind_rows(
  female_unadj_coef, female_model1_coef, female_model2_coef,
  male_unadj_coef, male_model1_coef, male_model2_coef
) %>%
  select(outcome, model, estimate, conf.low, conf.high, p.value, marginal_r2, conditional_r2)

# ============================================================================
# Format and save results
# ============================================================================

results_formatted <- results_df %>%
  mutate(
    est_CI = sprintf("%.3f (%.3f, %.3f)", estimate, conf.low, conf.high),
    p_value = sprintf("%.3f", p.value),
    marginal_r2 = sprintf("%.3f", marginal_r2),
    conditional_r2 = sprintf("%.3f", conditional_r2)
  ) %>%
  select(outcome, model, est_CI, p_value, marginal_r2, conditional_r2)

print(results_formatted)

write.csv(results_formatted, "cca/analysis/psbcan02_bmi mixed model results.csv", row.names = FALSE)
