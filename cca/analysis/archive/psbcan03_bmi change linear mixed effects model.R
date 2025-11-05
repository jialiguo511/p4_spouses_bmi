rm(list=ls());gc();source(".Rprofile")

library(lme4)
library(broom.mixed)

analytic_df_wide <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre02a_wide spouse bmi complete cases.RDS")) %>% 
  dplyr::filter(fup != 0) %>% 
  mutate(
    female_annual_bmi_change = female_bmi_change / female_fup_duration,
    male_annual_bmi_change = male_bmi_change / male_fup_duration
  )
  


# Unadjusted models

model_wife_unadj <- lmer(female_annual_bmi_change ~ male_annual_bmi_change + (1 | hhid), data = analytic_df_wide)
model_husband_unadj <- lmer(male_annual_bmi_change ~ female_annual_bmi_change + (1 | hhid), data = analytic_df_wide)

# model_wife_unadj <- lmer(female_bmi ~ male_bmi_lag + (1 | hhid), data = analytic_df_wide)

# Model 1: adjusted for age and baseline BMI

model1_wife <- lmer(female_annual_bmi_change ~ male_annual_bmi_change + female_age + female_bmi_baseline + (1 | hhid), data = analytic_df_wide)
model1_husband <- lmer(male_annual_bmi_change ~ female_annual_bmi_change + male_age + male_bmi_baseline + (1 | hhid), data = analytic_df_wide)

# Model 2

model2_wife <- lmer(female_annual_bmi_change ~ male_annual_bmi_change + female_age + female_bmi_baseline + female_fup_duration 
                    # Demographic
                    + female_edu_category + female_employ_category + female_hhincome + female_site + carrs
                    # Health risk
                    # + female_smk_curr + female_alc_curr
                    # Comorbidity
                    + female_diabetes + female_famhx_dm
                    + (1 | hhid), data = analytic_df_wide)
model2_husband <- lmer(male_annual_bmi_change ~ female_annual_bmi_change + male_age + male_bmi_baseline + male_fup_duration 
                       # Demographic
                      + male_edu_category + male_employ_category + male_hhincome + male_site + carrs
                      # Health risk
                      + male_smk_curr + male_alc_curr
                      # Comorbidity
                      + male_diabetes + male_famhx_dm
                      + (1 | hhid), data = analytic_df_wide)


library(performance)
library(tibble)

source("functions/extract_coefficient.R")

wife_unadj    <- extract_coefficient(model_wife_unadj,    "male_annual_bmi_change")
husband_unadj <- extract_coefficient(model_husband_unadj, "female_annual_bmi_change")
wife_mod1      <- extract_coefficient(model1_wife,      "male_annual_bmi_change")
husband_mod1   <- extract_coefficient(model1_husband,   "female_annual_bmi_change")
wife_mod2      <- extract_coefficient(model2_wife,      "male_annual_bmi_change")
husband_mod2   <- extract_coefficient(model2_husband,   "female_annual_bmi_change")

format_result <- function(x) {
  sprintf(
    "%.3f (95%% CI: %.3f, %.3f), p = %.3f [R²m = %.3f, R²c = %.3f]",
    x$estimate, x$conf.low, x$conf.high, x$p.value,
    x$marginal_r2, x$conditional_r2
  )
}

# Build summary table
summary_tbl <- tibble(
  Model = c("Unadjusted", "Model 1 (age + baseline BMI)", "Model 2 (all covariates)"),
  `Change in BMI in Wife (Outcome)` = c(format_result(wife_unadj), format_result(wife_mod1), format_result(wife_mod2)),
  `Change in BMI in Husband (Outcome)` = c(format_result(husband_unadj), format_result(husband_mod1), format_result(husband_mod2))
)

