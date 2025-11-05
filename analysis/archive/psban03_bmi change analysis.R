rm(list=ls());gc();source(".Rprofile")

library(lme4)
library(broom.mixed)
library(mice)
library(mitools)
library(performance)
library(tibble)

# Load imputed datasets in wide format
spouse_wide <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cleaned/psban01_wide spouse dfs.RDS"))


for (i in 1:length(spouse_dfs)) {
  analytic_df_wide <- spouse_wide[[i]] %>% 
    dplyr::filter(fup != 0 & carrs != 2) %>% 
    # set outliers to NA
    # mutate(
    #   female_bmi_change    = case_when(female_bmi_change < -5.37 | female_bmi_change > 7.53 ~ NA_real_, TRUE ~ female_bmi_change),
    #   male_bmi_change    = case_when(male_bmi_change < -5.37 | male_bmi_change > 7.53 ~ NA_real_, TRUE ~ male_bmi_change),
    #   female_bmi_bschange  = case_when(female_bmi_bschange < -5.59 | female_bmi_bschange > 7.75 ~ NA_real_, TRUE ~ female_bmi_bschange),
    #   male_bmi_bschange  = case_when(male_bmi_bschange < -5.59 | male_bmi_bschange > 7.75 ~ NA_real_, TRUE ~ male_bmi_bschange)
    # ) %>% 
    mutate(
      female_annual_bmi_change = female_bmi_change / female_fup_duration,
      male_annual_bmi_change = male_bmi_change / male_fup_duration
    )
  
  
  # Unadjusted models
  
  model_wife_unadj <- lmer(female_annual_bmi_change ~ male_annual_bmi_change + (1 | hhid), data = analytic_df_wide)
  model_husband_unadj <- lmer(male_annual_bmi_change ~ female_annual_bmi_change + (1 | hhid), data = analytic_df_wide)
  
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
                         + male_smk_overall + male_alc_overall
                         # Comorbidity
                         + male_diabetes + male_famhx_dm
                         + (1 | hhid), data = analytic_df_wide)

}







