rm(list=ls());gc();source(".Rprofile")

# unique hhid: 2,377
analytic_df_wide <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre02a_wide spouse bmi complete cases.RDS"))
analytic_df_long <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre02b_long spouse bmi complete cases.RDS"))


wife_rows <- analytic_df_wide %>%
  transmute(
    hhid, carrs, fup, pid = female_pid, sex = "female",
    bmi_change = female_bmi_change,
    spouse_bmi_change = male_bmi_change,
  )

husband_rows <- analytic_df_wide %>%
  transmute(
    hhid, carrs, fup, pid = male_pid, sex = "male",
    bmi_change = male_bmi_change,
    spouse_bmi_change = female_bmi_change,
  )

analytic_bmi <- bind_rows(wife_rows, husband_rows) %>%
  left_join(
    analytic_df_long %>% select(-c("sex","bmi_change")),  # drop duplicate column to avoid conflict
    by = c("pid", "hhid", "carrs", "fup")
  ) %>%
  mutate(age_category = case_when(
    age <= 29 ~ "18-29",
    age >= 30 & age <=49 ~ "30-49",
    age >= 50 & age <=69 ~ "50-69",
    TRUE ~ "≥70"
  )) %>% 
  dplyr::filter(!is.na(bmi_change), !is.na(spouse_bmi_change)) %>% 
  mutate(
    age_category = factor(age_category, levels = c("≥70","18-29","30-49","50-69")),
    bmi_category = factor(bmi_category, levels = c("Underweight or normal weight","Overweight","Obese")),
    edu_category = factor(edu_category, levels = c("Up to primary schooling","High school to secondary","College and above"))
    )

wives_df <- analytic_bmi %>% dplyr::filter(sex == "female")
husbands_df <- analytic_bmi %>% dplyr::filter(sex == "male")


# --------------------------------------------------------------------------------

library(broom)
library(ggplot2)
library(dplyr)
library(forcats)
library(glue)

# Step 1: Extract estimates, CIs, and p-values
tidy_lm <- function(model) {
  broom::tidy(model, conf.int = TRUE) %>%
    dplyr::filter(term != "(Intercept)") %>%
    mutate(term = recode(term,
                         "spouse_bmi_change" = "Spouse BMI Change",
                         "fup_duration" = "Follow-up Duration",
                         "age_category30-49" = "Age 30–49",
                         "age_category50-69" = "Age 50–69",
                         "age_category≥70" = "Age ≥70",
                         "bmi_categoryOverweight" = "Overweight",
                         "bmi_categoryObese" = "Obese",
                         "edu_categoryHigh school to secondary" = "High school to secondary",
                         "edu_categoryCollege and above" = "College and above",
                         "diabetes" = "Diabetes",
                         "famhx_dm" = "Family History of Diabetes")) %>%
    mutate(
      beta_ci = glue("{round(estimate, 2)} ({round(conf.low, 2)}, {round(conf.high, 2)})"),
      p_label = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    )
}


# Wives
model_wife <- lm(bmi_change ~ spouse_bmi_change + fup_duration + age_category + bmi_category + edu_category + diabetes,
                 data = wives_df)
wife_tidy <- tidy_lm(model_wife) %>% 
  mutate(model = "wife")

# Wives - Overall Model (unadjusted)
model_wife_overall <- lm(bmi_change ~ spouse_bmi_change, data = wives_df)
wife_overall <- tidy_lm(model_wife_overall) %>%
  mutate(term = "Overall", model = "wife")


# Husbands
model_husband <- lm(bmi_change ~ spouse_bmi_change + fup_duration + age_category + bmi_category + edu_category + diabetes,
                    data = husbands_df)
husband_tidy <- tidy_lm(model_husband) %>% 
  mutate(model = "husband")

# Husbands - Overall Model (unadjusted)
model_husband_overall <- lm(bmi_change ~ spouse_bmi_change, data = husbands_df)
husband_overall <- tidy_lm(model_husband_overall) %>%
  mutate(term = "Overall", model = "husband")


results <- bind_rows(wife_tidy, wife_overall,
                     husband_tidy, husband_overall) %>% 
  write.csv(.,"cca/analysis/psbcan05_spousal bmi change linear regression.csv")


