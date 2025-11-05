rm(list=ls());gc();source(".Rprofile")

library(broom)
library(dplyr)
library(tibble)

# ============================================================================
# Incident Obesity Analysis - Complete Case
# ============================================================================

analytic_df_long <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cca/psbcpre03_long spouse bmi complete cases.RDS"))

# ============================================================================
# Prepare data
# ============================================================================

# Create baseline BMI and obesity indicators
wife_df <- analytic_df_long %>%
  dplyr::filter(sex == "female") %>%
  select(hhid, carrs, fup, pid, bmi) %>%
  arrange(pid, carrs, fup) %>%
  group_by(pid) %>%
  mutate(
    bmi_baseline = first(bmi[fup == 0]),
    obese = case_when(bmi >= 30 ~ 1, TRUE ~ 0),
    obese_lag = dplyr::lag(obese)
  ) %>%
  ungroup() %>%
  rename_with(~ paste0("wife_", .), .cols = c(pid, obese, obese_lag))

husband_df <- analytic_df_long %>%
  dplyr::filter(sex == "male") %>%
  select(hhid, carrs, fup, pid, bmi) %>%
  arrange(pid, carrs, fup) %>%
  group_by(pid) %>%
  mutate(
    bmi_baseline = first(bmi[fup == 0]),
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
    analytic_df_long %>% select(-c("sex", "bmi")),  # drop duplicate columns
    by = c("pid", "hhid", "carrs", "fup")
  )

# Create exposure indicators (reference = Remained non-obese)
df <- analytic_obese %>%
  dplyr::filter(!is.na(became_obese), !is.na(spouse_obesity_change)) %>%
  mutate(
    spouse_became_obese = as.integer(spouse_obesity_change == "Became obese"),
    spouse_remained_obese = as.integer(spouse_obesity_change == "Remained obese"),
    spouse_became_nonobese = as.integer(spouse_obesity_change == "Became non-obese")
  )

wife_data <- df %>% dplyr::filter(sex == "female")
husband_data <- df %>% dplyr::filter(sex == "male")


# ============================================================================
# Fit WIFE models
# ============================================================================

wife_unadj <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese,
  data = wife_data,
  family = binomial(link = "cloglog")
)

wife_model1 <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
  + bmi_baseline + age,
  data = wife_data,
  family = binomial(link = "cloglog")
)

wife_model2 <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
  + bmi_baseline + age + fup_duration + carrs
  + edu_category + employ_category + hhincome + site
  + diabetes + famhx_dm,
  data = wife_data,
  family = binomial(link = "cloglog")
)

# ============================================================================
# Fit HUSBAND models
# ============================================================================

husband_unadj <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese,
  data = husband_data,
  family = binomial(link = "cloglog")
)

husband_model1 <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
  + bmi_baseline + age,
  data = husband_data,
  family = binomial(link = "cloglog")
)

husband_model2 <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
  + bmi_baseline + age + fup_duration + carrs
  + smk_overall + alc_overall 
  + edu_category + employ_category + hhincome + site
  + diabetes + famhx_dm,
  data = husband_data,
  family = binomial(link = "cloglog")
)

# ============================================================================
# Extract and format results
# ============================================================================

extract_cloglog <- function(model, outcome_sex) {
  coefs <- summary(model)$coefficients
  terms <- rownames(coefs)
  
  # Filter for the spouse effect terms
  terms_of_interest <- c("spouse_became_obese", "spouse_remained_obese", "spouse_became_nonobese")
  idx <- which(terms %in% terms_of_interest)
  
  # Estimates, SE, Wald CI
  est <- coefs[idx, "Estimate"]
  se <- coefs[idx, "Std. Error"]
  lower <- est - 1.96 * se
  upper <- est + 1.96 * se
  
  tibble(
    outcome = outcome_sex,
    term = terms[idx],
    estimate = exp(est),
    HR_CI = sprintf("%.2f (%.2f, %.2f)", exp(est), exp(lower), exp(upper))
  ) %>%
    mutate(term = dplyr::recode(term,
      "spouse_became_obese" = "Became Obese",
      "spouse_remained_obese" = "Remained Obese",
      "spouse_became_nonobese" = "Became Non-Obese"
    ))
}

# Wife results
wife_unadj_res <- extract_cloglog(wife_unadj, "wife") %>% mutate(model = "unadj")
wife_model1_res <- extract_cloglog(wife_model1, "wife") %>% mutate(model = "model1")
wife_model2_res <- extract_cloglog(wife_model2, "wife") %>% mutate(model = "model2")

# Husband results
husband_unadj_res <- extract_cloglog(husband_unadj, "husband") %>% mutate(model = "unadj")
husband_model1_res <- extract_cloglog(husband_model1, "husband") %>% mutate(model = "model1")
husband_model2_res <- extract_cloglog(husband_model2, "husband") %>% mutate(model = "model2")

# Combine all results
results_df <- bind_rows(
  wife_unadj_res, wife_model1_res, wife_model2_res,
  husband_unadj_res, husband_model1_res, husband_model2_res
) %>%
  select(outcome, model, term, estimate, HR_CI)

print(results_df)

write.csv(results_df, "cca/analysis/psbcan03_incident obesity results.csv", row.names = FALSE)

  
  
  