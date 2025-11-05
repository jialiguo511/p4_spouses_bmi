rm(list=ls());gc();source(".Rprofile")

# unique hhid: 2,165, N = 4,330
analytic_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre02b_long spouse bmi complete cases.RDS")) %>% 
  mutate(obese = case_when(bmi >= 30 ~ 1, TRUE ~0)) 

wife_df <- analytic_df %>%
  dplyr::filter(sex == "female") %>%
  select(hhid, carrs, fup, pid, obese) %>%
  arrange(pid, carrs, fup) %>%
  group_by(pid) %>%
  mutate(obese_lag = dplyr::lag(obese)) %>%
  ungroup() %>%
  rename_with(~ paste0("wife_", .), .cols = c(pid, obese, obese_lag))

husband_df <- analytic_df %>%
  dplyr::filter(sex == "male") %>%
  select(hhid, carrs, fup, pid, obese) %>%
  arrange(pid, carrs, fup) %>%
  group_by(pid) %>%
  mutate(obese_lag = dplyr::lag(obese)) %>%
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

# N = 4,936
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
    analytic_df %>% select(-c("sex","obese")),  # drop duplicate column to avoid conflict
    by = c("pid", "hhid", "carrs", "fup")
  )


# -----------------------------------------------------------------------------------------------------
library(survival)
library(broom)

# Create indicator dummies (reference = Remained non-obese)
df <- analytic_obese %>%
  dplyr::filter(!is.na(became_obese), !is.na(spouse_obesity_change)) %>%
  mutate(
    spouse_became_obese = as.integer(spouse_obesity_change == "Became obese"),
    spouse_remained_obese = as.integer(spouse_obesity_change == "Remained obese"),
    spouse_became_nonobese = as.integer(spouse_obesity_change == "Became non-obese")
  ) %>% 
  mutate(obesity_concordance = case_when(
    obese == 1 & spouse_obese == 1 ~ "Both obese",
    obese == 0 & spouse_obese == 0 ~ "Both non-obese",
    obese == 1 & spouse_obese == 0 ~ "Index only obese",
    obese == 0 & spouse_obese == 1 ~ "Spouse only obese",
    TRUE ~ NA_character_
  ) %>% factor(levels = c("Both non-obese", "Index only obese", "Spouse only obese", "Both obese")))

wives_df <- df %>% dplyr::filter(sex == "female")
husbands_df <- df %>% dplyr::filter(sex == "male")

### Unadjusted ###

wife_unadj <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese,
  data = wives_df,
  family = binomial(link = "cloglog")
)

husband_unadj <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese,
  data = husbands_df,
  family = binomial(link = "cloglog")
)

### Model 1: adjust for age, baseline BMI ###

wife_mod1 <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
  + bmi_baseline + age,
  data = wives_df,
  family = binomial(link = "cloglog")
)

husband_mod1 <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
  + bmi_baseline + age,
  data = husbands_df,
  family = binomial(link = "cloglog")
)

### Model 2 ###

wife_mod2 <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
  + bmi_baseline + age + fup_duration + carrs
  # + smk_curr + alc_curr 
  + edu_category + employ_category + hhincome + site
  + diabetes + famhx_dm,
  data = wives_df,
  family = binomial(link = "cloglog")
)


husband_mod2 <- glm(
  became_obese ~ spouse_became_obese + spouse_remained_obese + spouse_became_nonobese 
  + bmi_baseline + age + fup_duration + carrs
  + smk_curr + alc_curr 
  + edu_category + employ_category + hhincome + site
  + diabetes + famhx_dm,
  data = husbands_df,
  family = binomial(link = "cloglog")
)


# Extract results ----------------------------------------
extract_cloglog <- function(model) {
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
  
  tibble::tibble(
    term = terms[idx],
    estimate = exp(est),
    HR_CI = sprintf("%.2f (%.2f, %.2f)", exp(est), exp(lower), exp(upper))
  ) %>%
    mutate(term = dplyr::recode(term,
                                "spouse_became_obese" = "Nonobese to Obese",
                                "spouse_remained_obese" = "Stable Obese",
                                "spouse_became_nonobese" = "Obese to Nonobese"
    ))
}


wife_tab_unadj <- extract_cloglog(wife_unadj)
husband_tab_unadj <- extract_cloglog(husband_unadj)
wife_tab_mod1  <- extract_cloglog(wife_mod1)
husband_tab_mod1  <- extract_cloglog(husband_mod1)
wife_tab_mod2  <- extract_cloglog(wife_mod2)
husband_tab_mod2  <- extract_cloglog(husband_mod2)


build_result_table <- function(unadj, mod1, mod2) {
  all_terms <- c("Stable Obese", "Nonobese to Obese", "Obese to Nonobese")
  tibble(
    `Spouse Obesity Status` = all_terms,
    Unadjusted = c(unadj$HR_CI[match(all_terms, unadj$term)]),
    `Model 1` = c(mod1$HR_CI[match(all_terms, mod1$term)]),
    `Model 2` = c(mod2$HR_CI[match(all_terms, mod2$term)])
  )
}

wife_results <- build_result_table(wife_tab_unadj, wife_tab_mod1, wife_tab_mod2)
husband_results <- build_result_table(husband_tab_unadj, husband_tab_mod1, husband_tab_mod2)


# Count N and obs for each group
wife_counts <- wives_df %>%
  group_by(spouse_obesity_change) %>%
  summarise(
    N = n_distinct(pid),
    obs = n(),
    .groups = "drop"
  ) %>%
  mutate(model = "Wife")

husband_counts <- husbands_df %>%
  group_by(spouse_obesity_change) %>%
  summarise(
    N = n_distinct(pid),
    obs = n(),
    .groups = "drop"
  ) %>%
  mutate(model = "Husband")

counts_combined <- bind_rows(wife_counts, husband_counts)


bind_rows(wife_results %>% mutate(model = "Wife"),
          husband_results %>% mutate(model = "Husband")) %>% 
  write.csv(.,"cca/analysis/psbcan04_obesity change regression results.csv")

  
  
  