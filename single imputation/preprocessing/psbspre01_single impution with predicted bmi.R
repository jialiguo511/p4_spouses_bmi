# ==============================================================================
# Purpose: Perform single imputation for missing BMI using predicted values
# Output: single imputation/psbspre01_bmi complete cases.RDS
#
#   PREDICTION MODEL:
#   - Fits linear mixed-effects model on observed BMI values:
#     bmi ~ sex + age + (1|pid)
#   - Random intercept accounts for within-person correlation
#   - Predicts missing BMI only for individuals with ≥1 observed BMI value
#   - Excludes visits completely missing BMI across all participants
#     (CARRS-1: fup1, fup3, fup5, fup6 had no BMI assessments)
#   
#   POST-IMPUTATION PROCESSING:
#   - Applies same outlier exclusions as main analysis:
#     * BMI range: 16.5-50 kg/m²
#     * Baseline change: -5.59 to +7.75 kg/m²
#     * Visit-to-visit change: -5.37 to +7.53 kg/m²
#   - Requires complete follow-up: CARRS-1 (visits 0,2,4,7), CARRS-2 (visits 0,1,2)
#   - Requires complete demographics (no missing covariates)
#   
#   INTERPRETATION:
#   - Single imputation underestimates uncertainty (no between-imputation variance)
#   - Used as sensitivity check to compare with MI results
# ==============================================================================

rm(list=ls());gc();source(".Rprofile")

library(lme4)

carrs_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre02_carrs harmonized data.RDS")) 

model_bmi <- carrs_df %>%
  dplyr::filter(!is.na(bmi)) %>%
  lmer(bmi ~ sex + age + (1 | pid), data = .)

predict_df <- carrs_df %>%
  left_join(carrs_df %>%
              group_by(pid) %>%
              summarise(n_bmi_obs = sum(!is.na(bmi)), .groups = "drop"),
            by = "pid") %>% 
  # exclude visit without any BMI assessments (carrs1 fup1, fup3, fup5, fup6)
  group_by(carrs, fup) %>%
  dplyr::filter(mean(is.na(bmi)) < 1) %>%
  ungroup() %>% 
  
  mutate(predicted_bmi = predict(model_bmi, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(bmi = case_when(
    is.na(bmi) & n_bmi_obs >= 1 ~ predicted_bmi,
    TRUE ~ bmi
  )) %>%
  dplyr::filter(!is.na(bmi)) %>% 
  select(-predicted_bmi,-n_bmi_obs) 


demo_vars <- c("age", "doi", "smk_curr","alc_curr", "hhincome",
               "diabetes","hypertension","high_tg","famhx_dm",# "overweight","bmi_category",
               "edu_category","employ_category", "morbidity_category")

# Summarize missing values
predict_df %>%
  summarise(across(all_of(demo_vars), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(
    total = nrow(carrs_df),
    na_percent = round(100 * na_count / total, 1)
  ) %>%
  arrange(desc(na_percent))


# N = 18,313
outlier_df <- predict_df %>% 
  # exclude missing demographics
  dplyr::filter(if_all(all_of(demo_vars), ~ !is.na(.))) %>% 
  group_by(pid) %>%
  arrange(pid, carrs, fup) %>%
  mutate(bmi_baseline = bmi[fup == 0][1],   # grab the first baseline BMI
         bmi_bschange = bmi - bmi_baseline, # change from baseline
         bmi_change = bmi - dplyr::lag(bmi)) %>% # change from previous visit
  ungroup() %>% 
  
  # detect outliers
  # min 16.5: https://www.ncbi.nlm.nih.gov/books/NBK541070/?utm_source=chatgpt.com
  # max50: https://en.wikipedia.org/wiki/Classification_of_obesity
  dplyr::filter(bmi >= 16.5 & bmi <= 50) %>% 
  dplyr::filter(bmi_bschange >= -5.59 & bmi_bschange <= 7.75) %>% 
  dplyr::filter(is.na(bmi_change) | (bmi_change >= -5.37 & bmi_change <= 7.53))


required_visits <- list(
  `1` = c(0, 2, 4, 7),
  `2` = c(0, 1, 2)
)

# Check completeness per pid
complete_pids <- outlier_df %>%
  group_by(pid, carrs) %>%
  summarise(
    fup_set = list(sort(unique(fup))),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    keep = all(required_visits[[as.character(carrs)]] %in% fup_set)
  ) %>%
  dplyr::filter(keep) %>%
  pull(pid)

# Final dataset: only include pids with all required visits
bmi_ava <- outlier_df %>%
  dplyr::filter(pid %in% complete_pids)


saveRDS(bmi_ava, paste0(path_spouses_bmi_change_folder,"/working/cleaned/single_imputation/psbspre01_bmi_complete_cases.RDS"))

table(outlier_df$carrs,outlier_df$fup)
table(bmi_ava$carrs,bmi_ava$fup)

