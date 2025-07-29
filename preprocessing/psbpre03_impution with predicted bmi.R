rm(list=ls());gc();source(".Rprofile")

carrs_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre02_carrs harmonized data.RDS"))

library(lme4)

model_bmi <- carrs_df %>%
  dplyr::filter(!is.na(bmi), !is.na(sex), !is.na(age)) %>%
  lmer(bmi ~ sex + age + (1 | pid), data = .)

predict_df <- carrs_df %>%
  mutate(predicted_bmi = predict(model_bmi, newdata = ., allow.new.levels = TRUE)) %>%
  mutate(bmi = case_when(
    is.na(bmi) ~ predicted_bmi,
    TRUE ~ bmi
  )) %>%
  select(-predicted_bmi) # NA in BMI: 13318 due to missing age


demo_vars <- c("age", "doi", "smk_curr","alc_curr", "hhincome",
               "diabetes","overweight","hypertension","high_tg","famhx_dm",
               "edu_category","employ_category", "bmi_category","morbidity_category")

# N = 18,313
outlier_df <- predict_df %>% 
  # exclude missing demographics
  dplyr::filter(if_all(all_of(demo_vars), ~ !is.na(.))) %>% 
  # exclude visit without any BMI assessments (carrs1 fup1, fup3, fup5, fup6)
  group_by(carrs, fup) %>%
  dplyr::filter(mean(is.na(bmi)) < 1) %>%
  ungroup() %>% 
  
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


table(outlier_df$carrs,outlier_df$fup)
