rm(list=ls());gc();source(".Rprofile")

carrs_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cca/psbcpre01_carrs recoded data.RDS"))

############ COMPLETE CASES - BMI ####################

demo_vars <- c("age", "doi", "smk_overall","alc_overall", "hhincome",
               "diabetes","hypertension","high_tg","famhx_dm",# "overweight","bmi_category",
               "edu_category","employ_category", "morbidity_category")

# Summarize missing values
carrs_df %>%
  dplyr::filter(fup == 0) %>% 
  summarise(across(all_of(demo_vars), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(
    total = nrow(carrs_df),
    na_percent = round(100 * na_count / total, 1)
  ) %>%
  arrange(desc(na_percent))


# N = 18,672
outlier_df <- carrs_df %>% 
  # exclude missing demographics
  # dplyr::filter(if_all(all_of(demo_vars), ~ !is.na(.))) %>% 
  dplyr::filter(!is.na(hhincome) | fup != 0) %>% # only hhincome has NA
  
  # remove carrs2 fup1 cause it's missing labs
  dplyr::filter(!(carrs == 2 & fup == 1)) %>% 
  
  # exclude visit without any BMI assessments (carrs1 fup1, fup3, fup5, fup6)
  group_by(carrs, fup) %>%
  dplyr::filter(mean(is.na(bmi)) < 1) %>%
  ungroup() %>% 

  # detect outliers
  # min 16.5: https://www.ncbi.nlm.nih.gov/books/NBK541070/?utm_source=chatgpt.com
  # max50: https://en.wikipedia.org/wiki/Classification_of_obesity
  dplyr::filter(bmi >= 16.5 & bmi <= 50) %>% 
  dplyr::filter(bmi_bschange >= -5.59 & bmi_bschange <= 7.75) %>% 
  dplyr::filter(is.na(bmi_change) | (bmi_change >= -5.37 & bmi_change <= 7.53))



required_visits <- list(
  `1` = c(0, 2, 4, 7),
  `2` = c(0, 2) 
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
cca_bmi <- outlier_df %>%
  dplyr::filter(pid %in% complete_pids)


saveRDS(cca_bmi, paste0(path_spouses_bmi_change_folder,"/working/cca/psbcpre02_bmi complete cases.RDS"))

table(outlier_df$carrs,outlier_df$fup)
table(cca_bmi$carrs,cca_bmi$fup)

