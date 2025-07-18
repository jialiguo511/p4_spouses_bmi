rm(list=ls());gc();source(".Rprofile")

carrs_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre02_carrs harmonized data.RDS"))

############ COMPLETE CASES - BMI ####################

# N = 8,663
cca_bmi <- carrs_df %>% 
  # exclude visit without any BMI assessments (carrs1 fup1, fup3, fup5, fup6)
  group_by(carrs, fup) %>%
  dplyr::filter(mean(is.na(bmi)) < 1) %>%
  ungroup() %>% 
  
  # detect outliers
  # min 16.5: https://www.ncbi.nlm.nih.gov/books/NBK541070/?utm_source=chatgpt.com
  # max50: https://en.wikipedia.org/wiki/Classification_of_obesity
  mutate(bmi = case_when(bmi < 16.5 | bmi > 50 ~ NA_real_, 
                         TRUE ~ bmi)) %>% 
  
  # keep people with available bmi across all visits
  group_by(pid) %>%
  mutate(bmi_available = case_when(all(!is.na(bmi)) ~ 1,
                                   TRUE ~ 0)) %>%
  ungroup() %>% 
  dplyr::filter(bmi_available == 1) %>% 
  
  group_by(pid) %>%
  arrange(pid, carrs, fup) %>%
  # Keep all from carrs = 2; keep only those from carrs = 1 who have fup == 7
  dplyr::filter(any(carrs == 2) | any(carrs == 1 & fup == 7)) %>%
  mutate(bmi_baseline = bmi[fup == 0][1],   # grab the first baseline BMI
         bmi_bschange = bmi - bmi_baseline, # change from baseline
         bmi_change = bmi - dplyr::lag(bmi)) %>% # change from previous visit
  ungroup() %>% 

  # exclude outliers in BMI change
  dplyr::filter(bmi_bschange >= -5.59 & bmi_bschange <= 7.75,
                is.na(bmi_change) | (bmi_change >= -5.37 & bmi_change <= 7.53))


# missing BMI by visit
cca_bmi %>%
  group_by(carrs, fup) %>%
  summarise(
    total_n = n_distinct(pid),
    mean_bmi = mean(bmi, na.rm = TRUE)
  ) %>%
  arrange(carrs, fup)


saveRDS(cca_bmi, paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre01_bmi complete cases.RDS"))
