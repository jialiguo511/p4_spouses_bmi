rm(list=ls());gc();source(".Rprofile")

source("functions/egfr_ckdepi_2021.R")

# Recode --------------------------------
carrs_recoded <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbpre03_carrs observed data.RDS")) %>% 
  # family history: Unknown/NA to 0 
  mutate(famhx_cvd = case_when(is.na(famhx_cvd) ~ 0, 
                               TRUE ~ famhx_cvd),
         famhx_htn = case_when(is.na(famhx_htn) ~ 0, 
                               TRUE ~ famhx_htn),
         famhx_dm = case_when(is.na(famhx_dm) ~ 0, 
                              TRUE ~ famhx_dm)) %>% 
  mutate(
    alc_overall = case_when(
      alc_curr == 1 ~ 1,  # current (follow-up)
      alc_curr == 0 ~ 3,  # assume never
      alc_often %in% c(1, 2) | alc_overall == 1 ~ 1,  # current (baseline)
      alc_often %in% c(3, 4) | alc_overall == 2 ~ 2,  # former
      is.na(alc_often) | alc_overall == 0 ~ 0,        # never
      TRUE ~ alc_overall
    ),
    smk_overall = case_when(
      smk_curr == 1 | smk_smoke_freq %in% c(1, 2) | smk_chew_freq %in% c(1, 2) ~ 1, # current
      smk_curr == 0 | smk_ever == 0 | smk_overall == 0 ~ 0,                         # never
      smk_ever == 1 | smk_overall == 2 ~ 2,                                        # former
      TRUE ~ smk_overall
    ),
    chd = case_when(
      chd_allo == 1 ~ 1,
      TRUE ~ chd
    )
  ) %>% 
  select(-matches("(_allo|_rec|_med|_curr|_ever|_often|_freq)$"))


# Impute age with mean calendar year --------------------------------
carrs_age <- carrs_recoded %>% 
  mutate(baseline_age = age[fup == 0][1],
         baseline_doi = doi[fup == 0][1]) %>% 
  # clean age
  # mean calendar year for each visit
  left_join(carrs_recoded %>% 
              group_by(carrs, fup) %>%
              summarise(mean_year = round(mean(year, na.rm = TRUE)), .groups = "drop"),
            by = c("carrs","fup")) %>% 
  mutate(
    doi = case_when(
      !is.na(doi) ~ as.Date(doi),
      is.na(doi) & !is.na(mean_year) ~ ymd(paste0(mean_year, "-01-01")),
      TRUE ~ as.Date(NA)
    ),
    age = case_when(
      is.na(age) ~ baseline_age + (year(doi) - year(baseline_doi)),
      TRUE ~ age
    )) %>% 
  select(-c(baseline_age,baseline_doi,mean_year))


# Set outliers to NA ---------------------------------------

# 1. Define continuous variables
vars <- c("sbp1", "sbp2", "sbp3", "dbp1", "dbp2", "dbp3",
          "heightcm", "weightkg", "bmi","waistcm", "hipcm",
          "fpg", "fpg30", "fpg120", "chol", "tg", "hdl", "ldl", "vldl",
          "hba1c", "serumcreatinine")

# 2. Summarize each variable
summary_tbl <- carrs_age %>%
  rename(
    heightcm = height_cm,
    weightkg = weight_kg,
    waistcm = waist_cm,
    hipcm = hip_cm,
    fpg30 = fpg_30,
    fpg120 = fpg_120,
    serumcreatinine = serum_creatinine
  ) %>% 
  summarise(across(all_of(vars), list(
    n = ~sum(!is.na(.)),
    min = ~min(., na.rm = TRUE),
    q25 = ~quantile(., 0.25, na.rm = TRUE),
    median = ~median(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    q75 = ~quantile(., 0.75, na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
    IQR = ~IQR(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("variable", "stat"),
               names_sep = "_",
               values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

# 3. Add IQR-based range for outlier detection
summary_tbl <- summary_tbl %>%
  mutate(across(c(min, q25, median, mean, q75, max, IQR), as.numeric)) %>%
  mutate(
    lower_bound = q25 - 1 * IQR,
    upper_bound = q75 + 1 * IQR
  )

write.csv(summary_tbl, "preprocessing/check outliers in continuous variables.csv")


carrs_outliers <- carrs_age %>% 
  # group_by(pid) %>%
  # arrange(pid, carrs, fup) %>%
  # mutate(bmi_baseline = bmi[fup == 0][1],   # grab the first baseline BMI
  #        bmi_bschange = bmi - bmi_baseline, # change from baseline
  #        bmi_change = bmi - dplyr::lag(bmi)) %>% # change from previous visit
  # ungroup() %>% 
  
  mutate(
  
  across(starts_with("sbp"),
         ~ case_when(. < 88 | . > 169 ~ NA_real_, TRUE ~ .)),
  across(starts_with("dbp"),
         ~ case_when(. < 59 | . > 107 ~ NA_real_, TRUE ~ .)),
  
  height_cm = case_when(height_cm < 137 | height_cm > 200 ~ NA_real_, TRUE ~ height_cm),
  weight_kg = case_when(weight_kg < 38 | weight_kg > 100 ~ NA_real_, TRUE ~ weight_kg),
  # detect outliers
  # min 16.5: https://www.ncbi.nlm.nih.gov/books/NBK541070/?utm_source=chatgpt.com
  # max50: https://en.wikipedia.org/wiki/Classification_of_obesity
  bmi        = case_when(bmi < 16.5 | bmi > 50 ~ NA_real_, TRUE ~ bmi),
  waist_cm   = case_when(waist_cm < 64.6 | waist_cm > 113.5 ~ NA_real_, TRUE ~ waist_cm),
  hip_cm     = case_when(hip_cm < 76.6 | hip_cm > 116.8 ~ NA_real_, TRUE ~ hip_cm),
  
  # bmi_change    = case_when(bmi_change < -5.37 | bmi_change > 7.53 ~ NA_real_, TRUE ~ bmi_change),
  # bmi_bschange  = case_when(bmi_bschange < -5.59 | bmi_bschange > 7.75 ~ NA_real_, TRUE ~ bmi_bschange),
  
  fpg     = case_when(fpg < 68 | fpg > 137 ~ NA_real_, TRUE ~ fpg),
  fpg_30  = case_when(fpg_30 < 80.8 | fpg_30 > 231.4 ~ NA_real_, TRUE ~ fpg_30),
  fpg_120 = case_when(fpg_120 < 49 | fpg_120 > 172 ~ NA_real_, TRUE ~ fpg_120),
  
  chol = case_when(chol < 104 | chol > 260 ~ NA_real_, TRUE ~ chol),
  tg   = case_when(tg < 20 | tg > 262 ~ NA_real_, TRUE ~ tg),
  hdl  = case_when(hdl < 22 | hdl > 61 ~ NA_real_, TRUE ~ hdl),
  ldl  = case_when(ldl < 46 | ldl > 175 ~ NA_real_, TRUE ~ ldl),
  vldl = case_when(vldl < 2 | vldl > 50.8 ~ NA_real_, TRUE ~ vldl),
  
  hba1c            = case_when(hba1c < 4.4 | hba1c > 7.4 ~ NA_real_, TRUE ~ hba1c),
  serum_creatinine = case_when(serum_creatinine < 0.36 | serum_creatinine > 1.08 ~ NA_real_,
                               TRUE ~ serum_creatinine)
) 


saveRDS(carrs_outliers, paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbpre04_carrs recoded data.RDS"))


