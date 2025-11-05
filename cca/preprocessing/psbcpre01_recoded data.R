rm(list=ls());gc();source(".Rprofile")

# keep observed data only
carrs_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbpre03_carrs observed data.RDS"))

############ Additional variables ####################

# unique hhid: 2,546
carrs_df_add <- carrs_df %>% 
  arrange(hhid, pid, carrs, fup) %>%
  # Nielsen et al. 2023 (based on the mean of the second two of three blood pressure measurements)
  mutate(
    sbp = rowMeans(select(., sbp2, sbp3), na.rm = TRUE),
    dbp = rowMeans(select(., dbp2, dbp3), na.rm = TRUE)
  ) %>%
  # define disease indicators
  mutate(
    diabetes = case_when(
      fpg >= 126 | hba1c >= 6.5 | dm == 1 | dm_med == 1 | dm_rec == 1 | dm_allo == 1 ~ 1,
      TRUE ~ 0
    ),
    overweight = case_when(
      is.na(bmi) ~ NA_real_,
      bmi >= 25 ~ 1,
      TRUE ~ 0
    ),
    hypertension = case_when(
      sbp > 140 | dbp > 90 | htn == 1 | htn_med == 1 | htn_allo == 1 ~ 1,
      TRUE ~ 0
    ),
    high_tg = case_when(
      tg > 150 ~ 1,
      TRUE ~ 0
    ),
    chd = case_when(
      chd_allo == 1 ~ 1,
      TRUE ~ chd
    )
  ) %>% 
  mutate(morbidity_number = rowSums(across(c(hypertension, diabetes, chd, cva, ckd), ~ .x == 1), na.rm = TRUE)) %>% 
  mutate(
    morbidity_category = case_when(
      morbidity_number == 0 ~ "None",
      morbidity_number == 1 ~ "Single morbidity",
      TRUE                  ~ "Multimorbidity"
    )) %>% 
  # Unknown/NA to 0 
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
    )
  ) %>% 
  # copy from baseline 
  arrange(pid, carrs, fup) %>% 
  group_by(pid) %>%
  # borrow hhincome from baseline (fup = 0)
  mutate(
    hhincome_bs = hhincome[fup == 0][1],
    hhincome = if_else(is.na(hhincome), hhincome_bs, hhincome),
    smk_overall_bs = smk_overall[fup == 0][1],
    smk_overall = if_else(is.na(smk_overall), smk_overall_bs, smk_overall),
    height_cm_bs = height_cm[fup == 0][1],
    height_cm = if_else(is.na(height_cm), height_cm_bs, height_cm),
    bmi = if_else(is.na(bmi), weight_kg/((height_cm/100)^2), bmi),
    
    edu_category_bs = edu_category[fup == 0][1],
    edu_category = if_else(is.na(edu_category), edu_category_bs, edu_category),
    employ_category_bs = employ_category[fup == 0][1],
    employ_category = if_else(is.na(employ_category), employ_category_bs, employ_category)
  ) %>%
  ungroup() %>%
  mutate(year = case_when(
    is.na(year) ~ as.integer(format(doi, "%Y")),
    TRUE ~ year)) %>%
  select(-ends_with("_bs")) 

# clean age - should be no NA
carrs_age <- carrs_df_add %>% 
  # mean calendar year for each visit
  left_join(carrs_df_add %>% 
              group_by(carrs, fup) %>%
              summarise(mean_year = round(mean(year, na.rm = TRUE)), .groups = "drop"),
            by = c("carrs","fup")) %>% 
  mutate(
    age_baseline = age[fup == 0][1],
    doi_baseline = doi[fup == 0][1],
    doi = case_when(
      !is.na(doi) ~ as.Date(doi),
      is.na(doi) & !is.na(mean_year) ~ ymd(paste0(mean_year, "-01-01")),
      TRUE ~ as.Date(NA)
    ),
    age = case_when(
      is.na(age) ~ age_baseline + (year(doi) - year(doi_baseline)),
      TRUE ~ age
    ),
    fup_duration = as.numeric(difftime(doi, doi_baseline, units = "days")) / 365.25
  )


############ Set outliers to NA ####################

carrs_outliers <- carrs_age %>% 
  group_by(pid) %>%
  arrange(pid, carrs, fup) %>%
  mutate(
    bmi_lag = dplyr::lag(bmi),           # BMI at previous visit
    bmi_baseline = bmi[fup == 0][1],     # grab the first baseline BMI
    bmi_bschange = bmi - bmi_baseline,   # change from baseline
    bmi_change = bmi - bmi_lag           # change from previous visit using the defined lag
  ) %>% 
  ungroup() %>% 
  
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
    
    bmi_change    = case_when(bmi_change < -5.37 | bmi_change > 7.53 ~ NA_real_, TRUE ~ bmi_change),
    bmi_bschange  = case_when(bmi_bschange < -5.59 | bmi_bschange > 7.75 ~ NA_real_, TRUE ~ bmi_bschange),
    
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


saveRDS(carrs_outliers, paste0(path_spouses_bmi_change_folder,"/working/cca/psbcpre01_carrs recoded data.RDS"))

