rm(list=ls());gc();source(".Rprofile")

library(haven)

############ BASELINE ####################
# base unique pid: 30,874
baseline <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/baseline_2025_0312.sas7bdat")) %>% 
  rename(carrs = CARRS) %>% 
  
  dplyr::select(
    # ID
    carrs,hhid,pid,
    # Demographic
    doi,dob,ceb,age,sex,site,educstat,employ,occ,hhincome,
    # Tobacco
    smk_ever,smk_curr,smk_overall,smk_exp,
    # Alcohol
    alc_often,alc_overall,
    # CVD
    htn,htn_med,htn_allo,htn_rec, # HTN
    dm,dm_med,dm_allo,dm_rec, # DM
    hld,hld_med,hld_allo,hld_rec, # heart disease
    chd,chd_med,chd_allo,chd_rec,
    cva,cva_rec, # stroke
    ckd,ckd_med,ckd_allo,ckd_rec, # CKD
    cancer, # cancer
    # Family history
    famhx_htn,famhx_cvd,famhx_dm,
    # ANTHRO
    sbp1,sbp2,sbp3,dbp1,dbp2,dbp3,height_cm,weight_kg,bmi,waist_cm,hip_cm
    # Lab
    # fpg,tg,hba1c
  ) %>% 

  # keep Delhi, Chennai: 21,862
  dplyr::filter(site %in% c(1, 2)) %>% 
  mutate(
    fup = 0,
    site = case_when(site == 1 ~ "Chennai", TRUE ~ "Delhi"),
    sex = case_when(sex == 1 ~ "male", TRUE ~ "female"),
    bmi = case_when(is.na(bmi) ~ weight_kg / ((height_cm / 100) ^ 2), TRUE ~ bmi),
    year = as.integer(format(doi, "%Y"))
  ) %>% 
  mutate(
    edu_category = case_when(
      educstat %in% c(1, 2)    ~ "College and above",
      educstat %in% c(3, 4)    ~ "High school to secondary",
      educstat %in% c(5, 6, 7) ~ "Up to primary schooling",
      TRUE                     ~ NA_character_
    ),
    employ_category = case_when(
      employ %in% c(2, 3)                        ~ "Not in the labor force, student/housewives",
      employ == 4                                ~ "Not in the labor force, retired",
      employ == 5                                ~ "Unemployed",
      employ == 1 & occ %in% c(3, 4, 5)          ~ "Employed in a manual profession",
      employ == 1 & occ %in% c(1, 2)             ~ "Employed in a non-manual professional",
      TRUE                                       ~ NA_character_
    ),
    hhincome = case_when(
      hhincome %in% c(8,9) ~ NA_real_,
      TRUE ~ hhincome),
    bmibs_category = case_when(
      bmi >= 15 & bmi < 25 ~ "Underweight or normal weight",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30            ~ "Obese",
      TRUE                 ~ NA_character_
    )
  )

ids_df <- baseline %>% 
  select(pid,hhid,sex)
# baseline_age,baseline_doi,edu_category,employ_category,hhincome,bmibs_category


############ FUP + PCARRS ####################
# keep Delhi, Chennai; unique PID: 21,862; OBS: 104,528
followup <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/long_event_2025_0515.sas7bdat")) %>% 

  dplyr::select(
    # ID
    carrs,fup,pid,pcarrs,
    # REFUSE REASON
    reason, # reason_explain,
    # DEMOGRAPHIC
    site,doi,
    # Tobacco
    smk_curr,smk_smoke_freq,smk_chew_freq,smk_other_freq,
    # Alcohol
    alc_curr,
    # CVD
    htn,htn_allo,dm,dm_allo,hld,hld_allo,chd_allo,
    ckd,ckd_allo,cva,cancer,
    # ANTHRO
    sbp1,sbp2,sbp3,dbp1,dbp2,dbp3,height_cm,weight_kg,waist_cm,hip_cm
  ) %>% 
  
  group_by(pid) %>%
  # borrow height from baseline (fup = 0)
  mutate(height_cm_bs = height_cm[fup == 0][1],
         height_cm = if_else(is.na(height_cm), height_cm_bs, height_cm)) %>%
  ungroup() %>%
  mutate(
    bmi = weight_kg / ((height_cm / 100) ^ 2),
    year = as.integer(format(doi, "%Y"))
  ) %>%
  select(-height_cm_bs) %>%
  dplyr::filter(fup != 0, site %in% c("Chennai", "Delhi")) %>% 
  # assign hhid
  left_join(ids_df,by = "pid") 


############ LAB ####################

lab <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/lab_2025_0414.sas7bdat")) %>% 
  
  dplyr::select(
    # ID
    carrs,fup,pid,pcarrs,
    # DEMOGRAPHIC
    site,sex,
    # LAB
    fpg,fpg_30,fpg_120,chol,tg,hdl,ldl,vldl,hba1c,serum_creatinine
  ) %>% 
  
  # keep Delhi, Chennai: 21,862
  dplyr::filter(site %in% c(1, 2)) %>%
  mutate(site = case_when(site == 1 ~ "Chennai", TRUE ~ "Delhi"),
         sex = case_when(sex == 1 ~ "male", TRUE ~ "female"),
         fup = case_when(carrs == 1 & pcarrs == 1 ~ 7,
                         carrs == 2 & pcarrs == 1 ~ 2,
                         TRUE ~ fup))


############ HARMONIZATION ####################

# N = 21,862
spousedyads_clean <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/spouseyads cleaned.RDS"))

carrs_df <- bind_rows(baseline,
                      followup) %>% 
  mutate(pcarrs = case_when(is.na(pcarrs) ~ 0,
                            TRUE ~ pcarrs),
         # recode fup to indicate pcarrs
         fup = case_when(carrs == 1 & pcarrs == 1 ~ 7,
                         carrs == 2 & pcarrs == 1 ~ 2,
                         TRUE ~ fup)) %>% 
  arrange(pid,carrs,fup) %>% 
  left_join(lab,
            by = c('carrs','fup','pid','pcarrs','site','sex')) %>% 
  # add spouse indicator
  left_join(spousedyads_clean %>% 
              select(pid,hhid,spousedyad_new),
            by = c("pid","hhid")) 

saveRDS(carrs_df, paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbpre02_carrs harmonized data.RDS"))




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
  mutate(alc_curr = case_when(alc_curr == 1 | alc_overall == 1 | alc_often %in% c(1,2) ~ 1,
                              TRUE ~ 0),
         smk_curr = case_when(smk_curr == 1 | smk_overall == 1 | smk_smoke_freq %in% c(1,2) | 
                                smk_chew_freq %in% c(1,2) | smk_other_freq %in% c(1,2) ~ 1,
                              TRUE ~ 0))

# clean age - should be no NA
carrs_age <- carrs_df_add %>% 
  # mean calendar year for each visit
  left_join(carrs_df_add %>% 
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
    ),
    fup_duration = as.numeric(difftime(doi, baseline_doi, units = "days")) / 365.25
  )


saveRDS(carrs_age, paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre02_carrs harmonized data with additional variables.RDS"))

write.csv(carrs_age, paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre02_carrs harmonized data with additional variables.csv"))

