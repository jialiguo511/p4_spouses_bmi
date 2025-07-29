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
    doi,dob,age,sex,site,educstat,educstat_other_spec,employ,occ,occ_other_spec,hhincome,
    # Tobacco
    smk_ever,smk_curr,smk_overall,smk_exp,
    # Alcohol
    alc_often,alc_overall,
    # CVD
    htn,htn_med,htn_allo,dm,dm_med,dm_allo,dm_rec,chd,cva,ckd,
    # Family history
    famhx_htn,famhx_cvd,famhx_dm,
    # Female REPRO
    fr_preg,
    # ANTHRO
    sbp1,sbp2,sbp3,dbp1,dbp2,dbp3,height_cm,weight_kg,bmi,waist_cm,
    # Lab
    fpg,tg,hba1c
  ) %>% 

  # keep Delhi, Chennai: 21,862
  dplyr::filter(site %in% c(1, 2)) %>% 
  mutate(
    fup = 0,
    site = case_when(site == 1 ~ "Chennai", TRUE ~ "Delhi"),
    sex = case_when(sex == 1 ~ "male", TRUE ~ "female"),
    bmi = case_when(is.na(bmi) ~ weight_kg / ((height_cm / 100) ^ 2), TRUE ~ bmi),
    bs1_date = case_when(carrs == 1 ~ doi, TRUE ~ NA_Date_),
    bs2_date = case_when(carrs == 2 ~ doi, TRUE ~ NA_Date_),
    year = as.integer(format(doi, "%Y")),
    bs_age = age
  )

ids_df <- baseline %>% 
  select(pid,hhid,sex)

############ MAJOR FUP ####################
# keep Delhi, Chennai; unique PID: 21,862; OBS: 104,528
followup <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/Long_event_2024_0829.sas7bdat")) %>% 
  rename(carrs = CARRS, fup = FUP) %>% 
   
  dplyr::select(
    # ID
    carrs,fup,pid,
    # DEMOGRAPHIC
    site,doi,
    # Tobacco
    smk_curr,smk_smoke_freq,smk_chew_freq,smk_other_freq,
    # Alcohol
    alc_curr,
    # CVD
    htn,htn_age,htn_allo,dm,dm_age,dm_allo,cva,ckd,ckd_age,
    # ANTHRO
    sbp1,sbp2,sbp3,dbp1,dbp2,dbp3,height_cm,weight_kg,waist_cm
  ) %>% 
  
  group_by(pid) %>%
  # borrow height from baseline (fup = 0)
  mutate(height_cm_bs = height_cm[fup == 0][1],
         height_cm = if_else(is.na(height_cm), height_cm_bs, height_cm)) %>%
  ungroup() %>%
  mutate(
    bmi = weight_kg / ((height_cm / 100) ^ 2),
    year = as.integer(format(doi, "%Y")),
    end1_date = case_when(carrs == 1 & fup != 0 ~ doi, TRUE ~ NA_Date_),
    end2_date = case_when(carrs == 2 & fup != 0 ~ doi, TRUE ~ NA_Date_)
  ) %>%
  select(-height_cm_bs) %>%
  dplyr::filter(fup != 0, site %in% c("Chennai", "Delhi")) %>% 
  # assign hhid
  left_join(ids_df,by = "pid")


############ PCARRS ####################
# unique pid: 20,517
pcarrs <- read_dta(paste0(path_spouses_bmi_change_folder,"/working/raw/1.PCARRS_Round-1_20250312_Emory.dta")) %>% 
  
  dplyr::select(
    # ID
    pid,carrs = tcohort,tcity,doi,
    # Demographic
    age = t_age, tgender,
    # ANTHRO
    height_cm = t_ht,weight_kg = t_wt,t_bmi
  ) %>% 
  
  # keep Delhi, Chennai: 21,862
  dplyr::filter(tcity %in% c(1, 2)) %>%
  mutate(
    bmi = case_when(is.na(t_bmi) ~ weight_kg/((height_cm/100) ^2),
                    TRUE ~ t_bmi), # NA in BMI: 2,848
    sex = case_when(tgender == 1 ~ "male", TRUE ~ "female"),
    site = case_when(tcity == 1 ~ "Chennai", TRUE ~ "Delhi"),
    fup = case_when(carrs == 1 ~ 7, TRUE ~ 2),
    year = as.integer(format(doi, "%Y")),
    end1_date = case_when(carrs == 1 ~ doi, TRUE ~ NA_Date_),
    end2_date = case_when(carrs == 2 ~ doi, TRUE ~ NA_Date_)
  ) %>% 
  select(-c("tcity","t_bmi","tgender")) %>% 
  # assign hhid
  left_join(ids_df,by = c("pid","sex"))


############ HARMONIZATION ####################
# N = 21,862
spousedyads_clean <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouseyads cleaned.RDS"))

library(lubridate)

carrs_df <- bind_rows(baseline,
                      followup,
                      pcarrs) %>% 
  # add spouse indicator
  left_join(spousedyads_clean %>% 
              select(pid,hhid,spousedyad_new),
            by = c("pid","hhid")) 
  

# check missing BMI by visit
carrs_df %>%
  group_by(carrs, fup) %>%
  summarise(
    total_n = n_distinct(pid),
    bmi_n = n_distinct(pid[is.na(bmi)]),
    bmi_pct = round(100 * bmi_n / total_n, 1),
    mean_bmi = mean(bmi, na.rm = TRUE)
  ) %>%
  arrange(carrs, fup)


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
    bmi_category = case_when(
      bmi >= 15 & bmi < 25 ~ "Underweight or normal weight",
      bmi >= 25 & bmi < 30 ~ "Overweight",
      bmi >= 30            ~ "Obese",
      TRUE                 ~ NA_character_
    ),
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

# carry demographic vars from baseline to other visits
baseline_df <- carrs_df_add %>%
  dplyr::filter(fup == 0) %>%
  distinct(hhid, pid, sex, .keep_all = TRUE) %>% 
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
    )
  ) %>% 
  select(pid,hhid,dob,bs_age,bs1_date,bs2_date,edu_category,employ_category,hhincome) %>% 
  rename_with(~ paste0(.x, "_baseline"), .cols = -c(pid, hhid))


# carry baseline info to followup visits
analytic_df <- carrs_df_add %>%
  left_join(baseline_df, by = c("pid","hhid")) %>% 
  mutate(
    dob           = if_else(fup != 0, dob_baseline, dob),
    bs_age        = if_else(fup != 0, bs_age_baseline, bs_age),
    bs1_date      = if_else(fup != 0, bs1_date_baseline, bs1_date),
    bs2_date      = if_else(fup != 0, bs2_date_baseline, bs2_date),
    hhincome      = if_else(fup != 0, hhincome_baseline, hhincome),
    edu_category        = edu_category_baseline,
    employ_category     = employ_category_baseline
  ) %>%
  select(-ends_with("_baseline")) %>% 
  mutate(fup_duration = case_when(
    carrs == 1 & fup != 0 & !is.na(end1_date) & !is.na(bs1_date) ~ as.numeric(difftime(end1_date, bs1_date, units = "days")) / 365.25,
    carrs == 2 & fup != 0 & !is.na(end2_date) & !is.na(bs2_date) ~ as.numeric(difftime(end2_date, bs2_date, units = "days")) / 365.25,
    TRUE ~ NA_real_
  ),
  hhincome = case_when(
    hhincome %in% c(8,9) ~ NA_real_,
    TRUE ~ hhincome)
  ) %>% 
  mutate(doi = ymd(doi), dob = ymd(dob)) %>%
  # compute age
  mutate(age = case_when(
    # Use dob and doi if available
    is.na(age) & !is.na(dob) & !is.na(doi) ~ as.integer(floor(interval(ymd(dob), ymd(doi)) / years(1))),
    
    # If dob is missing but baseline age and follow-up duration are available
    is.na(age) & is.na(dob) & !is.na(doi) ~ as.integer(floor(bs_age + fup_duration)),
    
    TRUE ~ age
  ))


############ FIX AGE ####################


invalid_age <- analytic_df %>% dplyr::filter(age<18) %>% pull(pid)

# fix age <18
age_fixed1 <- analytic_df %>%
  dplyr::filter(pid%in%invalid_age, year(dob) < 2000) %>% 
  mutate(doi = ymd(doi), dob = ymd(dob),
         age_dob_based = floor(as.numeric(difftime(doi, dob, units = "days")) / 365.25)) %>%
  mutate(age = if_else(age<18 | age != age_dob_based, age_dob_based, age))
  

age_fixed2 <- analytic_df %>%
  dplyr::filter(pid%in%invalid_age, year(dob) >= 2000) %>% 
  mutate(age = if_else(age>=18, age, NA_real_)) %>% 
  select(pid,hhid,carrs,fup,doi,dob,age) %>% 
  arrange(pid, carrs, fup) %>%
  group_by(pid) %>%
  mutate(age_lag = dplyr::lag(age),
         age_lead = dplyr::lead(age),
         doi_lag = dplyr::lag(doi),
         doi_lead = dplyr::lead(doi)) %>% 
  mutate(
    age_lag_based = if_else(!is.na(age_lag) & !is.na(doi_lag),
                            floor(age_lag + as.numeric(difftime(doi, doi_lag, units = "days")) / 365.25),
                            NA_real_),
    age_lead_based = if_else(!is.na(age_lead) & !is.na(doi_lead),
                             floor(age_lead - as.numeric(difftime(doi_lead, doi, units = "days")) / 365.25),
                             NA_real_),
    
    # Combine logic based on DOB year
    age = case_when(
      !is.na(age_lag_based) ~ age_lag_based,
      is.na(age_lag_based) & !is.na(age_lead_based) ~ age_lead_based,
      TRUE ~ age
    )
  ) %>%
  select(-age_lag_based, -age_lead_based) %>%
  ungroup()

age_fixed <- bind_rows(age_fixed1, age_fixed2) %>% 
  select(pid,hhid,carrs,fup,age_fixed = age)

analytic_agefix <- analytic_df %>% 
  left_join(age_fixed, 
            by = c("pid","hhid","carrs","fup")) %>% 
  mutate(age = case_when(
    pid %in% invalid_age ~ age_fixed,
    TRUE ~ age
  )) 

df_check <- analytic_agefix %>% dplyr::filter(pid %in% invalid_age)

saveRDS(analytic_agefix, paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre02_carrs harmonized data.RDS"))

write.csv(analytic_agefix, paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre02_carrs harmonized data.csv"))

