rm(list=ls());gc();source(".Rprofile")

library(haven)

spousedyads_clean <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouseyads cleaned.RDS"))

# unique pid: 30,874
baseline <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/baseline_2025_0312.sas7bdat")) %>% 
  rename(carrs = CARRS) %>% 
  
  dplyr::select(
    # Demographic
    carrs,hhid,pid,site,age,sex,educstat,educstat_other_spec,employ,occ,occ_other_spec,hhincome,
    # Tobacco
    smk_ever,smk_curr,smk_overall,smk_exp,
    # Alcohol
    alc_often,alc_overall,
    # CVD
    htn,htn_med,htn_allo,dm,dm_med,dm_allo,dm_rec,chd,cva,ckd,
    # Family history
    famhx_htn,famhx_cvd,famhx_dm,
    # ANTHRO
    sbp1,sbp2,sbp3,dbp1,dbp2,dbp3,height_cm,weight_kg,bmi,waist_cm,
    # Lab
    fpg,tg,hba1c
  ) %>% 
  
  mutate(wave = case_when(
    carrs == 1 ~ "CARRS1 BS",
    TRUE ~ "CARRS2 BS"
  )) %>% 
  mutate(bmi = case_when(
    is.na(bmi) ~ weight_kg/((height_cm/100) ^2),
    TRUE ~ bmi
  )) # NA in BMI: 4,005

# keep Delhi, Chennai: 21,862
baseline_df <- spousedyads_clean %>% 
  select(pid, hhid, site = city, spousedyad_new, sex) %>% 
  left_join(baseline %>% 
              select(-sex) %>% 
              dplyr::filter(site != 3),
            by = c("pid","hhid","site")) %>% 
  mutate(site = case_when(
    site == 1 ~ "Chennai",
    TRUE ~ "Delhi"
  )) 

# keep Delhi, Chennai; unique PID: 21,862; OBS: 104,528
fup_df <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/Long_event_2024_0829.sas7bdat")) %>% 
  rename(carrs = CARRS,
         fup = FUP) %>% 
  # exclude baseline data
  dplyr::filter(fup != 0) %>% 
  
  dplyr::select(
    # Demographic
    carrs,pid,fup,site,
    # Tobacco
    smk_curr,
    # Alcohol
    alc_curr,
    # CVD
    htn,htn_age,htn_allo,dm,dm_age,dm_allo,cva,ckd,ckd_age,
    # ANTHRO
    sbp1,sbp2,sbp3,dbp1,dbp2,dbp3,height_cm,weight_kg,waist_cm
  ) %>% 
  
  left_join(baseline_df %>% 
              select(pid,hhid,spousedyad_new,sex,height_cm) %>% 
              rename(height_cm_bs = height_cm),
            by = "pid") %>%
  mutate(wave = case_when(
    # carrs == 1 & fup == 0 ~ "CARRS1 FUP0",
    carrs == 1 & fup == 1 ~ "CARRS1 FUP1",
    carrs == 1 & fup == 2 ~ "CARRS1 FUP2",
    carrs == 1 & fup == 3 ~ "CARRS1 FUP3",
    carrs == 1 & fup == 4 ~ "CARRS1 FUP4",
    carrs == 1 & fup == 5 ~ "CARRS1 FUP5",
    carrs == 1 & fup == 6 ~ "CARRS1 FUP6",
    # carrs == 2 & fup == 0 ~ "CARRS2 FUP0", 
    carrs == 2 & fup == 1 ~ "CARRS2 FUP1",
    TRUE ~ NA_character_
  )) %>% 
  mutate(height_cm = case_when(
    is.na(height_cm) ~ height_cm_bs,
    TRUE ~ height_cm
  )) %>% 
  mutate(bmi = weight_kg/((height_cm/100) ^2)) %>% 
  dplyr::filter(site %in% c("Chennai","Delhi")) 


# N = 21,862
carrs_all <- bind_rows(baseline_df, 
                       fup_df)


# analytic sample -------------------------------------------------------------------------
sociodemo_vars <- c(
  "age", "sex", "site"
)


analytic_df <- carrs_all %>% 
  # 1. spouses only, N = 13,208, OBS = 57,754
  dplyr::filter(spousedyad_new == 1) %>%
  # 2. exclude missing baseline BMI, N = 12,050, OBS = 50,093
  group_by(pid) %>%
  dplyr::filter(!any(wave %in% c("CARRS1 BS", "CARRS2 BS") & is.na(bmi))) %>%
  # 3. BMI available for at least 1 follow-up visit, N = 9,561, OBS = 43,958
  dplyr::filter(sum(grepl("FUP", wave) & !is.na(bmi)) >= 1) %>%
  # 4. available sociodemographic variables in baseline, N = 9,560, OBS = 43,951
  dplyr::filter(any(wave %in% c("CARRS1 BS", "CARRS2 BS") &
                      if_all(all_of(sociodemo_vars), ~ !is.na(.)))) %>%
  ungroup() 



# Define valid dyads: exactly 2 people in the same household, 1 male + 1 female 
valid_hhids <- analytic_df %>%
  distinct(hhid, pid, sex) %>%       # one row per person
  group_by(hhid) %>%
  dplyr::filter(
    n() == 2,                        # exactly 2 people in the household
    all(sex %in% c("male","female")),           # only male/female
    sum(sex == "male") == 1,             # one male
    sum(sex == "female") == 1              # one female
  ) %>%
  ungroup() %>%
  pull(hhid) %>%
  unique()

# N = 8,322, OBS = 38,490
analytic_df_spouse <- analytic_df %>%
  dplyr::filter(hhid %in% valid_hhids) %>% 
  arrange(hhid, pid)

# N = 116 spouses
age_gap18 <- analytic_df_spouse %>% 
  dplyr::filter(wave %in% c("CARRS1 BS", "CARRS2 BS")) %>% 
  distinct(hhid, pid, age) %>%
  group_by(hhid) %>% 
  reframe(age_diff = abs(diff(age))) %>% 
  dplyr::filter(age_diff > 18)

# exclude spouses with age gap >18y, N = 8,090, OBS = 37,320
analytic_age18 <- analytic_df_spouse %>% 
  dplyr::filter(!hhid %in% age_gap18$hhid)


# P-CARRS -----------------------------------------
# unique pid: 20,517
pcarrs <- read_dta(paste0(path_spouses_bmi_change_folder,"/working/raw/1.PCARRS_Round-1_20250312_Emory.dta"))
# pcarrs_pid <- read_dta(paste0(path_spouses_bmi_change_folder,"/working/raw/pid_ext_test_id_20250313.dta"))

# N = 7,901
pcarrs_valid <- pcarrs %>% 
  dplyr::filter(pid %in% analytic_age18$pid) %>% 
  select(
    # ID
    pid,tcohort,
    # Demographic
    t_age,tgender,tcity,
    # ANTHRO
    t_ht,t_wt,t_bmi
  ) %>% 
  mutate(
    bmi = case_when(is.na(t_bmi) ~ t_wt/((t_ht/100) ^2),
                    TRUE ~ t_bmi), # NA in BMI: 2,848
    sex = case_when(tgender == 1 ~ "male",
                    TRUE ~ "female"),
    site = case_when(tcity == 1 ~ "Chennai",
                     TRUE ~ "Delhi"),
    age = t_age,
    carrs = tchohort
  ) 

pcarrs_df <- pcarrs_valid %>% 
  mutate(wave = "PCARRS") %>% 
  left_join(analytic_age18 %>% 
              dplyr::filter(wave %in% c("CARRS1 BS", "CARRS2 BS")) %>%
              select(pid,hhid,sex,site),
            by = c("pid","site","sex")) %>% 
  select(pid,hhid,carrs,fup,site,sex,age,wave,bmi)

# N = 8,090, OBS = 45,221
analytic_df_final <- bind_rows(
  analytic_age18,
  pcarrs_df
) %>%
  arrange(hhid, pid, wave) %>%
  mutate(
    # sbp = rowMeans(select(., sbp1, sbp2, sbp3), na.rm = TRUE),
    # dbp = rowMeans(select(., dbp1, dbp2, dbp3), na.rm = TRUE)
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
  )


saveRDS(analytic_df_final, paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse analytic sample.RDS"))


# check: no duplicate sex in the same spouse
duplicate_hhid <- analytic_df_final %>%
  dplyr::filter(wave %in% c("CARRS1 BS", "CARRS2 BS")) %>% 
  group_by(hhid, site, sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  dplyr::filter(n > 2)

