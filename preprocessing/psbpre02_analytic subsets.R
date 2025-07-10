rm(list=ls());gc();source(".Rprofile")

library(haven)

############ BASELINE ####################
# base unique pid: 30,874
baseline <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/baseline_2025_0312.sas7bdat")) %>% 
  rename(carrs = CARRS) %>% 
  
  dplyr::select(
    # ID
    carrs,hhid,pid,doi,site,
    # Demographic
    age,sex,educstat,educstat_other_spec,employ,occ,occ_other_spec,hhincome,
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

  # keep Delhi, Chennai: 21,862
  dplyr::filter(site %in% c(1, 2)) %>% 
  mutate(
    fup = 0,
    site = case_when(site == 1 ~ "Chennai", TRUE ~ "Delhi"),
    sex = case_when(sex == 1 ~ "male", TRUE ~ "female"),
    bmi = case_when(is.na(bmi) ~ weight_kg / ((height_cm / 100) ^ 2), TRUE ~ bmi),
    bs1_date = case_when(carrs == 1 ~ doi, TRUE ~ NA_Date_),
    bs2_date = case_when(carrs == 2 ~ doi, TRUE ~ NA_Date_),
    year = as.integer(format(doi, "%Y"))
  )

ids_df <- baseline %>% 
  select(pid,hhid,sex,bs1_date,bs2_date)

############ MAJOR FUP ####################
# keep Delhi, Chennai; unique PID: 21,862; OBS: 104,528
followup <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/Long_event_2024_0829.sas7bdat")) %>% 
  rename(carrs = CARRS, fup = FUP) %>% 
   
  dplyr::select(
    # ID
    carrs,fup,pid,site,doi,
    # Tobacco
    smk_curr,
    # Alcohol
    alc_curr,
    # CVD
    htn,htn_age,htn_allo,dm,dm_age,dm_allo,cva,ckd,ckd_age,
    # ANTHRO
    sbp1,sbp2,sbp3,dbp1,dbp2,dbp3,height_cm,weight_kg,waist_cm
  ) %>% 
  
  # borrow height from baseline (fup = 0)
  group_by(pid) %>%
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

carrs_df <- bind_rows(baseline,
                      followup,
                      pcarrs) %>% 
  left_join(spousedyads_clean %>% 
              select(pid,hhid,spousedyad_new),
            by = c("pid","hhid")) %>% 
  mutate(fup_duration = case_when(
    carrs == 1 & fup != 0 & !is.na(end1_date) & !is.na(bs1_date) ~ as.numeric(difftime(end1_date, bs1_date, units = "days")) / 365.25,
    carrs == 2 & fup != 0 & !is.na(end2_date) & !is.na(bs2_date) ~ as.numeric(difftime(end2_date, bs2_date, units = "days")) / 365.25,
    TRUE ~ NA_real_
  ))

saveRDS(carrs_df, paste0(path_spouses_bmi_change_folder,"/working/cleaned/carrs harmonized data.RDS"))

# missing BMI by visit
carrs_df %>%
  group_by(carrs, fup) %>%
  summarise(
    total_n = n_distinct(pid),
    bmi_n = n_distinct(pid[is.na(bmi)]),
    bmi_pct = round(100 * bmi_n / total_n, 1),
    mean_bmi = mean(bmi, na.rm = TRUE)
  ) %>%
  arrange(carrs, fup)


############ COMPLETE CASES - BMI ####################

# N = 8,891
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
  
  # Keep all from carrs = 2; keep only those from carrs = 1 who have fup == 7
  group_by(pid) %>%
  dplyr::filter(any(carrs == 2) | any(carrs == 1 & fup == 7)) %>%
  mutate(bmi_baseline = bmi[fup == 0][1],   # grab the first baseline BMI
         bmi_change = bmi - bmi_baseline) %>%
  ungroup() 


# missing BMI by visit
cca_bmi %>%
  group_by(carrs, fup) %>%
  summarise(
    total_n = n_distinct(pid),
    mean_bmi = mean(bmi, na.rm = TRUE)
  ) %>%
  arrange(carrs, fup)

############ SPOUSE - ONE MALE + ONE FEMALE ####################
# N = 6,832
spouse_df <- cca_bmi %>% 
  dplyr::filter(spousedyad_new == 1)

# Define valid dyads: exactly 2 people in the same household, 1 male + 1 female 
valid_hhids <- spouse_df %>%
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

# N = 5,220, 2,610 couples, OBS = 17,934
analytic_df <- spouse_df %>%
  dplyr::filter(hhid %in% valid_hhids) %>% 
  arrange(hhid, pid) 


############ SPOUSE - AGE GAP <= 18Y ####################

# N = 2,468 spouses
age_gap18 <- analytic_df %>% 
  dplyr::filter(fup == 0) %>% 
  distinct(hhid, pid, age) %>%
  group_by(hhid) %>% 
  reframe(age_diff = abs(diff(age))) %>% 
  dplyr::filter(age_diff <= 18)

# exclude spouses with age gap >18y, N = 4,936, OBS = 16,944
analytic_age18 <- analytic_df %>% 
  dplyr::filter(hhid %in% age_gap18$hhid)


saveRDS(analytic_age18, paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse bmi complete cases.RDS"))

