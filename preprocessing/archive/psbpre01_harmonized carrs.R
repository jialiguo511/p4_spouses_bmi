rm(list=ls());gc();source(".Rprofile")

library(haven)

# select important variables; generate a harmonized CARRS dataset --------------------------

## add city variable
# unique PID: 30,874; unique HHID: 17,384
baseline <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/baseline_2025_0312.sas7bdat")) %>% 
  rename(carrs = CARRS) %>% 
  dplyr::select(site,
    carrs,hhid,pid,relation,relation_other_spec,age,dob,sex,bw,marital,marital_other_spec,
    educyrs,educstat,educstat_other_spec,employ,occ,occ_other_spec,employpast,occpast,occpast_other_spec,
    smk_ever,smk_curr,smk_overall,smk_other_stopmon,smk_age,smkls_age,smk_exp,smk_exp_day,smk_exp_hr,
    alc_ever,alc_often,alc_overall,
    sbp1,sbp2,sbp3,dbp1,dbp2,dbp3,height_cm,weight_kg,bmi,waist_cm,t_dci,
    htn,dm,chd,cva,ckd,famhx_htn,famhx_cvd,famhx_dm
  ) %>% 
  mutate(wave = case_when(
    carrs == 1 ~ "CARRS1 BS",
    TRUE ~ "CARRS2 BS"
  ))

# unique PID: 30,874
fup_long <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/Long_event_2024_0829.sas7bdat")) %>% 
  rename(carrs = CARRS,
         fup = FUP) %>% 
  dplyr::select(
    carrs,pid,fup,
    smk_curr,alc_curr,
    sbp1,sbp2,sbp3,dbp1,dbp2,dbp3,height_cm,weight_kg,waist_cm,
    htn,dm,cva,ckd
  ) %>% 
  mutate(wave = case_when(
    carrs == 1 & fup == 0 ~ "CARRS1 FUP0",
    carrs == 1 & fup == 1 ~ "CARRS1 FUP1",
    carrs == 1 & fup == 2 ~ "CARRS1 FUP2",
    carrs == 1 & fup == 3 ~ "CARRS1 FUP3",
    carrs == 1 & fup == 4 ~ "CARRS1 FUP4",
    carrs == 1 & fup == 5 ~ "CARRS1 FUP5",
    carrs == 1 & fup == 6 ~ "CARRS1 FUP6",
    carrs == 2 & fup == 0 ~ "CARRS2 FUP0", 
    carrs == 2 & fup == 1 ~ "CARRS2 FUP1",
    TRUE ~ NA_character_
  ))


# fill NA with baseline data
vars_to_add <- setdiff(names(baseline_original), names(fup_original))
baseline_subset <- baseline %>% select(c("carrs", "pid", all_of(vars_to_add)))

fup_df <- fup_long %>%
  left_join(baseline_subset, by = c("carrs", "pid"))

# unique PID: 30,874; unique HHID: 9,327; only includes "CARRS1 FUP4" "CARRS2 FUP1"
major_fup <- read_sas(paste0(path_spouses_bmi_change_folder,"/working/raw/Major_FU_2024_0520.sas7bdat")) %>% 
  select(pid, hhid, carrs, fup, relation, relation_other_spec) %>% 
  mutate(wave = case_when(
    carrs == 1 & fup == 0 ~ "CARRS1 FUP0",
    carrs == 1 & fup == 1 ~ "CARRS1 FUP1",
    carrs == 1 & fup == 2 ~ "CARRS1 FUP2",
    carrs == 1 & fup == 3 ~ "CARRS1 FUP3",
    carrs == 1 & fup == 4 ~ "CARRS1 FUP4",
    carrs == 1 & fup == 5 ~ "CARRS1 FUP5",
    carrs == 1 & fup == 6 ~ "CARRS1 FUP6",
    carrs == 2 & fup == 0 ~ "CARRS2 FUP0", 
    carrs == 2 & fup == 1 ~ "CARRS2 FUP1",
    TRUE ~ NA_character_
  ))
  
major_fup %>%
  group_by(wave) %>%
  summarise(
    total_n     = n(),
    missing_n   = sum(is.na(relation)),
    missing_pct = round(100 * missing_n / total_n, 2)
  )


carrs_all <- bind_rows(baseline_original,fup_df) %>% 
  left_join(major_fup,
            by = "pid")

saveRDS(carrs_all, paste0(path_spouses_bmi_change_folder, "/working/cleaned/harmonized carrs data.RDS"))


