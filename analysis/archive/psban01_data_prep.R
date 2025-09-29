rm(list = ls()); gc(); source(".Rprofile")

library(purrr)
library(stringr)

## ==== 1. Load Data & Initial Cleanup =========================================
carrs_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre03_carrs observed data.RDS")) %>%
  select(-c(dob, pcarrs, reason, reason_new, lab_date, year, observed,
            edu_category, employ_category, bmibs_category, educstat_other_spec, occ_other_spec,
            fpg_30, fpg_120))

## ==== 2. Recode Variables ====================================================

# (1) Recode alcohol based on follow-up and baseline info
# (2) Recode smoking status similarly
# (3) Remove non-essential detailed variables: *_allo, *_rec, *_med, *_curr, *_ever, *_often, *_freq

recode_df <- carrs_df %>%
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
  select(-matches("(_allo|_rec|_med|_curr|_ever|_often|_freq)$"))

prep_data <- function(df) {
  df %>%
    mutate(
      across(where(is.character), as.factor),
      across(where(~ n_distinct(.) == 2 && !is.factor(.)), as.factor),
      across(where(~ n_distinct(.) <= 7 && !is.character(.)), as.factor),
      educstat = as.factor(educstat),
      hhincome = as.factor(hhincome)
    )
}

recode_df <- prep_data(recode_df)

## ==== 3. Split by visit ===================================================

# save the demographic info from baseline
demo_df <- recode_df %>% 
  dplyr::filter(fup == 0) %>% 
  select(pid,hhid,carrs,fup,site,sex,age,educstat,employ)

# only add these demographic variables to baseline data
demo_extra <- recode_df %>% 
  dplyr::filter(fup == 0) %>% 
  select(pid,hhid,carrs,fup,occ,hhincome)

# drop DEMO info
drop_demo <- c("site","sex","age","educstat","employ",
               "occ","hhincome")

# Helper to split by fup within a carrs cohort
make_fup_splits <- function(df, carrs_val, fup_target) {
  fups <- sort(intersect(unique(df$fup[df$carrs == carrs_val]), fup_target))
  map(fups, ~ df %>%
        select(-all_of(drop_demo)) %>%
        dplyr::filter(carrs == carrs_val, fup == .x)) |>
    set_names(paste0("carrs", carrs_val, "_fup", fups))
}

carrs1_list <- make_fup_splits(recode_df, carrs_val = 1, fup_target = 0:7)
carrs2_list <- make_fup_splits(recode_df, carrs_val = 2, fup_target = 0:2)

# add suffix (visit number) ----------------------------------------------------
# function to add suffix to all but ID columns
suffix_fup <- function(df, suffix) {
  df %>%
    rename_with(~ paste0(.x, suffix), .cols = -c(pid, hhid, carrs, fup))
}

# carrs1: suffix pattern = "_i{fup}"
carrs1_renamed <- imap(carrs1_list, ~ {
  fup <- sub(".*fup", "", .y)   # extract number from name
  suffix_fup(.x, paste0("_i", fup))
})

# carrs2: suffix pattern = "_ii{fup}"
carrs2_renamed <- imap(carrs2_list, ~ {
  fup <- sub(".*fup", "", .y)
  suffix_fup(.x, paste0("_ii", fup))
})


## ==== 4. Merge the previous visit & Demo =====================================

# CARRS 1
carrs1_bs <- carrs1_renamed$carrs1_fup0 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(demo_extra,
            by = c('pid','hhid','carrs','fup'))

carrs1_fup1 <- carrs1_renamed$carrs1_fup1 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(carrs1_renamed$carrs1_fup0,
            by = c('pid','hhid','carrs','fup'))

carrs1_fup2 <- carrs1_renamed$carrs1_fup2 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(carrs1_renamed$carrs1_fup1,
            by = c('pid','hhid','carrs','fup'))

carrs1_fup3 <- carrs1_renamed$carrs1_fup3 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(carrs1_renamed$carrs1_fup2,
            by = c('pid','hhid','carrs','fup'))

carrs1_fup4 <- carrs1_renamed$carrs1_fup4 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(carrs1_renamed$carrs1_fup3,
            by = c('pid','hhid','carrs','fup'))

carrs1_fup5 <- carrs1_renamed$carrs1_fup5 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(carrs1_renamed$carrs1_fup4,
            by = c('pid','hhid','carrs','fup'))

carrs1_fup6 <- carrs1_renamed$carrs1_fup6 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(carrs1_renamed$carrs1_fup5,
            by = c('pid','hhid','carrs','fup'))

carrs1_fup7 <- carrs1_renamed$carrs1_fup7 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(carrs1_renamed$carrs1_fup6,
            by = c('pid','hhid','carrs','fup'))

# CARRS 2
carrs2_bs <- carrs2_renamed$carrs2_fup0 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(demo_extra,
            by = c('pid','hhid','carrs','fup'))

carrs2_fup1 <- carrs2_renamed$carrs2_fup1 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(carrs2_renamed$carrs2_fup0,
            by = c('pid','hhid','carrs','fup'))

carrs2_fup2 <- carrs2_renamed$carrs2_fup2 %>% 
  left_join(demo_df,
            by = c('pid','hhid','carrs','fup')) %>% 
  left_join(carrs2_renamed$carrs2_fup1,
            by = c('pid','hhid','carrs','fup'))


saveRDS(carrs1_bs,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs1_fup0.RDS")) 
saveRDS(carrs1_fup1,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs1_fup1.RDS")) 
saveRDS(carrs1_fup2,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs1_fup2.RDS")) 
saveRDS(carrs1_fup3,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs1_fup3.RDS")) 
saveRDS(carrs1_fup4,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs1_fup4.RDS")) 
saveRDS(carrs1_fup5,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs1_fup5.RDS")) 
saveRDS(carrs1_fup6,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs1_fup6.RDS")) 
saveRDS(carrs1_fup7,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs1_fup7.RDS")) 

saveRDS(carrs2_bs,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs2_fup0.RDS")) 
saveRDS(carrs2_fup1,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs2_fup1.RDS")) 
saveRDS(carrs2_fup2,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psban01/carrs2_fup2.RDS")) 

