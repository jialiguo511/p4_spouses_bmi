rm(list = ls()); gc(); source(".Rprofile")

## ==== 1. Load Data & Initial Cleanup =========================================
carrs_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre03_carrs observed data.RDS")) %>%
  select(-c(educstat_other_spec, occ_other_spec, 
            edu_category, employ_category, bmibs_category, year))

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

## ==== 3. Filter Out LTFU with 'Other' Reasons ================================

lossfup_df <- recode_df %>%
  mutate(loss_fup = case_when(
    is.na(reason)    ~ 0,
    reason == 1      ~ 1,  # death
    TRUE             ~ -1  # other (drop)
  )) %>%
  dplyr::filter(loss_fup != -1) %>%
  select(-loss_fup)

## ==== 4. Reshape to Wide Format =============================================

to_wide <- function(dat) {
  baseline_cols <- c(
    "carrs","hhid","sex","age","site","dob","reason",
    "educstat","employ","occ","hhincome","famhx_htn","famhx_cvd",
    "famhx_dm","spousedyad_new","pcarrs"
  )
  id_cols <- c("pid","fup")
  
  bs_once <- dat %>%
    arrange(fup) %>%
    group_by(pid) %>%
    slice(1) %>%
    ungroup() %>%
    select(pid, all_of(baseline_cols))
  
  wide_df <- dat %>%
    select(-all_of(baseline_cols)) %>%
    pivot_wider(
      id_cols = pid,
      names_from = fup,
      values_from = setdiff(names(dat), c(baseline_cols, "pid", "fup")),
      names_glue = "{.value}_{fup}"
    ) %>%
    left_join(bs_once, by = "pid") %>%
    select(where(~ !all(is.na(.))))  # drop fully missing columns
  
  return(wide_df)
}

carrs1_df <- filter(lossfup_df, carrs == 1)
carrs2_df <- filter(lossfup_df, carrs == 2)

carrs1_wide <- to_wide(carrs1_df)
carrs2_wide <- to_wide(carrs2_df)

## ==== 5. Pre-imputation Typing ==============================================

prep_data <- function(df) {
  df %>%
    mutate(
      across(where(is.character), as.factor),
      across(where(~ n_distinct(.) == 2 && !is.factor(.)), as.factor),
      across(where(~ n_distinct(.) <= 7 && !is.character(.)), as.factor),
      hhincome = as.factor(hhincome)
    )
}

carrs1_prep <- prep_data(carrs1_wide)
carrs2_prep <- prep_data(carrs2_wide)

saveRDS(carrs1_prep,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbhc01a_carrs1_prep.RDS")) 
saveRDS(carrs2_prep,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbhc01b_carrs2_prep.RDS")) 



