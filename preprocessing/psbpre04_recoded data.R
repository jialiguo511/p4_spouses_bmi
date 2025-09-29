rm(list=ls());gc();source(".Rprofile")

source("functions/egfr_ckdepi_2021.R")


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
    )
  ) %>% 
  select(-matches("(_allo|_rec|_med|_curr|_ever|_often|_freq)$"))

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


saveRDS(carrs_age, paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbpre04_carrs recoded data.RDS"))


