rm(list=ls());gc();source(".Rprofile")

cca_bmi <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre01_bmi complete cases.RDS"))

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


############ WIDE FORMAT ####################

# convert into “husband‑wife” wide format

value_cols <- setdiff(
  names(analytic_df),
  c("hhid", "sex", "carrs", "fup")    # drop your id‐cols here
)
# unique hhid: 2,468
analytic_df_wide <- analytic_age18 %>%
  pivot_wider(
    id_cols    = c(hhid, carrs, fup),   
    names_from = sex,
    values_from = all_of(value_cols),
    names_glue = "{sex}_{.value}"
  ) %>% 
  dplyr::filter(!is.na(female_pid), !is.na(male_pid))


saveRDS(analytic_df_wide, paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre02a_wide spouse bmi complete cases.RDS"))


############ LONG FORMAT ####################
# N = 4,882; 2,441 spouses
analytic_df_long <- analytic_df_wide %>%
  pivot_longer(
    cols = -c(hhid, carrs, fup),  # keep household/time IDs
    names_to = c("sex", ".value"),
    names_pattern = "^(male|female)_(.+)$"
  ) %>%
  arrange(hhid, carrs, fup, sex)


saveRDS(analytic_df_long, paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre02b_long spouse bmi complete cases.RDS"))
