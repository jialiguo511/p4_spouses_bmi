rm(list=ls());gc();source(".Rprofile")

carrs_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbpre02_carrs harmonized data.RDS"))
  
  
### EXCLUDED LOSS-TO-FOLLOWUP ###
  
# 1) Build the required visit grid per person
target_fup <- carrs_df %>%
  distinct(pid, carrs) %>%
  mutate(fup = if_else(carrs == 1L, list(0:7), list(0:2))) %>%
  unnest(fup)

# 2) Join back to original to keep all variables; missing visits become NA rows
carrs_full <- target_fup %>%
  left_join(carrs_df, by = c("pid", "carrs", "fup")) %>%
  arrange(pid, carrs, fup) %>% 
  mutate(
    observed = case_when(
      if_all(-c(pid, hhid, carrs, pcarrs, fup, sex, site, spousedyad_new, height_cm, reason), is.na) ~ 0,   # all non-ID cols are NA
      TRUE ~ 1 # any non-missing data
    ), 
    # Reason of missingness in follow-up visits (categorical) (1: next time, 2: moved away, 3: refused/not interested, 4: death, 5: others)
    reason_new = case_when(
      observed == 1 & is.na(reason) ~ 0,          # data available, show up 
      observed == 1 & reason %in% c(1,2,3,5) ~ 6, # data available, but give reason 
      observed == 0 & is.na(reason) ~ 7,          # data unavailable, and missing reason
      TRUE ~ reason
    )) %>% 
  arrange(pid,carrs,fup) %>% 
  group_by(pid) %>%
  mutate(ava_status = paste(reason_new, collapse = ",")) %>% # carrs1 8 visits; carrs2 3 visits
  ungroup()


### exclude OBS after death 
# Identify pids whose ava_status includes a 4 token
pids_with_death <- carrs_full %>%
  distinct(pid, ava_status) %>%
  dplyr::filter(str_detect(ava_status, "(^|,)4(,|$)")) %>%
  pull(pid)

carrs_keep_death <- carrs_full %>% 
  dplyr::filter(pid %in% pids_with_death)

carrs_death_trim <- carrs_keep_death %>%
  group_by(pid) %>%
  # find the first followup where reason_new == 4 (death)
  mutate(first_death_fup = min(fup[reason_new == 4], na.rm = TRUE)) %>%
  # filter to keep only visits before death
  dplyr::filter(fup < first_death_fup) %>%
  dplyr::filter(reason_new != 7) %>%
  mutate(ava_status = paste(reason_new, collapse = ",")) %>%
  ungroup()

### exclude any "7"
carrs_no_death <- carrs_full %>% 
  dplyr::filter(!pid %in% pids_with_death) %>% 
  group_by(pid) %>%
  dplyr::filter(reason_new != 7) %>%
  mutate(ava_status = paste(reason_new, collapse = ",")) %>%
  ungroup()

# N = 21,862
carrs_clean <- bind_rows(carrs_death_trim,
                         carrs_no_death) %>% 
  select(-c("first_death_fup","ava_status"))


saveRDS(carrs_clean,paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbpre03_carrs observed data.RDS")) 



