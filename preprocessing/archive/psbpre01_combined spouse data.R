rm(list=ls());gc();source(".Rprofile")

library(openxlsx)

carrs_all <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cleaned/harmonized carrs data.RDS"))

# sex: 1-female, 2-male
spousedyads <- read.xlsx(paste0(path_spouses_bmi_change_folder,"/working/raw/spousedyads.xlsx")) 

spouse_df <- spousedyads %>% 
  # Self-join to bring in male spousedyad per household
  left_join(
    spousedyads %>%
      dplyr::filter(sex == 2 & spousedyad == 1) %>%
      select(hhid, male_spousedyad = spousedyad),
    by = "hhid"
  ) %>%
  mutate(
    spousedyad = case_when(
      sex == 1 & is.na(spousedyad) ~ male_spousedyad,  # fill female NA with male value
      is.na(spousedyad)            ~ 0,                # default fill remaining NAs with 0
      TRUE                         ~ spousedyad
    )
  ) %>%
  select(-male_spousedyad) 
# 266 female do no have male partner

# harmonized dataset & spouse data
analytic_df <- baseline %>%
  rename(bs_pid = pid) %>% 
  # merge with spousal data by PID, check N
  full_join(spousedyads %>% 
              rename(spo_pid = pid),
            by = c("pid","hhid","sex")) %>% 
  arrange(hhid,pid,wave) %>% 
  select(bs_pid, spo_pid)

analytic_df <- spousedyads %>% 
  # merge with spousal data by PID, check N
  anti_join(baseline,
            by = c("pid","hhid","sex"))

df <- bind_cols(baseline %>% 
                  select(pid,hhid) %>% 
                  rename(bs_pid = pid),
                spouse_df %>% 
                  select(pid, hhid) %>% 
                  rename(spo_pid = pid))


df<- carrs_all %>% 
  dplyr::filter(!hhid %in% df1$hhid)






# create cross-sectional dataset for each visit ----------------------------

# ignore: 1 person is transgender (sex = 3)
male_df <- carrs_all %>%
  dplyr::filter(sex == 1) %>%
  rename_with(~ paste0("male_", .), .cols = -c(hhid, carrs, wave))

female_df <- carrs_all %>%
  dplyr::filter(sex == 0) %>%
  rename_with(~ paste0("female_", .), .cols = -c(hhid, carrs, wave))

# Merge to wide format by household and wave
carrs_wide <- male_df %>%
  inner_join(female_df, by = c("hhid", "carrs", "wave"))

# spousedyad NA
# N = 9,752
match <- carrs_wide %>% 
  dplyr::filter(!is.na(male_spousedyad))

# N = 3,347
non_match <- carrs_wide %>% 
  dplyr::filter(is.na(male_spousedyad))

# spousedyads non-NA

# check relationship
relation_freq <- carrs_wide %>%
  group_by(carrs, relation) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(carrs) %>%
  mutate(prop = n / sum(n))                       
