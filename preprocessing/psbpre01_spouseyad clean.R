rm(list=ls());gc();source(".Rprofile")

library(openxlsx)

# sex: 1-male, 2-female
# unique pid: 21,862; unique hhid: 12,087
spousedyads <- read.xlsx(paste0(path_spouses_bmi_change_folder,"/working/raw/spousedyads.xlsx")) %>% 
  mutate(sex = case_when(sex == 1 ~ "male",
                         TRUE ~ "female"))

# 22 spouses with same sex
duplicate_hhid <- spousedyads %>%
  group_by(hhid, city, sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  dplyr::filter(n > 1)

duplicate_df <- spousedyads %>% 
  dplyr::filter(hhid %in% duplicate_hhid$hhid)
  
# convert into “husband‑wife” wide format
value_cols <- setdiff(
  names(spousedyads),
  c("hhid", "sex", "city")    # drop your id‐cols here
)
# unique hhid: 12,087
spousedyads_wide <- spousedyads %>%
  dplyr::filter(!hhid %in% duplicate_hhid$hhid) %>% 
  pivot_wider(
    id_cols    = c(hhid, city),   
    names_from = sex,
    values_from = all_of(value_cols),
    names_glue = "{sex}_{.value}"
  ) %>% 
  mutate(spousedyad_new = case_when(
    male_rel_fem == 1 | male_s1a_rf == 1 | male_spouse_enroled == 1 ~ 1,
    TRUE ~ 0
  ))


# convert it back to long format
value_cols_long <- names(spousedyads_wide) %>%
  setdiff(c("hhid", "city", "spousedyad_new"))

spousedyads_long <- spousedyads_wide %>%
  pivot_longer(
    cols = -c(hhid, city, spousedyad_new),
    names_to = c("sex", "variable"),
    names_pattern = "^([^_]+)_(.+)$"  # split into sex and variable
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>% 
  dplyr::filter(!is.na(pid)) %>% 
  bind_rows(duplicate_df)


saveRDS(spousedyads_long, paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouseyads cleaned.RDS"))
write.csv(spousedyads_long, paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouseyads cleaned.csv"))
