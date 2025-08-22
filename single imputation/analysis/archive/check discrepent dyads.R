rm(list=ls());gc();source(".Rprofile")

library(haven)
library(openxlsx)

# sex: 1-male, 2-female
# unique pid: 21,862
spousedyads <- read.xlsx(paste0(path_spouses_bmi_change_folder,"/working/raw/spousedyads.xlsx")) %>% 
  mutate(sex = case_when(sex == 1 ~ "male",
                         TRUE ~ "female"))

disc_dyads <- spousedyads %>% 
  group_by(hhid) %>% 
  dplyr::filter(
    (any(sex == "male" & spousedyad == 0) & 
      any(sex == "female" & spousedyad == 1)) |
      (any(sex == "male" & spousedyad == 1) & 
      any(sex == "female" & spousedyad == 0))
  ) %>%
  ungroup()

write.csv(disc_dyads, paste0(path_spouses_bmi_change_folder,"/working/cleaned/discrepant dyads.csv"))


spousedyads_clean <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouseyads cleaned.RDS"))

disc_dyads <- spousedyads_clean %>% 
  group_by(hhid) %>% 
  dplyr::filter(
    (any(sex == "male" & spousedyad_new == 0) & 
       any(sex == "female" & spousedyad_new == 1)) |
      (any(sex == "male" & spousedyad_new == 1) & 
         any(sex == "female" & spousedyad_new == 0))
  ) %>%
  ungroup()