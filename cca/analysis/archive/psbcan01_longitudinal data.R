rm(list=ls());gc();source(".Rprofile")

# unique hhid: 2,468, unique pid: 4,936
analytic_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse bmi complete cases.RDS"))

# convert into “husband‑wife” wide format

value_cols <- setdiff(
  names(analytic_df),
  c("hhid", "sex", "carrs", "fup")    # drop your id‐cols here
)
# unique hhid: 2,468
analytic_df_wide <- analytic_df %>%
  pivot_wider(
    id_cols    = c(hhid, carrs, fup),   
    names_from = sex,
    values_from = all_of(value_cols),
    names_glue = "{sex}_{.value}"
  ) %>% 
  dplyr::filter(!is.na(female_pid), !is.na(male_pid))


saveRDS(analytic_df_wide, paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcan01_wide spouse bmi complete cases.RDS"))







