rm(list=ls());gc();source(".Rprofile")

# unique hhid: 4,045, unique pid: 8,090
analytic_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse analytic sample.RDS"))

# convert into “husband‑wife” wide format

value_cols <- setdiff(
  names(analytic_df),
  c("hhid", "sex", "wave", "carrs", "fup", "site")    # drop your id‐cols here
)
# unique hhid: 4,045, OBS = 22,719
analytic_df_wide <- analytic_df %>%
  pivot_wider(
    id_cols    = c(hhid, wave, carrs, fup, site),   
    names_from = sex,
    values_from = all_of(value_cols),
    names_glue = "{sex}_{.value}"
  )


saveRDS(analytic_df_wide, paste0(path_spouses_bmi_change_folder,"/working/cleaned/wide spouse analytic sample.RDS"))







