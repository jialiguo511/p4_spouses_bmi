rm(list=ls());gc();source(".Rprofile")

# unique hhid: 5,486, unique pid: 10,972
analytic_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse analytic sample.RDS"))

# convert into “husband‑wife” wide format

value_cols <- setdiff(
  names(analytic_df),
  c("hhid", "sex", "wave", "carrs", "fup", "site")    # drop your id‐cols here
)
# unique hhid: 5,487, OBS = 33.224
analytic_df_wide <- analytic_df %>%
  pivot_wider(
    id_cols    = c(hhid, wave, carrs, fup, site),   
    names_from = sex,
    values_from = all_of(value_cols),
    names_glue = "{sex}_{.value}"
  )


saveRDS(analytic_df_wide, paste0(path_spouses_bmi_change_folder,"/working/cleaned/wide spouse analytic sample.RDS"))







