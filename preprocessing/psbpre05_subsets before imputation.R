rm(list=ls());gc();source(".Rprofile")

carrs_recoded <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbpre04_carrs recoded data.RDS")) %>% 
  select(-c(bmibs_category,occ,spousedyad_new,edu_category,employ_category))

# 1. Prepare baseline non-NA dataset -----------------------------------------------
baseline_info <- carrs_recoded %>% 
  dplyr::filter(fup == 0) %>% 
  select(
    carrs,hhid,pid,ceb,age,sex,site,
    educstat,employ,alc_overall,smk_overall,
    famhx_htn,famhx_cvd,famhx_dm
  )

# 2. Split the harmonized dataset by visit -----------------------------------------------

# drop DEMO info
drop_demo <- c("ceb","site","sex","age","educstat","employ","alc_overall","smk_overall",
               "famhx_htn","famhx_cvd","famhx_dm")

fup_splits <- function(df, carrs_val, fup_target) {
  fups <- sort(intersect(unique(df$fup[df$carrs == carrs_val]), fup_target))
  map(fups, ~ df %>%
        select(-all_of(drop_demo)) %>%
        dplyr::filter(carrs == carrs_val, fup == .x)) |>
    set_names(paste0("carrs", carrs_val, "_fup", fups))
}

carrs1_list <- fup_splits(carrs_recoded, carrs_val = 1, fup_target = 0:7)
carrs2_list <- fup_splits(carrs_recoded, carrs_val = 2, fup_target = 0:2)

# 3. add suffix for variables (visit number) -----------------------------------------------

suffix_fup <- function(df, suffix) {
  df %>%
    rename_with(~ paste0(.x, suffix), .cols = -c(pid, hhid, carrs, fup))
}

# carrs1: suffix pattern = "_i{fup}"
carrs1_renamed <- imap(carrs1_list, ~ {
  fup <- sub(".*fup", "", .y)   # extract number from name
  suffix_fup(.x, paste0("_i", fup))
})

# carrs2: suffix pattern = "_ii{fup}"
carrs2_renamed <- imap(carrs2_list, ~ {
  fup <- sub(".*fup", "", .y)
  suffix_fup(.x, paste0("_ii", fup))
})

# 4. Delete variables that are all NA for all datasets ------------------------------------

is_all_na <- function(x) all(is.na(x))

carrs1_renamed <- map(carrs1_renamed, function(df) {
  cols_to_keep <- !sapply(df, is_all_na)
  df[, cols_to_keep, drop = FALSE]
})

carrs2_renamed <- map(carrs2_renamed, function(df) {
  cols_to_keep <- !sapply(df, is_all_na)
  df[, cols_to_keep, drop = FALSE]
})

# 5. Check variable names in each dataset -----------------------------------------------

print_var_names <- function(dataset_list, list_name) {
  cat("\n===", list_name, "===\n")
  for (i in seq_along(dataset_list)) {
    ds_name <- names(dataset_list)[i]
    var_names <- names(dataset_list[[i]])
    cat("\n", ds_name, ": ", length(var_names), " variables\n", sep="")
    cat(paste(var_names, collapse=", "), "\n")
  }
}

print_var_names(carrs1_renamed, "CARRS-1 Datasets")
print_var_names(carrs2_renamed, "CARRS-2 Datasets")

cat("\n=== Baseline Info ===\n")
cat(paste(names(baseline_info), collapse=", "), "\n")



### BEFORE IMPUTATION DATA PREP ###

# Baseline + complete demo info
carrs1_bs <- carrs1_renamed$carrs1_fup0 %>% 
  left_join(baseline_info,
            by = c('pid','hhid','carrs')) 

carrs2_bs <- carrs2_renamed$carrs2_fup0 %>% 
  left_join(baseline_info,
            by = c('pid','hhid','carrs')) 

saveRDS(carrs1_bs, paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs1_bs.RDS"))
saveRDS(carrs2_bs, paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs2_bs.RDS"))


# Follow-up + Demo + Other info from prior visits 

carrs1_fup2 <- carrs1_renamed$carrs1_fup2 %>%
  left_join(baseline_info,
            by = c('pid','hhid','carrs')) %>% 
  left_join(carrs1_renamed$carrs1_fup1 %>% 
              select(-fup),
            by = c('pid','hhid','carrs')) %>% 
  left_join(carrs1_renamed$carrs1_fup0 %>% 
              select(-c(intersect(names(carrs1_renamed$carrs1_fup0), 
                      c("fup","doi_i0", "htn_i0", "dm_i0", "hld_i0", "cva_i0", "ckd_i0", "cancer_i0", 
                        "height_cm_i0", "year_i0", "pcarrs_i0", "reason_i0", "observed_i0", 
                        "reason_new_i0")))),
            by = c('pid','hhid','carrs'))

carrs1_fup4 <- carrs1_renamed$carrs1_fup4 %>%
  left_join(baseline_info,
            by = c('pid','hhid','carrs')) %>% 
  left_join(carrs1_renamed$carrs1_fup3 %>% 
              select(-fup),
            by = c('pid','hhid','carrs')) %>% 
  left_join(carrs1_renamed$carrs1_fup2 %>% 
              select(-c(intersect(names(carrs1_renamed$carrs1_fup2), 
                                  c("fup","doi_i3", "htn_i3", "dm_i3", "hld_i3", "cva_i3", "ckd_i3", "cancer_i3", 
                                    "height_cm_i3", "year_i3", "pcarrs_i3", "reason_i3", "observed_i3", 
                                    "reason_new_i3")))),
            by = c('pid','hhid','carrs'))


carrs1_fup7 <- carrs1_renamed$carrs1_fup7 %>%
  left_join(baseline_info,
            by = c('pid','hhid','carrs')) %>% 
  left_join(carrs1_renamed$carrs1_fup6 %>% 
              select(-fup),
            by = c('pid','hhid','carrs')) %>% 
  left_join(carrs1_renamed$carrs1_fup4 %>% 
              select(-c(intersect(names(carrs1_renamed$carrs1_fup4), 
                                  c("fup","doi_i4", "htn_i4", "dm_i4", "hld_i4", "cva_i4", "ckd_i4", "cancer_i4", 
                                    "height_cm_i4", "year_i4", "pcarrs_i4", "reason_i4", "observed_i4", 
                                    "reason_new_i4")))),
            by = c('pid','hhid','carrs'))


carrs2_fup2 <- carrs2_renamed$carrs2_fup2 %>%
  left_join(baseline_info,
            by = c('pid','hhid','carrs')) %>% 
  left_join(carrs2_renamed$carrs2_fup1 %>% 
              select(-fup),
            by = c('pid','hhid','carrs')) %>% 
  left_join(carrs2_renamed$carrs2_fup0 %>% 
              select(-c(intersect(names(carrs2_renamed$carrs2_fup0), 
                                  c("fup","doi_ii0", "htn_ii0", "dm_ii0", "hld_ii0", "cva_ii0", "ckd_ii0", "cancer_ii0", 
                                    "sbp1_ii0", "sbp2_ii0", "sbp3_ii0", "dbp1_ii0", "dbp2_ii0", "dbp3_ii0", 
                                    "height_cm_ii0", "weight_kg_ii0", "bmi_ii0", "waist_cm_ii0", "hip_cm_ii0", 
                                    "year_ii0", "pcarrs_ii0", "spousedyad_new_ii0", "observed_ii0", "reason_new_ii0")))),
            by = c('pid','hhid','carrs'))


saveRDS(carrs1_fup2, paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs1_fup2.RDS"))
saveRDS(carrs1_fup4, paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs1_fup4.RDS"))
saveRDS(carrs1_fup7, paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs1_fup7.RDS"))
saveRDS(carrs2_fup2, paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs2_fup2.RDS"))


# Create summary table for variables -----------------------------------------------

create_dataset_summary <- function(df, name) {
  summary_df <- data.frame(
    Dataset = rep(name, ncol(df)),
    Variable = names(df),
    N_Total = nrow(df),
    Valid_N = sapply(df, function(x) sum(!is.na(x))),
    Missing_N = sapply(df, function(x) sum(is.na(x))),
    Percent_Missing = sapply(df, function(x) round(sum(is.na(x))/length(x)*100, 1)),
    Mean = sapply(df, function(x) if(is.numeric(x)) round(mean(x, na.rm = TRUE), 2) else NA)
  )
  return(summary_df)
}

all_summaries <- rbind(
  create_dataset_summary(carrs1_bs, "carrs1_bs"),
  create_dataset_summary(carrs1_fup2, "carrs1_fup2"),
  create_dataset_summary(carrs1_fup4, "carrs1_fup4"),
  create_dataset_summary(carrs1_fup7, "carrs1_fup7"),
  create_dataset_summary(carrs2_bs, "carrs2_bs"),
  create_dataset_summary(carrs2_fup2, "carrs2_fup2")
)

write.csv(all_summaries, 
          paste0(path_spouses_bmi_change_folder,
                 "/working/preprocessing/before_imputation/all_followups_summary.csv"),
          row.names = FALSE)


print_dataset_vars <- function(df_name, df) {
  cat("\n==== Variables in", df_name, "====\n")
  cat("Total variables:", length(names(df)), "\n")
  cat(paste(names(df), collapse=", "), "\n\n")
}

# Print variable names for all datasets
print_dataset_vars("carrs1_bs", carrs1_bs)
print_dataset_vars("carrs1_fup2", carrs1_fup2)
print_dataset_vars("carrs1_fup4", carrs1_fup4)
print_dataset_vars("carrs1_fup7", carrs1_fup7)
print_dataset_vars("carrs2_bs", carrs2_bs)
print_dataset_vars("carrs2_fup2", carrs2_fup2)



