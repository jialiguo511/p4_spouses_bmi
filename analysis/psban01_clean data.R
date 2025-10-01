rm(list=ls());gc();source(".Rprofile")

library(mice)

carrs1_bs_mi <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/mi_dfs/carrs1_bs_mi_dfs.RDS"))
carrs1_fup2_mi <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/mi_dfs/carrs1_fup2_mi_dfs.RDS"))
carrs1_fup4_mi <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/mi_dfs/carrs1_fup4_mi_dfs.RDS"))
carrs1_fup7_mi <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/mi_dfs/carrs1_fup7_mi_dfs.RDS"))
carrs2_bs_mi <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/mi_dfs/carrs2_bs_mi_dfs.RDS"))
carrs2_fup2_mi <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/mi_dfs/carrs2_fup2_mi_dfs.RDS"))

# check
carrs1_bs_complete <- complete(carrs1_bs_mi, action = 1) # N = 12,271
carrs1_fup2_complete <- complete(carrs1_fup2_mi, action = 1) # 12,096
carrs1_fup4_complete <- complete(carrs1_fup4_mi, action = 1) # 11,813
carrs1_fup7_complete <- complete(carrs1_fup7_mi, action = 1) # 10,837
carrs2_bs_complete <- complete(carrs2_bs_mi, action = 1) # 9,591
carrs2_fup2_complete <- complete(carrs2_fup2_mi, action = 1) # 8,834


na_summary_fup2 <- carrs1_fup2_complete %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(
    total_rows = nrow(carrs1_fup2_complete),
    na_percentage = round((na_count / total_rows) * 100, 2)
  ) %>%
  arrange(desc(na_percentage))


# -------------------------------------------------------------------------------------------------------------------
source("functions/egfr_ckdepi_2021.R")

# Function to process each dataset
process_visit_data <- function(mi_object, dataset_name, visit_suffix, visit_number) {
  
  cat("Processing", dataset_name, "- keeping variables with suffix", visit_suffix, "\n")
  
  # Extract all imputed datasets
  all_imputations <- complete(mi_object, action = "long", include = TRUE)
  
  # Define variables to keep (no suffix + current visit suffix)
  all_vars <- names(all_imputations)
  
  # Variables without suffix (baseline variables)
  no_suffix_vars <- c("pid","hhid","carrs","fup","ceb","site","sex","age",
                      "educstat","employ","alc_overall","smk_overall",
                      "famhx_htn","famhx_cvd","famhx_dm")
  
  # Variables with current visit suffix
  visit_vars <- all_vars[grepl(paste0(visit_suffix, "$"), all_vars)]
  
  # Keep mice metadata columns
  mice_vars <- c(".imp", ".id")
  
  # Combine all variables to keep
  vars_to_keep <- c(mice_vars, no_suffix_vars, visit_vars)
  vars_to_keep <- intersect(vars_to_keep, all_vars)  # Only keep existing variables
  
  cat("  Keeping", length(vars_to_keep), "variables\n")
  
  # Select relevant variables
  processed_data <- all_imputations %>%
    select(all_of(vars_to_keep)) %>%
    mutate(
      visit = visit_number,
      dataset_source = dataset_name,
      
      # Add derived variables
      female = case_when(sex == "female" ~ 1, TRUE ~ 0),
      
      # eGFR calculation (handle different suffix patterns)
      egfr_ckdepi_2021 = case_when(
        !is.na(get(paste0("serum_creatinine", visit_suffix))) ~ 
          egfr_ckdepi_2021(scr = get(paste0("serum_creatinine", visit_suffix)), 
                           female = female, 
                           age = age),
        TRUE ~ NA_real_
      ),
      
      # Weight-to-height ratio
      whtr = case_when(
        !is.na(get(paste0("weight_kg", visit_suffix))) & 
          !is.na(get(paste0("height_cm", visit_suffix))) ~ 
          get(paste0("weight_kg", visit_suffix)) / get(paste0("height_cm", visit_suffix)),
        TRUE ~ NA_real_
      )
    )
  
  # Rename variables to remove visit suffix for harmonization
  rename_map <- setNames(
    paste0(gsub(paste0(visit_suffix, "$"), "", visit_vars), visit_suffix),
    gsub(paste0(visit_suffix, "$"), "", visit_vars)
  )
  
  # Apply renaming
  for(old_name in names(rename_map)) {
    if(rename_map[old_name] %in% names(processed_data)) {
      names(processed_data)[names(processed_data) == rename_map[old_name]] <- old_name
    }
  }
  
  return(processed_data)
}

# Process each dataset
carrs1_bs_processed <- process_visit_data(carrs1_bs_mi, "carrs1_bs", "_i0", 0)
carrs1_fup2_processed <- process_visit_data(carrs1_fup2_mi, "carrs1_fup2", "_i2", 2)
carrs1_fup4_processed <- process_visit_data(carrs1_fup4_mi, "carrs1_fup4", "_i4", 4)
carrs1_fup7_processed <- process_visit_data(carrs1_fup7_mi, "carrs1_fup7", "_i7", 7)
carrs2_bs_processed <- process_visit_data(carrs2_bs_mi, "carrs2_bs", "_ii0", 0)
carrs2_fup2_processed <- process_visit_data(carrs2_fup2_mi, "carrs2_fup2", "_ii2", 2)


# Get all unique variables across all datasets (excluding mice metadata)
all_processed_data <- list(carrs1_bs_processed, carrs1_fup2_processed, carrs1_fup4_processed, 
                           carrs1_fup7_processed, carrs2_bs_processed, carrs2_fup2_processed)

all_variables <- unique(unlist(lapply(all_processed_data, names)))
core_variables <- setdiff(all_variables, c(".imp", ".id"))

# Create harmonized datasets for each imputation (1-30)
harmonized_datasets <- list()

for(imp_num in 1:30) {
  cat("Creating harmonized dataset for imputation", imp_num, "\n")
  
  # Extract specific imputation from each dataset
  imp_datasets <- list(
    carrs1_bs_processed %>% dplyr::filter(.imp == imp_num),
    carrs1_fup2_processed %>% dplyr::filter(.imp == imp_num),
    carrs1_fup4_processed %>% dplyr::filter(.imp == imp_num),
    carrs1_fup7_processed %>% dplyr::filter(.imp == imp_num),
    carrs2_bs_processed %>% dplyr::filter(.imp == imp_num),
    carrs2_fup2_processed %>% dplyr::filter(.imp == imp_num)
  )
  
  # Ensure all datasets have the same columns
  for(i in seq_along(imp_datasets)) {
    missing_cols <- setdiff(core_variables, names(imp_datasets[[i]]))
    if(length(missing_cols) > 0) {
      for(col in missing_cols) {
        imp_datasets[[i]][[col]] <- NA
      }
    }
    # Reorder columns to match
    imp_datasets[[i]] <- imp_datasets[[i]][c(".imp", ".id", core_variables)]
  }
  
  # Combine all visits for this imputation
  harmonized_datasets[[imp_num]] <- bind_rows(imp_datasets) %>%
    arrange(pid, carrs, fup)
}


saveRDS(harmonized_datasets,paste0(path_spouses_bmi_change_folder, "/working/cleaned/psban01_imputed harmonized dfs.RDS"))


# Summary
for(i in 1:5) {  # Show first 5 as example
  cat("Imputation", i, ":", nrow(harmonized_datasets[[i]]), "rows,", 
      ncol(harmonized_datasets[[i]]), "columns\n")
}

# Example: Access the first harmonized dataset
harmonized_imp1 <- harmonized_datasets[[1]] %>% 
  arrange(pid,carrs,fup)

# Check percentage of missing values
na_summary <- harmonized_imp1 %>%
  summarise(across(everything(), ~sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_count") %>%
  mutate(
    total_rows = nrow(harmonized_imp1),
    na_percentage = round((na_count / total_rows) * 100, 2)
  ) %>%
  arrange(desc(na_percentage))



