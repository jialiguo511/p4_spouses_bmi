rm(list=ls());gc();source(".Rprofile")

library(mice)
library(dplyr)
library(tidyr)
library(rlang)

# Define datasets to impute
datasets_to_impute <- list(
  "carrs1_bs" = paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs1_bs.RDS"),
  "carrs1_fup2" = paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs1_fup2.RDS"),
  "carrs1_fup4" = paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs1_fup4.RDS"),
  "carrs1_fup7" = paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs1_fup7.RDS"),
  "carrs2_bs" = paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs2_bs.RDS"),
  "carrs2_fup2" = paste0(path_spouses_bmi_change_folder,"/working/preprocessing/before_imputation/psbpre05_carrs2_fup2.RDS")
)

# Function to perform imputation 
perform_imputation <- function(dataset_name, file_path) {
  cat("\n=== Processing", dataset_name, "===\n")
  
  prep_data <- readRDS(file_path)
  cat("Loaded:", nrow(prep_data), "rows,", ncol(prep_data), "columns\n")
  
  # Build variable sets (keeping your exact selection logic)
  doi_vars <- grep("^doi", names(prep_data), value = TRUE)
  dob_vars <- grep("^dob_", names(prep_data), value = TRUE) 
  
  # ID vars: core IDs + demographic + non-imputable vars
  id_vars <- c("pid","hhid","carrs","fup","site","sex","ceb", 
               grep("^year_", names(prep_data), value = TRUE),
               grep("^pcarrs_", names(prep_data), value = TRUE),
               grep("^reason", names(prep_data), value = TRUE),
               grep("^observed_", names(prep_data), value = TRUE),
               doi_vars, dob_vars)
  
  # Binary factors: pattern-based detection
  proportion_vars <- prep_data %>%
    select(matches("^htn_"), matches("^dm_"), matches("^hld_"), 
           matches("^chd_"), matches("^cva_"), matches("^ckd_"), 
           matches("^cancer_"), matches("^smk_exp"), matches("^famhx_")) %>%
    select(-any_of(id_vars)) %>%
    names()
  
  # Multi-level factors: pattern-based detection
  grouped_vars <- prep_data %>%
    select(matches("^educstat$"), matches("^employ$"),
           matches("^alc_overall$"), matches("^smk_overall$")) %>%
    select(-any_of(id_vars)) %>%
    names()
  # Continuous vars: pattern-based detection for the actual variables in each dataset
  continuous_vars <- prep_data %>%
    select(matches("^sbp[0-9].*"), matches("^dbp[0-9].*"),
           matches("^height_cm"), matches("^weight_kg"),
           matches("^bmi"), matches("^waist_cm"), matches("^hip_cm"),
           matches("^fpg"), matches("^fpg_30"), matches("^fpg_120"),
           matches("^tg"), matches("^hba1c"), 
           matches("^chol"), matches("^hdl"), matches("^ldl"), matches("^vldl"),
           matches("^serum_creatinine"), matches("^hhincome"),
           any_of("age")) %>%
    select(-any_of(id_vars)) %>%
    names()
  
  before_imputation <- prep_data %>%
    select(any_of(id_vars), any_of(proportion_vars), any_of(grouped_vars), any_of(continuous_vars))
  
  # Variable summary
  missing_counts <- sapply(before_imputation, function(x) sum(is.na(x)))
  cat("Variables:", ncol(before_imputation), "| Complete:", sum(missing_counts == 0), "| To impute:", sum(missing_counts > 0), "\n")
  
  # Initialize mice
  mi0 <- mice(before_imputation, maxit = 0, print = FALSE)
  method <- mi0$method
  pred <- mi0$predictorMatrix
  nm <- names(before_imputation)
  
  # Exclude IDs from imputation
  to_zero <- intersect(id_vars, nm)
  method[to_zero] <- ""
  pred[, to_zero] <- 0
  pred[to_zero, ] <- 0
  
  # Set imputation methods
  method[intersect(proportion_vars, nm)] <- "logreg"
  method[intersect(grouped_vars, nm)] <- "polyreg"
  method[nm[sapply(before_imputation, is.numeric) & !(nm %in% id_vars)]] <- "pmm"
  
  # Passive rules for weight variables
  weight_vars <- grep("^weight_kg", nm, value = TRUE)
  for(wt in weight_vars) {
    suffix <- gsub("weight_kg", "", wt)
    bmi_var <- paste0("bmi", suffix)
    height_var <- paste0("height_cm", suffix)
    
    if (all(c(bmi_var, height_var) %in% nm)) {
      method[wt] <- paste0("~I(", bmi_var, " * (", height_var, " / 100)^2)")
      pred[, wt] <- 0
      cat("Added passive rule for", wt, "\n")
    }
  }
  
  # Set blocks
  blocks <- lapply(nm, identity)
  names(blocks) <- nm
  
  # Run imputation
  cat("Starting mice imputation...\n")
  mi_dfs <- mice(before_imputation, m = 30, maxit = 20, method = method, 
                 predictorMatrix = pred, blocks = blocks, seed = 123)
                 
  # Run imputation (no post-processing needed as we've already flagged extreme BMI values)
  
  # Save results immediately
  output_path <- paste0(path_spouses_bmi_change_folder,"/working/preprocessing/mi_dfs/", dataset_name, "_mi_dfs.RDS")
  saveRDS(mi_dfs, output_path)
  cat("*** SAVED:", dataset_name, "***\n")
  cat("File:", output_path, "\n")
  cat("Variables imputed:", sum(mi_dfs$nmis > 0), "of", ncol(mi_dfs$data), "\n")
  
  return(mi_dfs)
}

# Process each dataset individually - output immediately after each completes
for(i in seq_along(datasets_to_impute)) {
  dataset_name <- names(datasets_to_impute)[i]
  file_path <- datasets_to_impute[[i]]
  
  cat("\n", paste(rep("=", 60), collapse=""), "\n")
  cat("DATASET", i, "of", length(datasets_to_impute), ":", dataset_name, "\n")
  cat(paste(rep("=", 60), collapse=""), "\n")
  
  tryCatch({
    perform_imputation(dataset_name, file_path)
    cat("✓ COMPLETED:", dataset_name, "\n")
  }, error = function(e) {
    cat("✗ FAILED:", dataset_name, "-", conditionMessage(e), "\n")
  })
  
  cat("\n")
}


