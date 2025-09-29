rm(list=ls());gc();source(".Rprofile")

# load packages
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(mice)) install.packages("mice")

# Load required packages
library(dplyr)   # For data manipulation
library(tidyr)   # For data reshaping
library(mice)

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
  
  # Load the data
  prep_data <- readRDS(file_path)
  cat("Loaded dataset with", nrow(prep_data), "rows and", ncol(prep_data), "columns\n")
  
  # ---- 0) Build your variable sets ----
  doi_vars <- grep("^doi", names(prep_data), value = TRUE)
  
  # ID vars: core IDs + demographic + DOI + non-imputable vars
  id_vars <- c("pid","hhid","carrs","fup","site","sex","ceb", 
               grep("^year_", names(prep_data), value = TRUE),
               grep("^pcarrs_", names(prep_data), value = TRUE),
               grep("^reason", names(prep_data), value = TRUE),
               grep("^observed_", names(prep_data), value = TRUE),
               doi_vars)
  
  # Remove spousedyad_new if it exists (was removed in some datasets)
  if("spousedyad_new" %in% names(prep_data)) {
    id_vars <- c(id_vars, "spousedyad_new")
  }
  
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
           matches("^dob_"), any_of("age")) %>%
    names()
  
  before_imputation <- prep_data %>%
    select(any_of(id_vars), any_of(proportion_vars), any_of(grouped_vars), any_of(continuous_vars))
  
  # Debugging: show what variables are in each category
  cat("Variables selected for imputation:", ncol(before_imputation), "\n")
  cat("- ID vars:", length(intersect(id_vars, names(before_imputation))), "\n")
  cat("- Binary factors:", length(intersect(proportion_vars, names(before_imputation))), "\n")
  cat("- Multi-level factors:", length(intersect(grouped_vars, names(before_imputation))), "\n") 
  cat("- Continuous vars:", length(intersect(continuous_vars, names(before_imputation))), "\n")
  
  # Show which variables are NOT selected (potential missing variables)
  all_vars <- names(prep_data)
  selected_vars <- names(before_imputation)
  missing_vars <- setdiff(all_vars, selected_vars)
  if(length(missing_vars) > 0) {
    cat("- Variables NOT selected:", length(missing_vars), "\n")
    cat("  Missing vars:", paste(missing_vars, collapse=", "), "\n")
  }
  
  # Show missing data patterns
  cat("\n--- Missing Data Summary ---\n")
  missing_counts <- sapply(before_imputation, function(x) sum(is.na(x)))
  complete_vars <- names(missing_counts[missing_counts == 0])
  incomplete_vars <- names(missing_counts[missing_counts > 0])
  
  cat("Complete variables (no imputation needed):", length(complete_vars), "\n")
  if(length(complete_vars) > 0) {
    cat("  Complete:", paste(complete_vars, collapse=", "), "\n")
  }
  
  cat("Variables with missing data (will be imputed):", length(incomplete_vars), "\n")
  if(length(incomplete_vars) > 0) {
    cat("  Will impute:", paste(incomplete_vars, collapse=", "), "\n")
  }
  
  # Show which variables are NOT selected (potential missing variables)
  all_vars <- names(prep_data)
  selected_vars <- names(before_imputation)
  missing_vars <- setdiff(all_vars, selected_vars)
  if(length(missing_vars) > 0) {
    cat("- Variables NOT selected:", length(missing_vars), "\n")
    cat("  Missing vars:", paste(missing_vars, collapse=", "), "\n")
  }
  
  # ---- 1) Initialize mice objects ----
  mi0    <- mice(before_imputation, maxit = 0, print = FALSE)
  method <- mi0$method           # named vector, length = ncol(data)
  pred   <- mi0$predictorMatrix  # matrix (ncol x ncol)
  nm     <- names(before_imputation)
  
  # ---- 2) Exclude IDs from being imputed or used as predictors ----
  to_zero <- intersect(id_vars, nm)
  method[to_zero] <- ""
  pred[ , to_zero] <- 0
  pred[to_zero, ]  <- 0
  
  # ---- 3) Assign methods correctly by type ----
  # Binary factors: logreg ; 3+ levels: polyreg ; Numeric: pmm
  bin_fac  <- intersect(proportion_vars, nm)
  multi_fac<- intersect(grouped_vars, nm)
  num_vars <- nm[ sapply(before_imputation, is.numeric) & !(nm %in% id_vars) ]
  
  method[bin_fac]   <- "logreg"
  method[multi_fac] <- "polyreg"
  method[num_vars]  <- "pmm"
  
  # ---- 4) Passive rules: weight_kg = BMI * (height_cm/100)^2 ----
  # Find all weight variables and create corresponding passive rules
  weight_vars <- grep("^weight_kg", nm, value = TRUE)
  
  for(wt in weight_vars) {
    # Extract the suffix pattern (e.g., "_i0", "_ii2")
    suffix <- gsub("weight_kg", "", wt)
    bmi_var <- paste0("bmi", suffix)
    height_var <- paste0("height_cm", suffix)
    
    if (all(c(bmi_var, height_var) %in% nm)) {
      # mark as passive
      method[wt] <- paste0("~I(", bmi_var, " * (", height_var, " / 100)^2)")
      # passive vars should not be predicted from others
      pred[, wt] <- 0
      cat("Added passive rule for", wt, "\n")
    }
  }
  
  # ---- 5) (Important) Provide explicit blocks to avoid the mismatch ----
  blocks <- lapply(nm, identity)
  names(blocks) <- nm
  
  # ---- 6) Run mice ----
  cat("Starting mice imputation...\n")
  mi_dfs <- mice(
    before_imputation,
    m = 30,
    maxit = 20,
    method = method,
    predictorMatrix = pred,
    blocks = blocks,
    seed = 123
  )
  
  # Save the imputed dataset
  output_path <- paste0(path_spouses_bmi_change_folder,"/working/imputed/", dataset_name, "_mi_dfs.RDS")
  saveRDS(mi_dfs, output_path)
  
  return(mi_dfs)
}

output_dir <- paste0(path_spouses_bmi_change_folder,"/working/preprocessing/mi_dfs/")

# Process each dataset
imputed_results <- list()

for(i in seq_along(datasets_to_impute)) {
  dataset_name <- names(datasets_to_impute)[i]
  file_path <- datasets_to_impute[[i]]
  
  try({
    imputed_results[[dataset_name]] <- perform_imputation(dataset_name, file_path)
  }, silent = FALSE)
}

# Summary of imputation results
cat("\n=== IMPUTATION SUMMARY ===\n")
cat("Successfully imputed", length(imputed_results), "datasets:\n")
for(name in names(imputed_results)) {
  cat("-", name, "\n")
}

# Create a summary table of imputation results
if(length(imputed_results) > 0) {
  summary_df <- data.frame(
    Dataset = character(),
    Original_Vars = numeric(),
    Imputed_Vars = numeric(),
    N_Observations = numeric(),
    M_Imputations = numeric(),
    Iterations = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(name in names(imputed_results)) {
    mi_obj <- imputed_results[[name]]
    summary_df <- rbind(summary_df, data.frame(
      Dataset = name,
      Original_Vars = ncol(mi_obj$data),
      Imputed_Vars = sum(mi_obj$nmis > 0),
      N_Observations = nrow(mi_obj$data),
      M_Imputations = mi_obj$m,
      Iterations = mi_obj$iteration,
      stringsAsFactors = FALSE
    ))
  }
  
  # Save summary
  write.csv(summary_df, 
            paste0(path_spouses_bmi_change_folder,"/working/imputed/imputation_summary.csv"),
            row.names = FALSE)
  
  print(summary_df)
}


