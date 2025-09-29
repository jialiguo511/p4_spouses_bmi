# load packages
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(mice)) install.packages("mice")

# Load required packages
library(dplyr)   # For data manipulation
library(tidyr)   # For data reshaping
library(mice)

carrs1_df <- readRDS("psbhc01a_carrs1_prep.RDS") 
# carrs1_prep <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/preprocessing/psbhc01a_carrs1_prep.RDS")) 

# ---- 0) Build your variable sets (your code) ----
doi_vars <- grep("^doi", names(carrs1_prep), value = TRUE)
id_vars  <- c("pid","hhid","carrs","pcarrs","dob","site","sex","spousedyad_new", doi_vars)

proportion_vars <- names(carrs1_prep)[sapply(carrs1_prep, \(x) is.factor(x) && nlevels(x) <= 2) & !names(carrs1_prep) %in% id_vars]
grouped_vars    <- names(carrs1_prep)[sapply(carrs1_prep, \(x) is.factor(x) && nlevels(x) >  2) & !names(carrs1_prep) %in% id_vars]

continuous_vars <- carrs1_prep %>%
  select(starts_with("sbp"), starts_with("dbp"),
         starts_with("height"), starts_with("weight"),
         starts_with("bmi"), starts_with("waist"),
         starts_with("fpg"), starts_with("tg"),
         starts_with("hba1c"), age) %>%
  names()

before_imputation_carrs1 <- carrs1_prep %>%
  select(any_of(id_vars), any_of(proportion_vars), any_of(grouped_vars), any_of(continuous_vars))

# ---- 1) Initialize mice objects ----
mi0    <- mice(before_imputation_carrs1, maxit = 0, print = FALSE)
method <- mi0$method           # named vector, length = ncol(data)
pred   <- mi0$predictorMatrix  # matrix (ncol x ncol)
nm     <- names(before_imputation_carrs1)

# ---- 2) Exclude IDs from being imputed or used as predictors ----
to_zero <- intersect(id_vars, nm)
method[to_zero] <- ""
pred[ , to_zero] <- 0
pred[to_zero, ]  <- 0

# ---- 3) Assign methods correctly by type ----
# Binary factors: logreg ; 3+ levels: polyreg ; Numeric: pmm
bin_fac  <- intersect(proportion_vars, nm)
multi_fac<- intersect(grouped_vars, nm)
num_vars <- nm[ sapply(before_imputation_carrs1, is.numeric) & !(nm %in% id_vars) ]

method[bin_fac]   <- "logreg"
method[multi_fac] <- "polyreg"
method[num_vars]  <- "pmm"

# ---- 4) Passive rules: weight_kg_t = BMI_t * (height_cm_t/100)^2 ----
for (t in 0:7) {
  wt   <- paste0("weight_kg_", t)
  bmi  <- paste0("bmi_", t)
  h_cm <- paste0("height_cm_", t)
  if (all(c(wt, bmi, h_cm) %in% nm)) {
    # mark as passive
    method[wt] <- paste0("~I(", bmi, " * (", h_cm, " / 100)^2)")
    # passive vars should not be predicted from others
    pred[, wt] <- 0
  }
}

# ---- 5) (Important) Provide explicit blocks to avoid the mismatch ----
blocks <- lapply(nm, identity)
names(blocks) <- nm

# ---- 6) Run mice ----
imp_carrs1 <- mice(
  before_imputation_carrs1,
  m = 30,
  maxit = 20,
  method = method,
  predictorMatrix = pred,
  blocks = blocks,
  seed = 123
)

saveRDS(imp_carrs1,"psbhc02a_carrs1_mi_dfs.RDS") 
