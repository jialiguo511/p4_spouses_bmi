rm(list = ls()); gc(); source(".Rprofile")

library(mice)


## ==== 1. Load Data & Initial Cleanup =========================================
carrs_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/psbpre03_carrs observed data.RDS")) %>%
  select(-c(educstat_other_spec, occ_other_spec, 
            edu_category, employ_category, bmibs_category, year))

## ==== 2. Recode Variables ====================================================

# (1) Recode alcohol based on follow-up and baseline info
# (2) Recode smoking status similarly
# (3) Remove non-essential detailed variables: *_allo, *_rec, *_med, *_curr, *_ever, *_often, *_freq

recode_df <- carrs_df %>%
  mutate(
    alc_overall = case_when(
      alc_curr == 1 ~ 1,  # current (follow-up)
      alc_curr == 0 ~ 3,  # assume never
      alc_often %in% c(1, 2) | alc_overall == 1 ~ 1,  # current (baseline)
      alc_often %in% c(3, 4) | alc_overall == 2 ~ 2,  # former
      is.na(alc_often) | alc_overall == 0 ~ 0,        # never
      TRUE ~ alc_overall
    ),
    smk_overall = case_when(
      smk_curr == 1 | smk_smoke_freq %in% c(1, 2) | smk_chew_freq %in% c(1, 2) ~ 1, # current
      smk_curr == 0 | smk_ever == 0 | smk_overall == 0 ~ 0,                         # never
      smk_ever == 1 | smk_overall == 2 ~ 2,                                        # former
      TRUE ~ smk_overall
    )
  ) %>%
  select(-matches("(_allo|_rec|_med|_curr|_ever|_often|_freq)$"))

## ==== 3. Filter Out LTFU with 'Other' Reasons ================================

lossfup_df <- recode_df %>%
  mutate(loss_fup = case_when(
    is.na(reason)    ~ 0,
    reason == 1      ~ 1,  # death
    TRUE             ~ -1  # other (drop)
  )) %>%
  dplyr::filter(loss_fup != -1) %>%
  select(-loss_fup)

## ==== 4. Reshape to Wide Format =============================================

to_wide <- function(dat) {
  baseline_cols <- c(
    "carrs","hhid","sex","age","site","dob","reason",
    "educstat","employ","occ","hhincome","famhx_htn","famhx_cvd",
    "famhx_dm","spousedyad_new","pcarrs"
  )
  id_cols <- c("pid","fup")
  
  bs_once <- dat %>%
    arrange(fup) %>%
    group_by(pid) %>%
    slice(1) %>%
    ungroup() %>%
    select(pid, all_of(baseline_cols))
  
  wide_df <- dat %>%
    select(-all_of(baseline_cols)) %>%
    pivot_wider(
      id_cols = pid,
      names_from = fup,
      values_from = setdiff(names(dat), c(baseline_cols, "pid", "fup")),
      names_glue = "{.value}_{fup}"
    ) %>%
    left_join(bs_once, by = "pid") %>%
    select(where(~ !all(is.na(.))))  # drop fully missing columns
  
  return(wide_df)
}

carrs1_df <- filter(lossfup_df, carrs == 1)
carrs2_df <- filter(lossfup_df, carrs == 2)

carrs1_wide <- to_wide(carrs1_df)
carrs2_wide <- to_wide(carrs2_df)

## ==== 5. Pre-imputation Typing ==============================================

prep_data <- function(df) {
  df %>%
    mutate(
      across(where(is.character), as.factor),
      across(where(~ n_distinct(.) == 2 && !is.factor(.)), as.factor),
      across(where(~ n_distinct(.) <= 7 && !is.character(.)), as.factor),
      hhincome = as.factor(hhincome)
    )
}

carrs1_prep <- prep_data(carrs1_wide)
carrs2_prep <- prep_data(carrs2_wide)

## ==== 6. Variable Buckets for MICE ==========================================
# CARRS 1
doi_vars <- grep("^doi", names(carrs1_prep), value = TRUE)
id_vars <- c("pid", "hhid", "carrs", "pcarrs", "dob", "site", "sex", "spousedyad_new", doi_vars)

proportion_vars <- names(carrs1_prep)[sapply(carrs1_prep, \(x) is.factor(x) && nlevels(x) <= 2) & !names(carrs1_prep) %in% id_vars]
grouped_vars    <- names(carrs1_prep)[sapply(carrs1_prep, \(x) is.factor(x) && nlevels(x) >  2) & !names(carrs1_prep) %in% id_vars]

continuous_vars <- carrs1_prep %>%
  select(
    starts_with("sbp"), starts_with("dbp"),
    starts_with("height"), starts_with("weight"),
    starts_with("bmi"), starts_with("waist"),
    starts_with("fpg"),starts_with("tg"),
    starts_with("hba1c"), age
  ) %>%
  names()

before_imputation_carrs1 <- carrs1_prep %>%
  select(any_of(id_vars), any_of(proportion_vars), any_of(grouped_vars), any_of(continuous_vars))

# CARRS 2
doi_vars <- grep("^doi", names(carrs2_prep), value = TRUE)
id_vars <- c("pid", "hhid", "carrs", "pcarrs", "dob", "site", "sex", "spousedyad_new", doi_vars)

proportion_vars <- names(carrs2_prep)[sapply(carrs2_prep, \(x) is.factor(x) && nlevels(x) <= 2) & !names(carrs2_prep) %in% id_vars]
grouped_vars    <- names(carrs2_prep)[sapply(carrs2_prep, \(x) is.factor(x) && nlevels(x) >  2) & !names(carrs2_prep) %in% id_vars]

continuous_vars <- carrs2_prep %>%
  select(
    starts_with("sbp"), starts_with("dbp"),
    starts_with("height"), starts_with("weight"),
    starts_with("bmi"), starts_with("waist"),
    starts_with("fpg"),starts_with("tg"),
    starts_with("hba1c"), age
  ) %>%
  names()

before_imputation_carrs2 <- carrs2_prep %>%
  select(any_of(id_vars), any_of(proportion_vars), any_of(grouped_vars), any_of(continuous_vars))

## ==== 7. MICE Setup & Passive Rules =========================================
# CARRS 1
mi0 <- mice(before_imputation_carrs1, maxit = 0)
method <- mi0$method
pred <- mi0$predictorMatrix
nm <- names(before_imputation_carrs1)

# Exclude IDs from imputation and from being predictors
method[intersect(id_vars, names(method))] <- ""
pred[intersect(id_vars, colnames(pred)), ] <- 0
pred[, intersect(id_vars, colnames(pred))] <- 0

# Assign base methods
method[proportion_vars] <- "pmm"
method[grouped_vars]    <- "polyreg"

# Passive rules: derive weight from BMI & height (per wave)
for (t in 0:7) {
  wt   <- paste0("weight_kg_", t)
  bmi  <- paste0("bmi_", t)
  h_cm <- paste0("height_cm_", t)
  if (all(c(wt, bmi, h_cm) %in% nm)) {
    method[wt] <- paste0("~I(", bmi, " * (", h_cm, " / 100)^2)")
    pred[, wt] <- 0
  }
}


# CARRS 2
mi0 <- mice(before_imputation_carrs2, maxit = 0)
method <- mi0$method
pred <- mi0$predictorMatrix
nm <- names(before_imputation_carrs2)

# Exclude IDs from imputation and from being predictors
method[intersect(id_vars, names(method))] <- ""
pred[intersect(id_vars, colnames(pred)), ] <- 0
pred[, intersect(id_vars, colnames(pred))] <- 0

# Assign base methods
method[proportion_vars] <- "pmm"
method[grouped_vars]    <- "polyreg"

# Passive rules: derive weight from BMI & height (per wave)
for (t in 0:7) {
  wt   <- paste0("weight_kg_", t)
  bmi  <- paste0("bmi_", t)
  h_cm <- paste0("height_cm_", t)
  if (all(c(wt, bmi, h_cm) %in% nm)) {
    method[wt] <- paste0("~I(", bmi, " * (", h_cm, " / 100)^2)")
    pred[, wt] <- 0
  }
}


## ==== 8. Run Imputation =====================================================
# CARRS 1
mean_na <- mean(colMeans(is.na(before_imputation_carrs1)))
m1 <- ceiling(mean_na * 100)  # 32

imp_carrs1 <- mice(
  before_imputation_carrs1,
  m = 30,
  maxit = 20,
  method = method,
  predictorMatrix = pred,
  seed = 1234
)

saveRDS(imp_carrs1, paste0(path_spouses_bmi_change_folder,"/working/cleaned/psban_carrs1 mi_dfs.RDS"))

# CARRS 2
mean_na <- mean(colMeans(is.na(before_imputation_carrs2)))
m2 <- ceiling(mean_na * 100)  # 23

imp_carrs2 <- mice(
  before_imputation_carrs2,
  m = 30,
  maxit = 20,
  method = method,
  predictorMatrix = pred,
  seed = 1234
)

saveRDS(imp_carrs2, paste0(path_spouses_bmi_change_folder,"/working/cleaned/psban_carrs2 mi_dfs.RDS"))


## ==== 9. Quick sanity checks -----------------------------------------------
# completed <- complete(imp_carrs1, "long", include = TRUE)
# plot(imp_carrs1)  # imputation model has converged well  
# 
# completed <- complete(imp_carrs2, "long", include = TRUE)
# plot(imp_carrs2)   


