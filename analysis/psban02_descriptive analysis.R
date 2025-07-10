rm(list=ls());gc();source(".Rprofile")

# unique hhid: 2,546
analytic_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse bmi complete cases.RDS")) %>% 
  arrange(hhid, pid, carrs,fup) %>%
  mutate(
    sbp = rowMeans(select(., sbp2, sbp3), na.rm = TRUE),
    dbp = rowMeans(select(., dbp2, dbp3), na.rm = TRUE)
  ) %>%
  # define disease indicators
  mutate(
    diabetes = case_when(
      fpg >= 126 | hba1c >= 6.5 | dm == 1 | dm_med == 1 | dm_rec == 1 | dm_allo == 1 ~ 1,
      TRUE ~ 0
    ),
    overweight = case_when(
      is.na(bmi) ~ NA_real_,
      bmi >= 25 ~ 1,
      TRUE ~ 0
    ),
    hypertension = case_when(
      sbp > 140 | dbp > 90 | htn == 1 | htn_med == 1 | htn_allo == 1 ~ 1,
      TRUE ~ 0
    ),
    high_tg = case_when(
      tg > 150 ~ 1,
      TRUE ~ 0
    )
  ) %>% 
  # Unknown/NA to 0 
  mutate(famhx_cvd = case_when(is.na(famhx_cvd) ~ 0, 
                               TRUE ~ famhx_cvd),
         famhx_htn = case_when(is.na(famhx_htn) ~ 0, 
                               TRUE ~ famhx_htn),
         famhx_dm = case_when(is.na(famhx_d) ~ 0, 
                               TRUE ~ famhx_dm))

baseline <- analytic_df %>%
  dplyr::filter(fup == 0) %>%
  distinct(hhid, pid, sex, .keep_all = TRUE) %>% 
  mutate(morbidity_number = rowSums(across(c(hypertension, diabetes, chd, cva, ckd), ~ .x == 1), na.rm = TRUE)) %>% 
  mutate(
    bmi_category = case_when(
      bmi >= 15 & bmi < 25 ~ "underweight or normal weight",
      bmi >= 25 & bmi < 30 ~ "overweight",
      bmi >= 30            ~ "obese",
      TRUE                 ~ NA_character_
    ),
    morbidity_category = case_when(
      morbidity_number == 0 ~ "0",
      morbidity_number == 1 ~ "single morbidity",
      TRUE                  ~ "multimorbidity"
    ),
    edu_category = case_when(
      educstat %in% c(1, 2)    ~ "college and above",
      educstat %in% c(3, 4)    ~ "high school to secondary",
      educstat %in% c(5, 6, 7) ~ "up to primary schooling",
      TRUE                     ~ NA_character_
    ),
    employ_category = case_when(
      employ %in% c(2, 3)                        ~ "Not in the labor force, student/housewives",
      employ == 4                                ~ "Not in the labor force, retired",
      employ == 5                                ~ "Unemployed",
      employ == 1 & occ %in% c(3, 4, 5)          ~ "Employed in a manual profession",
      employ == 1 & occ %in% c(1, 2)             ~ "Employed in a non-manual professional",
      TRUE                                       ~ NA_character_
    ),
    alc_curr = case_when(
      alc_overall == 1 ~ 1,
      TRUE ~ 0
    )
  )


continuous_vars <- c("age", "bmi", "sbp", "dbp", "waist_cm")  
proportion_vars <- c("smk_curr","alc_curr","famhx_htn","famhx_dm","famhx_cvd",
                     "chd","cva","ckd",
                     "diabetes","overweight","hypertension","high_tg")
grouped_vars  <- c("edu_category","employ_category", "bmi_category","morbidity_category")     

# 1. Continuous variables
continuous_tbl <- map_dfr(continuous_vars, function(var) {
  f_vals <- baseline[[var]][baseline$sex == "female"]
  m_vals <- baseline[[var]][baseline$sex == "male"]
  tibble(
    variable       = var,
    female         = sprintf("%.1f (%.1f)", mean(f_vals, na.rm = TRUE), sd(f_vals, na.rm = TRUE)),
    male           = sprintf("%.1f (%.1f)", mean(m_vals, na.rm = TRUE), sd(m_vals, na.rm = TRUE)),
    correlation    = round(cor(f_vals, m_vals, use = "pairwise.complete.obs"), 2),
    missing_female = sprintf("%.1f%%", 100 * mean(is.na(f_vals))),
    missing_male   = sprintf("%.1f%%", 100 * mean(is.na(m_vals)))
  )
})

# 2. Binary proportion variables
proportion_tbl <- map_dfr(proportion_vars, function(var) {
  f_subset <- baseline[baseline$sex == "female", ]
  m_subset <- baseline[baseline$sex == "male", ]
  
  f_n <- sum(f_subset[[var]] == 1, na.rm = TRUE)
  m_n <- sum(m_subset[[var]] == 1, na.rm = TRUE)
  f_N <- nrow(f_subset)
  m_N <- nrow(m_subset)
  
  # logistic regression
  df_glm <- baseline %>%
    transmute(outcome = as.numeric(.data[[var]]),
              sex = factor(sex, levels = c("male", "female")))
  or_val <- exp(coef(glm(outcome ~ sex, data = df_glm, family = binomial))[2])
  
  tibble(
    variable       = var,
    female         = sprintf("%d (%.1f%%)", f_n, 100 * f_n / f_N),
    male           = sprintf("%d (%.1f%%)", m_n, 100 * m_n / m_N),
    OR             = round(or_val, 2),
    missing_female = sprintf("%.1f%%", 100 * mean(is.na(f_subset[[var]]))),
    missing_male   = sprintf("%.1f%%", 100 * mean(is.na(m_subset[[var]])))
  )
})

# 3. Grouped variables
grouped_tbl <- map_dfr(grouped_vars, function(var) {
  tab <- baseline %>%
    count(level = .data[[var]], sex) %>%
    pivot_wider(names_from = sex, values_from = n, values_fill = 0) %>%
    rename(female_n = female, male_n = male) %>%
    mutate(
      female = sprintf("%d (%.1f%%)", female_n, 100 * female_n / sum(baseline$sex == "female")),
      male   = sprintf("%d (%.1f%%)", male_n, 100 * male_n / sum(baseline$sex == "male"))
    )
  
  pval <- chisq.test(table(baseline[[var]], baseline$sex))$p.value
  pval_fmt <- ifelse(pval < 0.001, "< 0.001", formatC(pval, format = "f", digits = 3))
  
  f_miss <- 100 * mean(is.na(baseline[[var]][baseline$sex == "female"]))
  m_miss <- 100 * mean(is.na(baseline[[var]][baseline$sex == "male"]))
  
  tibble(
    variable       = var,
    level          = as.character(tab$level),
    female         = tab$female,
    male           = tab$male,
    p_value        = c(pval_fmt, rep("", nrow(tab) - 1)),
    missing_female = c(sprintf("%.1f%%", f_miss), rep("", nrow(tab) - 1)),
    missing_male   = c(sprintf("%.1f%%", m_miss), rep("", nrow(tab) - 1))
  )
})

# Format for export
continuous_tbl2 <- continuous_tbl %>%
  rename(compare = correlation) %>%
  mutate(compare = sprintf("%.2f", compare),
         level = NA_character_) %>%
  select(variable, level, female, male, compare, missing_female, missing_male)

proportion_tbl2 <- proportion_tbl %>%
  rename(compare = OR) %>%
  mutate(compare = sprintf("%.2f", compare),
         level = NA_character_) %>%
  select(variable, level, female, male, compare, missing_female, missing_male)

grouped_tbl2 <- grouped_tbl %>%
  rename(compare = p_value) %>%
  select(variable, level, female, male, compare, missing_female, missing_male)

# Combine
table1 <- bind_rows(continuous_tbl2, proportion_tbl2, grouped_tbl2)

# Save
write.csv(table1, "analysis/psban02_descriptive analysis.csv", row.names = FALSE)




# odds ratio 2x2 table -------------------

# convert into “husband‑wife” wide format

value_cols <- setdiff(
  names(baseline),
  c("hhid", "sex", "carrs", "fup", "site")    # drop your id‐cols here
)
# unique hhid: 2,546
baseline_wide <- baseline %>%
  pivot_wider(
    id_cols    = c(hhid, carrs, fup, site),   
    names_from = sex,
    values_from = all_of(value_cols),
    names_glue = "{sex}_{.value}"
  )

library(epitools)

# DM
# Step 1: Create 2x2 contingency table
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_dm) & !is.na(male_dm)) %>% 
  mutate(
    female_dm = as.integer(female_dm),
    male_dm = as.integer(male_dm)
  ) %>%
  count(male_dm, female_dm) %>%
  pivot_wider(
    names_from = female_dm, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_dm)

# Construct matrix: rows = male_dm (1 = Yes, 0 = No), cols = female_dm (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_dm == 1],
    or_matrix$female_0[or_matrix$male_dm == 1],
    or_matrix$female_1[or_matrix$male_dm == 0],
    or_matrix$female_0[or_matrix$male_dm == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)


# HTN
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_hypertension) & !is.na(male_hypertension)) %>% 
  mutate(
    female_hypertension = as.integer(female_hypertension),
    male_hypertension = as.integer(male_hypertension)
  ) %>%
  count(male_hypertension, female_hypertension) %>%
  pivot_wider(
    names_from = female_hypertension, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_hypertension)

# Construct matrix: rows = male_hypertension (1 = Yes, 0 = No), cols = female_hypertension (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_hypertension == 1],
    or_matrix$female_0[or_matrix$male_hypertension == 1],
    or_matrix$female_1[or_matrix$male_hypertension == 0],
    or_matrix$female_0[or_matrix$male_hypertension == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)


# CHD
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_chd) & !is.na(male_chd)) %>% 
  mutate(
    female_chd = as.integer(female_chd),
    male_chd = as.integer(male_chd)
  ) %>%
  count(male_chd, female_chd) %>%
  pivot_wider(
    names_from = female_chd, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_chd)

# Construct matrix: rows = male_chd (1 = Yes, 0 = No), cols = female_chd (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_chd == 1],
    or_matrix$female_0[or_matrix$male_chd == 1],
    or_matrix$female_1[or_matrix$male_chd == 0],
    or_matrix$female_0[or_matrix$male_chd == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)


# CVA
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_cva) & !is.na(male_cva)) %>% 
  mutate(
    female_cva = as.integer(female_cva),
    male_cva = as.integer(male_cva)
  ) %>%
  count(male_cva, female_cva) %>%
  pivot_wider(
    names_from = female_cva, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_cva)

# Construct matrix: rows = male_cva (1 = Yes, 0 = No), cols = female_cva (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_cva == 1],
    or_matrix$female_0[or_matrix$male_cva == 1],
    or_matrix$female_1[or_matrix$male_cva == 0],
    or_matrix$female_0[or_matrix$male_cva == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)



# CKD
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_ckd) & !is.na(male_ckd)) %>% 
  mutate(
    female_ckd = as.integer(female_ckd),
    male_ckd = as.integer(male_ckd)
  ) %>%
  count(male_ckd, female_ckd) %>%
  pivot_wider(
    names_from = female_ckd, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_ckd)

# Construct matrix: rows = male_ckd (1 = Yes, 0 = No), cols = female_ckd (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_ckd == 1],
    or_matrix$female_0[or_matrix$male_ckd == 1],
    or_matrix$female_1[or_matrix$male_ckd == 0],
    or_matrix$female_0[or_matrix$male_ckd == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)



# overweight
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_overweight) & !is.na(male_overweight)) %>% 
  mutate(
    female_overweight = as.integer(female_overweight),
    male_overweight = as.integer(male_overweight)
  ) %>%
  count(male_overweight, female_overweight) %>%
  pivot_wider(
    names_from = female_overweight, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_overweight)

# Construct matrix: rows = male_overweight (1 = Yes, 0 = No), cols = female_overweight (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_overweight == 1],
    or_matrix$female_0[or_matrix$male_overweight == 1],
    or_matrix$female_1[or_matrix$male_overweight == 0],
    or_matrix$female_0[or_matrix$male_overweight == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)



# high TG
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_high_tg) & !is.na(male_high_tg)) %>% 
  mutate(
    female_high_tg = as.integer(female_high_tg),
    male_high_tg = as.integer(male_high_tg)
  ) %>%
  count(male_high_tg, female_high_tg) %>%
  pivot_wider(
    names_from = female_high_tg, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_high_tg)

# Construct matrix: rows = male_high_tg (1 = Yes, 0 = No), cols = female_high_tg (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_high_tg == 1],
    or_matrix$female_0[or_matrix$male_high_tg == 1],
    or_matrix$female_1[or_matrix$male_high_tg == 0],
    or_matrix$female_0[or_matrix$male_high_tg == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)



# alcohol

or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_alc_curr) & !is.na(male_alc_curr)) %>% 
  mutate(
    female_alc_curr = as.integer(female_alc_curr),
    male_alc_curr = as.integer(male_alc_curr)
  ) %>%
  count(male_alc_curr, female_alc_curr) %>%
  pivot_wider(
    names_from = female_alc_curr, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_alc_curr)

# Construct matrix: rows = male_alc_curr (1 = Yes, 0 = No), cols = female_alc_curr (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_alc_curr == 1],
    or_matrix$female_0[or_matrix$male_alc_curr == 1],
    or_matrix$female_1[or_matrix$male_alc_curr == 0],
    or_matrix$female_0[or_matrix$male_alc_curr == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)




# tobacco
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_smk_curr) & !is.na(male_smk_curr)) %>% 
  mutate(
    female_smk_curr = as.integer(female_smk_curr),
    male_smk_curr = as.integer(male_smk_curr)
  ) %>%
  count(male_smk_curr, female_smk_curr) %>%
  pivot_wider(
    names_from = female_smk_curr, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_smk_curr)

# Construct matrix: rows = male_smk_curr (1 = Yes, 0 = No), cols = female_smk_curr (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_smk_curr == 1],
    or_matrix$female_0[or_matrix$male_smk_curr == 1],
    or_matrix$female_1[or_matrix$male_smk_curr == 0],
    or_matrix$female_0[or_matrix$male_smk_curr == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)


# FAM HX HTN
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_famhx_htn) & !is.na(male_famhx_htn)) %>% 
  mutate(
    female_famhx_htn = as.integer(female_famhx_htn),
    male_famhx_htn = as.integer(male_famhx_htn)
  ) %>%
  count(male_famhx_htn, female_famhx_htn) %>%
  pivot_wider(
    names_from = female_famhx_htn, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_famhx_htn)

# Construct matrix: rows = male_famhx_htn (1 = Yes, 0 = No), cols = female_famhx_htn (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_famhx_htn == 1],
    or_matrix$female_0[or_matrix$male_famhx_htn == 1],
    or_matrix$female_1[or_matrix$male_famhx_htn == 0],
    or_matrix$female_0[or_matrix$male_famhx_htn == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)


# FAM HX DM
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_famhx_dm) & !is.na(male_famhx_dm)) %>% 
  mutate(
    female_famhx_dm = as.integer(female_famhx_dm),
    male_famhx_dm = as.integer(male_famhx_dm)
  ) %>%
  count(male_famhx_dm, female_famhx_dm) %>%
  pivot_wider(
    names_from = female_famhx_dm, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_famhx_dm)

# Construct matrix: rows = male_famhx_dm (1 = Yes, 0 = No), cols = female_famhx_dm (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_famhx_dm == 1],
    or_matrix$female_0[or_matrix$male_famhx_dm == 1],
    or_matrix$female_1[or_matrix$male_famhx_dm == 0],
    or_matrix$female_0[or_matrix$male_famhx_dm == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)


# FAM HX Heart disease
or_matrix <- baseline_wide %>%
  dplyr::filter(!is.na(female_famhx_cvd) & !is.na(male_famhx_cvd)) %>% 
  mutate(
    female_famhx_cvd = as.integer(female_famhx_cvd),
    male_famhx_cvd = as.integer(male_famhx_cvd)
  ) %>%
  count(male_famhx_cvd, female_famhx_cvd) %>%
  pivot_wider(
    names_from = female_famhx_cvd, values_from = n, values_fill = 0,
    names_prefix = "female_"
  ) %>%
  arrange(male_famhx_cvd)

# Construct matrix: rows = male_famhx_cvd (1 = Yes, 0 = No), cols = female_famhx_cvd (1 = Yes, 0 = No)
table_matrix <- matrix(
  c(
    or_matrix$female_1[or_matrix$male_famhx_cvd == 1],
    or_matrix$female_0[or_matrix$male_famhx_cvd == 1],
    or_matrix$female_1[or_matrix$male_famhx_cvd == 0],
    or_matrix$female_0[or_matrix$male_famhx_cvd == 0]
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list("Husband" = c("Yes", "No"), "Wife" = c("Yes", "No"))
)

# Step 2: Calculate OR and CI
or_result <- oddsratio(table_matrix, method = "wald")

print(or_result$measure)







# List of conditions
conditions <- c("chd", "cva", "ckd", "diabetes", "hypertension", "high_tg", "overweight",
                "dm", "htn")

source("functions/get_or_ci.R")

or_ci_results <- sapply(conditions, function(x) get_or_ci_fisher(baseline_wide, x))
or_ci_table <- as.data.frame(t(or_ci_results))
or_ci_table$Condition <- rownames(or_ci_table)
rownames(or_ci_table) <- NULL
or_ci_table <- or_ci_table[, c("Condition", "OR", "lower_95CI", "upper_95CI")]

print(or_ci_table)


