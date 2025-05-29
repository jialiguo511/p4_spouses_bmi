rm(list=ls());gc();source(".Rprofile")

# unique hhid: 5,789, unique pid: 11,578
analytic_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse analytic sample.RDS")) %>% 
  select(pid,hhid,sex,bmi,wave) %>% 
  mutate(obese = case_when(
    bmi >= 30 ~ 1,
    is.na(bmi) ~ NA_real_,
    TRUE ~ 0
  ))


summary_table <- analytic_df %>%
  dplyr::filter(!is.na(sex), !is.na(wave)) %>%
  group_by(wave, sex) %>%
  summarise(
    n = n_distinct(pid),
    BMI_mean = mean(bmi, na.rm = TRUE),
    Obese_pct = mean(obese, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = sex,
    values_from = c(n, BMI_mean, Obese_pct),
    names_sep = "_"
  ) %>%
  # Reorder columns for readability
  select(
    wave,
    n_female, BMI_mean_female, Obese_pct_female,
    n_male, BMI_mean_male, Obese_pct_male
  ) %>%
  arrange(wave) %>% 
  write.csv(.,"analysis/psban03_bmi change descriptive analysis.csv")


# obese status change in BS ----------------------------------

# Step 1: Pivot wider to get BMI at each relevant wave
bmi_wide <- analytic_df %>%
  dplyr::filter(wave %in% c("CARRS1 BS", "CARRS2 BS", "PCARRS")) %>%
  select(pid, wave, bmi) %>%
  pivot_wider(names_from = wave, values_from = bmi, names_prefix = "bmi_")

# Step 2: Calculate BMI change variables
bmi_change <- bmi_wide %>%
  mutate(
    bmi_change_c1 = bmi_PCARRS - `bmi_CARRS1 BS`,
    bmi_change_c2 = bmi_PCARRS - `bmi_CARRS2 BS`
  )

analytic_df <- analytic_df %>%
  left_join(bmi_change %>% select(pid, bmi_change_c1, bmi_change_c2), by = "pid")

# Step 1: Combine CARRS1 and CARRS2 cohorts with BMI changes
bmi_change_combined <- analytic_df %>%
  select(pid, sex, bmi_change_c1, bmi_change_c2) %>%
  distinct() %>%  # one row per person
  mutate(
    bmi_change = coalesce(bmi_change_c1, bmi_change_c2)  # pick whichever is available
    # For each individual, use bmi_change_c1 if it's not missing; otherwise, use bmi_change_c2.
  ) %>%
  dplyr::filter(!is.na(bmi_change), !is.na(sex))

# Step 2: Summarize by sex
bmi_change_summary <- bmi_change_combined %>%
  group_by(sex) %>%
  summarise(
    mean_change = round(mean(bmi_change, na.rm = TRUE), 2),
    min_change = round(min(bmi_change, na.rm = TRUE), 2),
    max_change = round(max(bmi_change, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(
    range = paste0(min_change, " to ", max_change)
  ) %>%
  select(Sex = sex, Mean = mean_change, `Min–Max` = range)

