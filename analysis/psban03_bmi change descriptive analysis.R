rm(list=ls());gc();source(".Rprofile")

# PCARRS - BS
analytic_15y <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse bmi complete cases.RDS")) %>% 
  dplyr::filter((carrs == 1 & fup %in% c(0,7)) | (carrs == 2 & fup %in% c(0,2))) %>% 
  select(pid,hhid,,carrs,fup,sex,bmi,fup_duration) %>% 
  mutate(obese = case_when(bmi >= 30 ~ 1, 
                           TRUE ~ 0)) %>% 
  group_by(pid) %>%
  mutate(
    bmi_change = bmi[fup == max(fup)] - bmi[fup == min(fup)],
    annual_bmi_change = bmi_change / fup_duration[fup == max(fup)]
  ) %>%
  ungroup()

pcarrs <- analytic_15y %>% 
  dplyr::filter(fup != 0)


summarize_carrs <- function(df, carrs_value) {
  df %>%
    dplyr::filter(carrs == carrs_value) %>%
    group_by(sex) %>%
    summarise(
      bmi_change_mean = round(mean(bmi_change, na.rm = TRUE), 1),
      bmi_change_minmax = paste0(round(min(bmi_change, na.rm = TRUE), 1), " ~ ", round(max(bmi_change, na.rm = TRUE), 1)),
      duration_mean = round(mean(fup_duration, na.rm = TRUE), 2),
      duration_minmax = paste0(round(min(fup_duration, na.rm = TRUE), 2), " ~ ", round(max(fup_duration, na.rm = TRUE), 2)),
      annual_bmi_change_mean = round(mean(annual_bmi_change, na.rm = TRUE), 2),
      annual_bmi_change_minmax = paste0(round(min(annual_bmi_change, na.rm = TRUE), 2), " ~ ", round(max(annual_bmi_change, na.rm = TRUE), 2))
    ) %>%
    arrange(sex)
}

# Apply to carrs 1 and 2 separately
table_carrs1 <- summarize_carrs(pcarrs, carrs_value = 1)
table_carrs2 <- summarize_carrs(pcarrs, carrs_value = 2)

 
# Obese status change ----------------

# Step 1: Get obesity status at baseline and endline for each person
transitions <- analytic_15y %>%
  group_by(pid, carrs, sex) %>%
  summarise(
    obese_baseline = obese[fup == min(fup)],
    obese_endline = obese[fup == max(fup)],
    .groups = "drop"
  ) %>%
  mutate(
    obesity_status = case_when(
      obese_baseline == 0 & obese_endline == 0 ~ "Stable Nonobese",
      obese_baseline == 0 & obese_endline == 1 ~ "Nonobese to Obese",
      obese_baseline == 1 & obese_endline == 0 ~ "Obese to Nonobese",
      obese_baseline == 1 & obese_endline == 1 ~ "Stable Obese",
      TRUE ~ NA_character_
    )
  )

# Step 2: Count number of individuals by carrs, sex, and obesity transition
transition_table <- transitions %>%
  group_by(carrs, sex, obesity_status) %>%
  summarise(n = n_distinct(pid), .groups = "drop") %>%
  group_by(carrs, sex) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

# Step 3: Optional — Reshape to wide format (Women/Men columns)
transition_wide <- transition_table %>%
  pivot_wider(
    names_from = sex,
    values_from = c(n, pct),
    names_glue = "{sex}_{.value}"
  ) %>%
  arrange(carrs, factor(obesity_status, levels = c(
    "Stable Nonobese", "Nonobese to Obese", "Obese to Nonobese", "Stable Obese"
  )))





