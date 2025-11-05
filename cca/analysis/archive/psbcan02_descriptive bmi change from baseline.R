rm(list=ls());gc();source(".Rprofile")

# PCARRS - BS
analytic_15y <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre02b_long spouse bmi complete cases.RDS")) %>% 
  dplyr::filter((carrs == 1 & fup %in% c(0,7)) | (carrs == 2 & fup %in% c(0,2))) %>% 
  select(pid,hhid,,carrs,fup,sex,bmi,fup_duration) %>% 
  mutate(obese = case_when(bmi >= 30 ~ 1, 
                           TRUE ~ 0)) %>% 
  group_by(pid) %>%
  mutate(
    bmi_bschange = bmi[fup == max(fup)] - bmi[fup == min(fup)],
    annual_bmi_bschange = bmi_bschange / fup_duration[fup == max(fup)]
  ) %>%
  ungroup()

pcarrs <- analytic_15y %>% 
  dplyr::filter(fup != 0)


summarize_carrs <- function(df, carrs_value = NULL) {
  df_filtered <- if (!is.null(carrs_value)) dplyr::filter(df, carrs == carrs_value) else df
  
  df_filtered %>%
    group_by(sex) %>%
    summarise(
      bmi_bschange_mean = round(mean(bmi_bschange, na.rm = TRUE), 1),
      bmi_bschange_minmax = paste0(round(range(bmi_bschange, na.rm = TRUE), 1), collapse = " ~ "),
      duration_mean = round(mean(fup_duration, na.rm = TRUE), 2),
      duration_minmax = paste0(round(range(fup_duration, na.rm = TRUE), 2), collapse = " ~ "),
      annual_bmi_bschange_mean = round(mean(annual_bmi_bschange, na.rm = TRUE), 2),
      annual_bmi_bschange_minmax = paste0(round(range(annual_bmi_bschange, na.rm = TRUE), 2), collapse = " ~ ")
    ) %>%
    arrange(sex)
}

# Usage
table_carrs1 <- summarize_carrs(pcarrs, carrs_value = 1)
table_carrs2 <- summarize_carrs(pcarrs, carrs_value = 2)
table_carrs_all <- summarize_carrs(pcarrs)

 
# Obese status change ----------------

get_transition_wide <- function(data, group_by_carrs = TRUE) {
  grouping_vars <- if (group_by_carrs) c("pid", "carrs", "sex") else c("pid", "sex")
  
  # Step 1: Assign obesity transition per person
  transitions <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
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
  
  # Step 2: Count + % by sex and obesity status (plus carrs if selected)
  count_vars <- if (group_by_carrs) c("carrs", "sex", "obesity_status") else c("sex", "obesity_status")
  
  transition_table <- transitions %>%
    group_by(across(all_of(count_vars))) %>%
    summarise(n = n_distinct(pid), .groups = "drop") %>%
    group_by(across(all_of(setdiff(count_vars, "obesity_status")))) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()
  
  # Step 3: Pivot to wide format
  transition_table %>%
    pivot_wider(
      names_from = sex,
      values_from = c(n, pct),
      names_glue = "{sex}_{.value}"
    ) %>%
    arrange(
      if (group_by_carrs) carrs,
      factor(obesity_status, levels = c(
        "Stable Nonobese", "Nonobese to Obese", "Obese to Nonobese", "Stable Obese"
      ))
    )
}

# For carrs-stratified table
transition_wide_by_carrs <- get_transition_wide(analytic_15y, group_by_carrs = TRUE)

# For overall table (across both carrs)
transition_wide_all <- get_transition_wide(analytic_15y, group_by_carrs = FALSE)

