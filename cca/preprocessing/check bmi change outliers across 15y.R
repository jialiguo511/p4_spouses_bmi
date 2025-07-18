rm(list=ls());gc();source(".Rprofile")

# unique hhid: 2,468, unique pid: 4,936
# PCARRS - BS
analytic_15y <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse bmi complete cases.RDS")) %>% 
  dplyr::filter((carrs == 1 & fup %in% c(0,7)) | (carrs == 2 & fup %in% c(0,2))) %>% 
  select(pid,hhid,,carrs,fup,sex,bmi,fup_duration) %>% 
  group_by(pid) %>%
  arrange(pid, carrs, fup) %>%
  # Keep all from carrs = 2; keep only those from carrs = 1 who have fup == 7
  dplyr::filter(any(carrs == 2) | any(carrs == 1 & fup == 7)) %>%
  mutate(bmi_baseline = bmi[fup == 0][1],   # grab the first baseline BMI
         bmi_bschange = bmi - bmi_baseline, # change from baseline
         bmi_change = bmi - dplyr::lag(bmi)) %>% # change from previous visit
  ungroup()

# Step 1: Compute IQR, Q1, and Q3
bmi_bounds <- analytic_15y %>%
  summarise(
    Q1 = quantile(bmi_bschange, 0.25, na.rm = TRUE),
    Q3 = quantile(bmi_bschange, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    lower_bound = Q1 - 1.5 * IQR,
    upper_bound = Q3 + 1.5 * IQR
  )


# Step 2: Extract bounds
lower <- bmi_bounds$lower_bound
upper <- bmi_bounds$upper_bound

# Step 3: Calculate percentage outside the bounds
analytic_15y %>%
  summarise(
    total = sum(!is.na(bmi_bschange)),
    below = sum(bmi_bschange < lower, na.rm = TRUE),
    above = sum(bmi_bschange > upper, na.rm = TRUE)
  ) %>%
  mutate(
    percent_below = below / total * 100,
    percent_above = above / total * 100,
    percent_outside = (below + above) / total * 100
  )




# Identify pids with at least one outlier
n_outlier_pids <- analytic_15y %>%
  dplyr::filter(bmi_bschange < lower | bmi_bschange > upper) %>%
  distinct(pid) %>%
  summarise(n_outlier_pids = n())

# Get total number of unique pids
n_total_pids <- analytic_15y %>%
  distinct(pid) %>%
  summarise(n_total_pids = n())

# Calculate % of unique individuals with outliers
bind_cols(n_outlier_pids, n_total_pids) %>%
  mutate(
    percent_with_outlier = n_outlier_pids / n_total_pids * 100
  )


# Visual check:
analytic_15y %>%
  ggplot(aes(bmi_bschange)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = c(-5.59, 7.75), color = "red") +
  labs(title = "BMI change distribution with IQR-based bounds (PCARRS - Baseline)")

# Flag outliers:
analytic_15y <- analytic_15y %>%
  mutate(
    outlier_iqr = bmi_change < -5.59 | bmi_change > 7.75
  )
