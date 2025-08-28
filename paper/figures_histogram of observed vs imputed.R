library(ggplot2)
library(mice)

imp_carrs1 <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/psban_carrs1 mi_dfs.RDS"))
imp_carrs2 <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/psban_carrs2 mi_dfs.RDS"))

# Convert imputed dataset to long format
long_data <- complete(imp_carrs1, action = "long", include = TRUE)
# long_data <- complete(imp_carrs2, action = "long", include = TRUE)

# Stack bmi and fpg separately
long_bmi <- long_data %>%
  select(.imp, .id, pid, sex, starts_with("bmi_")) %>%
  pivot_longer(
    cols = starts_with("bmi_"),
    names_to = "fup",
    names_prefix = "bmi_",
    values_to = "bmi"
  ) %>%
  mutate(
    fup = as.integer(fup),
    source = ifelse(.imp == 0, "Observed", "Imputed")
  )

long_fpg <- long_data %>%
  select(.imp, .id, pid, sex, starts_with("fpg_")) %>%
  pivot_longer(
    cols = starts_with("fpg_"),
    names_to = "fup",
    names_prefix = "fpg_",
    values_to = "fpg"
  ) %>%
  mutate(
    fup = as.integer(fup),
    source = ifelse(.imp == 0, "Observed", "Imputed")
  )


ggplot() +
  geom_histogram(data = filter(long_bmi, source == "Observed"),
                 aes(x = bmi, y = after_stat(density)),
                 bins = 50, fill = "grey", alpha = 0.6) +
  geom_histogram(data = filter(long_bmi, source == "Imputed"),
                 aes(x = bmi, y = after_stat(density)),
                 bins = 50, fill = "darkgrey", alpha = 0.4) +
  labs(title = "Histogram of BMI: Imputed vs Observed",
       x = "BMI", y = "Density") +
  theme_minimal()


ggplot() +
  geom_histogram(data = filter(long_fpg, source == "Observed"),
                 aes(x = fpg, y = after_stat(density)),
                 bins = 50, fill = "grey", alpha = 0.6) +
  geom_histogram(data = filter(long_fpg, source == "Imputed"),
                 aes(x = fpg, y = after_stat(density)),
                 bins = 50, fill = "darkgrey", alpha = 0.4) +
  labs(title = "Histogram of FPG: Imputed vs Observed",
       x = "FPG", y = "Density") +
  theme_minimal()

# Kernel plot 

## ---------- BMI: histogram + kernel density ----------
p_bmi <- ggplot(long_bmi, aes(x = bmi)) +
  geom_histogram(
    data = filter(long_bmi, source == "Observed"),
    aes(y = after_stat(density), fill = "Observed"),
    bins = 40, position = "identity", alpha = 0.35, color = "white"
  ) +
  geom_histogram(
    data = filter(long_bmi, source == "Imputed"),
    aes(y = after_stat(density), fill = "Imputed"),
    bins = 40, position = "identity", alpha = 0.35, color = "white"
  ) +
  geom_density(
    data = filter(long_bmi, source == "Observed"),
    aes(y = after_stat(density), color = "Observed"),
    linewidth = 1.05, adjust = 1
  ) +
  geom_density(
    data = filter(long_bmi, source == "Imputed"),
    aes(y = after_stat(density), color = "Imputed"),
    linewidth = 1.05, adjust = 1
  ) +
  facet_grid(sex ~ fup, scales = "free_y") +
  labs(
    x = "BMI (kg/m²)", y = "Density",
    title = "BMI: Observed vs Imputed (by sex and follow-up)"
  ) +
  guides(fill = guide_legend(title = "Source"), color = guide_legend(title = "Source")) +
  theme_minimal(base_size = 12)

p_bmi


## ---------- FPG: histogram + kernel density ----------
p_fpg <- ggplot(long_fpg, aes(x = fpg)) +
  geom_histogram(
    data = filter(long_fpg, source == "Observed"),
    aes(y = after_stat(density), fill = "Observed"),
    bins = 40, position = "identity", alpha = 0.35, color = "white"
  ) +
  geom_histogram(
    data = filter(long_fpg, source == "Imputed"),
    aes(y = after_stat(density), fill = "Imputed"),
    bins = 40, position = "identity", alpha = 0.35, color = "white"
  ) +
  geom_density(
    data = filter(long_fpg, source == "Observed"),
    aes(y = after_stat(density), color = "Observed"),
    linewidth = 1.05, adjust = 1
  ) +
  geom_density(
    data = filter(long_fpg, source == "Imputed"),
    aes(y = after_stat(density), color = "Imputed"),
    linewidth = 1.05, adjust = 1
  ) +
  facet_grid(sex ~ fup, scales = "free_y") +
  labs(
    x = "FPG (mmol/L)", y = "Density",
    title = "FPG: Observed vs Imputed (by sex and follow-up)"
  ) +
  guides(fill = guide_legend(title = "Source"), color = guide_legend(title = "Source")) +
  theme_minimal(base_size = 12)

p_fpg
