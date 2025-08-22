rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

analytic_df_wide <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/cca/psbcpre02a_wide spouse bmi complete cases.RDS")) %>%
  mutate(
    female_bmi_abs_change = abs(female_bmi_change),
    male_bmi_abs_change = abs(male_bmi_change)
  )

# Spearman correlation
cor_result <- cor.test(
  analytic_df_wide$male_bmi_abs_change,
  analytic_df_wide$female_bmi_abs_change,
  method = "spearman"
)
rho <- round(cor_result$estimate, 2)
p_val <- signif(cor_result$p.value, 2)


plot = ggplot(analytic_df_wide, aes(x = male_bmi_abs_change, y = female_bmi_abs_change)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  theme_bw() +
  labs(
    x = "Absolute Male BMI Change",
    y = "Absolute Female BMI Change"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  ) +
  annotate(
    "text",
    x = min(analytic_df_wide$male_bmi_abs_change, na.rm = TRUE),
    y = max(analytic_df_wide$female_bmi_abs_change, na.rm = TRUE),
    label = paste0("Spearman ρ = ", rho, "\nP = ", p_val),
    hjust = 0, vjust = 1, size = 4.5, fontface = "italic"
  )

ggsave(plot,filename=paste0(path_spouses_bmi_change_folder,"/figures/scatterplot of bmi change by sex.jpg"),width = 8,height = 6)

