# ==============================================================================
# Purpose: Generate forest plot showing BMI change associations by subgroups
# Notes: Visualizes stratified analysis results for manuscript
# ==============================================================================

rm(list=ls());gc();source(".Rprofile")

plot_df <- read.csv("cca/analysis/psbcan05_spousal bmi change linear regression.csv") %>%
  mutate(
    term = recode(term,
                  "spouse_bmi_change" = "Spouse BMI Change",
                  "fup_duration" = "Follow-up Duration",
                  "age_category30-49" = "Age 30–49",
                  "age_category50-69" = "Age 50–69",
                  "age_category18-29" = "Age 18-29",
                  "bmi_categoryOverweight" = "Overweight",
                  "bmi_categoryObese" = "Obese",
                  "edu_categoryHigh school to secondary" = "High school to secondary",
                  "edu_categoryCollege and above" = "College and above",
                  "diabetes" = "Diabetes",
                  "famhx_dm" = "Family History of Diabetes",
                  "overall" = "Overall"
    ),
    beta_ci = glue("{round(estimate, 2)} ({round(conf.low, 2)}, {round(conf.high, 2)})"),
    p_label = ifelse(p.value < 0.001, "<0.001", sprintf("%.3f", p.value)),
    term = fct_rev(factor(term))  # reverse order for forest plot
  )

wife_df <- plot_df %>% dplyr::filter(model == "wife")
husband_df <- plot_df %>% dplyr::filter(model == "husband")


#---------------------------------------------------------------------------------------------
library(ggplot2)
library(forcats)
library(patchwork)

# Use monospace font to ensure alignment
font_family <- "Courier"

# 1. Left text: Subgroup label layer
wife_label_left <- ggplot(wife_df, aes(y = term)) +
  geom_text(aes(x = 1, label = term), hjust = 0, family = font_family, size = 3.2) +
  scale_x_continuous(limits = c(1, 2)) +
  theme_void() +
  theme(
    plot.margin = margin(t = 5, r = -25, b = 5, l = 0),
    axis.text.y = element_blank()
  )

# 2. Main plot layer: point estimates + CI
wife_plot <- ggplot(wife_df, aes(x = estimate, y = term)) +
  geom_point(size = 3, shape = 15, color = "black") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(
    limits = c(min(wife_df$conf.low) - 0.05, max(wife_df$conf.high) + 0.05),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(x = expression(beta ~ "(95% CI)"), y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),  # Don't repeat term display
    axis.text.x = element_text(size = 11)
  )

# 3. Right text layer: P-value and β(CI)
wife_df <- wife_df %>%
  mutate(label_right = paste0(sprintf("%7s", p_label), "   ", beta_ci))  # Ensure alignment spacing

wife_label_right <- ggplot(wife_df, aes(y = term)) +
  geom_text(aes(x = 1, label = label_right), hjust = 1, family = font_family, size = 3.2) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(
    plot.margin = margin(t = 5, r = 0, b = 5, l = -25),
    axis.text.y = element_blank()
  )

# 4. Combine three parts
wife_fig <- wife_label_left + wife_plot + wife_label_right + 
  plot_layout(widths = c(1.8, 3.2, 2))



# 1. Left side Subgroup label
husband_label_left <- ggplot(husband_df, aes(y = term)) +
  geom_text(aes(x = 1, label = term), hjust = 0, family = font_family, size = 3.2) +
  scale_x_continuous(limits = c(1, 2)) +
  theme_void() +
  theme(
    plot.margin = margin(t = 5, r = -25, b = 5, l = 0),
    axis.text.y = element_blank()
  )

# 2. Main plot layer (point estimates and CI)
husband_plot <- ggplot(husband_df, aes(x = estimate, y = term)) +
  geom_point(size = 3, shape = 15, color = "black") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black") +
  scale_x_continuous(
    limits = c(min(husband_df$conf.low) - 0.05, max(husband_df$conf.high) + 0.05),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  labs(x = expression(beta ~ "(95% CI)"), y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 11)
  )

# 3. Right side P-value and β(CI) label
husband_df <- husband_df %>%
  mutate(label_right = paste0(sprintf("%7s", p_label), "   ", beta_ci))  # Adjust spacing

husband_label_right <- ggplot(husband_df, aes(y = term)) +
  geom_text(aes(x = 1, label = label_right), hjust = 1, family = font_family, size = 3.2) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(
    plot.margin = margin(t = 5, r = 0, b = 5, l = -25),
    axis.text.y = element_blank()
  )

# 4. Combine three parts
husband_fig <- husband_label_left + husband_plot + husband_label_right +
  plot_layout(widths = c(1.8, 3.2, 2))


combined_fig <- wife_fig + husband_fig +
  plot_layout(ncol = 1) & 
  plot_annotation(tag_levels = "A")


# ---------------------------------------------------------------------------------
library(forcats)
library(patchwork)

plot_forest_custom <- function(df, fig_title) {
  df$term <- fct_rev(factor(df$term))  # Draw from top to bottom
  
  ggplot(df, aes(x = estimate, y = term)) +
    geom_point(size = 3, color = "black") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
    geom_vline(xintercept = 0, linetype = "solid", color = "black") +
    
    # Left side: subgroup name (term) left-aligned
    geom_text(aes(label = term), x = min(df$conf.low) - 0.06, hjust = 0, size = 3.3, family = "sans") +
    
    # Right side: beta CI and p-value right-aligned
    geom_text(aes(label = beta_ci), x = max(df$conf.high) + 0.12, hjust = 1, size = 3.3, family = "sans") +
    geom_text(aes(label = p_label), x = max(df$conf.high) + 0.25, hjust = 1, size = 3.3, family = "sans", color = "gray30") +
    
    scale_x_continuous(
      limits = c(min(df$conf.low) - 0.08, max(df$conf.high) + 0.3),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = expression(beta ~ "(95% CI)"),
      y = NULL,
      title = fig_title
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_blank(),       # Hide default y-axis labels
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(color = "black"),
      axis.ticks.x = element_line(color = "black"),
      axis.text.x = element_text(color = "black", family = "sans"),
      plot.title = element_text(hjust = 0, size = 14, face = "bold", family = "sans")
    )
}


wife_fig <- plot_forest_custom(wife_df, "A) Wife")
husband_fig <- plot_forest_custom(husband_df, "B) Husband")

combined_fig <- wife_fig / husband_fig + plot_layout(heights = c(1, 1))

ggsave(paste0(path_spouses_bmi_change_folder,"/figures/forest plot of bmi change by subgroup.jpg"), combined_fig, width = 10, height = 15, dpi = 300)

