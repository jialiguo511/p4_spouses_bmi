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

# 用等宽字体确保对齐
font_family <- "Courier"

# 1. 左侧文字：Subgroup label 图层
wife_label_left <- ggplot(wife_df, aes(y = term)) +
  geom_text(aes(x = 1, label = term), hjust = 0, family = font_family, size = 3.2) +
  scale_x_continuous(limits = c(1, 2)) +
  theme_void() +
  theme(
    plot.margin = margin(t = 5, r = -25, b = 5, l = 0),
    axis.text.y = element_blank()
  )

# 2. 主体图层：点估计 + CI
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
    axis.text.y = element_blank(),  # 不重复显示 term
    axis.text.x = element_text(size = 11)
  )

# 3. 右侧文字图层：P 值 和 β(CI)
wife_df <- wife_df %>%
  mutate(label_right = paste0(sprintf("%7s", p_label), "   ", beta_ci))  # 保证对齐间距

wife_label_right <- ggplot(wife_df, aes(y = term)) +
  geom_text(aes(x = 1, label = label_right), hjust = 1, family = font_family, size = 3.2) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(
    plot.margin = margin(t = 5, r = 0, b = 5, l = -25),
    axis.text.y = element_blank()
  )

# 4. 合并三部分
wife_fig <- wife_label_left + wife_plot + wife_label_right + 
  plot_layout(widths = c(1.8, 3.2, 2))



# 1. 左侧 Subgroup 标签
husband_label_left <- ggplot(husband_df, aes(y = term)) +
  geom_text(aes(x = 1, label = term), hjust = 0, family = font_family, size = 3.2) +
  scale_x_continuous(limits = c(1, 2)) +
  theme_void() +
  theme(
    plot.margin = margin(t = 5, r = -25, b = 5, l = 0),
    axis.text.y = element_blank()
  )

# 2. 主体图层（点估计与CI）
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

# 3. 右侧 P 值 和 β(CI) 标签
husband_df <- husband_df %>%
  mutate(label_right = paste0(sprintf("%7s", p_label), "   ", beta_ci))  # 调整间距

husband_label_right <- ggplot(husband_df, aes(y = term)) +
  geom_text(aes(x = 1, label = label_right), hjust = 1, family = font_family, size = 3.2) +
  scale_x_continuous(limits = c(0, 1)) +
  theme_void() +
  theme(
    plot.margin = margin(t = 5, r = 0, b = 5, l = -25),
    axis.text.y = element_blank()
  )

# 4. 合并三个部分
husband_fig <- husband_label_left + husband_plot + husband_label_right +
  plot_layout(widths = c(1.8, 3.2, 2))


combined_fig <- wife_fig + husband_fig +
  plot_layout(ncol = 1) & 
  plot_annotation(tag_levels = "A")


# ---------------------------------------------------------------------------------
library(forcats)
library(patchwork)

plot_forest_custom <- function(df, fig_title) {
  df$term <- fct_rev(factor(df$term))  # 从上往下画
  
  ggplot(df, aes(x = estimate, y = term)) +
    geom_point(size = 3, color = "black") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "black") +
    geom_vline(xintercept = 0, linetype = "solid", color = "black") +
    
    # 左侧：subgroup 名称（term）左对齐
    geom_text(aes(label = term), x = min(df$conf.low) - 0.06, hjust = 0, size = 3.3, family = "sans") +
    
    # 右侧：beta CI 和 p-value 右对齐
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
      axis.text.y = element_blank(),       # 隐藏默认 y 轴标签
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

