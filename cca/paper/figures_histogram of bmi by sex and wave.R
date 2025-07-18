rm(list=ls());gc();source(".Rprofile")


analytic_df <- readRDS(paste0(path_spouses_bmi_change_folder,"/working/cleaned/spouse analytic sample.RDS")) %>% 
  select(hhid,wave,pid,sex,bmi)

library(ggplot2)

hist_fig <- analytic_df %>%
  dplyr::filter(!is.na(bmi), !is.na(sex)) %>%
  ggplot(aes(x = bmi, fill = sex)) +
  geom_histogram(bins = 30, color = "white", alpha = 0.8) +
  facet_grid(sex ~ wave) +
  scale_fill_manual(values = c("female" = "tomato", "male" = "steelblue")) +
  labs(x = "BMI", y = "Frequency", fill = "Sex") +
  theme_light(base_size = 16) +  # set base font size for all text
  theme(
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 16),  # facet labels
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )


ggsave(hist_fig,filename=paste0(path_spouses_bmi_change_folder,"/figures/histogram of BMI by sex and wave.jpg"),width = 26,height = 8)
