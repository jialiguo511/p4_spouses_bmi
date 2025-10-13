rm(list=ls());gc();source(".Rprofile")

library(lme4)
library(broom.mixed)
library(mice)
library(mitools)
library(performance)
library(tibble)

# Load imputed datasets in wide format
spouse_wide <- readRDS(paste0(path_spouses_bmi_change_folder, "/working/cleaned/psban01_wide spouse dfs.RDS"))


for (i in 1:length(spouse_dfs)) {
  analytic_df_wide <- spouse_wide[[i]] %>% 
    dplyr::filter(fup != 0) %>% 
    mutate(
      female_bmi_change    = case_when(female_bmi_change < -5.37 | female_bmi_change > 7.53 ~ NA_real_, TRUE ~ female_bmi_change),
      male_bmi_change    = case_when(male_bmi_change < -5.37 | male_bmi_change > 7.53 ~ NA_real_, TRUE ~ male_bmi_change),
      female_bmi_bschange  = case_when(female_bmi_bschange < -5.59 | female_bmi_bschange > 7.75 ~ NA_real_, TRUE ~ female_bmi_bschange),
      male_bmi_bschange  = case_when(male_bmi_bschange < -5.59 | male_bmi_bschange > 7.75 ~ NA_real_, TRUE ~ male_bmi_bschange)
    ) %>% 
    mutate(
      female_annual_bmi_change = female_bmi_change / female_fup_duration,
      male_annual_bmi_change = male_bmi_change / male_fup_duration
    )
  
  
  # Unadjusted models
  
  model_wife_unadj <- lmer(female_annual_bmi_change ~ male_annual_bmi_change + (1 | hhid), data = analytic_df_wide)
  model_husband_unadj <- lmer(male_annual_bmi_change ~ female_annual_bmi_change + (1 | hhid), data = analytic_df_wide)
  
  # Model 1: adjusted for age and baseline BMI
  
  model1_wife <- lmer(female_annual_bmi_change ~ male_annual_bmi_change + female_age + female_bmi_baseline + (1 | hhid), data = analytic_df_wide)
  model1_husband <- lmer(male_annual_bmi_change ~ female_annual_bmi_change + male_age + male_bmi_baseline + (1 | hhid), data = analytic_df_wide)
  
  # Model 2
  
  model2_wife <- lmer(female_annual_bmi_change ~ male_annual_bmi_change + female_age + female_bmi_baseline + female_fup_duration 
                      # Demographic
                      + female_edu_category + female_employ_category + female_hhincome + female_site + carrs
                      # Health risk
                      # + female_smk_curr + female_alc_curr
                      # Comorbidity
                      + female_diabetes + female_famhx_dm
                      + (1 | hhid), data = analytic_df_wide)
  model2_husband <- lmer(male_annual_bmi_change ~ female_annual_bmi_change + male_age + male_bmi_baseline + male_fup_duration 
                         # Demographic
                         + male_edu_category + male_employ_category + male_hhincome + male_site + carrs
                         # Health risk
                         + male_smk_overall + male_alc_overall
                         # Comorbidity
                         + male_diabetes + male_famhx_dm
                         + (1 | hhid), data = analytic_df_wide)

}






# Function to prepare a dataset for analysis
prepare_dataset <- function(df) {
  df %>% 
    dplyr::filter(fup != 0) %>% 
    mutate(
      female_annual_bmi_change = female_bmi_change / female_fup_duration,
      male_annual_bmi_change = male_bmi_change / male_fup_duration
    )
}

# Prepare all imputed datasets
analytic_dfs <- lapply(spouse_dfs, prepare_dataset)

# Function to fit a model on each imputed dataset and pool results
pool_lmer_models <- function(formula, data_list) {
  # Fit model on each imputed dataset
  models <- lapply(data_list, function(data) {
    # Try with default optimizer first
    model <- try(lmer(formula, data = data), silent = TRUE)
    
    # If that fails, try with BOBYQA optimizer
    if(inherits(model, "try-error")) {
      model <- try(lmer(formula, data = data, 
                    control = lmerControl(optimizer = "bobyqa")), 
                silent = TRUE)
    }
    
    # If that still fails, use a simple linear model
    if(inherits(model, "try-error")) {
      fixed_formula <- as.formula(gsub("\\s*\\([^\\)]+\\)", "", as.character(formula)[3]))
      fixed_formula <- as.formula(paste(as.character(formula)[2], "~", fixed_formula))
      model <- lm(fixed_formula, data = data)
    }
    
    return(model)
  })
  
  # Extract coefficients and variance-covariance matrices
  coefs <- lapply(models, function(model) {
    if(inherits(model, "merMod")) return(fixef(model))
    else return(coef(model))
  })
  
  vcovs <- lapply(models, function(model) vcov(model))
  
  # Pool using Rubin's rules
  results <- MIcombine(coefs, vcovs)
  
  # Get R² values for each model
  r2_values <- lapply(models, function(model) {
    if(inherits(model, "merMod")) {
      tryCatch({
        r2_vals <- performance::r2(model)
        c(marginal = r2_vals$R2_marginal, conditional = r2_vals$R2_conditional)
      }, error = function(e) {
        c(marginal = NA, conditional = NA)
      })
    } else {
      c(marginal = summary(model)$r.squared, conditional = NA)
    }
  })
  
  # Calculate average R² values
  r2_df <- do.call(rbind, r2_values)
  mean_r2 <- colMeans(r2_df, na.rm = TRUE)
  
  list(
    results = results,
    r2_marginal = mean_r2["marginal"],
    r2_conditional = mean_r2["conditional"]
  )
}

# Function to extract coefficient with confidence interval, p-value, and R²
extract_coefficient_pooled <- function(pooled_model, var_name) {
  coef_summary <- summary(pooled_model$results)
  
  # Handle case where variable isn't found
  if(!(var_name %in% rownames(coef_summary))) {
    warning(paste("Variable", var_name, "not found in model results"))
    return(tibble(
      estimate = NA, std.error = NA, statistic = NA, p.value = NA,
      conf.low = NA, conf.high = NA, 
      marginal_r2 = pooled_model$r2_marginal, 
      conditional_r2 = pooled_model$r2_conditional
    ))
  }
  
  idx <- which(rownames(coef_summary) == var_name)
  
  tibble(
    estimate = coef_summary[idx, "results"],
    std.error = coef_summary[idx, "se"],
    statistic = coef_summary[idx, "t"],
    p.value = coef_summary[idx, "p"],
    conf.low = coef_summary[idx, "results"] - 1.96 * coef_summary[idx, "se"],
    conf.high = coef_summary[idx, "results"] + 1.96 * coef_summary[idx, "se"],
    marginal_r2 = pooled_model$r2_marginal,
    conditional_r2 = pooled_model$r2_conditional
  )
}

# Fit models -----------------------------------------------------------------

# Just fit the unadjusted model for now (as requested)
formula_wife_unadj <- female_annual_bmi_change ~ male_annual_bmi_change + (1 | hhid)

# Print the formula to verify
cat("Fitting model with formula:\n")
print(formula_wife_unadj)

# Fit model
model_wife_unadj <- pool_lmer_models(formula_wife_unadj, analytic_dfs)

# Print basic summary of the results
cat("\nSummary of pooled results:\n")
print(summary(model_wife_unadj$results))

# Print R-squared values
cat("\nR-squared values:\n")
cat("Marginal R²:", model_wife_unadj$r2_marginal, "\n")
cat("Conditional R²:", model_wife_unadj$r2_conditional, "\n")

# Extract coefficient for the key predictor
wife_unadj <- extract_coefficient_pooled(model_wife_unadj, "male_annual_bmi_change")

# Format results
format_result <- function(x) {
  sprintf(
    "%.3f (95%% CI: %.3f, %.3f), p = %.3f [R²m = %.3f, R²c = %.3f]",
    x$estimate, x$conf.low, x$conf.high, x$p.value,
    x$marginal_r2, x$conditional_r2
  )
}

# Print formatted result
cat("\nFormatted result:\n")
cat(format_result(wife_unadj), "\n\n")

# Create a simple result table
result_tbl <- tibble(
  Model = "Unadjusted",
  Predictor = "male_annual_bmi_change",
  Estimate = wife_unadj$estimate,
  SE = wife_unadj$std.error,
  `p-value` = wife_unadj$p.value,
  `95% CI Lower` = wife_unadj$conf.low,
  `95% CI Upper` = wife_unadj$conf.high,
  `R² Marginal` = wife_unadj$marginal_r2,
  `R² Conditional` = wife_unadj$conditional_r2
)

# Print and save the result
print(result_tbl)
write.csv(result_tbl, file = "analysis/psban03_bmi_change unadjusted model.csv", row.names = FALSE)





