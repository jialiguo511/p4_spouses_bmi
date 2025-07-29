
extract_coefficient <- function(model, exposure_var) {
  # Extract coefficient summary
  summ <- summary(model)$coefficients
  
  # Estimate, SE, t-value
  est <- summ[exposure_var, "Estimate"]
  se  <- summ[exposure_var, "Std. Error"]
  tval <- est / se
  
  # Degrees of freedom (approx)
  df <- nobs(model) - length(fixef(model))
  
  # P-value
  pval <- 2 * pt(-abs(tval), df = df)
  
  # Confidence interval
  ci_low  <- est - 1.96 * se
  ci_high <- est + 1.96 * se
  
  # R² using performance::r2
  r2_vals <- tryCatch(performance::r2(model), error = function(e) NULL)
  
  marginal_r2 <- if (!is.null(r2_vals)) r2_vals$R2_marginal else NA
  conditional_r2 <- if (!is.null(r2_vals) && "R2_conditional" %in% names(r2_vals)) {
    r2_vals$R2_conditional
  } else {
    NA
  }
  
  
  tibble(
    estimate = est,
    conf.low = ci_low,
    conf.high = ci_high,
    p.value = pval,
    marginal_r2 = marginal_r2,
    conditional_r2 = conditional_r2
  )
}