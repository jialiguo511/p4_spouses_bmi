extract_coefficient <- function(model, exposure_var) {
  # Extract coefficient summary as a matrix
  summ <- summary(model)$coefficients
  
  # Get estimate and standard error for the exposure variable
  est <- summ[exposure_var, "Estimate"]
  se  <- summ[exposure_var, "Std. Error"]
  tval <- est / se
  
  # Approximate degrees of freedom
  df <- nobs(model) - length(fixef(model))
  
  # Calculate p-value
  pval <- 2 * pt(-abs(tval), df = df)
  
  # Get confidence interval (Wald method is fast and usually fine)
  ci <- confint(model, method = "Wald")[exposure_var, ]
  
  tibble(
    estimate = est,
    conf.low = ci[1],
    conf.high = ci[2],
    p.value = pval
  )
}
