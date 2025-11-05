# Model diagnostics functions for mixed models
# This file contains helper functions to diagnose issues with linear mixed models
library(lme4)      # For lmer, isSingular, VarCorr functions
library(performance) # For r2 function

# Function to check for singularity and convergence issues in pooled models
check_model_issues <- function(model_list) {
  cat("Model Diagnostics Summary\n")
  cat("========================\n")
  
  # Check model types
  model_types <- sapply(model_list$models, function(m) class(m)[1])
  cat("Model types:", paste(unique(model_types), collapse=", "), "\n\n")
  
  # For each model in the pool
  for (i in seq_along(model_list$models)) {
    cat("Model", i, ":\n")
    
    # Check if it's a mixed model
    if(inherits(model_list$models[[i]], "merMod")) {
      # Check for singularity
      sing <- isSingular(model_list$models[[i]])
      cat("  - Singular:", sing, "\n")
      
      # Check convergence
      conv <- model_list$models[[i]]@optinfo$conv$opt
      cat("  - Convergence:", ifelse(is.null(conv), "OK", conv), "\n")
      
      # Check variance components
      vc <- VarCorr(model_list$models[[i]])
      cat("  - Random effect variances:\n")
      print(vc, comp = "Variance")
    } else {
      cat("  - Not a mixed model (fallback to linear model)\n")
    }
    
    cat("\n")
  }
  
  # Overall summary
  if(any(sapply(model_list$models, inherits, "merMod"))) {
    singular_models <- sum(sapply(model_list$models[sapply(model_list$models, inherits, "merMod")], 
                                 isSingular))
    cat("Summary:", singular_models, "out of", 
        sum(sapply(model_list$models, inherits, "merMod")), 
        "mixed models have singularity issues\n")
  }
}

# Function to attempt model simplification if needed
simplify_formula <- function(original_formula) {
  formula_str <- Reduce(paste, deparse(original_formula))
  
  # Check if there's a random slope
  has_random_slope <- grepl("\\|", formula_str) && !grepl("\\(1\\s*\\|", formula_str)
  
  if(has_random_slope) {
    # Simplify to random intercept only
    random_part <- regmatches(formula_str, gregexpr("\\([^)]+\\)", formula_str))[[1]]
    simplified_random <- gsub("[^|]+\\|", "1|", random_part)
    simplified_formula <- gsub("\\([^)]+\\)", simplified_random, formula_str, fixed = TRUE)
    return(as.formula(simplified_formula))
  } else {
    # Already a random intercept model or no random effects
    return(original_formula)
  }
}

# Function to refit a model with simplified random effects
refit_simplified_model <- function(formula, data_list, pool_lmer_models_fn) {
  # Simplify formula
  simple_formula <- simplify_formula(formula)
  
  cat("Attempting to fit with simplified formula:\n")
  print(simple_formula)
  
  # Refit using the simplified formula
  pool_lmer_models_fn(simple_formula, data_list)
}