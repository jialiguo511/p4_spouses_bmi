# Single Imputation Analysis

## Overview
This folder contains **sensitivity analyses** using a single imputation approach instead of multiple imputation. Missing BMI values are imputed once using predicted values from linear mixed-effects models, and missing covariates are handled with a single imputation.

**Purpose:** To compare results with the main multiple imputation analysis and assess sensitivity to the imputation approach.

**Key Difference from Main Analysis:** Uses single imputation (m=1) instead of multiple imputation (m=30), which does not account for imputation uncertainty.

---

## Analysis Structure

```
single imputation/
├── preprocessing/    # Single imputation and data preparation
├── analysis/        # Statistical models on singly imputed data
└── paper/          # Figures specific to single imputation
```

---

## Workflow

### Step 1: Preprocessing

1. **[psbspre01_single impution with predicted bmi.R](preprocessing/psbspre01_single impution with predicted bmi.R)**  
   - Performs single imputation for missing BMI values
   - Uses linear mixed-effects models to predict missing BMI
   - Imputes other missing covariates once

2. **[psbspre02_spouse identified data.R](preprocessing/psbspre02_spouse identified data.R)**  
   - Creates spouse-level dataset with singly imputed data

### Step 2: Analysis

3. **[psbsan01_descriptive characteristics.R](analysis/psbsan01_descriptive characteristics.R)**  
   - Descriptive statistics on singly imputed data

4. **[psbsan02_descriptive bmi change from baseline.R](analysis/psbsan02_descriptive bmi change from baseline.R)**  
   - Describes BMI changes over time

5. **[psbsan03_bmi change linear mixed effects model.R](analysis/psbsan03_bmi change linear mixed effects model.R)**  
   - Linear mixed-effects models for BMI change

6. **[psbsan04_incident obesity log-log regression.R](analysis/psbsan04_incident obesity log-log regression.R)**  
   - Discrete-time hazard models for incident obesity

7. **[psbsan05_spousal bmi change linear regression.R](analysis/psbsan05_spousal bmi change linear regression.R)**  
   - Additional linear regression models examining spousal BMI associations

---

## Methodological Considerations

### Advantages of Single Imputation
- Simpler to implement
- Faster computation
- Single complete dataset easier to work with

### Limitations of Single Imputation
- **Does not account for imputation uncertainty**
- Standard errors may be underestimated (confidence intervals too narrow)
- P-values may be too optimistic
- Less appropriate for datasets with substantial missing data

### Multiple Imputation (Main Analysis) vs Single Imputation

| Aspect                    | Multiple Imputation | Single Imputation |
|---------------------------|---------------------|-------------------|
| Number of imputations     | m = 30              | m = 1             |
| Imputation uncertainty    | Accounted for       | Ignored           |
| Standard errors           | Appropriately larger| Potentially biased|
| Computation time          | Longer              | Faster            |
| Statistical validity      | Preferred           | Sensitivity check |

---

## Interpretation

**If single imputation results are similar to main analysis:**  
→ Findings are robust across imputation methods  
→ Imputation uncertainty is minimal

**If single imputation results differ:**  
→ **Main multiple imputation analysis is more statistically valid**  
→ Differences likely due to underestimated uncertainty in single imputation  
→ Prioritize main analysis results for inference

---

## File Naming Convention

| Prefix   | Meaning                              |
|----------|--------------------------------------|
| `psbspre`| Single imputation preprocessing      |
| `psbsan` | Single imputation analysis           |

---

## Notes

- Single imputation is a **sensitivity analysis**, not the primary approach
- Standard errors from single imputation are likely underestimated
- Model specifications mirror the main analysis for comparison
- Archive folder contains superseded code versions

For the preferred analysis approach (multiple imputation), see the [main README](../README.md).
