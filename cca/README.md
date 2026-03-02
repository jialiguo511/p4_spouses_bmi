# Complete Case Analysis (CCA)

## Overview
This folder contains **sensitivity analyses** using only participants with complete data (no missing values). This approach excludes individuals with any missing covariates, resulting in a smaller but complete dataset.

**Purpose:** To assess whether results from the main multiple imputation analysis are robust when restricting to participants with complete data.

**Key Difference from Main Analysis:** No imputation is performed; only participants with complete data on all covariates are included.

---

## Analysis Structure

```
cca/
├── preprocessing/    # Data preparation for complete cases
├── analysis/        # Statistical models on complete cases
└── paper/          # Figures specific to CCA
```

---

## Workflow

### Step 1: Preprocessing
Run scripts in order:

1. **[psbcpre01_recoded data.R](preprocessing/psbcpre01_recoded data.R)**  
   - Recodes variables for complete case analysis

2. **[psbcpre02_bmi complete cases.R](preprocessing/psbcpre02_bmi complete cases.R)**  
   - Identifies participants with complete BMI data across visits

3. **[psbcpre03_spouse identified data.R](preprocessing/psbcpre03_spouse identified data.R)**  
   - Creates spouse-level dataset with complete cases only

### Step 2: Analysis

4. **[psbcan01_descriptive characteristics.R](analysis/psbcan01_descriptive characteristics.R)**  
   - Descriptive statistics for complete case sample

5. **[psbcan02_bmi linear mixed effects model.R](analysis/psbcan02_bmi linear mixed effects model.R)**  
   - Linear mixed-effects models for BMI change

6. **[psbcan03_incident obesity log-log regression.R](analysis/psbcan03_incident obesity log-log regression.R)**  
   - Discrete-time hazard models for incident obesity

---

## Expected Sample Size

The complete case analysis will have a **smaller sample size** than the main analysis due to:
- Exclusion of participants with any missing covariates
- Exclusion of participants with incomplete follow-up data

This reduced sample may affect:
- Statistical power
- Generalizability of findings
- Precision of estimates (wider confidence intervals)

---

## Interpretation

**If CCA results are similar to main analysis:**  
→ Findings are robust to missing data assumptions

**If CCA results differ substantially:**  
→ Missing data may not be missing at random (MNAR)  
→ Main multiple imputation results should be interpreted with caution  
→ Consider additional sensitivity analyses

---

## File Naming Convention

| Prefix   | Meaning                              |
|----------|--------------------------------------|
| `psbcpre`| CCA preprocessing                    |
| `psbcan` | CCA analysis                         |

---

## Notes

- All analyses mirror the main analysis workflow but on complete cases only
- Model specifications match those in the main analysis for direct comparison
- Archive folder contains superseded code versions

For the main analysis approach (multiple imputation), see the [main README](../README.md).
