# Spousal BMI and Obesity Study (CARRS P4)

## Overview
This project examines how changes in BMI and obesity status of one spouse affect their partner's BMI and obesity outcomes over time 
using data from the CARRS (Center for Cardiometabolic Risk Reduction in South Asia) study.

**Research Question:** How does the change in BMI and obesity status of one person affect their partner's BMI and obesity status?

**Study Population:** Married couples from Delhi and Chennai, India, followed longitudinally across multiple waves of CARRS-1 and CARRS-2.

---

## Project Structure

```
p4_spouses_bmi/
├── preprocessing/         # Data cleaning and preparation
├── analysis/              # Main statistical analyses (multiple imputation)
├── cca/                   # Complete case analysis (sensitivity)
├── single imputation/     # Single imputation analysis (sensitivity)
├── functions/             # Custom R functions used across scripts
├── paper/                 # Paper figures and visualizations
├── data/                  # Data storage (not in git)
└── archive/               # Superseded code (for reference only)
```

---

## Workflow

### Step 1: Data Preprocessing
Run scripts in this order:

1. **[psbpre01_spouseyad clean.R](preprocessing/psbpre01_spouseyad clean.R)**  
   - Identifies spouse dyads from household data
   - Creates clean spouse linkage file

2. **[psbpre02_analytic subsets.R](preprocessing/psbpre02_analytic subsets.R)**  
   - Loads baseline and follow-up data from CARRS
   - Filters to Delhi & Chennai sites
   - Creates person-level longitudinal datasets

3. **[psbpre03_observed data.R](preprocessing/psbpre03_observed data.R)**  
   - Merges spouse dyad information with CARRS data

4. **[psbpre04_recoded data.R](preprocessing/psbpre04_recoded data.R)**  
   - Recodes categorical variables
   - Derives analytic variables (education, employment categories)

5. **[psbpre05_subsets before imputation.R](preprocessing/psbpre05_subsets before imputation.R)**  
   - Creates visit-specific datasets ready for imputation

### Step 2: Multiple Imputation
6. **[psban_imputation_by_visit.R](analysis/psban_imputation_by_visit.R)**  
   - Performs multiple imputation by visit using MICE package

### Step 3: Main Analysis
7. **[psban01_clean data.R](analysis/psban01_clean data.R)**  
   - Processes imputed datasets
   - Creates wide-format spouse-level data
   - Generates lagged BMI variables

8. **[psban02_descriptive characteristics.R](analysis/psban02_descriptive characteristics.R)**  
   - Computes baseline characteristics by sex

9. **[psban03_bmi mixed effect models.R](analysis/psban03_bmi mixed effect models.R)**  
   - Fits linear mixed-effects models for BMI change
   - Pools results across imputed datasets

10. **[psban04_incident obesity log-log regression.R](analysis/psban04_incident obesity log-log regression.R)**  
    - Analyzes incident obesity using discrete-time hazard models

### Step 4: Sensitivity Analyses
- **`cca/`**: Complete case analysis (no imputation)
- **`single imputation/`**: Single imputation approach

### Step 5: Paper Outputs
- **`paper/`**: Generate figures for manuscript

---

## Setup Instructions

### Required R Packages
Install packages by running:

```r
packages <- c(
  "dplyr", "tidyr",        # Data manipulation
  "mice",                  # Multiple imputation
  "lme4", "broom.mixed",   # Mixed-effects models
  "survival", "broom",     # Survival analysis
  "haven",                 # Read SAS files
  "openxlsx",              # Excel file I/O
  "ggplot2", "patchwork",  # Visualization
  "forcats", "glue"        # Utilities
)

install.packages(packages)
```

### Data Requirements
The following raw data files must be present in your working directory (defined in `.Rprofile`):

- `spousedyads.xlsx` - Spouse linkage file
- `baseline_2025_0312.sas7bdat` - CARRS baseline data
- `long_event_2025_0515.sas7bdat` - CARRS follow-up data
- `lab_2025_0414.sas7bdat` - Laboratory data files

### Configuration
1. Copy `.Rprofile.template` to `.Rprofile` (if provided)
2. Edit `.Rprofile` to set your data paths:
   ```r
   path_spouses_bmi_change_folder <- "YOUR_PROJECT_PATH"
   ```
3. Ensure subdirectories exist: `working/raw/`, `working/cleaned/`, `working/preprocessing/`

---

## File Naming Convention

Scripts follow a systematic naming pattern:

| Prefix   | Meaning                              |
|----------|--------------------------------------|
| `psbpre` | Preprocessing (all data)             |
| `psban`  | Analysis (multiple imputation)       |
| `psbcan` | Complete case analysis               |
| `psbsan` | Single imputation analysis           |

Numbers indicate execution order (e.g., `01`, `02`, `03`).

---

## Key Variables

### Identifiers
- `pid` - Person ID (unique individual)
- `hhid` - Household ID
- `carrs` - Cohort (1 = CARRS-1, 2 = CARRS-2)
- `fup` - Follow-up visit number (0 = baseline)

### Outcomes
- `bmi` - Body Mass Index (kg/m²)
- `obese` - Obesity status (BMI ≥ 30)

### Exposures
- `male_bmi_lag` - Husband's lagged BMI (previous visit)
- `female_bmi_lag` - Wife's lagged BMI (previous visit)

### Covariates
- `sex` - "male" or "female"
- `age` - Age in years
- `site` - "Delhi" or "Chennai"
- `edu_category` - Education level (4 categories)
- `employ_category` - Employment status (4 categories)
- `hhincome` - Household income
- `diabetes`, `famhx_dm` - Diabetes status and family history
- `smk_overall`, `alc_overall` - Smoking and alcohol use

---

## Custom Functions

Located in `functions/` directory:

- **[calculate_binary_var.R](functions/calculate_binary_var.R)** - Calculate proportions for binary variables
- **[calculate_continuous_var.R](functions/calculate_continuous_var.R)** - Calculate means/medians for continuous variables
- **[calculate_categorical_var.R](functions/calculate_categorical_var.R)** - Calculate frequencies for categorical variables
- **[pool_mi_mixed_results.R](functions/pool_mi_mixed_results.R)** - Pool mixed model results across imputations
- **[pool_mi_glm_results.R](functions/pool_mi_glm_results.R)** - Pool GLM results across imputations
- **[extract_coefficient.R](functions/extract_coefficient.R)** - Extract coefficients from fitted models
- **[egfr_ckdepi_2021.R](functions/egfr_ckdepi_2021.R)** - Calculate eGFR using CKD-EPI 2021 equation
- **[get_or_ci.R](functions/get_or_ci.R)** - Format odds ratios with confidence intervals

---

## Analysis Approach

This project uses **multiple imputation** to handle missing data:

1. **Imputation:** Missing covariates imputed separately by visit using MICE (m=30 imputations)
2. **Analysis:** Models fit on each imputed dataset independently
3. **Pooling:** Results combined using Rubin's rules

**Primary Models:**
- **BMI change:** Linear mixed-effects models with household random effects
- **Incident obesity:** Discrete-time log-log regression (complementary log-log link)

**Adjustments:**
- Unadjusted (crude)
- Model 1: Age, baseline BMI
- Model 2: + Demographics, clinical factors

---

## Reproducibility Notes

- All scripts source `.Rprofile` which sets working directories and loads common packages
- Run preprocessing scripts sequentially before running analysis scripts
- Imputation may take several hours depending on machine specs
- Set random seed in imputation scripts for reproducibility
- Archive folders contain old code versions - **do not use for current analysis**

---

## Output Files

Author:** Jiali Guo
**Email:** jguo2581@gmail.com
**Institution:** Emory University  
**Last Updated:** March 2, 2026

For questions about this analysis pipeline, please contact the author or refer to the research paper [P4 spousal BMI].

