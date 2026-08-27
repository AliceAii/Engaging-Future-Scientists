# Engaging Future Scientists: The Effect of Science Museum Visits on High School Students' Science Identity Development

**Author:** Shuhan Ai

## Overview

This repository contains the R code used in the paper *"Engaging Future Scientists: The Effect of Science Museum Visits on High School Students' Science Identity Development"*, published in the *International Journal of STEM Education*.

Using data from the High School Longitudinal Study of 2009 (HSLS:09), this study examines whether visiting a science museum or planetarium between ninth and 11th grade is associated with the development of students' science identity. The study employs a quasi-experimental design with propensity score–based inverse probability of treatment weighting (IPTW) and doubly robust estimation.

## Research Questions

1. What individual and contextual factors are associated with students' likelihood of visiting science museums or planetariums during high school?
2. Do students who visited science museums and planetariums between ninth and eleventh grade demonstrate higher science identity at the end of eleventh grade compared to peers with similar demographic and academic backgrounds who did not visit?

## Data

This study uses public-use data from the [High School Longitudinal Study of 2009 (HSLS:09)](https://nces.ed.gov/surveys/hsls09/).

The analytic sample includes **7,290 students** from **935 high schools** who had complete data on key variables and non-zero longitudinal sample weights (W2W1STU). After trimming to the region of common support, **7,261 students** remain in the weighted analyses.

Missing data rates ranged from 0% to 2.1%; Little's MCAR test was non-significant, and missing values were imputed via MICE. Standardized continuous variables were winsorized at ±3 SD.

## Repository Structure

```
├── README.md
├── R/
│   ├── 00_data_preparation.R      # Read raw HSLS:09 files, recode/clean variables, impute missing data
│   ├── 01_setup.R                  # Load packages, read imputed .rds file, apply effect coding
│   ├── 02_descriptive_analysis.R   # Descriptive statistics, naive estimates
│   ├── 03_propensity_score.R       # Propensity score estimation, common support, trimming 
│   ├── 04_iptw_balance.R           # IPTW construction, covariate balance checks 
│   ├── 05_outcome_models.R         # Doubly robust ATE/ATT/ATC linear regression
│   └── 06_sensitivity_analysis.R   # Sensitivity analysis via sensemakr
└── data/                 
```

## Analytic Approach

| Step | Script | Description |
|------|--------|-------------|
| **Data Preparation** | `00_data_preparation.R` | Read raw HSLS:09 SPSS files; select and recode all analytic variables (treatment, outcome, covariates); winsorize extreme standardized values; check missingness; impute missing values via MICE with stochastic single imputation; export the `.rds` analytic file |
| **Setup** | `01_setup.R` | Load R packages, read the imputed `.rds` file, apply effect coding for race, define helper functions for coefficient tables |
| **Descriptive Analysis** | `02_descriptive_analysis.R` | Compare visitor vs. non-visitor groups on all baseline covariates; estimate naive (unweighted) linear regression models with HC2 robust standard errors |
| **Propensity Score Estimation** | `03_propensity_score.R` | Fit logistic regression to estimate propensity scores; check VIF for multicollinearity; visualize common support on the probability and logit scales; trim extreme propensity scores |
| **IPTW & Balance** | `04_iptw_balance.R` | Compute ATE, ATT, and ATC inverse probability weights; combine with normalized survey weights; assess balance using standardized mean differences; produce post-weighting descriptives |
| **Outcome Models** | `05_outcome_models.R` | Estimate nested linear regression models; report ATE, ATT, ATC estimates; test treatment × race, sex, and SES interactions |
| **Sensitivity Analysis** | `06_sensitivity_analysis.R` | Compute robustness values (RV) using `sensemakr` to quantify how strong an unobserved confounder must be to nullify the treatment effect, benchmarked against ninth-grade science identity |

## Key Findings

- Students who visited a science museum or planetarium between ninth and 11th grade scored higher on 11th-grade science identity than observationally similar peers who did not (ATE = 0.069 SD).
- Relative to students in the lowest SES quintile, those in the third (OR = 1.421, p < 0.001), fourth (OR = 1.746, p < 0.001), and fifth quintiles (OR = 2.276, p < 0.001) had progressively higher odds of visiting a science museum, indicating a steep socioeconomic gradient in access to these informal learning environments. 
- Regarding demographic characteristics, women reported significantly lower science identity than men (b = -0.092, p < 0.001). In comparison to the grand mean, Asian students reported significantly higher science identity (b = 0.163, p < 0.01), while Latino (b = -0.078, p < 0.05) and Multiracial students (b = -0.094, p < 0.05) reported significantly lower science identity.

## Citation

> Ai, S. (2026). Engaging future scientists: The effect of science museum visits on high school students' science identity development. *International Journal of STEM Education*. https://doi.org/10.1186/s40594-026-00644-9

## License

This repository is provided for academic reproducibility purposes. The code is available under the [MIT License](https://opensource.org/licenses/MIT). The HSLS:09 data are subject to NCES public-use data license agreements.
