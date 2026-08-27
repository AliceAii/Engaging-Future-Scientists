###############################################################################
# 01_setup.R
# Engaging Future Scientists: Science Museum Visits and Science Identity
# Author: Shuhan (Alice) Ai
#
# Purpose: Load packages, read the imputed analytic file, apply effect coding,
#          and define helper functions used throughout the analysis.
###############################################################################

rm(list = ls())

# ---- 1. Load packages -------------------------------------------------------

library(tidyverse)
library(skimr)
library(flextable)
library(table1)
library(tableone)
library(knitr)
library(MatchIt)
library(WeightIt)
library(cobalt)          # love plots for balance diagnostics
library(marginaleffects)
library(sensemakr)       # sensitivity analysis
library(patchwork)
library(car)
library(naniar)
library(mice)
library(haven)
library(lm.beta)         # standardized beta coefficients
library(lme4)            # mixed-effects / multilevel models
library(lmtest)
library(survey)          # survey weights & robust SEs
library(broom)           # tidy model output
library(merTools)
library(lmerTest)
library(WeMix)           # MLM with survey weights
library(sandwich)

# ---- 2. Read data -----------------------------------------------------------

# NOTE: Update this path to match your local data directory.
# Produced by 00_data_preparation.R
data <- readRDS("data/sciid_muse_v2.rds")

# ---- 3. Effect coding for race ----------------------------------------------

str(data$x1race)
contrasts(data$x1race) <- contr.sum(7)
contrasts(data$x1race)

# ---- 4. Helper functions: coefficient tables --------------------------------

# Linear model coefficient table
tidy.coeftable <- function(model, digits = 3) {
  model_summary <- summary(model)
  b    <- model_summary$coefficients[, "Estimate"]
  se   <- model_summary$coefficients[, "Std. Error"]
  pval <- model_summary$coefficients[, "Pr(>|t|)"]
  beta <- lm.beta(model)$standardized.coefficients

  model_data <- model$model
  y_var      <- names(model_data)[1]
  y_numeric  <- if (is.factor(model_data[[y_var]])) {
    as.numeric(model_data[[y_var]]) - 1
  } else {
    model_data[[y_var]]
  }

  corr <- rep(NA, length(b)); names(corr) <- names(b)
  for (i in 2:length(b)) {
    var_name <- names(b)[i]
    if (var_name %in% names(model_data)) {
      corr[i] <- cor(model_data[[var_name]], y_numeric, use = "complete.obs")
    } else {
      for (col_name in names(model_data)) {
        if (startsWith(var_name, col_name) && is.factor(model_data[[col_name]])) {
          corr[i] <- cor(as.numeric(model_data[[col_name]]), y_numeric,
                         use = "complete.obs")
          break
        }
      }
    }
  }

  get_stars <- function(p) {
    if (is.na(p)) return("")
    else if (p < 0.001) return("***")
    else if (p < 0.01)  return("**")
    else if (p < 0.05)  return("*")
    else if (p < 0.1)   return(" ")
    else return("")
  }

  data.frame(
    Variable    = names(b),
    Correlation = round(corr, digits),
    Coefficient = round(b, digits),
    SE          = round(se, digits),
    Beta        = round(beta, digits),
    p_value     = round(pval, digits),
    Sig         = sapply(pval, get_stars),
    row.names   = NULL
  )
}

# Logistic model coefficient table
tidy.logit.table <- function(model, digits = 3) {
  model_summary <- summary(model)
  b    <- model_summary$coefficients[, "Estimate"]
  se   <- model_summary$coefficients[, "Std. Error"]
  pval <- model_summary$coefficients[, "Pr(>|z|)"]
  OR   <- exp(b)

  model_data <- model$model
  y_var      <- names(model_data)[1]
  y_numeric  <- if (is.factor(model_data[[y_var]])) {
    as.numeric(model_data[[y_var]])
  } else {
    model_data[[y_var]]
  }

  corr <- rep(NA, length(b)); names(corr) <- names(b)
  for (i in 2:length(b)) {
    var_name <- names(b)[i]
    if (var_name %in% names(model_data)) {
      corr[i] <- cor(model_data[[var_name]], y_numeric, use = "complete.obs")
    } else {
      for (col_name in names(model_data)) {
        if (startsWith(var_name, col_name) && is.factor(model_data[[col_name]])) {
          corr[i] <- cor(as.numeric(model_data[[col_name]]), y_numeric,
                         use = "complete.obs")
          break
        }
      }
    }
  }

  get_stars <- function(p) {
    if (is.na(p)) return("")
    else if (p < 0.001) return("***")
    else if (p < 0.01)  return("**")
    else if (p < 0.05)  return("*")
    else if (p < 0.1)   return(" ")
    else return("")
  }

  data.frame(
    Variable    = names(b),
    Correlation = round(corr, digits),
    Coefficient = round(b, digits),
    SE          = round(se, digits),
    OR          = round(OR, digits),
    p_value     = round(pval, digits),
    Sig         = sapply(pval, get_stars)
  )
}

# ---- 5. Shared covariate list -----------------------------------------------
# Used by the propensity score model, balance diagnostics, and outcome models.

covars <- c(
  "x1locale", "x1control", "a1mspdintrst", "a1msmentor", "a1msspeaker",
  "a1msfldtrip", "a1msprgms",
  "x1race", "x1sex", "x1sesq5",
  "x1txmscr.sd", "x1sciint.sd", "x1stuedexpct",
  "s1alg1m09", "s1alg2m09", "s1tgeom09", "s1sfall09", "s1advbios09",
  "s1mclub", "s1sclub",
  "s1sbooks.sd", "s1webinfo.sd", "x1sciid.sd"
)

cat("Setup complete.\n")
cat("  Science identity sample: n =", nrow(data),
    "students in", length(unique(data$SCH_ID)), "schools\n")
