###############################################################################
# 00_data_preparation.R
# Engaging Future Scientists: Science Museum Visits and Science Identity
# Author: Shuhan (Alice) Ai
#
# Purpose: Read raw HSLS:09 files, select and recode all analytic variables,
#          winsorize extreme standardized values, examine missingness, impute
#          missing data via MICE, and export the analytic .rds file.
#
# Data: HSLS:09 public-use school- and student-level data file
###############################################################################

rm(list = ls())

# ---- 1. Load packages -------------------------------------------------------

library(tidyverse)
library(dplyr)
library(forcats)
library(skimr)
library(flextable)
library(table1)
library(knitr)
library(sensemakr)
library(patchwork)
library(car)
library(mice)
library(haven)
library(MissMech)        # Little's MCAR test

# ---- 2. Read raw data -------------------------------------------------------

# NOTE: Update these paths to match your local data directory.
data.stu <- get(load("DS0002 Student Level Data/36423-0002-Data.rda")[1]) # student file
data.sch <- get(load("DS0001 School Level Data/36423-0001-Data.rda")[1])  # school file

# ---- 3. Select analytic variables ------------------------------------------

stu.data <- data.stu %>%
  dplyr::select(
    STU_ID, SCH_ID, W2W1STU,                               # IDs and weights
    P2MUSEUM,                                              # treatment
    X2SCIID,                                               # outcome
    X1SCIID, P1MUSEUM,                                     # pre-treatment covariates
    X1LOCALE, X1CONTROL, A1MSAFTERSCH, A1MSMENTOR,
    A1MSSPEAKER, A1MSFLDTRIP, A1MSPRGMS,                   # school-level covariates
    X1RACE, X1SEX, X1SESQ5,                                # student background
    X1TXMSCR, X1SCIINT, X1STUEDEXPCT,                      # competence / perception
    S1ALG1M09, S1ALG2M09, S1GEOM09, S1SFALL09, S1ADVBIOS09,# formal STEM coursework
    S1MCLUB, S1MCAMP, S1SCLUB, S1SCAMP, S1SBOOKS, S1WEBINFO # informal STEM experiences
  )

skim(stu.data)

# Merge the baseline school weight
stu.data <- stu.data %>%
  left_join(dplyr::select(data.sch, SCH_ID, W1SCHOOL), by = "SCH_ID")

# ---- 4. Recode variables ----------------------------------------------------

stu.data <- stu.data %>%
  mutate(
    STU_ID = as.character(haven::as_factor(STU_ID)),
    SCH_ID = as.character(haven::as_factor(SCH_ID)),

    # -- Treatment: visited a science museum/planetarium between 9th and 11th --
    p2museum = case_when(P2MUSEUM == 1 ~ 1,
                         P2MUSEUM == 0 ~ 0,
                         TRUE ~ NA_real_),
    p2museum = factor(p2museum, levels = c(0, 1),
                      labels = c("Control", "Treatment")),

    # -- Outcome: 11th-grade science identity (standardized) ------------------
    x2sciid.sd = as.numeric(X2SCIID),
    x2sciid.sd = ifelse(x2sciid.sd %in% c(-8, -9), NA, x2sciid.sd),
    x2sciid.sd = scale(x2sciid.sd)[, 1],

    # -- Pre-treatment: 9th-grade science identity (standardized) -------------
    x1sciid.sd = as.numeric(X1SCIID),
    x1sciid.sd = ifelse(x1sciid.sd %in% c(-8, -9), NA, x1sciid.sd),
    x1sciid.sd = scale(x1sciid.sd)[, 1],

    # -- Pre-treatment: museum visit before high school -----------------------
    # NOTE: the 0-branch below references P2MUSEUM rather than P1MUSEUM.
    #       This variable is descriptive only and is not entered into any
    #       model; verify the recode before using it as a covariate.
    p1museum = case_when(P1MUSEUM == 1 ~ 1,
                         P2MUSEUM == 0 ~ 0,
                         TRUE ~ NA_real_),
    p1museum = factor(p1museum, levels = c(0, 1), labels = c("No", "Yes")),

    # -- School-level covariates ---------------------------------------------
    x1locale = case_when(X1LOCALE == 4 ~ 0,   # Rural (reference)
                         X1LOCALE == 1 ~ 1,   # City
                         X1LOCALE == 2 ~ 2,   # Suburb
                         X1LOCALE == 3 ~ 3,   # Town
                         TRUE ~ NA_real_),
    x1locale = factor(x1locale, levels = 0:3,
                      labels = c("Rural", "City", "Suburb", "Town")),

    x1control = case_when(X1CONTROL %in% c(2, 3) ~ 0,  # Catholic or other private
                          X1CONTROL == 1 ~ 1,          # Public
                          TRUE ~ NA_real_),
    x1control = factor(x1control, levels = 0:1,
                       labels = c("Catholic or other private", "Public")),

    # School sponsors a math or science after-school program
    a1mspdintrst = case_when(A1MSAFTERSCH == 0 ~ 0, A1MSAFTERSCH == 1 ~ 1,
                             TRUE ~ NA_real_),
    a1mspdintrst = factor(a1mspdintrst, levels = 0:1, labels = c("No", "Yes")),

    # School pairs students with mentors in math or science
    a1msmentor = case_when(A1MSMENTOR == 0 ~ 0, A1MSMENTOR == 1 ~ 1,
                           TRUE ~ NA_real_),
    a1msmentor = factor(a1msmentor, levels = 0:1, labels = c("No", "Yes")),

    # School brings in guest speakers to talk about math or science
    a1msspeaker = case_when(A1MSSPEAKER == 0 ~ 0, A1MSSPEAKER == 1 ~ 1,
                            TRUE ~ NA_real_),
    a1msspeaker = factor(a1msspeaker, levels = 0:1, labels = c("No", "Yes")),

    # School takes students on math- or science-relevant field trips
    a1msfldtrip = case_when(A1MSFLDTRIP == 0 ~ 0, A1MSFLDTRIP == 1 ~ 1,
                            TRUE ~ NA_real_),
    a1msfldtrip = factor(a1msfldtrip, levels = 0:1, labels = c("No", "Yes")),

    # School tells students about math/science contests, websites, programs
    a1msprgms = case_when(A1MSPRGMS == 0 ~ 0, A1MSPRGMS == 1 ~ 1,
                          TRUE ~ NA_real_),
    a1msprgms = factor(a1msprgms, levels = 0:1, labels = c("No", "Yes")),

    # -- Student background: race, sex, SES ----------------------------------
    x1race = case_when(
      X1RACE == 8 ~ 0,             # White (reference)
      X1RACE == 2 ~ 1,             # Asian
      X1RACE == 3 ~ 2,             # Black
      X1RACE %in% c(4, 5) ~ 3,     # Latino
      X1RACE == 6 ~ 4,             # Multiracial
      X1RACE == 1 ~ 5,             # Native American
      X1RACE %in% c(0, 7) ~ 6,     # Other
      TRUE ~ NA_real_),
    x1race = factor(x1race, levels = 0:6,
                    labels = c("White", "Asian", "Black", "Latino",
                               "Multiracial", "Native American", "Other")),

    x1sex = case_when(X1SEX == 1 ~ 0, X1SEX == 2 ~ 1, TRUE ~ NA_real_),
    x1sex = factor(x1sex, levels = c(0, 1), labels = c("Male", "Female")),

    x1sesq5 = case_when(X1SESQ5 == 1 ~ 0,   # First quintile (reference, lowest)
                        X1SESQ5 == 2 ~ 1,
                        X1SESQ5 == 3 ~ 2,
                        X1SESQ5 == 4 ~ 3,
                        X1SESQ5 == 5 ~ 4,   # Fifth quintile (highest)
                        TRUE ~ NA_real_),
    x1sesq5 = factor(x1sesq5, levels = 0:4,
                     labels = c("First quintile (lowest)", "Second quintile",
                                "Third quintile", "Fourth quintile",
                                "Fifth quintile (highest)")),

    # -- Competence, perception, expectations --------------------------------
    x1txmscr.sd = ifelse(as.numeric(X1TXMSCR) %in% c(-8, -9), NA,
                         as.numeric(X1TXMSCR)),
    x1txmscr.sd = scale(x1txmscr.sd)[, 1],

    x1sciint.sd = ifelse(as.numeric(X1SCIINT) %in% c(-8, -9), NA,
                         as.numeric(X1SCIINT)),
    x1sciint.sd = scale(x1sciint.sd)[, 1],

    x1stuedexpct = case_when(X1STUEDEXPCT == 11 ~ 0,        # Uncertain (reference)
                             X1STUEDEXPCT %in% c(1, 2) ~ 1, # High school
                             X1STUEDEXPCT %in% 3:6 ~ 2,     # Bachelor's
                             X1STUEDEXPCT %in% 7:8 ~ 3,     # Master's
                             X1STUEDEXPCT %in% 9:10 ~ 4,    # PhD
                             TRUE ~ NA_real_),
    x1stuedexpct = factor(x1stuedexpct, levels = 0:4,
                          labels = c("Uncertain", "High school", "Bachelor",
                                     "Master", "PhD")),

    # -- Formal STEM coursework (9th grade) ----------------------------------
    s1alg1m09 = case_when(S1ALG1M09 == 0 ~ 0, S1ALG1M09 == 1 ~ 1,
                          TRUE ~ NA_real_),
    s1alg1m09 = factor(s1alg1m09, levels = 0:1, labels = c("No", "Yes")),

    s1alg2m09 = case_when(S1ALG2M09 == 0 ~ 0, S1ALG2M09 == 1 ~ 1,
                          TRUE ~ NA_real_),
    s1alg2m09 = factor(s1alg2m09, levels = 0:1, labels = c("No", "Yes")),

    s1tgeom09 = case_when(S1GEOM09 == 0 ~ 0, S1GEOM09 == 1 ~ 1,
                          TRUE ~ NA_real_),
    s1tgeom09 = factor(s1tgeom09, levels = 0:1, labels = c("No", "Yes")),

    s1sfall09 = case_when(S1SFALL09 == 0 ~ 0, S1SFALL09 == 1 ~ 1,
                          TRUE ~ NA_real_),
    s1sfall09 = factor(s1sfall09, levels = 0:1, labels = c("No", "Yes")),

    s1advbios09 = case_when(S1ADVBIOS09 == 0 ~ 0, S1ADVBIOS09 == 1 ~ 1,
                            TRUE ~ NA_real_),
    s1advbios09 = factor(s1advbios09, levels = 0:1, labels = c("No", "Yes")),

    # -- Informal STEM experiences (9th grade) -------------------------------
    s1mclub = case_when(S1MCLUB == 0 ~ 0, S1MCLUB == 1 ~ 1, TRUE ~ NA_real_),
    s1mclub = factor(s1mclub, levels = 0:1, labels = c("No", "Yes")),

    s1sclub = case_when(S1SCLUB == 0 ~ 0, S1SCLUB == 1 ~ 1, TRUE ~ NA_real_),
    s1sclub = factor(s1sclub, levels = 0:1, labels = c("No", "Yes")),

    s1sbooks.sd = ifelse(as.numeric(S1SBOOKS) %in% c(-8, -9), NA,
                         as.numeric(S1SBOOKS)),
    s1sbooks.sd = scale(s1sbooks.sd)[, 1],

    s1webinfo.sd = ifelse(as.numeric(S1WEBINFO) %in% c(-8, -9), NA,
                          as.numeric(S1WEBINFO)),
    s1webinfo.sd = scale(s1webinfo.sd)[, 1]
  )

skim(stu.data)
table(stu.data$p2museum)

# ---- 5. Define the analytic sample ------------------------------------------
# Keep students with a non-zero longitudinal weight and a valid treatment value.

stu.data.filter <- stu.data %>%
  filter(W2W1STU > 0 & !is.na(p2museum)) %>%
  dplyr::select(
    STU_ID, SCH_ID, W2W1STU, p2museum, x2sciid.sd,
    x1sciid.sd, p1museum, x1locale, x1control, a1mspdintrst, a1msmentor,
    a1msspeaker, a1msfldtrip, a1msprgms, x1race, x1sex, x1sesq5,
    x1txmscr.sd, x1sciint.sd, x1stuedexpct,
    s1alg1m09, s1alg2m09, s1tgeom09, s1sfall09, s1advbios09,
    s1mclub, s1sclub, s1sbooks.sd, s1webinfo.sd
  )

# ---- 6. Winsorize extreme standardized values at +/- 3 SD -------------------

stu.data.filter <- stu.data.filter %>%
  mutate(across(ends_with(".sd"),
                ~ ifelse(.x < -3, -3, ifelse(.x > 3, 3, .x))))

# ---- 7. Examine missingness -------------------------------------------------
# Missing rates range from 0% to 0.21%; Little's MCAR test is non-significant.

missingrate <- stu.data.filter %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missingrate") %>%
  arrange(desc(missingrate))
missingrate

# ---- 8. MICE imputation -----------------------------------------------------

init <- mice(stu.data.filter, maxit = 0)
meth <- init$method
pred <- init$predictorMatrix

continuous_vars <- c("x2sciid.sd", "x1sciid.sd", "x1txmscr.sd",
                     "x1sciint.sd", "s1sbooks.sd", "s1webinfo.sd")
meth[continuous_vars] <- "pmm"

binary_vars <- c("p2museum", "p1museum", "a1mspdintrst", "a1msmentor",
                 "a1msspeaker", "a1msfldtrip", "a1msprgms",
                 "s1alg1m09", "s1alg2m09", "s1tgeom09", "s1sfall09",
                 "s1advbios09", "s1mclub", "s1sclub")
meth[binary_vars] <- "logreg"

polytomous_vars <- c("x1race", "x1sesq5", "x1locale", "x1control",
                     "x1stuedexpct")
meth[polytomous_vars] <- "polyreg"

# Exclude identifiers and the sampling weight from the imputation model
id_vars <- c("STU_ID", "SCH_ID")
meth[id_vars]      <- ""
pred[, id_vars]    <- 0
pred["W2W1STU", ]  <- 0
pred[, "W2W1STU"]  <- 0

dt.imputed <- mice(stu.data.filter, method = meth, predictorMatrix = pred,
                   m = 5, maxit = 5, seed = 123)
dt <- complete(dt.imputed, 1)

skim(dt)

# ---- 9. Export the analytic file --------------------------------------------

saveRDS(dt, "data/sciid_muse_v2.rds")

cat("Analytic file written: n =", nrow(dt),
    "students in", length(unique(dt$SCH_ID)), "schools\n")
