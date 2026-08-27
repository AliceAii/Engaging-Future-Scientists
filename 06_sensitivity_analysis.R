###############################################################################
# 06_sensitivity_analysis.R
# Engaging Future Scientists: Science Museum Visits and Science Identity
# Author: Shuhan (Alice) Ai
#
# Purpose: Sensitivity analysis using sensemakr (Cinelli & Hazlett, 2020).
#          Ninth-grade science identity serves as the benchmark covariate:
#          robustness values indicate how strong an unobserved confounder
#          would need to be, relative to that benchmark, to nullify the
#          treatment effect.
# Prerequisite: Run 01_setup.R through 05_outcome_models.R first.
###############################################################################

# ---- 1. Naive estimate (no PS, no survey weights, covariate-adjusted) ------

sen.m01 <- sensemakr(
  model                = m01,
  treatment            = "p2museumTreatment",
  benchmark_covariates = "x1sciid.sd",
  kd     = 0.25:0.75,   # strength of confounding with the treatment
  ky     = 0.25:0.75,   # strength of confounding with the outcome
  q      = 1,           # threshold for nullifying the estimate
  alpha  = 0.05,
  reduce = TRUE         # test whether confounding could reduce the effect
)
summary(sen.m01)
plot(sen.m01)

# ---- 2. Doubly robust ATE (PS model + IPTW x survey weight) -----------------

sen1 <- sensemakr(
  model                = ate_dr_sciid,
  treatment            = "p2museumTreatment",
  benchmark_covariates = "x1sciid.sd",
  kd     = 0.25:0.75,
  ky     = 0.25:0.75,
  q      = 1,
  alpha  = 0.05,
  reduce = TRUE
)
summary(sen1)
plot(sen1)
