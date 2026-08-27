###############################################################################
# 04_iptw_balance.R
# Engaging Future Scientists: Science Museum Visits and Science Identity
# Author: Shuhan (Alice) Ai
#
# Purpose: Construct inverse probability of treatment weights (IPTW), combine
#          them with the longitudinal survey weight, and assess covariate
#          balance.
# Prerequisite: Run 01_setup.R and 03_propensity_score.R first.
###############################################################################

# ---- 1. Normalize survey weights and compute IPTW ---------------------------

data.trimmed <- data.trimmed %>%
  mutate(
    W2W1STU_norm = W2W1STU / mean(W2W1STU, na.rm = TRUE),
    D = ifelse(p2museum == "Treatment", 1, 0),
    # ATE weights
    w.ate = (D * (1 / pscore)) + ((1 - D) * (1 / (1 - pscore))),
    # ATT weights
    w.att = D + ((1 - D) * (pscore / (1 - pscore))),
    # ATC weights
    w.atc = (D * ((1 - pscore) / pscore)) + (1 - D),
    # Combined IPTW x normalized survey weights
    w.ate_W2W1STU = w.ate * W2W1STU_norm,
    w.att_W2W1STU = w.att * W2W1STU_norm,
    w.atc_W2W1STU = w.atc * W2W1STU_norm
  )

table(data.trimmed$D)

# ---- 2. Covariate balance check (ATE weights) -------------------------------
# `covars` is defined in 01_setup.R

bal_ate.sciid <- bal.tab(
  x           = data.trimmed[, covars],
  treat       = data.trimmed$D,
  weights     = data.trimmed$w.ate_W2W1STU,
  estimand    = "ATE",
  s.d.denom   = "pooled",      # standardized-difference denominator
  m.threshold = 0.10,          # balance threshold
  un          = TRUE
)
bal_ate.sciid

# Love plot
love.plot(
  bal_ate.sciid,
  stats      = "mean.diffs",
  abs        = FALSE,
  binary     = "std",
  thresholds = c(m = 0.10),
  colors     = c("#ee84a8", "#71bced"),
  title      = "Covariate Balance after IPTW (ATE) for Science Identity"
) + theme_bw()

# ---- 3. Post-weighting descriptive statistics (Table 1 weighted) -----------

design.sciid <- svydesign(ids = ~1,
                          weights = ~w.ate_W2W1STU,
                          data = data.trimmed)

table_weighted.sciid <- svyCreateTableOne(
  vars   = covars,
  strata = "p2museum",
  data   = design.sciid
)
print(table_weighted.sciid)
