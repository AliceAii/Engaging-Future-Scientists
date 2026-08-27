###############################################################################
# 05_outcome_models.R
# Engaging Future Scientists: Science Museum Visits and Science Identity
# Author: Shuhan (Alice) Ai
#
# Purpose: Estimate the effect of science museum/planetarium visits on 11th-
#          grade science identity using doubly robust IPTW linear regression
#          (ATE, ATT, ATC) with nested model specifications and moderation
#          tests.
# Prerequisite: Run 01_setup.R, 03_propensity_score.R, 04_iptw_balance.R first.
###############################################################################

# ---- 1. Unconditional outcome model with ATE weights ------------------------

ate_sciid <- lm(
  x2sciid.sd ~
    x1sciid.sd +
    p2museum,
  data = data.trimmed, weights = w.ate_W2W1STU
)
sqrt(hccm(ate_sciid)[3, 3])   # HC2 robust SE for the treatment coefficient

table.ate_sciid <- tidy.coeftable(ate_sciid)
knitr::kable(table.ate_sciid, digits = 3,
             caption = "ATE weighted linear regression: unconditional model")

# ---- 2. Nested model - Background block -------------------------------------

ate_sciid2 <- lm(
  x2sciid.sd ~
    x1sciid.sd + p2museum +
    x1locale + x1control + a1mspdintrst + a1msmentor + a1msspeaker +
    a1msfldtrip + a1msprgms +
    x1race + x1sex + x1sesq5,
  data = data.trimmed, weights = w.ate_W2W1STU
)
sqrt(hccm(ate_sciid2)[3, 3])

table.ate_sciid2 <- tidy.coeftable(ate_sciid2)
knitr::kable(table.ate_sciid2, digits = 3,
             caption = "Nested ATE model: Background block")

# ---- 3. Nested model - Competence / Perception / Expectations block ---------

ate_sciid3 <- lm(
  x2sciid.sd ~
    x1sciid.sd + p2museum +
    x1locale + x1control + a1mspdintrst + a1msmentor + a1msspeaker +
    a1msfldtrip + a1msprgms +
    x1race + x1sex + x1sesq5 +
    x1txmscr.sd + x1sciint.sd + x1stuedexpct,
  data = data.trimmed, weights = w.ate_W2W1STU
)
sqrt(hccm(ate_sciid3)[3, 3])

table.ate_sciid3 <- tidy.coeftable(ate_sciid3)
knitr::kable(table.ate_sciid3, digits = 3,
             caption = "Nested ATE model: Competence/Perception/Expectations block")

# ---- 4. Doubly robust ATE estimate (Table 3) --------------------------------

ate_dr_sciid <- lm(
  x2sciid.sd ~
    x1sciid.sd + p2museum +
    x1locale + x1control + a1mspdintrst + a1msmentor + a1msspeaker +
    a1msfldtrip + a1msprgms +
    x1race + x1sex + x1sesq5 +
    x1txmscr.sd + x1sciint.sd + x1stuedexpct +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1advbios09 + s1sfall09 +
    s1mclub + s1sclub +
    s1sbooks.sd + s1webinfo.sd,
  data = data.trimmed, weights = w.ate_W2W1STU
)
sqrt(hccm(ate_dr_sciid)[3, 3])

table.ate_dr_sciid <- tidy.coeftable(ate_dr_sciid)
knitr::kable(table.ate_dr_sciid, digits = 3,
             caption = "Doubly robust ATE: 11th-grade science identity (Table 4)")

# ---- 5. Doubly robust ATT estimate ------------------------------------------

att_dr_sciid <- lm(
  x2sciid.sd ~
    x1sciid.sd + p2museum +
    x1locale + x1control + a1mspdintrst + a1msmentor + a1msspeaker +
    a1msfldtrip + a1msprgms +
    x1race + x1sex + x1sesq5 +
    x1txmscr.sd + x1sciint.sd + x1stuedexpct +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 + s1advbios09 +
    s1mclub + s1sclub +
    s1sbooks.sd + s1webinfo.sd,
  data = data.trimmed, weights = w.att_W2W1STU
)
sqrt(hccm(att_dr_sciid)[3, 3])

table.att_dr_sciid <- tidy.coeftable(att_dr_sciid)
knitr::kable(table.att_dr_sciid, digits = 3,
             caption = "Doubly robust ATT: 11th-grade science identity")

# ---- 6. Doubly robust ATC estimate ------------------------------------------

atc_dr_sciid <- lm(
  x2sciid.sd ~
    x1sciid.sd + p2museum +
    x1locale + x1control + a1mspdintrst + a1msmentor + a1msspeaker +
    a1msfldtrip + a1msprgms +
    x1race + x1sex + x1sesq5 +
    x1txmscr.sd + x1sciint.sd + x1stuedexpct +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 + s1advbios09 +
    s1mclub + s1sclub +
    s1sbooks.sd + s1webinfo.sd,
  data = data.trimmed, weights = w.atc_W2W1STU
)
sqrt(hccm(atc_dr_sciid)[3, 3])

table.atc_dr_sciid <- tidy.coeftable(atc_dr_sciid)
knitr::kable(table.atc_dr_sciid, digits = 3,
             caption = "Doubly robust ATC: 11th-grade science identity")

# ---- 7. Moderation by race, sex, and SES ------------------------------------

ate_sciid4 <- lm(
  x2sciid.sd ~
    x1sciid.sd + p2museum +
    x1locale + x1control + a1mspdintrst + a1msmentor + a1msspeaker +
    a1msfldtrip + a1msprgms +
    x1race + x1sex + x1sesq5 +
    x1txmscr.sd + x1sciint.sd + x1stuedexpct +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 + s1advbios09 +
    s1mclub + s1sclub +
    s1sbooks.sd + s1webinfo.sd +
    x1race:p2museum + x1sex:p2museum + x1sesq5:p2museum,
  data = data.trimmed, weights = w.ate_W2W1STU
)
sqrt(hccm(ate_sciid4)[3, 3])

table.ate_sciid4 <- tidy.coeftable(ate_sciid4)
knitr::kable(table.ate_sciid4, digits = 3,
             caption = "ATE weighted results with treatment x moderator interactions")

# ---- 8. Model comparison (AIC / BIC) ----------------------------------------

AIC(ate_sciid, ate_sciid2, ate_sciid3, ate_dr_sciid)
BIC(ate_sciid, ate_sciid2, ate_sciid3, ate_dr_sciid)
