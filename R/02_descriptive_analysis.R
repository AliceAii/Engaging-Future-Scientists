###############################################################################
# 02_descriptive_analysis.R
# Engaging Future Scientists: Science Museum Visits and Science Identity
# Author: Shuhan (Alice) Ai
#
# Purpose: Descriptive statistics (Table 1) and naive (unweighted) estimates
#          of the association between museum visits and science identity.
# Prerequisite: Run 01_setup.R first.
###############################################################################

# ---- 1. Sample overview -----------------------------------------------------

cat("Science identity sample: n =", nrow(data),
    "students in", length(unique(data$SCH_ID)), "schools\n")

# Treatment / control split
autofit(as_flextable(table(data$p2museum)))

# ---- 2. Descriptive table by treatment group (Table 1) ----------------------

descr <- table1(
  ~ x2sciid.sd +                                            # outcome
    x1sciid.sd +                                            # pre-treatment
    x1locale + x1control + a1mspdintrst + a1msmentor +
    a1msspeaker + a1msfldtrip + a1msprgms +                 # school-level
    x1race + x1sex + x1sesq5 +                              # background
    x1txmscr.sd + x1sciint.sd + x1stuedexpct +              # competence/perception
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 +
    s1advbios09 + s1mclub + s1sclub +
    s1sbooks.sd + s1webinfo.sd |                            # STEM experiences
    as.factor(p2museum),
  data = data
)
descr

# ---- 3. Naive estimates (no propensity score, no survey weights) -----------

# Unconditional naive estimate, with HC2 robust standard error
m00 <- lm(x2sciid.sd ~ p2museum, data = data)
summary(m00)
sqrt(hccm(m00)[2, 2])   # HC2 robust SE for the treatment coefficient

# Covariate-adjusted naive estimate
m01 <- lm(
  x2sciid.sd ~
    x1sciid.sd + p2museum +
    x1locale + x1control + a1mspdintrst + a1msmentor + a1msspeaker +
    a1msfldtrip + a1msprgms +
    x1race + x1sex + x1sesq5 +
    x1txmscr.sd + x1sciint.sd + x1stuedexpct +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 + s1advbios09 +
    s1mclub + s1sclub +
    s1sbooks.sd + s1webinfo.sd,
  data = data
)
sqrt(hccm(m01)[3, 3])   # HC2 robust SE for the treatment coefficient

table.m01 <- tidy.coeftable(m01)
knitr::kable(table.m01, digits = 3,
             caption = "Unweighted linear regression predicting 11th-grade science identity")
