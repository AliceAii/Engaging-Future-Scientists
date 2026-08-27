###############################################################################
# 03_propensity_score.R
# Engaging Future Scientists: Science Museum Visits and Science Identity
# Author: Shuhan (Alice) Ai
#
# Purpose: Estimate propensity scores via logistic regression, examine common
#          support, and trim the data to the region of overlap.
# Prerequisite: Run 01_setup.R first.
###############################################################################

# ---- 1. Propensity score model ----------------------------------------------

pscore.m.sciid <- glm(
  p2museum ~
    x1sciid.sd +
    x1locale + x1control + a1mspdintrst + a1msmentor + a1msspeaker +
    a1msfldtrip + a1msprgms +
    x1race + x1sex + x1sesq5 +
    x1txmscr.sd + x1sciint.sd + x1stuedexpct +
    s1alg1m09 + s1alg2m09 + s1tgeom09 + s1sfall09 + s1advbios09 +
    s1mclub + s1sclub +
    s1sbooks.sd + s1webinfo.sd,
  data = data, family = "binomial"
)

# Table 2: Propensity score model results
table.pscore.m.sciid <- tidy.logit.table(pscore.m.sciid)
knitr::kable(table.pscore.m.sciid, digits = 3,
             caption = "Logistic regression predicting science museum/planetarium visits (Table 2)")

# Store predicted propensity scores on the probability and logit scales
data <- data %>%
  mutate(
    pscore       = predict(pscore.m.sciid, type = "response"),
    pscorelogodd = predict(pscore.m.sciid)
  )

# ---- 2. VIF check -----------------------------------------------------------

vif_values <- vif(pscore.m.sciid)
print(vif_values)

# ---- 3. Common support visualisation ----------------------------------

p1 <- ggplot(data, aes(x = pscore, fill = p2museum)) +
  geom_histogram(binwidth = 0.01, position = "identity",
                 alpha = 0.6, color = "black") +
  scale_fill_manual(values = c("#ee84a8", "#71bced"),
                    labels = c("Control", "Treatment")) +
  labs(title = "Propensity Score (Linear Scale)",
       x = "Propensity Score", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 12))

p2 <- ggplot(data, aes(x = pscore, y = p2museum, color = p2museum)) +
  geom_jitter(height = 0.2, alpha = 0.4, size = 1) +
  scale_color_manual(values = c("#ee84a8", "#71bced"),
                     labels = c("Control", "Treatment")) +
  labs(title = "Common Support Check", x = "Propensity Score", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12))

p3 <- ggplot(data, aes(x = pscorelogodd, y = p2museum, color = p2museum)) +
  geom_jitter(height = 0.2, alpha = 0.4, size = 1) +
  scale_color_manual(values = c("#ee84a8", "#71bced"),
                     labels = c("Control", "Treatment")) +
  scale_x_continuous(
    breaks = seq(floor(min(data$pscorelogodd, na.rm = TRUE)),
                 ceiling(max(data$pscorelogodd, na.rm = TRUE)), by = 0.5)
  ) +
  labs(title = "Common Support Check", x = "Logit Score", y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 12))

fig1 <- p1 / p2 / p3 + plot_layout(ncol = 1, heights = c(4, 3, 3))
fig1

ggsave("output/Fig1_commonsupport.png", plot = fig1,
       width = 7, height = 8, dpi = 1200, bg = "white")

# ---- 4. Trim data to the common support region ------------------------------
# Trimming cutoffs on the logit scale: [-1.5, 0.75]

data.trimmed <- data %>%
  filter(pscorelogodd >= -1.5 & pscorelogodd <= 0.75)

cat("Cases trimmed:", nrow(data) - nrow(data.trimmed), "\n")
table(data.trimmed$p2museum)
