# ============================================================
# 0) Packages & data import
# ============================================================

# Load required libraries:
# - data handling (haven)
# - multilevel modelling (lme4, lmerTest)
# - coefficient visualisation (ggstatsplot, ggplot2)
# - figure composition (patchwork)
# - regression tables and model summaries (modelsummary)
# - survey-weighted analyses (survey)
# - table formatting (flextable)

library(haven)
library(lme4)
library(lmerTest)
library(ggstatsplot)
library(ggplot2)
library(patchwork)
library(modelsummary)
library(survey)
library(flextable)

# Load ESS dataset (replace with your local path)
load("INSERT YOUR PATHWAY/Data_ESS.RData")

# Create copy used later for appendix analyses
data_ESS_app <- data_ESS

# ============================================================
# 1) Recode / factor labels
# ============================================================

# Goal:
# Convert categorical variables to factors and assign interpretable labels for modelling & plotting

# -----------------------------
# War exposure timing
# -----------------------------
# 'period' indicates when respondents were interviewed relative to the Russian invasion of Ukraine.
data_ESS$period <- as_factor(data_ESS$period)

levels(data_ESS$period) <- c(
  "Before the invasion",
  "Up to 1 month",
  "From 1 to 3 months",
  "From 3 to 6 months",
  "From 6 months to 1.5 years",
  "1.5 years or more"
)

# -----------------------------
# Gender
# -----------------------------
# Drop unused categories after conversion
data_ESS$gndr <- as_factor(data_ESS$gndr)
data_ESS$gndr <- droplevels(data_ESS$gndr)

# -----------------------------
# Urban vs rural residence
# -----------------------------
data_ESS$urban <- as_factor(data_ESS$urban)

# -----------------------------
# Born in country of residence
# -----------------------------
# Drop unused categories
data_ESS$brncntr <- as_factor(data_ESS$brncntr)
data_ESS$brncntr <- droplevels(data_ESS$brncntr)

# -----------------------------
# Main household income source
# -----------------------------
# Drop unused categories
data_ESS$hincsrca <- as_factor(data_ESS$hincsrca)
data_ESS$hincsrca <- droplevels(data_ESS$hincsrca)

# -------------------------
# Country identifier
# -------------------------
# Used as grouping variable for random intercepts
data_ESS$cntry <- as_factor(data_ESS$cntry)
data_ESS$cntry <- droplevels(data_ESS$cntry)

# ============================================================
# 2) Reverse coding & scaling
# ============================================================

# Goal:
# Ensure higher values consistently represent MORE pro-EU / pro-immigration / egalitarian attitudes

# Financial comfort (higher = more comfortable)
data_ESS$hincfel <- 4 - data_ESS$hincfel

# Support for redistribution (higher = more support)
data_ESS$gincdif <- 5 - data_ESS$gincdif

# Loyalty to leaders (higher = stronger loyalty)
data_ESS$loylead <- 5 - data_ESS$loylead

# Immigration acceptance indicators (higher = more acceptance)
data_ESS$imdfetn <- 4 - data_ESS$imdfetn
data_ESS$impcntr <- 4 - data_ESS$impcntr

# -------------------------
# Age centering
# -------------------------
# Mean age = 0
data_ESS$agea <- scale(data_ESS$agea,
                       center = T,
                       scale  = F)

# !!! The values of all the numeric variables (excluding age) start at 0

# ============================================================
# 3) Multilevel models (LMM)
#    Fixed effects: period + controls
#    Random intercept: country
# ============================================================

# How emotionally attached to Europe
m_atcherp <- lmer(
  atcherp ~ period + gndr + agea + urban +
    brncntr + eduyrs + hincsrca + hincfel +
    (1 | cntry),
  data = data_ESS
)

# European Union: European unification go further or gone too far
m_euftf <- lmer(
  euftf ~ period + gndr + agea + urban +
    brncntr + eduyrs + hincsrca + hincfel +
    (1 | cntry),
  data = data_ESS
)

# Country needs most loyalty towards its leaders
m_loylead <- lmer(
  loylead ~ period + gndr + agea + urban +
    brncntr + eduyrs + hincsrca + hincfel +
    (1 | cntry),
  data = data_ESS
)

# Government should reduce differences in income levels
m_gincdif <- lmer(
  gincdif ~ period + gndr + agea + urban +
    brncntr + eduyrs + hincsrca + hincfel +
    (1 | cntry),
  data = data_ESS
)

# Allow many/few immigrants of different race/ethnic group from majority
m_imdfetn <- lmer(
  imdfetn ~ period + gndr + agea + urban +
    brncntr + eduyrs + hincsrca + hincfel +
    (1 | cntry),
  data = data_ESS
)

# Allow many/few immigrants from poorer countries outside Europe
m_impcntr <- lmer(
  impcntr ~ period + gndr + agea + urban +
    brncntr + eduyrs + hincsrca + hincfel +
    (1 | cntry),
  data = data_ESS
)

# Immigration bad or good for country's economy
m_imbgeco <- lmer(
  imbgeco ~ period + gndr + agea + urban +
    brncntr + eduyrs + hincsrca + hincfel +
    (1 | cntry),
  data = data_ESS
)

# Country's cultural life undermined or enriched by immigrants
m_imueclt <- lmer(
  imueclt ~ period + gndr + agea + urban +
    brncntr + eduyrs + hincsrca + hincfel +
    (1 | cntry),
  data = data_ESS
)

# ============================================================
# 4) Figure 1: political attitudes
# ============================================================

# Coefficient plots visualising ONLY period effects.
#
# Colour coding:
#   red  = statistically significant (p ≤ .05)
#   black = non-significant
#
# Rows reordered so temporal progression is intuitive.

p1 <- ggcoefstats(cbind(ggcoefstats(m_atcherp)[["data"]][6:2,], ifelse(ggcoefstats(m_atcherp)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2")),
                  point.args = list(size = 3.2, color = cbind(ggcoefstats(m_atcherp)[["data"]][6:2,], ifelse(ggcoefstats(m_atcherp)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2"))[[14]], na.rm = TRUE)) +
  scale_y_discrete(labels = function(l)
    gsub("period", "", l)) +
  scale_x_continuous(limits = c(-0.2, 0.8), breaks = seq(-0.2, 0.8, 0.2)) +
  xlab("Emotional attachment to Europe") + ylab("") +
  theme(axis.text.y = element_text(size = 12.5, color = "black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p2 <- ggcoefstats(cbind(ggcoefstats(m_euftf)[["data"]][6:2,], ifelse(ggcoefstats(m_euftf)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2")),
                  point.args = list(size = 3.2, color = cbind(ggcoefstats(m_euftf)[["data"]][6:2,], ifelse(ggcoefstats(m_euftf)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2"))[[14]], na.rm = TRUE)) +
  scale_y_discrete(labels = function(l)
    gsub("period", "", l)) +
  scale_x_continuous(limits = c(-0.2, 0.8), breaks = seq(-0.2, 0.8, 0.2)) +
  xlab("European unification should go further") + ylab("") +
  theme(axis.text.y = element_text(size = 12.5, color = "black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p3 <- ggcoefstats(cbind(ggcoefstats(m_loylead)[["data"]][6:2,], ifelse(ggcoefstats(m_loylead)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2")),
                  point.args = list(size = 3.2, color = cbind(ggcoefstats(m_loylead)[["data"]][6:2,], ifelse(ggcoefstats(m_loylead)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2"))[[14]], na.rm = TRUE)) +
  scale_y_discrete(labels = function(l)
    gsub("period", "", l)) +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.2)) +
  xlab("People should be loyal to their leaders") + ylab("") +
  theme(axis.text.y = element_text(size = 12.5, color = "black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p4 <- ggcoefstats(cbind(ggcoefstats(m_gincdif)[["data"]][6:2,], ifelse(ggcoefstats(m_gincdif)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2")),
                  point.args = list(size = 3.2, color = cbind(ggcoefstats(m_gincdif)[["data"]][6:2,], ifelse(ggcoefstats(m_gincdif)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2"))[[14]], na.rm = TRUE)) +
  scale_y_discrete(labels = function(l)
    gsub("period", "", l)) +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.2)) +
  xlab("The government should take measures \n to reduce differences in income levels") + ylab("") +
  theme(axis.text.y = element_text(size = 12.5, color = "black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

# Figure 1 (2x2 layout)
(p1 + p3)/(p2 + p4)

# ============================================================
# 5) Figure 2: immigration attitudes
# ============================================================

# Coefficient plots visualising ONLY period effects.
#
# Colour coding:
#   red  = statistically significant (p ≤ .05)
#   black = non-significant
#
# Rows reordered so temporal progression is intuitive.

p5 <- ggcoefstats(cbind(ggcoefstats(m_imdfetn)[["data"]][6:2,], ifelse(ggcoefstats(m_imdfetn)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2")),
                  point.args = list(size = 3.2, color = cbind(ggcoefstats(m_imdfetn)[["data"]][6:2,], ifelse(ggcoefstats(m_imdfetn)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2"))[[14]], na.rm = TRUE)) +
  scale_y_discrete(labels = function(l)
    gsub("period", "", l)) +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.2)) +
  xlab("Allow people of a different race to \n come and live in this country") + ylab("") +
  theme(axis.text.y = element_text(size = 12.5, color = "black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p6 <- ggcoefstats(cbind(ggcoefstats(m_impcntr)[["data"]][6:2,], ifelse(ggcoefstats(m_impcntr)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2")),
                  point.args = list(size = 3.2, color = cbind(ggcoefstats(m_impcntr)[["data"]][6:2,], ifelse(ggcoefstats(m_impcntr)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2"))[[14]], na.rm = TRUE)) +
  scale_y_discrete(labels = function(l)
    gsub("period", "", l)) +
  scale_x_continuous(limits = c(-0.2, 0.2), breaks = seq(-0.2, 0.2, 0.2)) +
  xlab("Allow people from poorer countries outside \n Europe to come and live in this country") + ylab("") +
  theme(axis.text.y = element_text(size = 12.5, color = "black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p7 <- ggcoefstats(cbind(ggcoefstats(m_imbgeco)[["data"]][6:2,], ifelse(ggcoefstats(m_imbgeco)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2")),
                  point.args = list(size = 3.2, color = cbind(ggcoefstats(m_imbgeco)[["data"]][6:2,], ifelse(ggcoefstats(m_imbgeco)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2"))[[14]], na.rm = TRUE)) +
  scale_y_discrete(labels = function(l)
    gsub("period", "", l)) +
  scale_x_continuous(limits = c(-0.6, 0.4), breaks = seq(-0.6, 0.4, 0.2)) +
  xlab("Immigration is good for the country’s \n economy") + ylab("") +
  theme(axis.text.y = element_text(size = 12.5, color = "black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

p8 <- ggcoefstats(cbind(ggcoefstats(m_imueclt)[["data"]][6:2,], ifelse(ggcoefstats(m_imueclt)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2")),
                  point.args = list(size = 3.2, color = cbind(ggcoefstats(m_imueclt)[["data"]][6:2,], ifelse(ggcoefstats(m_imueclt)[["data"]][6:2,]$p.value > 0.05, "grey20", "firebrick2"))[[14]], na.rm = TRUE)) +
  scale_y_discrete(labels = function(l)
    gsub("period", "", l)) +
  scale_x_continuous(limits = c(-0.6, 0.4), breaks = seq(-0.6, 0.4, 0.2)) +
  xlab("Immigration is good for the country’s \n cultural life") + ylab("") +
  theme(axis.text.y = element_text(size = 12.5, color = "black"),
        axis.title.x = element_text(size = 13, face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

# Figure 2 (2x2 layout)
(p5 + p7)/(p6 + p8)

# ============================================================
# Appendiсes 1-2
# Country-specific weighted Wilcoxon tests
# Means for respective variables and countries (waves 10-11) see here:
# https://zenodo.org/records/18595343
# ============================================================

# Countries included in appendix analyses
countries <- c(
  "BE", # Belgium
  "CH", # Switzerland
  "GB", # United Kingdom
  "GR", # Greece
  "IE", # Ireland
  "IT", # Italy
  "ME", # Montenegro
  "NL", # Netherlands
  "NO", # Norway
  "PT"  # Portugal
)

# Variables tested across ESS rounds
test_vars <- names(data_ESS_app[, c(21, 14, 20, 15:19)])

# Loop over countries
for (ctry in countries) {
  
  cat("\n====================================\n")
  cat("Country:", ctry, "\n")
  cat("====================================\n")
  
  # Create survey design for country
  weight_design <- svydesign(
    ids = ~1,
    data = data_ESS_app[data_ESS_app$cntry == ctry, ],
    weights = ~pspwght
  )
  
  # Run weighted Wilcoxon tests
  pvals <- sapply(test_vars, function(x)
    svyranktest(
      as.formula(paste(x, "~ essround")),
      weight_design,
      test = "wilcoxon"
    )$p.value
  )
  
  # Print p-values with Holm correction
  print(p.adjust(pvals, method = "holm"))
}

# ============================================================
# 7) Appendix 3
#    Regression table (modelsummary)
# ============================================================

modelsummary(list("(1) Emotional attachment to Europe" = m_atcherp, 
                  "(2) European unification should go further or has already gone too far" = m_euftf,
                  "(3) People should be loyal to their leaders" = m_loylead, 
                  "(4) The government should take measures to reduce differences in income levels" = m_gincdif, 
                  "(5) Allow people of a different race or ethnic group to come and live in this country" = m_imdfetn, 
                  "(6) Allow people from poorer countries outside Europe to come and live in this country" = m_impcntr, 
                  "(7) Immigration is good or bad for the country’s economy" = m_imbgeco, 
                  "(8) Immigration is good or bad for the country’s cultural life" = m_imueclt),
             stars = T, coef_rename = c("(Intercept)", 
                                        "Period: Up to 1 month",
                                        "Period: From 1 to 3 months",
                                        "Period: From 3 to 6 months",
                                        "Period: From 6 months to 1.5 years",
                                        "Period: 1.5 years or more",
                                        "Gender: Female", 
                                        "Age", 
                                        "Urban residence: No",
                                        "Born in the country of residence: Yes", 
                                        "Education (years of education)",
                                        "Income source: Self-employment", 
                                        "Income source: Farming or household production",
                                        "Income source: Pension", 
                                        "Income source: Unemployment or redundancy benefits",
                                        "Income source: Other social benefits, subsidies, or grants", 
                                        "Income source: Investments, savings, insurance, or property",
                                        "Income source: Other sources", 
                                        "Financial comfort",
                                        "SD (Intercept, country level)", 
                                        "SD (Observations)"),
             notes = c("Reference category for Period: Before the invasion; for Income source: Wages"))

# ============================================================
# 8) Additional analysis
#    Standardised coefficients for period
#    (to assess effect size magnitude)
#
#    Analysis conducted by Valentyn Hatsko,
#    Senior Analyst, Center for Sociological Research,
#    KSE University
# ============================================================

# For a factor predictor, semi-standardization divides the raw coef by SD (outcome)
# This gives the effect of each period level (vs. reference "Before the invasion") expressed in SD units of the outcome

# Function computing semi-standardized period effects
get_std_period <- function(model, outcome_var, label) {
  coefs <- fixef(model)
  period_idx <- grep("^period", names(coefs))
  raw <- coefs[period_idx]
  sd_y <- sd(model@frame[[outcome_var]], na.rm = TRUE)
  std  <- raw / sd_y
  data.frame(
    Outcome  = label,
    Period   = sub("^period", "", names(std)),
    Raw_value = round(raw, 3),
    Std_value = round(std, 3)
  )
}

# Combined table of raw and standardized period effects across all outcomes
std_period_coefs <- rbind(
  get_std_period(m_atcherp, "atcherp", "Emotional attachment to Europe"),
  get_std_period(m_euftf,   "euftf",   "European unification should go further or has already gone too far"),
  get_std_period(m_loylead, "loylead", "People should be loyal to their leaders"),
  get_std_period(m_gincdif, "gincdif", "The government should take measures to reduce differences in income levels"),
  get_std_period(m_imdfetn, "imdfetn", "Allow people of a different race or ethnic group to come and live in this country"),
  get_std_period(m_impcntr, "impcntr", "Allow people from poorer countries outside Europe to come and live in this country"),
  get_std_period(m_imbgeco, "imbgeco", "Immigration is good or bad for the country’s economy"),
  get_std_period(m_imueclt, "imueclt", "Immigration is good or bad for the country’s cultural life")
)

# Print the table
std_period_coefs |>
  as.data.frame() |> 
  flextable() |>
  align(align = "c", part = "header") |>
  align(align = "c", j = 3:4, part = "body") |>
  bold(part = "header") |>
  width(width = c(3, 2.5, 1.5, 1.5))
  

# All effects are small in magnitude, including statistically significant ones
# The most noticeable changes are observed for attitudes toward European unification, which increase during the 1-3 month and 3-6 month periods compared to the pre-invasion baseline