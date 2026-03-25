# ── Load libraries ────────────────────────────────────────────────────────
library(haven)
library(dplyr)
library(lme4)
library(lmerTest)
library(sjPlot)
library(ggplot2)
library(patchwork)

# ── 1. Data import via ESS API ────────────────────────────────────────────
# userId is available in your profile at https://ess.sikt.no (registration required)

user_id <- "Your user ID"

ess_api_url <- function(doi) {
  paste0("https://api.ess.sikt.no/v1/data/dataFile/", doi,
         "?userId=", user_id, "&fileFormat=sav")
}

ess_missing_recode <- function(df) {
  mutate(df,
    stflife  = ifelse(as.numeric(stflife) %in% c(77, 88, 99),       NA_real_, as.numeric(stflife)),
    happy    = ifelse(as.numeric(happy)   %in% c(77, 88, 99),       NA_real_, as.numeric(happy)),
    agea     = ifelse(as.numeric(agea)    %in% c(777, 888, 999),    NA_real_, as.numeric(agea)),
    gndr     = ifelse(as.numeric(gndr)    %in% c(7, 8, 9),          NA_real_, as.numeric(gndr)),
    yrbrn    = ifelse(as.numeric(yrbrn)   %in% c(7777, 8888, 9999), NA_real_, as.numeric(yrbrn)),
    essround = as.numeric(essround)
  )
}

vars <- c("essround", "cntry", "yrbrn", "agea", "gndr", "stflife", "happy")

# DOIs for ESS integrated files (all countries) — current editions:
rounds_doi <- c(
  "10.21338/ess2e03_6",   # round 2, 2004
  "10.21338/ess3e03_7",   # round 3, 2006
  "10.21338/ess4e04_5",   # round 4, 2008
  "10.21338/ess5e03_4",   # round 5, 2010
  "10.21338/ess6e02_7"    # round 6, 2012
)

# Download rounds 2–6: keep only Ukraine (cntry == "UA")
df_26 <- bind_rows(lapply(rounds_doi, function(doi) {
  message("Downloading: ", doi)
  df <- read_sav(ess_api_url(doi), user_na = TRUE)
  df <- df[df$cntry == "UA", intersect(vars, names(df)), drop = FALSE]
  ess_missing_recode(df)
}))

# Round 10 Ukraine: open GitHub repository (no userId required)
df_10 <- read_sav(
  "https://raw.githubusercontent.com/KSE-Sociological-Center/ESS10_Ukraine/main/ESS10UAe4.sav",
  user_na = TRUE
) %>%
  select(any_of(vars)) %>%
  ess_missing_recode()

# Combine all rounds
data_raw <- bind_rows(df_26, df_10)

# ── 2. Variable preparation ───────────────────────────────────────────────

data_ESS <- data_raw %>%
  mutate(
    age    = as.numeric(agea),
    sat    = as.numeric(stflife),
    happy  = as.numeric(happy),
    yrbrn  = as.numeric(yrbrn),
    # Sex: Female is the reference category
    gender = factor(
      case_when(gndr == 1 ~ "Male", gndr == 2 ~ "Female"),
      levels = c("Female", "Male")
    ),
    # Survey round as factor (random effect in CCREM)
    period = factor(essround)
  ) %>%
  select(-agea, -stflife, -gndr, -essround, -cntry)

# ── 3. Cohort variable ────────────────────────────────────────────────────
# Five-year birth cohorts aligned to 2007 (youngest possible 15+ respondent
# in the 2022 round). Intervals (right=TRUE): (-Inf, 1907], (1907, 1912], ..., (2002, 2007]

data_ESS <- data_ESS %>%
  mutate(
    cohort5_15 = cut(
      yrbrn,
      breaks        = c(-Inf, seq(1907, 2007, by = 5)),
      right         = TRUE,
      ordered_result = FALSE
    )
  )

# ── 4. Descriptive statistics (Table 2) ──────────────────────────────────

cat("=== Mean age by round ===\n")
tapply(data_ESS$age, data_ESS$period, mean, na.rm = TRUE) |> round(2) |> print()

cat("=== Sex distribution (%) ===\n")
table(data_ESS$gender, data_ESS$period) |> prop.table(2) |> round(3) * 100

cat("=== Mean happiness by round ===\n")
tapply(data_ESS$happy, data_ESS$period, mean, na.rm = TRUE) |> round(2) |> print()

cat("=== Mean life satisfaction by round ===\n")
tapply(data_ESS$sat, data_ESS$period, mean, na.rm = TRUE) |> round(2) |> print()

cat("=== Missing values by round ===\n")
na_count <- function(x) sum(is.na(x))
na_pct   <- function(x) round(mean(is.na(x)) * 100, 1)

for (p in levels(data_ESS$period)) {
  cat("\nRound", p, ":\n")
  sub <- data_ESS[data_ESS$period == p, ]
  cat(sprintf("  age:    %d NA (%.1f%%)\n", na_count(sub$age),    na_pct(sub$age)))
  cat(sprintf("  sat:    %d NA (%.1f%%)\n", na_count(sub$sat),    na_pct(sub$sat)))
  cat(sprintf("  happy:  %d NA (%.1f%%)\n", na_count(sub$happy),  na_pct(sub$happy)))
  cat(sprintf("  gender: %d NA (%.1f%%)\n", na_count(sub$gender), na_pct(sub$gender)))
}

# ── 5. Means by sex and Mann-Whitney test (Table 3) ──────────────────────

for (p in levels(data_ESS$period)) {
  cat("\n=== Round", p, "===\n")
  idx <- !is.na(data_ESS$period) & data_ESS$period == p & !is.na(data_ESS$gender)

  cat("Happiness by sex:\n")
  tapply(data_ESS$happy[idx], data_ESS$gender[idx], mean, na.rm = TRUE) |> round(2) |> print()

  cat("Life satisfaction by sex:\n")
  tapply(data_ESS$sat[idx], data_ESS$gender[idx], mean, na.rm = TRUE) |> round(2) |> print()

  cat("Mann-Whitney test (happiness):\n")
  tryCatch(
    wilcox.test(data_ESS$happy[idx] ~ data_ESS$gender[idx]) |> print(),
    error = function(e) cat(e$message, "\n")
  )

  cat("Mann-Whitney test (life satisfaction):\n")
  tryCatch(
    wilcox.test(data_ESS$sat[idx] ~ data_ESS$gender[idx]) |> print(),
    error = function(e) cat(e$message, "\n")
  )
}

# ── 6. Sample for regressions ─────────────────────────────────────────────────

data_ESS1 <- data_ESS %>%
  filter(!is.na(age)) %>%
  mutate(
    age_c = as.numeric(scale(age, center = TRUE, scale = FALSE))
  )

# ── 7. CCREM models (Table 4) ─────────────────────────────────────────────

# For the table — centred age
fit1_ESS_15  <- lmer(sat   ~ poly(age_c, 2) + gender + (1|period) + (1|cohort5_15),
                     data = data_ESS1, REML = TRUE)
fit2_ESS_15  <- lmer(happy ~ poly(age_c, 2) + gender + (1|period) + (1|cohort5_15),
                     data = data_ESS1, REML = TRUE)

# For the figure — uncentred age (x-axis shows actual age)
fit1_ESS_151 <- lmer(sat   ~ poly(age, 2) + gender + (1|period) + (1|cohort5_15),
                     data = data_ESS1, REML = TRUE)
fit2_ESS_151 <- lmer(happy ~ poly(age, 2) + gender + (1|period) + (1|cohort5_15),
                     data = data_ESS1, REML = TRUE)

# ── 8. Table 4 ────────────────────────────────────────────────────────────

labels <- c("(Intercept)", "Age", "Age²", "Gender: Male")

tab_model(
  fit1_ESS_15, fit2_ESS_15,
  show.ci     = FALSE,
  show.se     = TRUE,
  show.aic    = TRUE,
  p.style     = "stars",
  collapse.se = TRUE,
  dv.labels   = c("Life satisfaction", "Happiness"),
  string.pred = "Coefficients",
  string.est  = " ",
  pred.labels = labels
)

# ── 9. Figure — predicted values by age ──────────────────────────────────

p1_1 <- plot_model(fit1_ESS_151, type = "pred", terms = "age[all]") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(title = "") + xlab("Age") + ylab("Life satisfaction")

p1_2 <- plot_model(fit2_ESS_151, type = "pred", terms = "age[all]") +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  labs(title = "") + xlab("Age") + ylab("Happiness")

p1_1 + p1_2
