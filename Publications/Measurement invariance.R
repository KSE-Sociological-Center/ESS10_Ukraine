# ── Load libraries ────────────────────────────────────────────────────────
library(haven)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lavaan)
library(psych)
library(semTools)
library(DBM.functions)

# ── 0. Data import ────────────────────────────────────────────────────────
# userId is available in your profile at https://ess.sikt.no (registration required)
user_id <- "62a78a85-3cae-494c-8807-346f00b3734c"

ess_api_url <- function(doi) {
  paste0("https://api.ess.sikt.no/v1/data/dataFile/", doi,
         "?userId=", user_id, "&fileFormat=sav")
}

# rounds 2–4 store Ukraine's region as `regionua`; rounds 5–6 and 10 use `region`
vars_ri <- c("essround", "cntry", "region", "regionua", "rlgdgr", "rlgatnd", "pray")

# DOIs for ESS integrated files (all countries) — current editions
rounds_doi <- c(
  "10.21338/ess2e03_6",   # round 2, 2005
  "10.21338/ess3e03_7",   # round 3, 2006-2007
  "10.21338/ess4e04_5",   # round 4, 2009
  "10.21338/ess5e03_4",   # round 5, 2011
  "10.21338/ess6e02_7"    # round 6, 2013
)

# Download rounds 2–6: keep only Ukraine (cntry == "UA")
# Normalise the region variable: rounds 2–4 call it `regionua`, rounds 5–6 `region`.
# Both are converted to character and unified into a single `region` column.
df_26 <- bind_rows(lapply(rounds_doi, function(doi) {
  message("Downloading: ", doi)
  df <- read_sav(ess_api_url(doi), user_na = TRUE)
  df_ua <- df[df$cntry == "UA", intersect(vars_ri, names(df)), drop = FALSE]
  # Unify into `region`
  if ("regionua" %in% names(df_ua)) {
    df_ua$region <- as.character(as_factor(df_ua$regionua))
    df_ua$regionua <- NULL
  } else if ("region" %in% names(df_ua)) {
    df_ua$region <- as.character(as_factor(df_ua$region))
  } else {
    df_ua$region <- NA_character_
  }
  df_ua
}))

# Round 10 Ukraine: open GitHub repository (no userId required)
df_10 <- read_sav(
  "https://raw.githubusercontent.com/KSE-Sociological-Center/ESS10_Ukraine/main/ESS10UAe4.sav",
  user_na = TRUE
) |> select(any_of(vars_ri))

df_10$region <- as.character(as_factor(df_10$region))

# Unify `region` value labels: in df_26 they should be the same as in df_10

# English names (waves 5–6): only entries that differ from the df_10 form
df_26$region[df_26$region == "Cherkasska oblast"]          <- "Cherkaska oblast"
df_26$region[df_26$region == "Chernigivska oblast"]        <- "Chernihivska oblast"
df_26$region[df_26$region == "Chernovytska oblast"]        <- "Chernivetska oblast"
df_26$region[df_26$region == "Crimea, Autonomy Republic"]  <- "AR Crimea"
df_26$region[df_26$region == "Khmelnitska oblast"]         <- "Khmelnytska oblast"
df_26$region[df_26$region == "Kirovogradska oblast"]       <- "Kirovohradska oblast"
df_26$region[df_26$region == "Luganska oblast"]            <- "Luhanska oblast"
df_26$region[df_26$region == "Odesska oblast"]             <- "Odeska oblast"
df_26$region[df_26$region == "Rivenska oblast"]            <- "Rivnenska oblast"
df_26$region[df_26$region == "Vynnytska oblast"]           <- "Vinnytska oblast"
df_26$region[df_26$region == "Zaporizska oblast"]          <- "Zaporizka oblast"
# Ukrainian Cyrillic names (waves 2–4)
df_26$region[df_26$region == "Автономна Республіка Крим"] <- "AR Crimea"
df_26$region[df_26$region == "Вінницька Область"]         <- "Vinnytska oblast"
df_26$region[df_26$region == "Волинська Область"]         <- "Volynska oblast"
df_26$region[df_26$region == "Дніпропетровська Область"]  <- "Dnipropetrovska oblast"
df_26$region[df_26$region == "Донецька Область"]          <- "Donetska oblast"
df_26$region[df_26$region == "Житомирська Область"]       <- "Zhytomyrska oblast"
df_26$region[df_26$region == "Закарпатська Область"]      <- "Zakarpatska oblast"
df_26$region[df_26$region == "Запорізька Область"]        <- "Zaporizka oblast"
df_26$region[df_26$region == "Івано-Франківська Область"] <- "Ivano-Frankivska oblast"
df_26$region[df_26$region == "Київська  Область"]         <- "Kyivska oblast"
df_26$region[df_26$region == "Кіровоградська Область"]    <- "Kirovohradska oblast"
df_26$region[df_26$region == "Луганська Область"]         <- "Luhanska oblast"
df_26$region[df_26$region == "Львівська Область"]         <- "Lvivska oblast"
df_26$region[df_26$region == "М. Київ"]                   <- "Kyiv city"
df_26$region[df_26$region == "Миколаївська Область"]      <- "Mykolaivska oblast"
df_26$region[df_26$region == "Одеська Область"]           <- "Odeska oblast"
df_26$region[df_26$region == "Полтавська Область"]        <- "Poltavska oblast"
df_26$region[df_26$region == "Рівненська Область"]        <- "Rivnenska oblast"
df_26$region[df_26$region == "Сумська Область"]           <- "Sumska oblast"
df_26$region[df_26$region == "Тернопільська Область"]     <- "Ternopilska oblast"
df_26$region[df_26$region == "Харківська Область"]        <- "Kharkivska oblast"
df_26$region[df_26$region == "Херсонська Область"]        <- "Khersonska oblast"
df_26$region[df_26$region == "Хмельницька Область"]       <- "Khmelnytska oblast"
df_26$region[df_26$region == "Черкаська  Область"]        <- "Cherkaska oblast"
df_26$region[df_26$region == "Чернівецька Область"]       <- "Chernivetska oblast"
df_26$region[df_26$region == "Чернігівська Область"]      <- "Chernihivska oblast"

# Combine all rounds and recode
data_ri <- bind_rows(df_26, df_10) |>
  mutate(
    # Missing value codes: 77 = refusal, 88 = don't know, 99 = no answer
    rlgdgr  = ifelse(as.numeric(rlgdgr)  %in% c(77, 88, 99), NA_real_, as.numeric(rlgdgr)),
    # rlgatnd and pray are originally 1 (every day) to 7 (never); reversed so 1 = never
    rlgatnd = ifelse(as.numeric(rlgatnd) %in% c(77, 88, 99), NA_real_, 8 - as.numeric(rlgatnd)),
    pray    = ifelse(as.numeric(pray)    %in% c(77, 88, 99), NA_real_, 8 - as.numeric(pray)),
    wave    = as.character(as.numeric(essround))
  )

# ess_ua_all_1: all waves; columns 1:3 = rlgdgr, rlgatnd, pray
ess_ua_all_1 <- data_ri[, c("rlgdgr", "rlgatnd", "pray", "wave")]

# ess_ua_all_r: all waves, excluding Crimea, Donetsk, and Luhansk oblasts
excluded <- c("AR Crimea", "Donetska oblast", "Luhanska oblast")
ess_ua_all_r <- data_ri[
  is.na(data_ri$region) | !data_ri$region %in% excluded,
  c("rlgdgr", "rlgatnd", "pray", "wave")
]

# ── 1. Multidimensional scaling of correlation patterns (Figure 1) ────────

# Compute Spearman correlations for each ESS wave
cor2  <- cor(ess_ua_all_1[ess_ua_all_1$wave == "2",  1:3], method = "spearman", use = "pairwise.complete.obs")
cor3  <- cor(ess_ua_all_1[ess_ua_all_1$wave == "3",  1:3], method = "spearman", use = "pairwise.complete.obs")
cor4  <- cor(ess_ua_all_1[ess_ua_all_1$wave == "4",  1:3], method = "spearman", use = "pairwise.complete.obs")
cor5  <- cor(ess_ua_all_1[ess_ua_all_1$wave == "5",  1:3], method = "spearman", use = "pairwise.complete.obs")
cor6  <- cor(ess_ua_all_1[ess_ua_all_1$wave == "6",  1:3], method = "spearman", use = "pairwise.complete.obs")
cor10 <- cor(ess_ua_all_1[ess_ua_all_1$wave == "10", 1:3], method = "spearman", use = "pairwise.complete.obs")

# Combine lower-triangle values into a summary table (Appendix 1)
cors <- rbind(cor2[lower.tri(cor2)],
              cor3[lower.tri(cor3)],
              cor4[lower.tri(cor4)],
              cor5[lower.tri(cor5)],
              cor6[lower.tri(cor6)],
              cor10[lower.tri(cor10)])
rownames(cors) <- c("2005", "2006-2007", "2009", "2011", "2013", "2022")
colnames(cors) <- c("rlgdgr|rlgatnd", "rlgdgr|pray", "rlgatnd|pray")

ms <- cmdscale(dist(cors)) |> as.data.frame()

ggplot(ms, aes(x = V1, y = V2)) +
  geom_point(size = 3) +
  xlim(c(-0.1, 0.1)) +
  ylim(c(-0.1, 0.1)) +
  xlab("") +
  ylab("") +
  annotate("text", x = -0.06, y = -0.09, label = "Lower correlation", size = 3.5, fontface = "bold") +
  annotate("text", x =  0.06, y = -0.09, label = "Higher correlation", size = 3.5, fontface = "bold") +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_label_repel(aes(label = rownames(cors)),
                   nudge_x = c(0, 0.007, 0, 0.006, 0.006, 0),
                   nudge_y = c(0, 0.007, -0.005, 0.006, 0.006, 0)) +
  theme(axis.title = element_text(face = "bold"))

# ── 2. MGCFA: measurement invariance testing ──────────────────────────────

# CFA model: Religious Involvement latent factor
# rlgdgr (self-assessed religiosity) is fixed at 1 as the referent item
model <- "Involv =~ rlgdgr + rlgatnd + pray"

## 2a. All time points (waves 2, 3, 4, 5, 6, 10) — Table 2 ─────────────────

# Full metric invariance (M1) — rejected
fit_m <- cfa(model, data = ess_ua_all_1,
             estimator = "WLS",
             group = "wave", group.equal = "loadings")
summary(fit_m, fit.measures = TRUE, standardized = TRUE)

# Inspect modification indices to identify the most non-invariant loading
lavTestScore(fit_m)
parTable(fit_m)

# Partial metric invariance — pray loading freed (M1a) — rejected
fit_mp <- cfa(model, data = ess_ua_all_1,
              estimator = "WLS",
              group = "wave", group.equal = "loadings",
              group.partial = "Involv =~ pray")
summary(fit_mp, fit.measures = TRUE, standardized = TRUE)

## 2b. Waves 2–6 (excluding wave 10) — Table 3 ─────────────────────────────

# Full metric invariance (M1) — rejected
fit_m1 <- cfa(model, data = ess_ua_all_1[ess_ua_all_1$wave != "10", ],
              estimator = "WLS",
              group = "wave", group.equal = "loadings")
summary(fit_m1, fit.measures = TRUE, standardized = TRUE)

# Inspect modification indices
lavTestScore(fit_m1)
parTable(fit_m1)

# Partial metric invariance — pray loading freed (M1a) — accepted
fit_mp1 <- cfa(model, data = ess_ua_all_1[ess_ua_all_1$wave != "10", ],
               estimator = "WLS",
               group = "wave", group.equal = "loadings",
               group.partial = "Involv =~ pray")
summary(fit_mp1, fit.measures = TRUE, standardized = TRUE)

# Partial scalar invariance — pray loading freed (M2a) — rejected
fit_sp1 <- cfa(model, data = ess_ua_all_1[ess_ua_all_1$wave != "10", ],
               estimator = "WLS",
               group = "wave", group.equal = c("loadings", "intercepts"),
               group.partial = "Involv =~ pray")
summary(fit_sp1, fit.measures = TRUE, standardized = TRUE)
summary(compareFit(fit_mp1, fit_sp1))

## 2c. Waves 3–6 (excluding waves 2 and 10) — Table 4 ─────────────────────

# Full metric invariance (M2) — accepted
fit_m2 <- cfa(model, data = ess_ua_all_1[ess_ua_all_1$wave != "2" & ess_ua_all_1$wave != "10", ],
              estimator = "WLS",
              group = "wave", group.equal = "loadings")
summary(fit_m2, fit.measures = TRUE, standardized = TRUE)

# Full scalar invariance (M3) — rejected
fit_s2 <- cfa(model, data = ess_ua_all_1[ess_ua_all_1$wave != "2" & ess_ua_all_1$wave != "10", ],
              estimator = "WLS",
              group = "wave", group.equal = c("loadings", "intercepts"))
summary(fit_s2, fit.measures = TRUE, standardized = TRUE)
summary(compareFit(fit_m2, fit_s2))

# Inspect modification indices to identify the most non-invariant intercept
lavTestScore(fit_s2)
parTable(fit_s2)

# Partial scalar invariance — rlgatnd (attendance) intercept freed (M3a) — rejected
fit_sp2 <- cfa(model, data = ess_ua_all_1[ess_ua_all_1$wave != "2" & ess_ua_all_1$wave != "10", ],
               estimator = "WLS",
               group = "wave", group.equal = c("loadings", "intercepts"),
               group.partial = c("rlgatnd ~ 1"))
summary(fit_sp2, fit.measures = TRUE, standardized = TRUE)
summary(compareFit(fit_m2, fit_sp2))

## 2d. All time points, excluding Crimea, Donetsk, and Luhansk oblasts — Table 5 ─

# Full metric invariance (M1) — rejected
fit_mr <- cfa(model, data = ess_ua_all_r,
              estimator = "WLS",
              group = "wave", group.equal = "loadings")
summary(fit_mr, fit.measures = TRUE, standardized = TRUE)

# Inspect modification indices
lavTestScore(fit_mr)
parTable(fit_mr)

# Partial metric invariance — rlgatnd (attendance) loading freed (M1a) — rejected
fit_mpr <- cfa(model, data = ess_ua_all_r,
               estimator = "WLS",
               group = "wave", group.equal = "loadings",
               group.partial = "Involv =~ rlgatnd")
summary(fit_mpr, fit.measures = TRUE, standardized = TRUE)


# ── Spearman correlations between RI scale items across waves (Appendix 1) ──────────
round(cors, 3)

# ── Comparing correlation coefficients across waves (Appendix 2) ───────

# Sample sizes for each correlation coefficient
cor2n  <- corr.test(ess_ua_all_1[ess_ua_all_1$wave == "2",  1:3], method = "spearman", adjust = "none")[["n"]]
cor3n  <- corr.test(ess_ua_all_1[ess_ua_all_1$wave == "3",  1:3], method = "spearman", adjust = "none")[["n"]]
cor4n  <- corr.test(ess_ua_all_1[ess_ua_all_1$wave == "4",  1:3], method = "spearman", adjust = "none")[["n"]]
cor5n  <- corr.test(ess_ua_all_1[ess_ua_all_1$wave == "5",  1:3], method = "spearman", adjust = "none")[["n"]]
cor6n  <- corr.test(ess_ua_all_1[ess_ua_all_1$wave == "6",  1:3], method = "spearman", adjust = "none")[["n"]]
cor10n <- corr.test(ess_ua_all_1[ess_ua_all_1$wave == "10", 1:3], method = "spearman", adjust = "none")[["n"]]

corsn <- rbind(cor2n[lower.tri(cor2n)],
               cor3n[lower.tri(cor3n)],
               cor4n[lower.tri(cor4n)],
               cor5n[lower.tri(cor5n)],
               cor6n[lower.tri(cor6n)],
               cor10n[lower.tri(cor10n)])
rownames(corsn) <- c("2005", "2006-2007", "2009", "2011", "2013", "2022")
colnames(corsn) <- c("rlgdgr|rlgatnd", "rlgdgr|pray", "rlgatnd|pray")

# rlgdgr|rlgatnd: pairwise comparisons across waves
Comparing_Multiple_Independent_Correlation_Coefficients(
  Correlation_Coefficients         = cors[, 1],
  Numbers_of_Observations          = corsn[, 1],
  Identifiers                      = rownames(cors),
  Control_for_Experimentwise_Error = FALSE)

# rlgdgr|pray: pairwise comparisons across waves
Comparing_Multiple_Independent_Correlation_Coefficients(
  Correlation_Coefficients         = cors[, 2],
  Numbers_of_Observations          = corsn[, 2],
  Identifiers                      = rownames(cors),
  Control_for_Experimentwise_Error = FALSE)

# rlgatnd|pray: pairwise comparisons across waves
Comparing_Multiple_Independent_Correlation_Coefficients(
  Correlation_Coefficients         = cors[, 3],
  Numbers_of_Observations          = corsn[, 3],
  Identifiers                      = rownames(cors),
  Control_for_Experimentwise_Error = FALSE)