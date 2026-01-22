############################################################
# ESS Wave 6 vs Wave 10: table construction & output
############################################################

# ==========================================================
# 0) Packages (install all of them + Hmisc package before attaching)
# ==========================================================

library(haven)
library(labelled)
library(gsheet)
library(questionr)
library(flextable)
library(officer)
library(tibble)

# ==========================================================
# 1) Data import
# ==========================================================

# Import datasets for the 6th and 10th waves via the ESS API (registration on the ESS website is required; insert your User ID into the download URLs for ESS 6 and ESS 10 excluding Ukraine)
ESS6     <- read_sav(
  "https://api.ess.sikt.no/v1/data/dataFile/10.21338/ess6e02_7?userId=INSERT_YOUR_USER_ID&fileFormat=sav",
  user_na = T
)
ESS10    <- read_sav(
  "https://api.ess.sikt.no/v1/data/dataFile/10.21338/ess10e03_3?userId=INSERT_YOUR_USER_ID&fileFormat=sav",
  user_na = T
)
ESS10_UA <- read_sav(
  "https://raw.githubusercontent.com/KSE-Sociological-Center/ESS10_Ukraine/main/ESS10UAe4.sav", 
  user_na = T
)

### Alternatively, you can download all datasets locally and read them using the read_sav() function with the user_na = T parameter

# Load pre-prepared dictionary in Google Spreadsheets (variables are common for both datasets)
c_names <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1UqX1MuVd6oj1GO6ZsPNzdxELYl12gQYI/edit?usp=sharing&ouid=102750169329527721605&rtpof=true&sd=true")

# In dictionary:
# var   - variable names
# label - variable labels
# p_m   - p (percentage), m (mean), pm (percentage + mean)

# ==========================================================
# 2) Check variable names (optional)
# ==========================================================

names(ESS6)
names(ESS10)
names(ESS10_UA)

# ==========================================================
# 3) Keep required variables only
# ==========================================================

ESS6_an     <- ESS6[, c("pspwght", "cntry", c_names$var6)]
ESS10_an    <- ESS10[, c("pspwght", "cntry", c_names$var10)]
ESS10_UA_an <- ESS10_UA[, c("pspwght", "cntry", c_names$var10)]

# Harmonize column names across waves (wave 6 -> wave 10)
colnames(ESS6_an) <- colnames(ESS10_an)

# Cleanup (optional)
rm(ESS6, ESS10, ESS10_UA)

# ==========================================================
# 4) Keep only common countries across waves
# ==========================================================

# Select only common countries for ESS 6 and ESS 10 datasets
ESS6_an <- subset(
  ESS6_an,
  is.na(match(ESS6_an$cntry, c(ESS10_an$cntry, ESS10_UA_an$cntry))) == F
)

ESS10_an <- subset(
  ESS10_an,
  is.na(match(ESS10_an$cntry, ESS6_an$cntry)) == F
)

# Harmonize country variable as factor + drop unused levels
ESS6_an$cntry <- as_factor(ESS6_an$cntry)
ESS6_an <- rm.unused.levels(ESS6_an, "cntry")

ESS10_an$cntry <- as_factor(ESS10_an$cntry)
ESS10_an <- rm.unused.levels(ESS10_an, "cntry")

# Vector of countries common for waves 6 and 10
Countries <- levels(ESS6_an$cntry)

# ==========================================================
# 5) Refresh dictionary
# ==========================================================

# Use unified variable names (wave 10 naming after harmonization) in dictionary
c_names$var <- colnames(ESS10_an)[3:length(ESS10_an)]

# Keep only needed columns from the dictionary
c_names <- c_names[, c(5, 3, 4)]

# ==========================================================
# 6) Variable groups (percent / mean / percent+mean)
# ==========================================================

cp  <- c_names$var[c_names$p_m == "p"]
cm  <- c_names$var[c_names$p_m == "m"]
cpm <- c_names$var[c_names$p_m == "pm"]

# ==========================================================
# 7) Percent tables (Wave 6)
# ==========================================================

perc6 <- vector("list", length(cp))
perc6 <- lapply(
  1:length(cp), function(x) {
    perc6[[x]] <- wtd.table(ESS6_an$cntry, as_factor(ESS6_an[[cp[x]]]), weights = ESS6_an$pspwght) |>
      prop.table(1) |>
      round(3) |>
      as.data.frame.matrix(perc6[[x]]) * 100
    perc6[[x]] <- cbind(perc6[[x]], perc6[[x]][, "Refusal"] + perc6[[x]][, "No answer"])
    perc6[[x]] <- perc6[[x]][, !colnames(perc6[[x]]) %in% c("Refusal", "No answer")]
  }
)

# ==========================================================
# 8) Percent tables (Wave 10 + Ukraine row)
# ==========================================================

perc10 <- vector("list", length(cp))
perc10 <- lapply(
  1:length(cp), function(x) {
    perc10[[x]] <- wtd.table(ESS10_an$cntry, as_factor(ESS10_an[[cp[x]]]), weights = ESS10_an$pspwght) |>
      prop.table(1) |>
      round(3) |>
      as.data.frame.matrix(perc10[[x]]) * 100
    perc10[[x]] <- rbind(
      perc10[[x]],
      wtd.table(as_factor(ESS10_UA_an[[cp[x]]]), weights = ESS10_UA_an$pspwght) |>
        prop.table() |>
        round(3) * 100
    )
    perc10[[x]] <- cbind(perc10[[x]], perc10[[x]][, "Refusal"] + perc10[[x]][, "No answer"])
    perc10[[x]] <- perc10[[x]][, !colnames(perc10[[x]]) %in% c("Refusal", "No answer")]
  }
)

# ==========================================================
# 9) Combine percent tables (Wave 6 and Wave 10)
# ==========================================================

perc <- vector("list", length(cp))
perc <- lapply(
  1:length(cp), function(x) {
    perc[[x]] <- sapply(
      1:length(perc10[[x]]), function(y)
        data.frame(perc6[[x]][, y], perc10[[x]][, y]),
      simplify = F
    )
    perc[[x]] <- do.call(cbind, perc[[x]])
  }
)

# ==========================================================
# 10) Add country column and column names for percent tables
# ==========================================================

for (i in seq_along(perc)) {
  perc[[i]] <- data.frame(Countries, perc[[i]])
  colnames(perc[[i]])[2:length(perc[[i]])] <-
    as.vector(rbind(
      c(colnames(perc6[[i]])[2:length(perc6[[i]]) - 1], "Refusal, no answer"),
      1:length(perc6[[i]])
    )
  )
}

# ==========================================================
# 11) Prepare variables for means (remove missing values)
# ==========================================================

ESS10_an$wkhct <- labelled_spss(ESS10_an$wkhct, na_range = c(555, 999))

for (i in cm) {
  ESS6_an[, i]     <- zap_missing(ESS6_an[, i])
  ESS10_an[, i]    <- zap_missing(ESS10_an[, i])
  ESS10_UA_an[, i] <- zap_missing(ESS10_UA_an[, i])
}

# ==========================================================
# 12) Means tables (Wave 6)
# ==========================================================

mean6 <- vector("list", length(cm))
mean6 <- lapply(
  1:length(cm), function(x) {
    mean6[[x]] <- sapply(
      1:length(Countries), function(y)
        cbind(
          n = wtd.table(
            ESS6_an$cntry[!is.na(ESS6_an[[cm[x]]])],
            weights = ESS6_an$pspwght[!is.na(ESS6_an[[cm[x]]])]
          )[y] |>
            round(0),
          mean = wtd.mean(
            as.numeric(ESS6_an[[cm[x]]])[as.numeric(ESS6_an$cntry) == y],
            ESS6_an$pspwght[as.numeric(ESS6_an$cntry) == y],
            na.rm = T
          ) |>
            round(2),
          var = Hmisc::wtd.var(
            as.numeric(ESS6_an[[cm[x]]])[as.numeric(ESS6_an$cntry) == y],
            ESS6_an$pspwght[as.numeric(ESS6_an$cntry) == y],
            na.rm = T
          ) |>
            sqrt() |>
            round(2)
      )
    )
    mean6[[x]] <- mean6[[x]] |> t() |> as.data.frame.matrix()
  }
)

# ==========================================================
# 13) Means tables (Wave 10 + Ukraine row)
# ==========================================================

mean10 <- vector("list", length(cm))
mean10 <- lapply(
  1:length(cm), function(x) {
    mean10[[x]] <- sapply(
      2:length(Countries) - 1, function(y)
        cbind(
          n = wtd.table(
            ESS10_an$cntry[!is.na(ESS10_an[[cm[x]]])],
            weights = ESS10_an$pspwght[!is.na(ESS10_an[[cm[x]]])]
          )[y] |>
            round(0),
          mean = wtd.mean(
            as.numeric(ESS10_an[[cm[x]]])[as.numeric(ESS10_an$cntry) == y],
            ESS10_an$pspwght[as.numeric(ESS10_an$cntry) == y],
            na.rm = T
          ) |>
            round(2),
          var = Hmisc::wtd.var(
            as.numeric(ESS10_an[[cm[x]]])[as.numeric(ESS10_an$cntry) == y],
            ESS10_an$pspwght[as.numeric(ESS10_an$cntry) == y],
            na.rm = T
          ) |>
            sqrt() |>
            round(2)
      )
    )
    mean10[[x]] <- mean10[[x]] |> t() |> as.data.frame.matrix()
    mean10[[x]] <- rbind(
      mean10[[x]],
      c(
        wtd.table(
          as.factor(ESS10_UA_an$cntry)[!is.na(ESS10_UA_an[[cm[x]]])],
          weights = ESS10_UA_an$pspwght[!is.na(ESS10_UA_an[[cm[x]]])]
        ) |>
          round(0),
        wtd.mean(
          as.numeric(ESS10_UA_an[[cm[x]]]),
          ESS10_UA_an$pspwght,
          na.rm = T
        ) |>
          round(2),
        Hmisc::wtd.var(
          as.numeric(ESS10_UA_an[[cm[x]]]),
          ESS10_UA_an$pspwght,
          na.rm = T
        ) |>
          sqrt() |>
          round(2)
      )
    )
  }
)

# ==========================================================
# 14) Combine means tables (Wave 6 and Wave 10)
# ==========================================================

mean <- vector("list", length(cm))
mean <- lapply(
  1:length(cm), function(x) {
    mean[[x]] <- sapply(
      1:length(mean10[[x]]), function(j)
        data.frame(mean6[[x]][, j], mean10[[x]][, j]),
      simplify = F
    )
    mean[[x]] <- do.call(cbind, mean[[x]])
  }
)

# ==========================================================
# 15) Add country column and column names for means tables
# ==========================================================

for (i in seq_along(mean)) {
  mean[[i]] <- data.frame(Countries, mean[[i]])
  colnames(mean[[i]])[2:length(mean[[i]])] <-
    as.vector(rbind(c("N", "Mean", "Standard deviation"), 1:3))
}

# ==========================================================
# 16) Percent tables for pm variables (Wave 6)
# ==========================================================

percm6 <- vector("list", length(cpm))
percm6 <- lapply(
  1:length(cpm), function(x) {
    percm6[[x]] <- wtd.table(ESS6_an$cntry, as_factor(ESS6_an[[cpm[x]]]), weights = ESS6_an$pspwght) |>
      prop.table(1) |>
      round(3) |>
      as.data.frame.matrix(percm6[[x]]) * 100
    percm6[[x]] <- cbind(percm6[[x]], percm6[[x]][, "Refusal"] + percm6[[x]][, "No answer"])
    percm6[[x]] <- percm6[[x]][, !colnames(percm6[[x]]) %in% c("Refusal", "No answer")]
  }
)

# ==========================================================
# 17) Percent tables for pm variables (Wave 10 + Ukraine row)
# ==========================================================

percm10 <- vector("list", length(cpm))
percm10 <- lapply(
  1:length(cpm), function(x) {
    percm10[[x]] <- wtd.table(ESS10_an$cntry, as_factor(ESS10_an[[cpm[x]]]), weights = ESS10_an$pspwght) |>
      prop.table(1) |>
      round(3) |>
      as.data.frame.matrix(percm10[[x]]) * 100
    percm10[[x]] <- rbind(
      percm10[[x]],
      wtd.table(as_factor(ESS10_UA_an[[cpm[x]]]), weights = ESS10_UA_an$pspwght) |>
        prop.table() |>
        round(3) * 100
    )
    percm10[[x]] <- cbind(percm10[[x]], percm10[[x]][, "Refusal"] + percm10[[x]][, "No answer"])
    percm10[[x]] <- percm10[[x]][, !colnames(percm10[[x]]) %in% c("Refusal", "No answer")]
  }
)

# ==========================================================
# 18) Combine percent tables for pm variables
# ==========================================================

percm <- vector("list", length(cpm))
percm <- lapply(
  1:length(cpm), function(x) {
    percm[[x]] <- sapply(
      1:length(percm10[[x]]), function(y)
        data.frame(percm6[[x]][, y], percm10[[x]][, y]),
      simplify = F
    )
    percm[[x]] <- do.call(cbind, percm[[x]])
  }
)

# ==========================================================
# 19) Prepare variables for means (remove missing values)
# ==========================================================

for (i in cpm) {
  ESS6_an[, i]     <- zap_missing(ESS6_an[, i])
  ESS10_an[, i]    <- zap_missing(ESS10_an[, i])
  ESS10_UA_an[, i] <- zap_missing(ESS10_UA_an[, i])
}

# ==========================================================
# 20) Means for pm variables (Wave 6)
# ==========================================================

meanp6 <- lapply(
  1:length(cpm), function(x)
    sapply(
      1:length(Countries), function(y)
        wtd.mean(
          as.numeric(ESS6_an[[cpm[x]]])[as.numeric(ESS6_an$cntry) == y],
          ESS6_an$pspwght[as.numeric(ESS6_an$cntry) == y],
          na.rm = T
        ) |>
        round(2)
    )
)

# ==========================================================
# 21) Means for pm variables (Wave 10 + Ukraine)
# ==========================================================

meanp10 <- vector("list", length(cpm))
meanp10 <- lapply(
  1:length(cpm), function(x) {
    meanp10[[x]] <- sapply(
      2:length(Countries) - 1, function(y)
        wtd.mean(
          as.numeric(ESS10_an[[cpm[x]]])[as.numeric(ESS10_an$cntry) == y],
          ESS10_an$pspwght[as.numeric(ESS10_an$cntry) == y],
          na.rm = T
        ) |>
        round(2)
    )
    meanp10[[x]] <- c(
      meanp10[[x]],
      wtd.mean(
        as.numeric(ESS10_UA_an[[cpm[x]]]),
        ESS10_UA_an$pspwght,
        na.rm = T
      ) |>
        round(2)
    )
  }
)

# ==========================================================
# 22) Combine means tables for pm variables
# ==========================================================

meanp <- lapply(
  1:length(cpm), function(x)
    data.frame(meanp6[[x]], meanp10[[x]])
)

# ==========================================================
# 23) Combine pm percent + mean
# ==========================================================

perc_mean <- lapply(
  1:length(cpm), function(x)
    cbind(percm[[x]], meanp[[x]])
)

# ==========================================================
# 24) Add country column and column names for pm tables
# ==========================================================

for (i in seq_along(perc_mean)) {
  perc_mean[[i]] <- data.frame(Countries, perc_mean[[i]])
  colnames(perc_mean[[i]])[2:length(perc_mean[[i]])] <-
    as.vector(rbind(
      c(colnames(percm6[[i]])[2:length(percm6[[i]]) - 1], "Refusal, no answer", "Mean"),
      0:length(percm6[[i]]) + 1
    )
  )
}

# ==========================================================
# 25) Collect all data frames in original order
# ==========================================================

dfs <- c(perc, mean, perc_mean)
names(dfs) <- c(
  which(c_names$p_m == "p"),
  which(c_names$p_m == "m"),
  which(c_names$p_m == "pm")
)
dfs <- dfs[order(as.numeric(names(dfs)))]

# ==========================================================
# 26) Flextable defaults
# ==========================================================

set_flextable_defaults(
  font.family = "Times New Roman",
  hansi.family = "Times New Roman",
  font.size = 11,
  decimal.mark = ".",
  big.mark = "",
  border.color = "black",
  padding.bottom = 1.5,
  padding.top = 1.5
)

# ==========================================================
# 27) Create captions for tables in dictionary file
# ==========================================================

# Create scale descriptions for captions of means tables (these variables have scale from 0 to 10 except of two last)
c_names$label[c_names$p_m == "m" & c_names$var != "wkhct" & c_names$var != "wkhtot"] <- 
  sapply(
    3:length(cm) - 2, function(i) {
      vlabels <- get_value_labels(ESS10_an[[cm[i]]])
      scale_text <- paste0(
        "(", vlabels[1],  " — ", names(vlabels)[1], ", ",
        vlabels[11], " — ", names(vlabels)[11], ")"
      )
      c_names$label[c_names$var == cm[i]] <-
        paste(
          c_names$label[c_names$var == cm[i]],
          scale_text,
          sep = " "
        )
    }
  )

# Captions for percent tables
c_names$label[c_names$p_m == "p"] <- 
  sapply(
    cp, function(x) {
      paste(c_names[c_names$var == x, 2], "%")
    }
  )

# Captions for pm tables
c_names$label[c_names$p_m == "pm"] <- 
  sapply(
    cpm, function(x) {
      paste(c_names[c_names$var == x, 2], "% and mean")
    }
  )

# ==========================================================
# 28) Build flextables (output tables)
# ==========================================================

# Check number of columns in data frames
sapply(dfs, function(x) length(x)) |> unique() |> sort()

# Build tables depending on the number of columns
tables <- NULL

for (i in 1:length(dfs)) {
  
  if (length(dfs[[i]]) == 7) {
    tables[[i]] <- flextable(dfs[[i]]) |>
      set_caption(c_names$label[i]) |>
      add_header_row(values = c(" ", rep(c("2013", "2022"), 3)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      bold(i = 19, part = "body") |>
      bold(i = 1, part = "header") |>
      italic(i = 2, part = "header") |>
      align(align = "c", part = "header") |>
      align(align = "l", i = 1, j = 1, part = "header") |>
      align(align = "c", part = "body") |>
      align(align = "l", j = 1, part = "body")
  }
  
  if (length(dfs[[i]]) == 9) {
    tables[[i]] <- flextable(dfs[[i]]) |>
      set_caption(c_names$label[i]) |>
      add_header_row(values = c(" ", rep(c("2013", "2022"), 4)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      bold(i = 19, part = "body") |>
      bold(i = 1, part = "header") |>
      italic(i = 2, part = "header") |>
      align(align = "c", part = "header") |>
      align(align = "l", i = 1, j = 1, part = "header") |>
      align(align = "c", part = "body") |>
      align(align = "l", j = 1, part = "body")
  }
  
  if (length(dfs[[i]]) == 11) {
    tables[[i]] <- flextable(dfs[[i]]) |>
      set_caption(c_names$label[i]) |>
      add_header_row(values = c(" ", rep(c("2013", "2022"), 5)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      bold(i = 19, part = "body") |>
      bold(i = 1, part = "header") |>
      italic(i = 2, part = "header") |>
      align(align = "c", part = "header") |>
      align(align = "l", i = 1, j = 1, part = "header") |>
      align(align = "c", part = "body") |>
      align(align = "l", j = 1, part = "body")
  }
  
  if (length(dfs[[i]]) == 13) {
    tables[[i]] <- flextable(dfs[[i]]) |>
      set_caption(c_names$label[i]) |>
      add_header_row(values = c(" ", rep(c("2013", "2022"), 6)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      merge_at(j = 12:13, i = 1, part = "header") |>
      bold(i = 19, part = "body") |>
      bold(i = 1, part = "header") |>
      italic(i = 2, part = "header") |>
      align(align = "c", part = "header") |>
      align(align = "l", i = 1, j = 1, part = "header") |>
      align(align = "c", part = "body") |>
      align(align = "l", j = 1, part = "body")
  }
  
  if (length(dfs[[i]]) == 15) {
    tables[[i]] <- flextable(dfs[[i]]) |>
      set_caption(c_names$label[i]) |>
      add_header_row(values = c(" ", rep(c("2013", "2022"), 7)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      merge_at(j = 12:13, i = 1, part = "header") |>
      merge_at(j = 14:15, i = 1, part = "header") |>
      bold(i = 19, part = "body") |>
      bold(i = 1, part = "header") |>
      italic(i = 2, part = "header") |>
      align(align = "c", part = "header") |>
      align(align = "l", i = 1, j = 1, part = "header") |>
      align(align = "c", part = "body") |>
      align(align = "l", j = 1, part = "body")
  }
  
  if (length(dfs[[i]]) == 17) {
    tables[[i]] <- flextable(dfs[[i]]) |>
      set_caption(c_names$label[i]) |>
      add_header_row(values = c(" ", rep(c("2013", "2022"), 8)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      merge_at(j = 12:13, i = 1, part = "header") |>
      merge_at(j = 14:15, i = 1, part = "header") |>
      merge_at(j = 16:17, i = 1, part = "header") |>
      bold(i = 19, part = "body") |>
      bold(i = 1, part = "header") |>
      italic(i = 2, part = "header") |>
      align(align = "c", part = "header") |>
      align(align = "l", i = 1, j = 1, part = "header") |>
      align(align = "c", part = "body") |>
      align(align = "l", j = 1, part = "body")
  }
  
  if (length(dfs[[i]]) == 19) {
    tables[[i]] <- flextable(dfs[[i]]) |>
      set_caption(c_names$label[i]) |>
      add_header_row(values = c(" ", rep(c("2013", "2022"), 9)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      merge_at(j = 12:13, i = 1, part = "header") |>
      merge_at(j = 14:15, i = 1, part = "header") |>
      merge_at(j = 16:17, i = 1, part = "header") |>
      merge_at(j = 18:19, i = 1, part = "header") |>
      bold(i = 19, part = "body") |>
      bold(i = 1, part = "header") |>
      italic(i = 2, part = "header") |>
      align(align = "c", part = "header") |>
      align(align = "l", i = 1, j = 1, part = "header") |>
      align(align = "c", part = "body") |>
      align(align = "l", j = 1, part = "body")
  }
  
  if (length(dfs[[i]]) == 21) {
    tables[[i]] <- flextable(dfs[[i]]) |>
      set_caption(c_names$label[i]) |>
      add_header_row(values = c(" ", rep(c("2013", "2022"), 10)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      merge_at(j = 12:13, i = 1, part = "header") |>
      merge_at(j = 14:15, i = 1, part = "header") |>
      merge_at(j = 16:17, i = 1, part = "header") |>
      merge_at(j = 18:19, i = 1, part = "header") |>
      merge_at(j = 20:21, i = 1, part = "header") |>
      bold(i = 19, part = "body") |>
      bold(i = 1, part = "header") |>
      italic(i = 2, part = "header") |>
      align(align = "c", part = "header") |>
      align(align = "l", i = 1, j = 1, part = "header") |>
      align(align = "c", part = "body") |>
      align(align = "l", j = 1, part = "body")
  }
}

# ==========================================================
# 29) Final formatting for pm tables (italicize mean columns)
# ==========================================================

for (i in (which(c_names$p_m == "pm"))) {
  len0 <- length(dfs[[i]]) - 1
  len  <- length(dfs[[i]])
  tables[[i]] <- italic(tables[[i]], j = len0:len, part = "all")
}

# ==========================================================
# 30) Add footnotes where necessary
# ==========================================================

tables[[68]] <- footnote(
  tables[[68]], ref_symbols = " ", i = 1,
  value = as_paragraph(as_i("Note: "),
                       "This question was asked only to respondents who believe that the best for democracy if the government change its policies in response to what most people think")) |>
  fontsize(size = 9, part = "footer")

tables[[69]] <- footnote(
  tables[[69]], ref_symbols = " ", i = 1,
  value = as_paragraph(as_i("Note: "),
                       "This question was asked only of respondents who believe that it is best for democracy when the government changes its policies in response to what most people think, as well as those who answered “It depends on the circumstances”, “Refusal”, or “Don’t know” in the earlier filter question")) |>
  fontsize(size = 9, part = "footer")

tables[[70]] <- footnote(
  tables[[70]], ref_symbols = " ", i = 1,
  value = as_paragraph(as_i("Note: "),
                       "This question was asked only to respondents who believe that the best for democracy if the government sticks its policies regardless of what most people think")) |>
  fontsize(size = 9, part = "footer")

tables[[71]] <- footnote(
  tables[[71]], ref_symbols = " ", i = 1,
  value = as_paragraph(as_i("Note: "),
                       "This question was asked only to respondents who believe that the best for democracy if the government sticks its policies regardless of what most people think")) |>
  fontsize(size = 9, part = "footer")

### All tables can be exported to Word using R Markdown and the flextable_to_rmd() function 