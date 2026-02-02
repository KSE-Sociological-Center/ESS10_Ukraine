############################################################
# ESS Wave 10 vs Wave 11: table construction & output
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

# Import datasets for the 6th and 10th waves via the ESS API (registration on the ESS website is required; insert your User ID into the download URLs for ESS 11 and ESS 10 excluding Ukraine)
ESS10    <- read_sav(
  "https://api.ess.sikt.no/v1/data/dataFile/10.21338/ess10e03_3?userId=INSERT_YOUR_USER_ID&fileFormat=sav",
  user_na = T
)
ESS10_UA <- read_sav(
  "https://raw.githubusercontent.com/KSE-Sociological-Center/ESS10_Ukraine/main/ESS10UAe4.sav", 
  user_na = T
)
ESS11    <- read_sav(
  "https://api.ess.sikt.no/v1/data/dataFile/10.21338/ess11e04_0?userId=INSERT_YOUR_USER_ID&fileFormat=sav",
  user_na = T
)

### Alternatively, you can download all datasets locally and read them using the read_sav() function with the user_na = T parameter

# Load pre-prepared dictionary in Google Spreadsheets (variables are common for both datasets)
c_names <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1VEm95zgv3S0M1To6513vjsgVUw4Y346h/edit?usp=sharing&ouid=102750169329527721605&rtpof=true&sd=true")

# In dictionary:
# var   - variable names
# label - variable labels
# p_m   - p (percentage), m (mean), pm (percentage + mean)

# ==========================================================
# 2) Check variable names (optional)
# ==========================================================

names(ESS10)
names(ESS10_UA)
names(ESS11)

# ==========================================================
# 3) Keep required variables only
# ==========================================================

ESS10_an     <- ESS10[, c("pspwght", "cntry", c_names$var10)]
ESS10_UA_an  <- ESS10_UA[, c("pspwght", "cntry", c_names$var10)]
ESS11_an     <- ESS11[, c("pspwght", "cntry", c_names$var11)]

# Harmonize column names across waves (wave 11 -> wave 10)
colnames(ESS11_an) <- colnames(ESS10_an)

# Cleanup (optional)
rm(ESS10, ESS10_UA, ESS11)

# ==========================================================
# 4) Keep only common countries across waves
# ==========================================================

# Select only common countries for ESS 6 and ESS 10 datasets
ESS10_an <- subset(
  ESS10_an,
  is.na(match(ESS10_an$cntry, ESS11_an$cntry)) == F
)

ESS11_an <- subset(
  ESS11_an,
  is.na(match(ESS11_an$cntry, c(ESS10_an$cntry, ESS10_UA_an$cntry))) == F
)

# Harmonize country variable as factor + drop unused levels
ESS10_an$cntry <- as_factor(ESS10_an$cntry)
ESS10_an <- rm.unused.levels(ESS10_an, "cntry")

ESS11_an$cntry <- as_factor(ESS11_an$cntry)
ESS11_an <- rm.unused.levels(ESS11_an, "cntry")

# Vector of countries common for waves 10 and 11
Countries <- levels(ESS11_an$cntry)

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
# 7) Percent tables (Wave 10 + Ukraine row)
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
# 8) Percent tables (Wave 11)
# ==========================================================

perc11 <- vector("list", length(cp))
perc11 <- lapply(
  1:length(cp), function(x) {
    perc11[[x]] <- wtd.table(ESS11_an$cntry, as_factor(ESS11_an[[cp[x]]]), weights = ESS11_an$pspwght) |>
      prop.table(1) |>
      round(3) |>
      as.data.frame.matrix(perc11[[x]]) * 100
    perc11[[x]] <- cbind(perc11[[x]], perc11[[x]][, "Refusal"] + perc11[[x]][, "No answer"])
    perc11[[x]] <- perc11[[x]][, !colnames(perc11[[x]]) %in% c("Refusal", "No answer")]
  }
)

# ==========================================================
# 9) Combine percent tables (Wave 10 and Wave 11)
# ==========================================================

perc <- vector("list", length(cp))
perc <- lapply(
  1:length(cp), function(x) {
    perc[[x]] <- sapply(
      1:length(perc10[[x]]), function(y)
        data.frame(perc10[[x]][, y], perc11[[x]][, y]),
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
      c(colnames(perc11[[i]])[2:length(perc11[[i]]) - 1], "Refusal, no answer"),
      1:length(perc11[[i]])
    )
  )
}

# ==========================================================
# 11) Prepare variables for means (remove missing values)
# ==========================================================

ESS10_an$wkhct <- labelled_spss(ESS10_an$wkhct, na_range = c(555, 999))

for (i in cm) {
  ESS10_an[, i]     <- zap_missing(ESS10_an[, i])
  ESS10_UA_an[, i]  <- zap_missing(ESS10_UA_an[, i])
  ESS11_an[, i]     <- zap_missing(ESS11_an[, i])
}

# ==========================================================
# 12) Means tables (Wave 10 + Ukraine row)
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
# 13) Means tables (Wave 11)
# ==========================================================

mean11 <- vector("list", length(cm))
mean11 <- lapply(
  1:length(cm), function(x) {
    mean11[[x]] <- sapply(
      1:length(Countries), function(y)
        cbind(
          n = wtd.table(
            ESS11_an$cntry[!is.na(ESS11_an[[cm[x]]])],
            weights = ESS11_an$pspwght[!is.na(ESS11_an[[cm[x]]])]
          )[y] |>
            round(0),
          mean = wtd.mean(
            as.numeric(ESS11_an[[cm[x]]])[as.numeric(ESS11_an$cntry) == y],
            ESS11_an$pspwght[as.numeric(ESS11_an$cntry) == y],
            na.rm = T
          ) |>
            round(2),
          var = Hmisc::wtd.var(
            as.numeric(ESS11_an[[cm[x]]])[as.numeric(ESS11_an$cntry) == y],
            ESS11_an$pspwght[as.numeric(ESS11_an$cntry) == y],
            na.rm = T
          ) |>
            sqrt() |>
            round(2)
        )
      )
    mean11[[x]] <- mean11[[x]] |> t() |> as.data.frame.matrix()
  }
)

# ==========================================================
# 14) Combine means tables (Wave 10 and Wave 11)
# ==========================================================

mean <- vector("list", length(cm))
mean <- lapply(
  1:length(cm), function(x) {
    mean[[x]] <- sapply(
      1:length(mean10[[x]]), function(j)
        data.frame(mean10[[x]][, j], mean11[[x]][, j]),
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
# 16) Percent tables for pm variables (Wave 10 + Ukraine row)
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

percm10[22:42] <- lapply(22:42, function(x) {
    percm10[[x]] <- data.frame(
      percm10[[x]][,1:which(names(percm10[[x]]) == "Not like me at all")],
      rep("—", nrow(percm10[[x]])),
      percm10[[x]][,which(names(percm10[[x]]) == "Don't know"):length(percm10[[x]])]
    )
  }
)

# ==========================================================
# 17) Percent tables for pm variables (Wave 11)
# ==========================================================

percm11 <- vector("list", length(cpm))
percm11 <- lapply(
  1:length(cpm), function(x) {
    percm11[[x]] <- wtd.table(ESS11_an$cntry, as_factor(ESS11_an[[cpm[x]]]), weights = ESS11_an$pspwght) |>
      prop.table(1) |>
      round(3) |>
      as.data.frame.matrix(percm11[[x]]) * 100
    percm11[[x]] <- cbind(percm11[[x]], percm11[[x]][, "Refusal"] + percm11[[x]][, "No answer"])
    percm11[[x]] <- percm11[[x]][, !colnames(percm11[[x]]) %in% c("Refusal", "No answer")]
  }
)

percm11[[6]] <- format(percm11[[6]], digits = 1, nsmall = 1)
percm11[[6]][1,] <- rep("—", length(percm10[[6]]))

# ==========================================================
# 18) Combine percent tables for pm variables
# ==========================================================

percm <- vector("list", length(cpm))
percm <- lapply(
  1:length(cpm), function(x) {
    percm[[x]] <- sapply(
      1:length(percm10[[x]]), function(y)
        data.frame(percm10[[x]][, y], percm11[[x]][, y]),
      simplify = F
    )
    percm[[x]] <- do.call(cbind, percm[[x]])
  }
)

# ==========================================================
# 19) Prepare pm variables for means (remove missing values)
# ==========================================================

ESS10_an$ccnthum    <- labelled_spss(ESS10_an$ccnthum, na_range = c(55, 99))
ESS10_UA_an$ccnthum <- labelled_spss(ESS10_UA_an$ccnthum, na_range = c(55, 99))
ESS11_an$ccnthum    <- labelled_spss(ESS11_an$ccnthum, na_range = c(55, 99))

for (i in cpm) {
  ESS10_an[, i]     <- zap_missing(ESS10_an[, i])
  ESS10_UA_an[, i]  <- zap_missing(ESS10_UA_an[, i])
  ESS11_an[, i]     <- zap_missing(ESS11_an[, i])
}

# ==========================================================
# 20) Means for pm variables (Wave 10 + Ukraine)
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
# 21) Means for pm variables (Wave 11)
# ==========================================================

meanp11 <- lapply(
  1:length(cpm), function(x)
    sapply(
      1:length(Countries), function(y)
        wtd.mean(
          as.numeric(ESS11_an[[cpm[x]]])[as.numeric(ESS11_an$cntry) == y],
          ESS11_an$pspwght[as.numeric(ESS11_an$cntry) == y],
          na.rm = T
        ) |>
        round(2)
   )
)

meanp11[[6]] <- format(meanp11[[6]], digits = 2, nsmall = 2)
meanp11[[6]][1] <- "—"

# ==========================================================
# 22) Combine means tables for pm variables
# ==========================================================

meanp <- lapply(
  1:length(cpm), function(x)
    data.frame(meanp10[[x]], meanp11[[x]])
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
      c(colnames(percm11[[i]])[2:length(percm11[[i]]) - 1], "Refusal, no answer", "Mean"),
      0:length(percm11[[i]]) + 1
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
c_names$label[c_names$p_m == "m" & c_names$var != "nwspol" & c_names$var != "wkhct" & c_names$var != "wkhtot"] <- 
  sapply(
    2:(length(cm) - 2), function(i) {
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
      add_header_row(values = c(" ", rep(c("2022", "2024"), 3)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      bold(i = 21, part = "body") |>
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
      add_header_row(values = c(" ", rep(c("2022", "2024"), 4)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      bold(i = 21, part = "body") |>
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
      add_header_row(values = c(" ", rep(c("2022", "2024"), 5)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      bold(i = 21, part = "body") |>
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
      add_header_row(values = c(" ", rep(c("2022", "2024"), 6)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      merge_at(j = 12:13, i = 1, part = "header") |>
      bold(i = 21, part = "body") |>
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
      add_header_row(values = c(" ", rep(c("2022", "2024"), 7)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      merge_at(j = 12:13, i = 1, part = "header") |>
      merge_at(j = 14:15, i = 1, part = "header") |>
      bold(i = 21, part = "body") |>
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
      add_header_row(values = c(" ", rep(c("2022", "2024"), 8)), top = F) |>
      merge_at(j = 1, i = 1:2, part = "header") |>
      merge_at(j = 2:3, i = 1, part = "header") |>
      merge_at(j = 4:5, i = 1, part = "header") |>
      merge_at(j = 6:7, i = 1, part = "header") |>
      merge_at(j = 8:9, i = 1, part = "header") |>
      merge_at(j = 10:11, i = 1, part = "header") |>
      merge_at(j = 12:13, i = 1, part = "header") |>
      merge_at(j = 14:15, i = 1, part = "header") |>
      merge_at(j = 16:17, i = 1, part = "header") |>
      bold(i = 21, part = "body") |>
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
      add_header_row(values = c(" ", rep(c("2022", "2024"), 9)), top = F) |>
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
      bold(i = 21, part = "body") |>
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
      add_header_row(values = c(" ", rep(c("2022", "2024"), 10)), top = F) |>
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
      bold(i = 21, part = "body") |>
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

tables[[63]] <- footnote(
  tables[[63]], ref_symbols = " ", i = 1,
  value = as_paragraph(as_i("Note: "),
                       "This question was asked only to respondents who believe that climate change is happening")) |>
  fontsize(size = 9, part = "footer")

tables[[64]] <- footnote(
  tables[[64]], ref_symbols = " ", i = 1,
  value = as_paragraph(as_i("Note: "),
                       "This question was asked only to respondents who believe that climate change is happening")) |>
  fontsize(size = 9, part = "footer")

### All tables can be exported to Word using R Markdown and the flextable_to_rmd() function 