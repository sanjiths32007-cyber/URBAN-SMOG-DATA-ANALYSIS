# OPTIONAL CONSOLE REPORT: Person 3 Part C, adapted to integrated file names.
# =============================================================================
# PERSON 3 - PART C: VALIDATION, SAMPLE ANALYSIS AND INTERPRETATION
# Keep Part-A.R, Part-B.R and cleaned_smog.csv in the same folder.
# Run this file from that folder.
# =============================================================================

source("04_inferential_statistics.R")
source("05_correlation_regression.R")

run_person3_sample_analysis <- function(file = "cleaned_smog.csv", alpha = 0.05) {
  check_alpha(alpha)

  if (!file.exists(file)) {
    stop("File not found: ", file,
         "\nPlace cleaned_smog.csv in the working directory or provide its full path.")
  }

  data <- read.csv(file, stringsAsFactors = FALSE, na.strings = c("", "NA", "N/A"))
  required <- c(
    "pm10_ug_m3", "pm2_5_ug_m3", "carbon_monoxide_ug_m3",
    "nitrogen_dioxide_ug_m3", "ozone_ug_m3", "european_aqi",
    "hazardous_event"
  )
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols)) {
    stop("Required columns missing from CSV: ", paste(missing_cols, collapse = ", "))
  }

  numeric_cols <- c(
    "pm10_ug_m3", "pm2_5_ug_m3", "carbon_monoxide_ug_m3",
    "nitrogen_dioxide_ug_m3", "ozone_ug_m3", "european_aqi",
    "hazardous_event"
  )
  for (nm in numeric_cols) {
    if (!is.numeric(data[[nm]])) {
      converted <- suppressWarnings(as.numeric(as.character(data[[nm]])))
      if (sum(!is.na(data[[nm]]) & is.na(converted)) > 0) {
        stop("Column ", nm, " contains values that cannot be converted to numeric.")
      }
      data[[nm]] <- converted
    }
  }

  event_values <- unique(data$hazardous_event[!is.na(data$hazardous_event)])
  if (!all(event_values %in% c(0, 1))) {
    stop("hazardous_event must be coded as 0 and 1 (or missing).")
  }

  cat("\n============================================================\n")
  cat("PERSON 3: SMOG INFERENTIAL STATISTICS AND CORRELATION REPORT\n")
  cat("============================================================\n")
  cat("File:", file, "\n")
  cat("Rows:", nrow(data), "| Columns:", ncol(data), "\n")
  cat("Significance level alpha:", alpha, "\n\n")

  cat("--- MISSING-VALUE SUMMARY ---\n")
  print(sort(colSums(is.na(data)), decreasing = TRUE))

  cat("\n--- 1. CONFIDENCE INTERVAL FOR MEAN PM2.5 ---\n")
  pm25 <- data$pm2_5_ug_m3
  pm25_clean <- pm25[is.finite(pm25)]
  if (length(pm25_clean) >= 2) {
    print(mean_confidence_interval(pm25_clean, confidence = 1 - alpha))
    cat("Sample mean PM2.5:", round(mean(pm25_clean), 4), "\n")
    cat("Interpretation: the interval estimates the population mean PM2.5 at the selected confidence level.\n")
  } else {
    cat("Skipped: insufficient valid PM2.5 observations.\n")
  }

  cat("\n--- 2. PM2.5 BY HAZARDOUS EVENT: INDEPENDENT T-TEST ---\n")
  group0 <- pm25[data$hazardous_event == 0 & is.finite(pm25)]
  group1 <- pm25[data$hazardous_event == 1 & is.finite(pm25)]
  if (length(group0) >= 2 && length(group1) >= 2) {
    print(two_sample_t_test(group0, group1, alpha = alpha, var_equal = FALSE))
    cat("Mean PM2.5 where hazardous_event = 0:", round(mean(group0), 4), "\n")
    cat("Mean PM2.5 where hazardous_event = 1:", round(mean(group1), 4), "\n")
    cat("Interpretation: this compares average PM2.5 between the two labelled groups; it does not prove causation.\n")
  } else {
    cat("Skipped: both hazardous_event groups need at least two valid PM2.5 observations.\n")
  }

  cat("\n--- 3. CHI-SQUARE TEST: CITY AND HAZARDOUS EVENT ---\n")
  if ("city" %in% names(data)) {
    tryCatch({
      print(chi_square_independence_test(data$city, data$hazardous_event, alpha))
      cat("Interpretation: tests whether city and hazardous-event labels are associated.\n")
    }, error = function(e) cat("Skipped:", conditionMessage(e), "\n"))
  } else {
    cat("Skipped: city column is not present.\n")
  }

  cat("\n--- 4. ONE-WAY ANOVA: PM2.5 ACROSS CITIES ---\n")
  if ("city" %in% names(data)) {
    tryCatch({
      print(one_way_anova(data$pm2_5_ug_m3, data$city, alpha))
      cat("Interpretation: tests whether mean PM2.5 differs across at least two cities.\n")
    }, error = function(e) cat("Skipped:", conditionMessage(e), "\n"))
  } else {
    cat("Skipped: city column is not present.\n")
  }

  cat("\n--- 5. CORRELATION: PM2.5 AND EUROPEAN AQI ---\n")
  tryCatch({
    print(pearson_correlation(data$pm2_5_ug_m3, data$european_aqi, alpha))
  }, error = function(e) cat("Pearson correlation skipped:", conditionMessage(e), "\n"))

  tryCatch({
    print(spearman_correlation(data$pm2_5_ug_m3, data$european_aqi, alpha))
  }, error = function(e) cat("Spearman correlation skipped:", conditionMessage(e), "\n"))

  cat("\n--- 6. COVARIANCE: PM2.5 AND EUROPEAN AQI ---\n")
  tryCatch({
    print(covariance_analysis(data$pm2_5_ug_m3, data$european_aqi))
  }, error = function(e) cat("Covariance skipped:", conditionMessage(e), "\n"))

  cat("\n--- 7. POLLUTANT/AQI CORRELATION MATRIX ---\n")
  corr_vars <- c("pm10_ug_m3", "pm2_5_ug_m3", "carbon_monoxide_ug_m3",
                 "nitrogen_dioxide_ug_m3", "ozone_ug_m3", "european_aqi")
  tryCatch({
    print(round(correlation_matrix(data, corr_vars), 3))
  }, error = function(e) cat("Correlation matrix skipped:", conditionMessage(e), "\n"))

  cat("\n--- NOTES FOR REPORTING ---\n")
  cat("* Statistical significance does not automatically mean practical importance.\n")
  cat("* Correlation and association do not prove cause and effect.\n")
  cat("* Tests rely on assumptions; repeated hourly observations may not be independent.\n")
  cat("* hazardous_event is treated as the existing 0/1 label; its definition should come from the dataset source.\n")
  cat("\nANALYSIS COMPLETE.\n")

  invisible(list(data = data, pm25_ci = if (length(pm25_clean) >= 2)
    mean_confidence_interval(pm25_clean, confidence = 1 - alpha) else NULL))
}

# To run:
# source("Part-C.R")
# smog_results <- run_person3_sample_analysis("cleaned_smog.csv", alpha = 0.05)
