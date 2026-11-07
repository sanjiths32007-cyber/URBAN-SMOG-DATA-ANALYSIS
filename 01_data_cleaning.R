# PERSON 1: DATA CLEANING AND PREPARATION
# Input: global_urban_smog_pm25_hourly.csv
# Output: cleaned_smog.csv

if (!requireNamespace("lubridate", quietly = TRUE)) {
  stop("Install lubridate first: install.packages('lubridate')")
}
library(lubridate)

raw_file <- "global_urban_smog_pm25_hourly.csv"
output_file <- "cleaned_smog.csv"
if (!file.exists(raw_file)) {
  stop("Cannot find ", raw_file,
       ". Copy the original dataset into this project folder, then rerun this script.")
}
raw_data <- read.csv(raw_file, stringsAsFactors = FALSE, check.names = FALSE)
cat("Original rows:", nrow(raw_data), "\n")
cat("Original columns:", ncol(raw_data), "\n")

cleaned_data <- raw_data
names(cleaned_data) <- tolower(trimws(names(cleaned_data)))
names(cleaned_data) <- gsub("[ .-]+", "_", names(cleaned_data))
names(cleaned_data) <- gsub("[^a-z0-9_]", "", names(cleaned_data))
names(cleaned_data) <- make.unique(names(cleaned_data))

before <- nrow(cleaned_data)
cleaned_data <- cleaned_data[!duplicated(cleaned_data), , drop = FALSE]
cat("Exact duplicate rows removed:", before - nrow(cleaned_data), "\n")

if ("timestamp" %in% names(cleaned_data)) {
  original <- cleaned_data$timestamp
  parsed <- suppressWarnings(lubridate::parse_date_time(
    as.character(original),
    orders = c("ymd HMS", "ymd HM", "ymd", "dmy HMS", "dmy HM", "dmy",
               "mdy HMS", "mdy HM", "mdy"), quiet = TRUE
  ))
  cleaned_data$timestamp <- parsed
  cat("Unparsed non-missing timestamps:", sum(!is.na(original) & is.na(parsed)), "\n")
}

numeric_candidates <- c(
  "latitude", "longitude", "pm10_ug_m3", "pm2_5_ug_m3",
  "carbon_monoxide_ug_m3", "nitrogen_dioxide_ug_m3", "ozone_ug_m3",
  "dust_ug_m3", "uv_index", "european_aqi", "hazardous_event",
  "pm2_5", "pm25", "pm10", "no2", "so2", "co", "o3",
  "temperature", "humidity"
)
for (nm in intersect(numeric_candidates, names(cleaned_data))) {
  if (!is.numeric(cleaned_data[[nm]])) {
    cleaned_data[[nm]] <- suppressWarnings(as.numeric(trimws(as.character(cleaned_data[[nm]]))))
  }
}

write.csv(cleaned_data, output_file, row.names = FALSE, na = "")
cat("Cleaned file saved to:", normalizePath(output_file), "\n")
cat("Cleaned rows:", nrow(cleaned_data), "\n")
cat("Missing cells remaining:", sum(is.na(cleaned_data)), "\n")
cat("Columns:\n"); print(names(cleaned_data))
cat("\nSummary:\n"); print(summary(cleaned_data))
