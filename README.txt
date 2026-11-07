SMOG ANALYTICS DASHBOARD - INTEGRATED TEAM PROJECT

This ZIP integrates the original starter dashboard with Person 2's visualization/EDA work and Person 3's inferential statistics/correlation code. The app uses horizontal navigation tabs and includes an Auto Insights tab.

IMPORTANT: The ZIP does not include the original dataset. Copy global_urban_smog_pm25_hourly.csv into this folder before running the app. Do not create an empty cleaned_smog.csv manually.

SETUP
1. Install R and RStudio.
2. Open this project folder in RStudio (Session > Set Working Directory > Choose Directory).
3. Install required packages once:
   install.packages(c("shiny", "ggplot2", "DT", "lubridate"))
4. Create the cleaned dataset:
   source("01_data_cleaning.R")
5. Launch the dashboard:
   shiny::runApp("app.R")

FILES
01_data_cleaning.R              Cleans raw CSV and creates cleaned_smog.csv.
02_descriptive_statistics.R     Mean, median, variance, SD, quartiles, skewness, kurtosis, etc.
03_visualizations.R             Person 2's reusable plots and EDA summaries; no auto-running plots.
04_inferential_statistics.R     Person 3 Part A tests and confidence intervals.
05_correlation_regression.R     Person 3 Part B correlation plus regression functions.
06_person3_sample_analysis.R    Optional console analysis report from Person 3 Part C. Run only when desired.
app.R                           Shiny app with horizontal navbar and dynamic Auto Insights.
team_reference/                 Copies of original team code for reference.

DASHBOARD TABS
Overview, Dataset, Descriptive Statistics, Visualizations, Exploratory Analysis, Correlation, Regression, Inferential Statistics, Auto Insights.

NOTES
- Auto Insights summarizes numerical patterns, missing values, possible IQR outliers, and strongest absolute numeric correlation. It does not automatically determine regulatory safety for arbitrary datasets.
- Air-quality interpretation requires correct units, averaging periods, and official thresholds.
- Correlation does not prove causation. Statistical tests have assumptions; choose the test that matches your study design.
- The optional Part C report expects the air-quality column names listed in that script. It is specific to this SMOG dataset, unlike the generic Auto Insights tab.
- This environment did not have R installed, so the app was assembled and statically inspected but not executed in R. After opening in RStudio, test all tabs with your actual CSV.
