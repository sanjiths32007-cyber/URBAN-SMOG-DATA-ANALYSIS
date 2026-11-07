# SHARED: DESCRIPTIVE STATISTICS
clean_numeric <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  x <- suppressWarnings(as.numeric(x))
  x[is.finite(x)]
}
calculate_mean <- function(x) { x <- clean_numeric(x); if (!length(x)) NA_real_ else mean(x) }
calculate_median <- function(x) { x <- clean_numeric(x); if (!length(x)) NA_real_ else median(x) }
calculate_mode <- function(x) {
  x <- clean_numeric(x); if (!length(x)) return(NA_real_)
  values <- unique(x); values[which.max(tabulate(match(x, values)))]
}
calculate_minimum <- function(x) { x <- clean_numeric(x); if (!length(x)) NA_real_ else min(x) }
calculate_maximum <- function(x) { x <- clean_numeric(x); if (!length(x)) NA_real_ else max(x) }
calculate_range <- function(x) { x <- clean_numeric(x); if (!length(x)) NA_real_ else diff(range(x)) }
calculate_variance <- function(x) { x <- clean_numeric(x); if (length(x) < 2) NA_real_ else var(x) }
calculate_sd <- function(x) { x <- clean_numeric(x); if (length(x) < 2) NA_real_ else sd(x) }
calculate_quartiles <- function(x) {
  x <- clean_numeric(x)
  if (!length(x)) return(c(Q1=NA_real_, Median=NA_real_, Q3=NA_real_))
  quantile(x, c(.25,.5,.75), names=TRUE)
}
calculate_iqr <- function(x) { x <- clean_numeric(x); if (!length(x)) NA_real_ else IQR(x) }
calculate_cv <- function(x) { x <- clean_numeric(x); if (length(x)<2 || mean(x)==0) NA_real_ else sd(x)/abs(mean(x))*100 }
calculate_skewness <- function(x) {
  x <- clean_numeric(x); n <- length(x)
  if (n<3 || sd(x)==0) return(NA_real_)
  z <- (x-mean(x))/sd(x); n/((n-1)*(n-2))*sum(z^3)
}
calculate_kurtosis <- function(x) {
  x <- clean_numeric(x); n <- length(x)
  if (n<4 || sd(x)==0) return(NA_real_)
  z <- (x-mean(x))/sd(x)
  n*(n+1)/((n-1)*(n-2)*(n-3))*sum(z^4)-3*(n-1)^2/((n-2)*(n-3))
}
descriptive_summary <- function(x) {
  raw <- x; x <- clean_numeric(x)
  if (!length(x)) return(data.frame(Statistic=c("Valid observations","Missing/non-numeric observations"), Value=c(0,length(raw))))
  q <- calculate_quartiles(x)
  data.frame(Statistic=c("Valid observations","Missing/non-numeric observations","Mean","Median","Mode","Minimum","Maximum","Range","Variance","Standard deviation","Q1","Q2 (Median)","Q3","Interquartile range (IQR)","Coefficient of variation (%)","Skewness","Excess kurtosis"),
    Value=c(length(x), length(raw)-length(x), calculate_mean(x), calculate_median(x), calculate_mode(x), calculate_minimum(x), calculate_maximum(x), calculate_range(x), calculate_variance(x), calculate_sd(x), unname(q[1]),unname(q[2]),unname(q[3]),calculate_iqr(x),calculate_cv(x),calculate_skewness(x),calculate_kurtosis(x)))
}
