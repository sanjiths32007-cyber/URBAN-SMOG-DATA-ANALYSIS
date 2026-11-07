# INTEGRATED TEAM CODE: PERSON 3 PART B - CORRELATION
# =============================================================================
# PERSON 3 - PART B: CORRELATION ANALYSIS
# Source Part-A.R first for shared validation and result functions.
# =============================================================================

covariance_analysis <- function(x, y) {
  if (!is.numeric(x) || !is.numeric(y) || length(x) != length(y)) {
    stop("x and y must be numeric vectors of equal length.")
  }
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]
  if (length(x) < 2) stop("At least two complete pairs are required.")
  if (sd(x) == 0 || sd(y) == 0) stop("Covariance cannot be meaningfully calculated for a constant variable.")
  cov(x, y)
}

pearson_correlation <- function(x, y, alpha = 0.05) {
  check_alpha(alpha)
  if (!is.numeric(x) || !is.numeric(y) || length(x) != length(y)) {
    stop("x and y must be numeric vectors of equal length.")
  }
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]
  if (length(x) < 3) stop("Pearson correlation needs at least three complete pairs.")
  if (sd(x) == 0 || sd(y) == 0) stop("Correlation is undefined for a constant variable.")
  result <- cor.test(x, y, method = "pearson", conf.level = 1 - alpha)
  make_test_result("Pearson correlation",
                   unname(result$statistic), result$p.value,
                   unname(result$parameter), unname(result$estimate),
                   if (!is.null(result$conf.int)) result$conf.int else c(NA_real_, NA_real_),
                   alpha,
                   paste("Correlation coefficient r =", round(unname(result$estimate), 4),
                         "-", interpret_p_value(result$p.value, alpha)))
}

spearman_correlation <- function(x, y, alpha = 0.05) {
  check_alpha(alpha)
  if (!is.numeric(x) || !is.numeric(y) || length(x) != length(y)) {
    stop("x and y must be numeric vectors of equal length.")
  }
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]
  if (length(x) < 3) stop("Spearman correlation needs at least three complete pairs.")
  if (length(unique(x)) < 2 || length(unique(y)) < 2) stop("Correlation is undefined for a constant variable.")
  result <- suppressWarnings(cor.test(x, y, method = "spearman", exact = FALSE))
  make_test_result("Spearman rank correlation",
                   unname(result$statistic), result$p.value,
                   estimate = unname(result$estimate), alpha = alpha,
                   details = paste("Rank-based association; rho =",
                                   round(unname(result$estimate), 4), "-",
                                   interpret_p_value(result$p.value, alpha)))
}

correlation_matrix <- function(data, variables) {
  if (!is.data.frame(data)) stop("data must be a data frame.")
  missing_vars <- setdiff(variables, names(data))
  if (length(missing_vars)) stop("Missing variables: ", paste(missing_vars, collapse = ", "))
  if (!all(vapply(data[variables], is.numeric, logical(1)))) {
    stop("All selected correlation variables must be numeric.")
  }
  result <- cor(data[variables], use = "pairwise.complete.obs", method = "pearson")
  result
}


# REGRESSION FUNCTIONS retained from the starter project
clean_pair <- function(x, y) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  y <- suppressWarnings(as.numeric(as.character(y)))
  valid <- is.finite(x) & is.finite(y)
  list(x=x[valid], y=y[valid])
}
calculate_covariance <- function(x,y) covariance_analysis(x,y)
calculate_pearson <- function(x,y) unname(stats::cor(x,y,use="complete.obs",method="pearson"))
calculate_spearman <- function(x,y) unname(stats::cor(x,y,use="complete.obs",method="spearman"))
calculate_correlation_matrix <- function(data) {
  numeric_data<-data[vapply(data,is.numeric,logical(1))]
  if(ncol(numeric_data)<2) stop("At least two numeric columns are required.")
  stats::cor(numeric_data,use="pairwise.complete.obs")
}
simple_regression <- function(data,response,predictor) {
  if(response==predictor) stop("Choose different response and predictor variables.")
  model<-stats::lm(stats::reformulate(predictor,response=response),data=data,na.action=na.exclude)
  if(stats::df.residual(model)<=0) stop("Not enough complete observations for regression.")
  model
}
multiple_regression <- function(data,response,predictors) {
  if(!length(predictors)) stop("Choose at least one predictor.")
  if(response %in% predictors) stop("Response cannot also be a predictor.")
  model<-stats::lm(stats::reformulate(predictors,response=response),data=data,na.action=na.exclude)
  if(stats::df.residual(model)<=0) stop("Not enough complete observations for regression.")
  model
}
regression_results <- function(model) {
  s<-summary(model)
  list(coefficients=s$coefficients,r_squared=s$r.squared,adjusted_r_squared=s$adj.r.squared,
       residuals=residuals(model),fitted_values=fitted(model),formula=paste(deparse(formula(model)),collapse=""))
}
