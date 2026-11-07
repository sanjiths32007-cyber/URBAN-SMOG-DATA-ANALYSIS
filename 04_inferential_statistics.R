# INTEGRATED TEAM CODE: PERSON 3 PART A - INFERENTIAL STATISTICS
# =============================================================================
# PERSON 3 - PART A: INFERENTIAL STATISTICS
# Project: SMOG Analytics Dashboard
# Uses cleaned_smog.csv prepared by Person 1.
# Base R only; no additional packages are required.
# =============================================================================

# --------------------------- INPUT VALIDATION -------------------------------

check_numeric_vector <- function(x, name = "x", min_n = 2) {
  if (!is.numeric(x)) {
    stop(name, " must be numeric.")
  }
  x <- x[is.finite(x)]
  if (length(x) < min_n) {
    stop(name, " must contain at least ", min_n, " finite observations.")
  }
  x
}

check_alpha <- function(alpha) {
  if (length(alpha) != 1L || is.na(alpha) ||
      !is.numeric(alpha) || alpha <= 0 || alpha >= 1) {
    stop("alpha must be a single number between 0 and 1.")
  }
  invisible(TRUE)
}

make_test_result <- function(test_name, statistic = NA_real_,
                             p_value = NA_real_, df = NA_real_,
                             estimate = NA_real_, conf_int = c(NA_real_, NA_real_),
                             alpha = 0.05, details = "") {
  check_alpha(alpha)
  decision <- if (is.finite(p_value)) {
    if (p_value < alpha) "Reject the null hypothesis"
    else "Fail to reject the null hypothesis"
  } else {
    "No decision available"
  }

  structure(list(
    test = test_name,
    statistic = statistic,
    p_value = p_value,
    degrees_of_freedom = df,
    estimate = estimate,
    confidence_interval = conf_int,
    alpha = alpha,
    decision = decision,
    details = details
  ), class = "smog_test_result")
}

print.smog_test_result <- function(x, ...) {
  cat("\nTest:", x$test, "\n")
  if (is.finite(x$statistic)) cat("Test statistic:", round(x$statistic, 4), "\n")
  if (is.finite(x$degrees_of_freedom)) cat("Degrees of freedom:", round(x$degrees_of_freedom, 4), "\n")
  if (is.finite(x$p_value)) cat("P-value:", format.pval(x$p_value, digits = 4), "\n")
  if (all(is.finite(x$confidence_interval))) {
    cat("Confidence interval:", paste(round(x$confidence_interval, 4), collapse = " to "), "\n")
  }
  if (!is.na(x$estimate[1])) cat("Estimate:", paste(round(x$estimate, 4), collapse = ", "), "\n")
  cat("Decision at alpha =", x$alpha, ":", x$decision, "\n")
  if (nzchar(x$details)) cat("Interpretation:", x$details, "\n")
  invisible(x)
}

interpret_p_value <- function(p_value, alpha = 0.05) {
  check_alpha(alpha)
  if (length(p_value) != 1L || is.na(p_value)) return("P-value is unavailable.")
  if (p_value < alpha) {
    paste0("The result is statistically significant at alpha = ", alpha, ".")
  } else {
    paste0("The result is not statistically significant at alpha = ", alpha, ".")
  }
}

# ------------------------- CONFIDENCE INTERVALS -----------------------------

mean_confidence_interval <- function(x, confidence = 0.95) {
  x <- check_numeric_vector(x, "x", 2)
  if (confidence <= 0 || confidence >= 1) stop("confidence must be between 0 and 1.")
  n <- length(x)
  mean_x <- mean(x)
  margin <- qt(1 - (1 - confidence) / 2, df = n - 1) * sd(x) / sqrt(n)
  c(lower = mean_x - margin, upper = mean_x + margin)
}

proportion_confidence_interval <- function(successes, n, confidence = 0.95) {
  if (length(successes) != 1L || length(n) != 1L ||
      is.na(successes) || is.na(n) || n <= 0 ||
      successes < 0 || successes > n) {
    stop("Provide valid successes and sample size n.")
  }
  if (confidence <= 0 || confidence >= 1) stop("confidence must be between 0 and 1.")
  p <- successes / n
  z <- qnorm(1 - (1 - confidence) / 2)
  margin <- z * sqrt(p * (1 - p) / n)
  c(lower = max(0, p - margin), upper = min(1, p + margin))
}

# ------------------------------ T-TESTS -------------------------------------

one_sample_t_test <- function(x, mu = 0, alpha = 0.05) {
  check_alpha(alpha)
  x <- check_numeric_vector(x, "x", 2)
  result <- t.test(x, mu = mu, conf.level = 1 - alpha)
  make_test_result("One-sample t-test", unname(result$statistic),
                   result$p.value, unname(result$parameter),
                   unname(result$estimate), result$conf.int, alpha,
                   paste("Tests whether the population mean differs from", mu))
}

two_sample_t_test <- function(x, y, alpha = 0.05, var_equal = FALSE) {
  check_alpha(alpha)
  x <- check_numeric_vector(x, "x", 2)
  y <- check_numeric_vector(y, "y", 2)
  result <- t.test(x, y, var.equal = var_equal, conf.level = 1 - alpha)
  make_test_result("Independent two-sample t-test", unname(result$statistic),
                   result$p.value, unname(result$parameter),
                   unname(result$estimate), result$conf.int, alpha,
                   "Compares the means of two independent groups.")
}

paired_t_test <- function(x, y, alpha = 0.05) {
  check_alpha(alpha)
  if (!is.numeric(x) || !is.numeric(y) || length(x) != length(y)) {
    stop("Paired t-test needs numeric vectors of equal length with real matching pairs.")
  }
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]
  if (length(x) < 2) stop("At least two complete pairs are required.")
  result <- t.test(x, y, paired = TRUE, conf.level = 1 - alpha)
  make_test_result("Paired t-test", unname(result$statistic),
                   result$p.value, unname(result$parameter),
                   unname(result$estimate), result$conf.int, alpha,
                   "Compares paired measurements; use only when observations are genuinely matched.")
}

# ---------------------------- CHI-SQUARE ------------------------------------

chi_square_independence_test <- function(x, y, alpha = 0.05) {
  check_alpha(alpha)
  if (length(x) != length(y)) stop("x and y must have the same length.")
  keep <- !is.na(x) & !is.na(y)
  tab <- table(x[keep], y[keep])
  if (nrow(tab) < 2 || ncol(tab) < 2) stop("Chi-square test needs at least two categories in each variable.")
  result <- suppressWarnings(chisq.test(tab, correct = FALSE))
  make_test_result("Chi-square test of independence",
                   unname(result$statistic), result$p.value,
                   unname(result$parameter), details =
                     "Tests whether the two categorical variables are associated.",
                   alpha = alpha)
}

chi_square_goodness_of_fit <- function(observed, probabilities = NULL, alpha = 0.05) {
  check_alpha(alpha)
  if (!is.numeric(observed) || length(observed) < 2 ||
      any(is.na(observed)) || any(observed < 0)) {
    stop("observed must be a numeric vector of non-negative counts.")
  }
  result <- chisq.test(observed, p = probabilities, rescale.p = TRUE)
  make_test_result("Chi-square goodness-of-fit test",
                   unname(result$statistic), result$p.value,
                   unname(result$parameter), alpha = alpha,
                   details = "Tests whether observed category counts match the expected proportions.")
}

# ------------------------------ F-TEST --------------------------------------

variance_f_test <- function(x, y, alpha = 0.05) {
  check_alpha(alpha)
  x <- check_numeric_vector(x, "x", 2)
  y <- check_numeric_vector(y, "y", 2)
  result <- var.test(x, y, conf.level = 1 - alpha)
  make_test_result("F-test for equality of variances",
                   unname(result$statistic), result$p.value,
                   unname(result$parameter), result$estimate,
                   result$conf.int, alpha,
                   "Compares population variances; the test is sensitive to non-normality.")
}

# ------------------------------ ANOVA ---------------------------------------

one_way_anova <- function(response, group, alpha = 0.05) {
  check_alpha(alpha)
  if (!is.numeric(response) || length(response) != length(group)) {
    stop("response must be numeric and have the same length as group.")
  }
  keep <- is.finite(response) & !is.na(group)
  response <- response[keep]
  group <- droplevels(as.factor(group[keep]))
  if (nlevels(group) < 2) stop("ANOVA requires at least two groups.")
  if (any(table(group) < 2)) stop("Each ANOVA group must contain at least two observations.")
  fit <- aov(response ~ group)
  tab <- summary(fit)[[1]]
  f_stat <- tab[1, "F value"]
  p_val <- tab[1, "Pr(>F)"]
  make_test_result("One-way ANOVA", f_stat, p_val, tab[1, "Df"],
                   details = "Tests whether at least one group mean differs from the others.",
                   alpha = alpha)
}

# ------------------------------- Z-TESTS ------------------------------------
# Use Z-tests only when the population standard deviation is known (or when
# the course's stated large-sample approximation is appropriate).

one_sample_z_test <- function(x, mu, sigma, alpha = 0.05) {
  check_alpha(alpha)
  x <- check_numeric_vector(x, "x", 1)
  if (length(sigma) != 1 || !is.finite(sigma) || sigma <= 0) stop("sigma must be a known positive population SD.")
  z <- (mean(x) - mu) / (sigma / sqrt(length(x)))
  p <- 2 * pnorm(-abs(z))
  make_test_result("One-sample Z-test", z, p, estimate = mean(x), alpha = alpha,
                   details = "Two-sided test; requires a known population standard deviation.")
}

two_sample_z_test <- function(x, y, sigma_x, sigma_y, alpha = 0.05) {
  check_alpha(alpha)
  x <- check_numeric_vector(x, "x", 1)
  y <- check_numeric_vector(y, "y", 1)
  if (any(!is.finite(c(sigma_x, sigma_y))) || any(c(sigma_x, sigma_y) <= 0)) {
    stop("Known population standard deviations must be positive.")
  }
  se <- sqrt(sigma_x^2 / length(x) + sigma_y^2 / length(y))
  z <- (mean(x) - mean(y)) / se
  p <- 2 * pnorm(-abs(z))
  make_test_result("Two-sample Z-test", z, p,
                   estimate = c(mean_x = mean(x), mean_y = mean(y)),
                   alpha = alpha,
                   details = "Two-sided test; requires known population standard deviations.")
}

two_proportion_z_test <- function(success_x, n_x, success_y, n_y, alpha = 0.05) {
  check_alpha(alpha)
  vals <- c(success_x, n_x, success_y, n_y)
  if (any(!is.finite(vals)) || n_x <= 0 || n_y <= 0 ||
      success_x < 0 || success_x > n_x || success_y < 0 || success_y > n_y) {
    stop("Provide valid successes and positive sample sizes.")
  }
  p1 <- success_x / n_x
  p2 <- success_y / n_y
  pooled <- (success_x + success_y) / (n_x + n_y)
  se <- sqrt(pooled * (1 - pooled) * (1 / n_x + 1 / n_y))
  if (se == 0) stop("The pooled standard error is zero; Z-test cannot be calculated.")
  z <- (p1 - p2) / se
  make_test_result("Two-proportion Z-test", z, 2 * pnorm(-abs(z)),
                   estimate = c(p1 = p1, p2 = p2), alpha = alpha,
                   details = "Compares two independent proportions.")
}
