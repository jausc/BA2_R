################################################################################
# Weighted.LM.test - The Weighted Li-Mak type test
################################################################################
#
# Von https://github.com/cran/WeightedPortTest
#
# x     - The residuals or initial unfitted time series
# h.t   - The sample conditional variances
# lag   - The lag we wish to test at, this is m or M in literature, default = 1
# type  - The type of test, here just based on acfs or partial-acfs
# fitdf - The number of ARCH parameters that have been fit to the series x
#
# For backwards compatibility
#
# weighted - For backwards compatibility; if set to FALSE, you will perform the
# Li-Mak test
################################################################################
fun_weighted_lm_test_corr <- function(x, h.t, lag = 1, type = c("correlation",
    "partial"), fitdf = 1, weighted = TRUE) {
    # Error Checking
    if (NCOL(x) > 1)
        stop("x is not a vector or univariate time series")
    if (fitdf >= lag)
        stop("Lag must exceed fitted degrees of freedom")
    if (fitdf < 1)
        stop("Fitted degrees of freedom must be positive")
    if (!(length(x) == length(h.t)))
        stop("Length of x and h.t must match")

    DNAME <- deparse(substitute(x))
    type <- match.arg(type)

    x <- x ^ 2 / h.t

    if (type == "partial") {
        cor <- acf(x, lag.max = lag, type = "partial", plot = FALSE,
            na.action = na.pass)
        obs <- cor$acf[1:lag]
    }
    else {
        cor <- acf(x, lag.max = (lag + 1), type = "correlation", plot = FALSE,
            na.action = na.pass) # lag.max = lag zu lag.max = (lag + 1)
        obs <- cor$acf[2:(lag + 1)]
    }

    if (type == "correlation" && weighted) {
        METHOD <- "Weighted Li-Mak test on autocorrelations (Gamma Approximation)"
    }
    else if (type == "partial" && weighted) {
        METHOD <- "Weighted Li-Mak test on partial autocorrelations (Gamma Approximation)"
    }
    else if (type == "correlation" && !weighted) {
        METHOD <- "Li-Mak test on autocorrelations (Chi-Squared Approximation)"
    }
    else {
        METHOD <- "Li-Mak test on partial autocorrelations (Chi-Squared Approximation)"
    }

    n <- sum(!is.na(x))
    if (weighted) {
        weights <- (lag - (fitdf + 1):lag + (fitdf + 1)) / lag
        obs <- obs[(fitdf + 1):lag]
        STATISTIC <- n * sum(weights * obs ^ 2)
        names(STATISTIC) <- "Weighted X-squared on Squared Residuals for fitted ARCH process"
        shape <- (3 / 4) * (lag + fitdf + 1) ^ 2 * (lag - fitdf) / (2 * lag ^ 2
            + 3 * lag + 2 * lag * fitdf + 2 * fitdf ^ 2 + 3 * fitdf + 1)
        scale <- (2 / 3) * (2 * lag ^ 2 + 3 * lag + 2 * lag * fitdf + 2
            * fitdf ^ 2 + 3 * fitdf + 1) / (lag * (lag + fitdf + 1))
        PARAMETER <- c(shape, scale)
        names(PARAMETER) <- c("Shape", "Scale")
    }
    else {
        weights <- rep(1, (lag - fitdf))
        obs <- obs[(fitdf + 1):lag]
        STATISTIC <- n * sum(weights * obs ^ 2)
        names(STATISTIC) <- "X-squared on Squared Residuals for fitted ARCH process"
        shape <- (lag - fitdf) / 2 # Chi-squared df in Gamma form
        scale <- 2
        PARAMETER <- c((lag - fitdf))
        names(PARAMETER) <- c("Degrees of Freedom")
    }

    PVAL <- 1 - pgamma(STATISTIC, shape = shape, scale = scale)
    names(PVAL) <- "Approximate p-value"

    structure(list(statistic = STATISTIC, parameter = PARAMETER,
        p.value = PVAL, method = METHOD, data.name = DNAME),  class = "htest")
}
