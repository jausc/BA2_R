# Iteratives Suchverfahren zur Bestimmung von Lambda für EWMA
# lambda_estimator: Schätzwert für Lambda; beginnend bei 0; $\in[0;1]$
# precision: welche Kommastelle berechnet werden soll; Input als 10^(-x); x aus
# natürliche Zahlen
fun_lambda_param <- function(lambda_estimator, precision) {
    x1 <- matrix(nrow = nrow(atx_xts_cont_ret), ncol = 9)
    for (i in 3:nrow(x1)) {
        for (j in 1:ncol(x1)) {
            x1[i, j] <- (lambda_estimator + j / (precision ^ (-1))) *
                abs(coredata(atx_xts_cont_ret[i])) +
                (1 - (lambda_estimator + j / (precision ^ (-1)))) *
                abs(coredata(atx_xts_cont_ret[i - 1]))
        }
    }

    x2 <- cbind(x1, coredata(atx_xts_cont_ret))
    x3 <- matrix(nrow = nrow(x2), ncol = 9)
    for (i in 3:nrow(x2)) {
        for (j in 1:(ncol(x2) - 1)) {
            x3[i - 2, j] <- (x2[i, 10] - x2[i, j]) ^ 2
        }
    }

    x4 <- matrix(nrow = 1, ncol = 9)
    for (i in 1:ncol(x3)) {
        x4[1, i] <- sum(x3[, i], na.rm = TRUE) / length(na.omit(x3[, 1]))
    }

    print(x4)
    print(which(x4 == min(x4)) / (precision ^ (-1)))
}
