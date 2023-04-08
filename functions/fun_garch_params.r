# Suchverfahren, um das GARCH-Modell mit den besten Parametern zu finden
fun_garch_params <- function(x, k, l, p, d, q, type = c("AIC", "BIC", "SIC",
    "HQIC")) {
    if (d != 0) z <- 1 else z <- 0

    temp1 <- matrix(nrow = k * (l + 1), ncol = 3)
    temp2 <- 0
    y <- 0

    if (type == "AIC") y <- 1
    else if (type == "BIC") y <- 2
    else if (type == "SIC") y <- 3
    else if (type == "HQIC") y <- 4

    for (i in 1:k) {
        for (j in z:l) {
            temp3 <- ugarchspec(variance.model = list(garchOrder = c(i, j)),
                mean.model = list(armaOrder = c(p, q)))
            temp4 <- ugarchfit(spec = temp3, data = x)
            temp1[(j + temp2 + 1), 1] <- i
            temp1[(j + temp2 + 1), 2] <- j
            temp1[(j + temp2 + 1), 3] <- infocriteria(temp4)[y]

        }
        temp2 <- temp2 + l + 1
    }
    temp1 <- as.data.frame(temp1)

    names(temp1)[names(temp1) == "V1"] <- "k"
    names(temp1)[names(temp1) == "V2"] <- "l"
    if (y == 1) names(temp1)[names(temp1) == "V3"] <- "AIC"
    else if (y == 2) names(temp1)[names(temp1) == "V3"] <- "BIC"
    else if (y == 3) names(temp1)[names(temp1) == "V3"] <- "SIC"
    else if (y == 4) names(temp1)[names(temp1) == "V3"] <- "HQIC"

    print(na.omit(temp1))
}
