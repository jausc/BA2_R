# Suchverfahren, um das ARIMA-Modell mit den besten Parametern zu finden
fun_arima_params <- function(x, a, b, c, type = c("AIC", "AICc", "BIC",
    "HQIC")) {
    final_criterion <- Inf
    final_order <- c(0, 0, 0)
    for (i in 0:a) for (j in 0:b) for (k in 0:c) {
        if (type == "AIC") {
            current_criterion <- AIC(arima(x, order = c(i, j, k)))
            if (current_criterion < final_criterion) {
                final_criterion <- current_criterion
                final_order <- c(i, j, k)
            }
        }
        else if (type == "AICc") {
            current_criterion <- AICc(arima(x, order = c(i, j, k)))
            if (current_criterion < final_criterion) {
                final_criterion <- current_criterion
                final_order <- c(i, j, k)
            }
        }
        else if (type == "BIC") {
            current_criterion <- BIC(arima(x, order = c(i, j, k)))
            if (current_criterion < final_criterion) {
                final_criterion <- current_criterion
                final_order <- c(i, j, k)
            }
        }
        else if (type == "HQIC") {
            current_criterion <- HQIC(arima(x, order = c(i, j, k)))
            if (current_criterion < final_criterion) {
                final_criterion <- current_criterion
                final_order <- c(i, j, k)
            }
        }
        else {
           stop()
        }
    }
    cat("Criterion: ", final_criterion, "\n")
    cat("Order: ", final_order, "\n")
}