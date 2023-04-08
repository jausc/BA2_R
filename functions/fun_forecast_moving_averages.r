# Funktion, um Prognosen für SMA, LWMA, HMA, EWMA, ARIMA, GARCH zu berechnen
fun_forecast_moving_averages <- function(df, h, n, weights = NULL) {
    # Setting the average weights
    if (is.null(weights)) {
        weights <- rep(1 / n, n)
    }

    # Setting the training and testing partition according to the forecast
    # horizon
    df$type <- c(rep("train", nrow(df) - h), rep("test", h))

    # Spreading the table by the partition type
    df1 <- df %>% tidyr::spread(key = type, value = data)

    # Create the target variable
    df1$yhat <- df1$train

    # Simple moving average function
    for (i in (nrow(df1) - h + 1):nrow(df1)) {
        r <- (i - n):(i - 1)
        df1$yhat[i] <- sum(df1$yhat[r] * weights)
    }

    # Dropping from the yhat variable the actual values that were used for the
    # rolling window
    df1$yhat <- ifelse(is.na(df1$test), NA, df1$yhat)
    df1$data <- ifelse(is.na(df1$test), df1$train, df1$test)

    return(df1)
}
