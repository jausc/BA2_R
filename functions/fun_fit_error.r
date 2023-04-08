# Funktion, um MSE, RMSE und NRMSE für Modelle schnell durchzuführen
fun_fit_error <- function(x, date) {
    v_sma <- atx_xts_cont_ret_vola30_sma[date]
    v_lwma <- atx_xts_cont_ret_vola30_lwma[date]
    v_hma <- atx_xts_cont_ret_vola30_hma[date]
    v_ewma <- atx_xts_cont_ret_vola_ewma[date]
    v_arima <- atx_xts_cont_ret_arima_res[date]
    v_garch <- atx_xts_cont_ret_garch_sig[date]
    temp1 <- x[date]
    temp2 <- matrix(nrow = 6, ncol = 3)
    temp3 <- mean(temp1)
    temp4 <- matrix(nrow = 6, ncol = 3)

    # MSE
    temp2[1, 1] <- mse(temp1, v_sma)
    temp2[2, 1] <- mse(temp1, v_lwma)
    temp2[3, 1] <- mse(temp1, v_hma)
    temp2[4, 1] <- mse(temp1, v_ewma)
    temp2[5, 1] <- mse(temp1, v_arima)
    temp2[6, 1] <- mse(temp1, v_garch)

    # RMSE
    temp2[1, 2] <- rmse(temp1, v_sma)
    temp2[2, 2] <- rmse(temp1, v_lwma)
    temp2[3, 2] <- rmse(temp1, v_hma)
    temp2[4, 2] <- rmse(temp1, v_ewma)
    temp2[5, 2] <- rmse(temp1, v_arima)
    temp2[6, 2] <- rmse(temp1, v_garch)

    # NRMSE
    temp2[1, 3] <- rmse(temp1, v_sma) / temp3
    temp2[2, 3] <- rmse(temp1, v_lwma) / temp3
    temp2[3, 3] <- rmse(temp1, v_hma) / temp3
    temp2[4, 3] <- rmse(temp1, v_ewma) / temp3
    temp2[5, 3] <- rmse(temp1, v_arima) / temp3
    temp2[6, 3] <- rmse(temp1, v_garch) / temp3

    temp4[, 1] <- rank(temp2[, 1])
    temp4[, 2] <- rank(temp2[, 2])
    temp4[, 3] <- rank(temp2[, 3])

    temp5 <- list("values" = temp2, "rank" = temp4)
    return(temp5)
}
