# MASE der Prognosen bestimmen
fun_forecast_mase <- function(h) {
    mase_naiv <- mase(forecast_df_vola$data[(length(forecast_df_vola$data) -
        h + 1):length(forecast_df_vola$data)],
        forecast_naive[(length(forecast_naive) - h + 1):length(forecast_naive)])
    mase_sma <- mase(forecast_df_vola$data[(length(forecast_df_vola$data) - h +
        1):length(forecast_df_vola$data)], na.omit(forecast_sma$yhat))
    mase_lwma <- mase(forecast_df_vola$data[(length(forecast_df_vola$data) -
        h + 1):length(forecast_df_vola$data)], na.omit(forecast_lwma$yhat))
    mase_hma <- mase(forecast_df_vola$data[(length(forecast_df_vola$data) - h +
        1):length(forecast_df_vola$data)], na.omit(forecast_hma$yhat))
    mase_ewma <- mase(forecast_df_vola$data[(length(forecast_df_vola$data) -
        h + 1):length(forecast_df_vola$data)], forecast_ewma$mean)
    mase_arima <- mase(forecast_df_vola$data[(length(forecast_df_vola$data) -
        h + 1):length(forecast_df_vola$data)], coredata(forecast_arima_xts))
    mase_garch <- mase(forecast_df_vola$data[(length(forecast_df_vola$data) -
        h + 1):length(forecast_df_vola$data)], coredata(forecast_garch_xts))

    temp1 <- matrix(nrow = 7, ncol = 1)
    temp1[1, 1] <- mase_naiv
    temp1[2, 1] <- mase_sma
    temp1[3, 1] <- mase_lwma
    temp1[4, 1] <- mase_hma
    temp1[5, 1] <- mase_ewma
    temp1[6, 1] <- mase_arima
    temp1[7, 1] <- mase_garch

    return(temp1)
}
