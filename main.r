################################################################################
# Pakete
################################################################################

# install.packages("FinTS")
# install.packages("forecast")
# install.packages("ICglm")
# install.packages("MASS")
# install.packages("Metrics")
# install.packages("MuMIn")
# install.packages("pwr")
# install.packages("rugarch")
# install.packages("smooth")
# install.packages("tidyr")
# install.packages("tseries")
# install.packages("TTR")
# install.packages("vars")
# install.packages("xtable")
# install.packages("xts")
# install.packages("zoo")

# install.packages("ggplot2")
# install.packages("ggforce")
# install.packages("ggpubr")
# install.packages("ggthemes")

# install.packages("here")
# install.packages("latex2exp")
# install.packages("scales")

library(FinTS)
library(forecast)
library(ICglm)
library(MASS)
library(Metrics)
library(MuMIn)
library(pwr)
library(rugarch)
library(smooth)
library(tidyr)
library(tseries)
library(TTR)
library(vars)
library(xtable)
library(xts)
library(zoo)

library(ggplot2)
library(ggforce)
library(ggpubr)
library(ggthemes)

library(here)
library(latex2exp)
library(scales)

################################################################################
# Funktionen
################################################################################

source(file = here("functions", "fun_arima_params.r"))
source(file = here("functions", "fun_fit_error.r"))
source(file = here("functions", "fun_forecast_mase.r"))
source(file = here("functions", "fun_forecast_moving_averages.r"))
source(file = here("functions", "fun_garch_params.r"))
source(file = here("functions", "fun_lambda_param.r"))
source(file = here("functions", "fun_theme_ba2.r"))
source(file = here("functions", "fun_weighted_lm_test_corr.r"))

# Laden erst möglich, wenn bis MASE kompiliert wurde
source(file = here("functions", "fun_graphs_atx.r"))
source(file = here("functions", "fun_graphs_anhang.r"))

################################################################################
# Sonstiges
################################################################################

# Chi-Quadrat-Tabelle
chi2 <- data.frame(
    "0.99" = qchisq(1 - 0.99, seq(1, 30)),
    "0.975" = qchisq(1 - 0.975, seq(1, 30)),
    "0.95" = qchisq(1 - 0.95, seq(1, 30)),
    "0.05" = qchisq(1 - 0.05, seq(1, 30)),
    "0.025" = qchisq(1 - 0.025, seq(1, 30)),
    "0.005" = qchisq(1 - 0.005, seq(1, 30))
)

# Exportieren
print(xtable(, type = "latex", digits = 6), file = "export.tex")

# Komma statt Punkt als Separator
options(OutDec = ",")

################################################################################
# MSCI World
################################################################################

msci_df_ges <- read.csv2(here("data", "processed", "historyIndex_MSCI.csv"),
    encoding = "UTF-8", header = TRUE, stringsAsFactors = FALSE)

names(msci_df_ges)[names(msci_df_ges) == "X.U.FEFF.Datum"] <- "Datum"
msci_df_ges$Datum <- as.Date(msci_df_ges$Datum, format = "%d.%m.%Y")
msci_xts_ges <- xts(msci_df_ges$Schlusspreis, msci_df_ges$Datum)

date_vector_msci <- c(seq(min(index(msci_xts_ges)), max(index(msci_xts_ges)),
    "3 months"), as.Date("2021-01-01"))

# Verlauf der täglichen Schlusspreise des MSCI World
ggplot() +
    geom_line(data = msci_xts_ges, aes(x = index(msci_xts_ges),
        y = coredata(msci_xts_ges), color = "MSCI World")) +
    scale_color_manual(values = c("black")) +
    scale_x_date(name = "Zeit", breaks = date_vector_msci,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Schlusspreis", breaks = seq(1500, 2700, 200)) +
    expand_limits(x = as.Date("2019-01-01"), y = 1500) +
    expand_limits(x = c(as.Date("2019-01-01"), as.Date("2021-01-01")),
        y = c(1500, 2700)) +
    theme_ba2(legend_pos = "none")

################################################################################
# ATX
################################################################################

################################################################################
# Daten einlesen und bearbeiten
################################################################################

# .CSV einlesen
atx_df_ges <- read.csv2(here("data", "processed", "historicalData_ATX.csv"),
    encoding = "UTF-8", header = TRUE)

# Spalten bearbeiten
names(atx_df_ges)[names(atx_df_ges) == "X.U.FEFF.Datum"] <- "Datum"
atx_df_ges$Eröffnungspreis <- NULL
atx_df_ges$Tageshoch <- NULL
atx_df_ges$Tagestief <- NULL
atx_df_ges$Diff.. <- NULL
atx_df_ges$X <- NULL # "Leere" Spalte

# Datumsformat ändern
atx_df_ges$Datum <- as.Date(atx_df_ges$Datum, format = "%d.%m.%Y")

# atx_df von Dataframe zu xts konvertieren
atx_xts_ges <- xts(atx_df_ges$Schlusspreis, atx_df_ges$Datum)

# Auf Zeitraum einschränken
atx_xts <- atx_xts_ges["2019-07-01/2020-06-30"]

################################################################################
# Rendite
################################################################################

# Diskrete Rendite
atx_xts_disc_ret <- (coredata(atx_xts[2:length(atx_xts)]) -
    coredata(atx_xts[1:(length(atx_xts) - 1)])) /
    coredata(atx_xts[1:(length(atx_xts) - 1)])
atx_xts_disc_ret <- xts(x = coredata(atx_xts_disc_ret),
    order.by = index(atx_xts[-1]))

# Stetige Rendite
atx_xts_cont_ret <- diff(log(atx_xts), lag = 1) # log entspricht ln

# Stetige Rendite auf gesamten Zeitraum
atx_xts_ges_cont_ret <- diff(log(atx_xts_ges), lag = 1)

# Anzahl Reihen in atx_xts_cont_ret
atx_xts_cont_ret_rows <- nrow(atx_xts_cont_ret[-1])

# Anzahl Ausschläge
nrow(atx_xts_cont_ret[coredata(atx_xts_cont_ret[-1]) > 0])
nrow(atx_xts_cont_ret[coredata(atx_xts_cont_ret[-1]) < 0])
nrow(atx_xts_cont_ret[coredata(atx_xts_cont_ret[-1]) == 0])

# Jährliche bzw. annualisierte Renditen
atx_xts_cont_ret_arith <- sum(coredata(atx_xts_cont_ret[-1])) /
    atx_xts_cont_ret_rows
atx_xts_cont_ret_geom <- prod(1 + coredata(atx_xts_cont_ret[-1])) ^ (1 /
    atx_xts_cont_ret_rows) - 1
# Sheppard'sche Korrektur
atx_xts_cont_ret_geom + 1 / 2 * atx_xts_cont_ret_sd ^ 2

# Mittelwert
atx_xts_cont_ret_mean <- sum(coredata(atx_xts_cont_ret[-1])) /
    atx_xts_cont_ret_rows

# Median
atx_xts_cont_ret_median <- median(atx_xts_cont_ret[-1])

# MAD (mean absolute deviation)
atx_xts_cont_ret_mad <- sum(abs(coredata(atx_xts_cont_ret[-1]) -
    atx_xts_cont_ret_mean)) / atx_xts_cont_ret_rows

# Varianz
atx_xts_cont_ret_var <- sum((coredata(atx_xts_cont_ret[-1]) -
    atx_xts_cont_ret_mean) ^ 2) / atx_xts_cont_ret_rows

# Standardabweichung
atx_xts_cont_ret_sd <- sqrt(sum((coredata(atx_xts_cont_ret[-1]) -
    atx_xts_cont_ret_mean) ^ 2) / atx_xts_cont_ret_rows)

# Schiefe
atx_xts_cont_ret_skewness <- (sum((coredata(atx_xts_cont_ret[-1]) -
    atx_xts_cont_ret_mean) ^ 3) / atx_xts_cont_ret_rows) /
    (atx_xts_cont_ret_sd ^ 3)

# Kurtosis (Pearson)
atx_xts_cont_ret_p_kurt <- sum(((coredata(atx_xts_cont_ret[-1]) -
    atx_xts_cont_ret_mean) / atx_xts_cont_ret_sd) ^ 4) / atx_xts_cont_ret_rows

# Exzess-Kurtosis (Fisher)
atx_xts_cont_ret_f_kurt <- sum(((coredata(atx_xts_cont_ret[-1]) -
    atx_xts_cont_ret_mean) / atx_xts_cont_ret_sd) ^ 4) /
    atx_xts_cont_ret_rows - 3

# Prüfung auf Stationarität
# Klassische Box-Cox-Transformation
atx_xts_cont_ret_boxcox_lambda <- boxcox(abs(coredata(atx_xts_cont_ret[-1])) ~
    index(atx_xts_cont_ret[-1]), lambda = seq(-3, 3, 0.0001))
atx_xts_cont_ret_boxcox_lambda <- as.numeric(atx_xts_cont_ret_boxcox_lambda$x[which.max(atx_xts_cont_ret_boxcox_lambda$y)])
atx_xts_cont_ret_boxcox <- (abs(coredata(atx_xts_cont_ret[-1])) ^
    atx_xts_cont_ret_boxcox_lambda - 1) / atx_xts_cont_ret_boxcox_lambda
atx_xts_cont_ret_boxcox <- xts(x = atx_xts_cont_ret_boxcox,
    order.by = index(atx_xts_cont_ret[-1]))

# Einfach differenzieren
atx_xts_cont_ret_boxcox_diff <- diff(atx_xts_cont_ret_boxcox)

# VAR(p)-Modell
atx_xts_cont_ret_boxcox_var <- VARselect(atx_xts_cont_ret_boxcox,
    type = "trend", lag.max = 20) # 9 Lags

temp1 <- data.frame(V1 = atx_xts_cont_ret_boxcox_var$criteria[1, ],
    V2 = atx_xts_cont_ret_boxcox_var$criteria[3, ],
    V3 = atx_xts_cont_ret_boxcox_var$criteria[2, ],
    V4 = atx_xts_cont_ret_boxcox_var$criteria[4, ])

atx_xts_cont_ret_boxcox_diff_var <- VARselect(atx_xts_cont_ret_boxcox_diff[-1],
    type = "trend", lag.max = 20) # 8 Lags

temp2 <- data.frame(V1 = atx_xts_cont_ret_boxcox_diff_var$criteria[1, ],
    V2 = atx_xts_cont_ret_boxcox_diff_var$criteria[3, ],
    V3 = atx_xts_cont_ret_boxcox_diff_var$criteria[2, ],
    V4 = atx_xts_cont_ret_boxcox_diff_var$criteria[4, ])

# ADF-Test
adf.test(atx_xts_cont_ret_boxcox, k = 9)
adf.test(atx_xts_cont_ret_boxcox_diff[-1], k = 8)

# ADF-GLS-Test
atx_xts_cont_ret_boxcox_diff_adf_gls <- ur.ers(atx_xts_cont_ret_boxcox_diff[-1],
    type = "DF-GLS", model = "trend", lag.max = 8)

# Schwert-Kriterium
ceiling(12 * (length(atx_xts_cont_ret_boxcox) / 100) ^ (1 / 4)) # 16 Lags
ceiling(12 * (length(atx_xts_cont_ret_boxcox_diff[-1]) / 100) ^ (1 / 4))
# 16 Lags

adf.test(atx_xts_cont_ret_boxcox, k = 16)
adf.test(atx_xts_cont_ret_boxcox_diff[-1], k = 16)

atx_xts_cont_ret_boxcox_diff_adf_gls <- ur.ers(atx_xts_cont_ret_boxcox_diff[-1],
    type = "DF-GLS", model = "trend", lag.max = 16)

# Weitere Einheitswurzeltests
kpss.test(atx_xts_cont_ret_boxcox_diff[-1])
pp.test(atx_xts_cont_ret_boxcox_diff[-1])

# Trend bestimmen und gegenrechnen (nach VAR(p)-Modell)
atx_xts_cont_ret_boxcox_diff_trend <- atx_xts_cont_ret_boxcox_diff["2019-07-16/2020-06-30"] - atx_xts_cont_ret_boxcox_diff_adf_gls@testreg$residuals

atx_xts_cont_ret_boxcox_diff_adf_gls@testreg$residuals + atx_xts_cont_ret_boxcox_diff_trend - atx_xts_cont_ret_boxcox_diff["2019-07-16/2020-06-30"] # = 0

# Differenzierte, Box-Cox-transformierte Renditenzeitreihe ohne Trend
temp1 <- coredata(atx_xts_cont_ret_boxcox_diff_adf_gls@testreg$residuals)
temp2 <- index(atx_xts_cont_ret_boxcox_diff["2019-07-16/2020-06-30"])
atx_xts_cont_ret_boxcox_diff_detrend <- xts(x = temp1, order.by = temp2)

# Stärke des Trends F_{t}
max(0, (1 - (var(atx_xts_cont_ret_boxcox_diff_detrend) /
    (var(atx_xts_cont_ret_boxcox_diff["2019-07-16/2020-06-30"])))))

# Test auf Gauß'sches weißes Rauschen
jarque.bera.test(atx_xts_cont_ret_boxcox_diff_detrend)

################################################################################
# Volatilität
################################################################################

# Realisierte Volatilität
atx_xts_cont_ret_vola <- atx_xts_cont_ret[-1] ^ 2

# Realisierte Volatilität auf gesamten Zeitraum
atx_xts_ges_cont_ret_vola <- atx_xts_ges_cont_ret ^ 2

# Volatilität SMA30
atx_xts_cont_ret_vola30_sma <- SMA(atx_xts_cont_ret[-1] ^ 2, n = 30)

# Volatilität LWMA30
atx_xts_cont_ret_vola30_lwma <- WMA(atx_xts_cont_ret[-1] ^ 2, n = 30,
    wts = 1:30)

# Volatilität HMA30
atx_xts_cont_ret_vola30_hma_log <- WMA((2 * WMA(log(atx_xts_cont_ret[-1] ^ 2),
    n = 15, wts = 1:15)) - (WMA(log(atx_xts_cont_ret[-1] ^ 2), n = 30,
    wts = 1:30)), n = round(sqrt(30), 0), wts = 1:round(sqrt(30), 0))
atx_xts_cont_ret_vola30_hma <- exp(atx_xts_cont_ret_vola30_hma_log)

# Volatilität EWMA
fun_lambda_param(0, 0.1) # Lambda = 0.2
fun_lambda_param(0.2, 0.01) # Lambda = 0.25
fun_lambda_param(0.25, 0.001) # Lambda = 0.251
fun_lambda_param(0.251, 0.0001) # Lambda = 0.2511

atx_xts_cont_ret_vola_ewma <- EMA(atx_xts_cont_ret[-1] ^ 2,
    ratio = 0.2511, wilder = FALSE)

# Volatilität ARIMA
# Kandidatenmodelle über Informationskriterien bestimmen
fun_arima_params(atx_xts_cont_ret_boxcox_diff[-1], 5, 0, 5, type = "AIC")
fun_arima_params(atx_xts_cont_ret_boxcox_diff[-1], 5, 0, 5, type = "AICc")
fun_arima_params(atx_xts_cont_ret_boxcox_diff[-1], 5, 0, 5, type = "BIC")
fun_arima_params(atx_xts_cont_ret_boxcox_diff[-1], 5, 0, 5, type = "HQIC")

auto.arima(atx_xts_cont_ret_boxcox_diff[-1], stepwise = TRUE, trace = TRUE)
# 0, 1, 3

arima_model_1 <- arima(atx_xts_cont_ret_boxcox_diff[-1], order = c(0, 0, 1))
arima_model_2 <- arima(atx_xts_cont_ret_boxcox_diff[-1], order = c(0, 0, 3))

# Inverse charakteristische Wurzeln
arima_model_1_uc_point <- 1 / (1 / abs(arima_model_1$model$theta))

arima_model_2_points <- polyroot(z = c(1, arima_model_2$model$theta[1],
    arima_model_2$model$theta[2], arima_model_2$model$theta[3]))

# v_{1}^{-1}
arima_model_2_uc_point_1 <- 1 / arima_model_2_points[1]

# Re(v_{2}^{-1})
arima_model_2_uc_point_2_x <- (Re(arima_model_2_points[2]) /
    (Re(arima_model_2_points[2]) ^ 2 + Im(arima_model_2_points[2]) ^ 2))
# Im(v_{2}^{-1})
arima_model_2_uc_point_2_y <- (Im(arima_model_2_points[2]) /
    (Re(arima_model_2_points[2]) ^ 2 + Im(arima_model_2_points[2]) ^ 2))

arima_model_2_uc_point_2 <- complex(real = arima_model_2_uc_point_2_x,
    imaginary = arima_model_2_uc_point_2_y)

# Re(v_{3}^{-1})
arima_model_2_uc_point_3_x <- (Re(arima_model_2_points[3]) /
    (Re(arima_model_2_points[3]) ^ 2 + Im(arima_model_2_points[3]) ^ 2))
# Im(v_{3}^{-1})
arima_model_2_uc_point_3_y <- (Im(arima_model_2_points[3]) /
    (Re(arima_model_2_points[3]) ^ 2 + Im(arima_model_2_points[3]) ^ 2))

arima_model_2_uc_point_3 <- complex(real = arima_model_2_uc_point_3_x,
    imaginary = arima_model_2_uc_point_3_y)

# ARIMA(0, 1, 3)-Modell mit vollen Trainingsdaten atx_xts_cont_ret[-1] ^ 2
# erzeugen
atx_xts_cont_ret_arima <- Arima(atx_xts_cont_ret[-1] ^ 2, order = c(0, 1, 3),
    lambda = "auto")

# Box-Cox-Transformation invertieren und skalieren
atx_xts_cont_ret_arima_res_boxcox <-
    InvBoxCox(atx_xts_cont_ret_arima$residuals, atx_xts_cont_ret_arima$lambda)
temp1 <- max(atx_xts_cont_ret_arima_res_boxcox) / max(atx_xts_cont_ret[-1] ^ 2)
atx_xts_cont_ret_arima_res <- atx_xts_cont_ret_arima_res_boxcox / temp1

atx_xts_cont_ret_arima_res <- as.xts(coredata(atx_xts_cont_ret_arima_res),
    order.by = index(atx_xts_cont_ret[-1]))

# Volatilität GARCH
# Prüfung auf Heteroskedastizität in Renditenzeitreihe
jarque.bera.test(atx_xts_cont_ret[-1])
Box.test(atx_xts_cont_ret[-1], lag = 10, type = "Ljung-Box")
Box.test(atx_xts_cont_ret[-1] ^ 2, lag = 10, type = "Ljung-Box") # McLeod-Li
adf.test(atx_xts_cont_ret[-1], alternative = "stationary")
ArchTest(atx_xts_cont_ret[-1], lags = 10)

# Kandidatenmodelle über Informationskriterien bestimmen
temp1 <- fun_garch_params(atx_xts_cont_ret[-1], 5, 5, 0, 0, 0, type = "AIC")
temp2 <- fun_garch_params(atx_xts_cont_ret[-1], 5, 5, 0, 0, 0, type = "BIC")
temp3 <- fun_garch_params(atx_xts_cont_ret[-1], 5, 5, 0, 0, 0, type = "HQIC")
temp4 <- fun_garch_params(atx_xts_cont_ret[-1], 5, 5, 0, 0, 0, type = "SIC")

temp1[which.min(temp1$AIC), ]
temp2[which.min(temp2$BIC), ]
temp3[which.min(temp4$HQIC), ]
temp4[which.min(temp3$SIC), ]

# GARCH(1, 1)-Modell mit vollen Trainingsdaten atx_xts_cont_ret[-1] ^ 2 erzeugen
atx_xts_cont_ret_garch_spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0)))
atx_xts_cont_ret_garch_fit <- ugarchfit(spec = atx_xts_cont_ret_garch_spec,
    data = atx_xts_cont_ret[-1] ^ 2)
atx_xts_cont_ret_garch_sig <- as.xts(atx_xts_cont_ret_garch_fit@fit$sigma,
    order.by = index(atx_xts_cont_ret[-1]))

# Ergebnisse
plot(atx_xts_cont_ret_garch_fit, which = "all")

# Wichtige Kennzahlen
coef(atx_xts_cont_ret_garch_fit)
uncvariance(atx_xts_cont_ret_garch_fit)
sqrt(uncvariance(atx_xts_cont_ret_garch_fit))
fitted(atx_xts_cont_ret_garch_fit)
sigma(atx_xts_cont_ret_garch_fit)

# Li-Mak-Test/gewichteter Portmanteau-Test
# Autokorrelationen
temp1 <- 25
df <- 2
temp2 <- matrix(nrow = temp1, ncol = 3)
for (i in (1 + df):temp1) {
    temp3 <- fun_weighted_lm_test_corr(
        x = atx_xts_cont_ret_garch_fit@fit$residuals,
        h.t = (atx_xts_cont_ret_garch_fit@fit$sigma ^ 2), lag = i,
        type = "correlation", fitdf = df, weighted = TRUE)
    temp2[i, 1] <- sum(((i - (df + 1):i + (df + 1)) / i) * qchisq(1 - 0.05, 1))
    temp2[i, 2] <- temp3$statistic
    temp2[i, 3] <- temp3$p.value
}
temp2

# Partielle Autokorrelationen
temp2 <- matrix(nrow = temp1, ncol = 3)
for (i in (1 + df):temp1) {
    temp3 <- fun_weighted_lm_test_corr(
        x = atx_xts_cont_ret_garch_fit@fit$residuals,
        h.t = (atx_xts_cont_ret_garch_fit@fit$sigma ^ 2), lag = i,
        type = "partial", fitdf = df, weighted = TRUE)
    temp2[i, 1] <- sum(((i - (df + 1):i + (df + 1)) / i) * qchisq(1 - 0.05, 1))
    temp2[i, 2] <- temp3$statistic
    temp2[i, 3] <- temp3$p.value
}
temp2

################################################################################
# Fit
################################################################################

# H1 = Stabiler Zeitraum (16.08.2019 bis 30.12.2019)
# H2 = Stark wechselnder Zeitraum (02.01.2020 bis 30.06.2020)
# Gesamt = Gesamter Zeitraum (16.08.2019 bis 30.06.2020)

h1_date <- "2019-08-16/2019-12-30"
h2_date <- "2020-01-02/2020-06-30"
ges_date <- "2019-08-16/2020-06-30"

h1_fit <- fun_fit_error(atx_xts_cont_ret_vola, h1_date)
h2_fit <- fun_fit_error(atx_xts_cont_ret_vola, h2_date)
ges_fit <- fun_fit_error(atx_xts_cont_ret_vola, ges_date)

################################################################################
# Prognosen
################################################################################

# Zeithorizont
horizon <- 60

# Datensatz in Trainingsdaten und Vergleichsdaten einteilen
forecast_df_vola <- data.frame(date = index(atx_xts_cont_ret_vola),
    data = coredata(atx_xts_cont_ret_vola))

forecast_df_vola_train_test <- forecast_df_vola
forecast_df_vola_train_test$type <- c(rep("train",
    nrow(forecast_df_vola) - horizon), rep("test", horizon))
forecast_df_vola_train_test <- forecast_df_vola_train_test %>%
    tidyr::spread(key = type, value = data)

# Naive Prognose (Referenz)
forecast_naive <- c(NA,
    atx_xts_ges_cont_ret_vola[-length(atx_xts_ges_cont_ret_vola)])
forecast_naive <- xts(forecast_naive,
    order.by = index(atx_xts_ges_cont_ret_vola))
forecast_naive <- forecast_naive["2019-07-02/2020-06-30"]
forecast_naive <- forecast_naive[(length(forecast_naive) - horizon +
    1):length(forecast_naive)]

# SMA
forecast_sma <- fun_forecast_moving_averages(df = forecast_df_vola, h = horizon,
    n = 30, weights = NULL)

# LWMA
forecast_lwma_weights <- matrix(nrow = horizon, ncol = 1)
for (i in 1:horizon) {
    forecast_lwma_weights[i, 1] <- (2 * i) / (horizon * (horizon + 1))
}

forecast_lwma <- fun_forecast_moving_averages(df = forecast_df_vola,
    h = horizon, n = 30, weights = c(forecast_lwma_weights))

# HMA
temp1 <- 2 * WMA(na.omit(forecast_df_vola_train_test$train), n = 15, wts = 1:15)
temp2 <- WMA(na.omit(forecast_df_vola_train_test$train), n = 30, wts = 1:30)
hma_inner_lwma <- temp1 - temp2

hma_inner_lwma <- as.matrix(hma_inner_lwma)
hma_inner_lwma_ref <- na.omit(as.matrix(forecast_df_vola_train_test$test))
hma_inner_lwma <- c(hma_inner_lwma, hma_inner_lwma_ref)
hma_inner_lwma <- data.frame(date = forecast_df_vola$date,
    data = hma_inner_lwma)

forecast_hma_weights <- matrix(nrow = round(sqrt(30), 0), ncol = 1)
for (i in 1:round(sqrt(30), 0)) {
    forecast_hma_weights[i, 1] <- (2 * i) / (round(sqrt(30), 0) *
    (round(sqrt(30), 0) + 1))
}

forecast_hma <- fun_forecast_moving_averages(df = hma_inner_lwma, h = horizon,
    n = round(sqrt(30), 0), weights = forecast_hma_weights)

# EWMA
# Alpha entspricht Lambda
forecast_ewma <- ses(na.omit(forecast_df_vola_train_test$train), h = horizon,
    initial = "optimal", alpha = 0.2511)

# ARIMA
atx_xts_cont_ret_arima_vola <- Arima(na.omit(forecast_df_vola_train_test$train),
    order = c(0, 1, 3), lambda = "auto")

# Box-Cox-Transformation invertieren und skalieren
atx_xts_cont_ret_arima_vola_res_boxcox_vola <-
    InvBoxCox(atx_xts_cont_ret_arima_vola$residuals,
        atx_xts_cont_ret_arima_vola$lambda)
temp1 <- max(atx_xts_cont_ret_arima_vola_res_boxcox_vola) /
    max(na.omit(forecast_df_vola_train_test$train))
atx_xts_cont_ret_arima_vola_res <- atx_xts_cont_ret_arima_vola_res_boxcox_vola /
    temp1

atx_xts_cont_ret_arima_vola_res <-
    as.xts(coredata(atx_xts_cont_ret_arima_vola_res),
        order.by = forecast_df_vola_train_test$date[1:(length(forecast_df_vola_train_test$date) - horizon)])

forecast_arima <- forecast(atx_xts_cont_ret_arima_vola_res, h = horizon,
    model = atx_xts_cont_ret_arima_vola)
forecast_arima_xts <- xts(forecast_arima$mean,
    order.by = forecast_df_vola_train_test$date[(length(forecast_df_vola_train_test$date) - horizon + 1):length(forecast_df_vola_train_test$date)])

# GARCH
atx_xts_cont_ret_garch_fit_vola <- ugarchfit(spec = atx_xts_cont_ret_garch_spec,
    data = na.omit(forecast_df_vola_train_test$train))
atx_xts_cont_ret_garch_sig <- as.xts(atx_xts_cont_ret_garch_fit_vola@fit$sigma,
    order.by = forecast_df_vola_train_test$date[1:(length(forecast_df_vola_train_test$date) - horizon)])

forecast_garch <- ugarchforecast(fit = atx_xts_cont_ret_garch_fit_vola,
    n.ahead = horizon)
forecast_garch_xts <- xts(forecast_garch@forecast$sigmaFor,
    order.by = forecast_df_vola_train_test$date[(length(forecast_df_vola_train_test$date) - horizon + 1):length(forecast_df_vola_train_test$date)])

# MASE
fun_forecast_mase(h = horizon)

################################################################################
# Graphen ATX
################################################################################

# Für Export folgende Befehle verwenden:
# png("plot.png", width = 4000, height = 2000, res = 300)
# dev.off()
# Height +500 pro Diagramm, wenn mehrere Diagramme in einem plot sind

# Verlauf der täglichen Schlusspreise des ATX
atx_schluss

# Verlauf der täglichen Schlusspreise des ATX mit COVID-19-Beginn
atx_schluss_covid

# Verlauf der täglichen diskreten und stetigen Renditen des ATX
atx_disc_cont_ret

# Verlauf der täglichen stetigen Renditen des ATX
atx_cont_ret

# McLeod-Li-Test 1
plot_ml

# Verlauf der täglichen stetigen Renditen des ATX Box-Cox-transformiert
atx_cont_ret_box_cox

# McLeod-Li-Test 2
plot_ml_2

# Verlauf der täglichen stetigen Renditen des ATX Box-Cox-transformiert und
# differenziert
atx_cont_ret_box_cox_diff

# McLeod-Li-Test 3
plot_ml_3

# Korrelogramm 1
atx_xts_cont_ret_acf_pacf

# Verlauf der täglichen stetigen Renditen des ATX Box-Cox-transformiert und
# differenziert ohne Trend
atx_cont_ret_box_cox_diff_o_trend

# McLeod-Li-Test 4
plot_ml_4

# Periodogramm
perio

# Ljung-Box-Test
plot_lb

# Korrelogramm 2
atx_xts_cont_ret_acf_pacf_2

# Verlauf der Volatilität
atx_vola

# Verlauf der Volatilität und Verlauf der Renditen
atx_vola_cont_ret

# Korrelogramm 3
atx_xts_cont_ret_acf_pacf_3

# Inverse charakteristische Wurzel von ARIMA(0, 1, 1) im Einheitskreis
arima_011_char_roots

# Inverse charakteristische Wurzel von ARIMA(0, 1, 3) im Einheitskreis
arima_013_char_roots

# Residuen und quadrierte Residuen des ARIMA(0, 1, 3)-Modells
atx_xts_cont_ret_arima_res_plot_arrange

# Korrelogramm der Residuen und quadrierten Residuen des ARIMA(0, 1, 3)-Modells
atx_xts_cont_ret_arima_res_korr_plot_arrange

# Fits von SMA, LWMA, HMA, EWMA, ARIMA, GARCH H1
vola_fits_h1

# Fits von SMA, LWMA, HMA, EWMA, ARIMA, GARCH H2
vola_fits_h2

# Fits von SMA, LWMA, HMA, EWMA, ARIMA, GARCH Gesamt
vola_fits_ges

# Prognosen
vola_prognosen

################################################################################
# Graphen Anhang
################################################################################

# Achsenskalierungen müssen ggf. in fun_graphs_anhang.r angepasst werden
atx_xts_cont_ret_acf_pacf_wn
atx_xts_cont_ret_acf_pacf_rw_od
atx_xts_cont_ret_acf_pacf_rw_md
atx_xts_cont_ret_acf_pacf_brown
