################################################################################
# Graphen
################################################################################

# Daten
date_vector_atx_2019_2021 <- c(seq(min(as.Date("2019-01-01")),
    max(index(atx_xts_ges)), "3 months"), as.Date("2021-01-01"))
date_vector_atx_2019_2020 <- c(seq(as.Date("2019-07-01"),
    as.Date("2020-07-01"), "2 months"))
date_vector_atx_2019_2020_1m <- c(seq(as.Date("2019-07-01"),
    as.Date("2020-07-01"), "1 months"))

# Verlauf der täglichen Schlusspreise des ATX
atx_schluss <- ggplot() +
    geom_line(data = atx_xts_ges, aes(x = index(atx_xts_ges),
        y = coredata(atx_xts_ges), color = "ATX")) +
    scale_color_manual(values = c("black")) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2021,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Schlusspreis", breaks = seq(1500, 3500, 500)) +
    expand_limits(x = as.Date("2019-01-01"), y = 1600) +
    expand_limits(x = c(as.Date("2019-01-01"), as.Date("2021-01-01")),
        y = c(1500, 3500)) +
    theme_ba2(legend_pos = "none")

# Verlauf der täglichen Schlusspreise des ATX mit COVID-19-Beginn
atx_schluss_covid <- ggplot() +
    geom_line(data = atx_xts, aes(x = index(atx_xts), y = coredata(atx_xts),
        color = "ATX")) +
    geom_vline(aes(xintercept = as.Date("2020-01-01")), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_color_manual(values = c("black")) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Schlusspreis", breaks = seq(1500, 3500, 500)) +
    expand_limits(x = as.Date("2019-07-01"), y = 1600) +
    expand_limits(x = c(as.Date("2019-07-01"), as.Date("2020-07-01")),
        y = c(1500, 3500)) +
    theme_ba2(legend_pos = "none")

# Verlauf der täglichen diskreten und stetigen Renditen des ATX
atx_disc_cont_ret <- ggplot() +
    geom_line(data = atx_xts_disc_ret, aes(x = index(atx_xts_disc_ret),
        y = coredata(atx_xts_disc_ret), color = "Diskrete Rendite"),
        linewidth = 1, alpha = 0.5) +
    geom_line(data = atx_xts_cont_ret, aes(x = index(atx_xts_cont_ret),
        y = coredata(atx_xts_cont_ret), color = "Stetige Rendite"),
        linewidth = 0.5) +
    scale_color_manual(values = c("Diskrete Rendite" = "red",
        "Stetige Rendite" = "blue"), labels = unname(TeX(c("$R_{t,diskret}$",
        "$R_{t,stetig}$")))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Rendite", breaks = seq(-0.16, 0.12, 0.04)) +
    expand_limits(x = as.Date("2019-07-01"), y = -0.16) +
    expand_limits(x = c(as.Date("2019-07-01"), as.Date("2020-07-01")),
        y = c(-0.16, 0.12)) +
    theme_ba2(legend_pos = "right")

# Verlauf der täglichen stetigen Renditen des ATX
atx_cont_ret <- ggplot() +
    geom_line(data = atx_xts_cont_ret, aes(x = index(atx_xts_cont_ret),
        y = coredata(atx_xts_cont_ret), color = "Stetige Rendite")) +
    scale_color_manual(values = c("black")) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = TeX("\\textbf{Rendite} $R_{t}$"),
        breaks = seq(-0.16, 0.12, 0.04)) +
    expand_limits(x = as.Date("2019-07-01"), y = -0.16) +
    expand_limits(x = c(as.Date("2019-07-01"), as.Date("2020-07-01")),
        y = c(-0.16, 0.12)) +
    theme_ba2(legend_pos = "none")

# McLeod-Li-Test 1
atx_xts_cont_ret_ml_matrix <- matrix(nrow = length(atx_xts_cont_ret[-1]),
    ncol = 3)
for (i in 0:length(atx_xts_cont_ret[-1])) {
    x1 <- Box.test(coredata(atx_xts_cont_ret[-1]) ^ 2, type = "Ljung-Box",
        lag = i)
    atx_xts_cont_ret_ml_matrix[i, 1] <- x1$statistic
    atx_xts_cont_ret_ml_matrix[i, 2] <- x1$p.value
    atx_xts_cont_ret_ml_matrix[i, 3] <- qchisq(1 - 0.05, i)
}

atx_xts_cont_ret_ml_matrix <- as.data.frame(atx_xts_cont_ret_ml_matrix)

ml_qk <- ggplot() +
    geom_line(data = atx_xts_cont_ret_ml_matrix, aes(x = seq(1, 252, 1),
        y = V3, color = "A")) +
    geom_line(data = atx_xts_cont_ret_ml_matrix, aes(x = seq(1, 252, 1),
        y = V1, color = "B")) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "Teststatistik", breaks = seq(0, 300, 50)) +
    scale_color_manual(values = c("A" = "black", "B" = "blue"),
        labels = unname(TeX(c("$\\chi_{k;1-\\alpha}^{2}$", "$Q(k)$")))) +
    expand_limits(y = 300) +
    expand_limits(y = c(0, 300)) +
    theme_ba2(legend_pos = "right")

ml_p_wert <- ggplot() +
    geom_line(data = atx_xts_cont_ret_ml_matrix, aes(x = seq(1, 252, 1),
        y = V2)) +
    geom_hline(aes(yintercept = 0.05), color = "blue", linetype = "dashed",
        linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "p-Wert", breaks = seq(0, 1, 0.2)) +
    expand_limits(y = 1) +
    expand_limits(y = c(0, 1)) +
    theme_ba2(legend_pos = "right")

plot_ml <- ggarrange(ml_qk, ml_p_wert, ncol = 1, nrow = 2,
    labels = c("(A)", "(B)"), label.x = -0.01)

# Verlauf der täglichen stetigen Renditen des ATX Box-Cox-transformiert
atx_cont_ret_box_cox <- ggplot() +
    geom_line(data = atx_xts_cont_ret_boxcox,
        aes(x = index(atx_xts_cont_ret_boxcox),
        y = coredata(atx_xts_cont_ret_boxcox), color = "Rendite")) +
    geom_line(data = atx_xts_cont_ret_boxcox,
        aes(x = index(atx_xts_cont_ret_boxcox),
        y = sma(coredata(atx_xts_cont_ret_boxcox))$fitted,
        color = "\"Trend\""), linewidth = 1) +
    scale_color_manual(values = c("Rendite" = "black",
        "\"Trend\"" = "blue"), labels = unname(TeX(c("\"Trend\"",
        "$R_{t}(\\lambda)$")))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = TeX("\\textbf{Rendite} $R_{t}(\\lambda)$"),
        breaks = seq(-5, -1.5, 0.5)) +
    expand_limits(y = -5) +
    expand_limits(y = c(-5, -1.5)) +
    theme_ba2(legend_pos = "right")

# McLeod-Li-Test 2
atx_xts_cont_ret_ml_matrix_2 <- matrix(nrow = length(atx_xts_cont_ret_boxcox),
    ncol = 3)
for (i in 0:length(atx_xts_cont_ret_boxcox)) {
    x1 <- Box.test(coredata(atx_xts_cont_ret_boxcox) ^ 2, type = "Ljung-Box",
        lag = i)
    atx_xts_cont_ret_ml_matrix_2[i, 1] <- x1$statistic
    atx_xts_cont_ret_ml_matrix_2[i, 2] <- x1$p.value
    atx_xts_cont_ret_ml_matrix_2[i, 3] <- qchisq(1 - 0.05, i)
}

atx_xts_cont_ret_ml_matrix_2 <- as.data.frame(atx_xts_cont_ret_ml_matrix_2)

ml_qk_2 <- ggplot() +
    geom_line(data = atx_xts_cont_ret_ml_matrix_2, aes(x = seq(1, 252, 1),
        y = V3, color = "A")) +
    geom_line(data = atx_xts_cont_ret_ml_matrix_2, aes(x = seq(1, 252, 1),
        y = V1, color = "B")) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "Teststatistik", breaks = seq(0, 1500, 500)) +
    scale_color_manual(values = c("A" = "black", "B" = "blue"),
        labels = unname(TeX(c("$\\chi_{k;1-\\alpha}^{2}$", "$Q(k)$")))) +
    expand_limits(y = 1500) +
    expand_limits(y = c(0, 1500)) +
    theme_ba2(legend_pos = "right")

ml_p_wert_2 <- ggplot() +
    geom_line(data = atx_xts_cont_ret_ml_matrix_2, aes(x = seq(1, 252, 1),
        y = V2)) +
    geom_hline(aes(yintercept = 0.05), color = "blue", linetype = "dashed",
        linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "p-Wert", breaks = seq(0, 1, 0.2)) +
    expand_limits(y = 1) +
    expand_limits(y = c(0, 1)) +
    theme_ba2(legend_pos = "right")

plot_ml_2 <- ggarrange(ml_qk_2, ml_p_wert_2, ncol = 1, nrow = 2,
    labels = c("(A)", "(B)"), label.x = -0.01)

# Verlauf der täglichen stetigen Renditen des ATX Box-Cox-transformiert und
# differenziert
atx_cont_ret_box_cox_diff <- ggplot() +
    geom_line(data = atx_xts_cont_ret_boxcox_diff[-1],
        aes(x = index(atx_xts_cont_ret_boxcox_diff[-1]),
        y = coredata(atx_xts_cont_ret_boxcox_diff[-1]),
        color = "Stetige Rendite")) +
    scale_color_manual(values = c("black")) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(
        name = TeX("\\textbf{Rendite} $\\Delta R_{t}(\\lambda)$"),
        breaks = seq(-2, 2, 0.5)) +
    expand_limits(y = -2) +
    expand_limits(y = c(-2, 2)) +
    theme_ba2(legend_pos = "none")

# McLeod-Li-Test 3
atx_xts_cont_ret_ml_matrix_3 <- matrix(nrow =
    length(atx_xts_cont_ret_boxcox_diff[-1]), ncol = 3)
for (i in 0:length(atx_xts_cont_ret_boxcox_diff[-1])) {
    x1 <- Box.test(coredata(atx_xts_cont_ret_boxcox_diff[-1]) ^ 2,
        type = "Ljung-Box", lag = i)
    atx_xts_cont_ret_ml_matrix_3[i, 1] <- x1$statistic
    atx_xts_cont_ret_ml_matrix_3[i, 2] <- x1$p.value
    atx_xts_cont_ret_ml_matrix_3[i, 3] <- qchisq(1 - 0.05, i)
}

atx_xts_cont_ret_ml_matrix_3 <- as.data.frame(atx_xts_cont_ret_ml_matrix_3)

ml_qk_3 <- ggplot() +
    geom_line(data = atx_xts_cont_ret_ml_matrix_3, aes(x = seq(1, 251, 1),
        y = V3, color = "A")) +
    geom_line(data = atx_xts_cont_ret_ml_matrix_3, aes(x = seq(1, 251, 1),
        y = V1, color = "B")) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "Teststatistik", breaks = seq(0, 500, 100)) +
    scale_color_manual(values = c("A" = "black", "B" = "blue"),
        labels = unname(TeX(c("$\\chi_{k;1-\\alpha}^{2}$", "$Q(k)$")))) +
    expand_limits(y = 500) +
    expand_limits(y = c(0, 500)) +
    theme_ba2(legend_pos = "right")

ml_p_wert_3 <- ggplot() +
    geom_line(data = atx_xts_cont_ret_ml_matrix_3, aes(x = seq(1, 251, 1),
        y = V2)) +
    geom_hline(aes(yintercept = 0.05), color = "blue", linetype = "dashed",
        linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "p-Wert", breaks = seq(0, 1, 0.2)) +
    expand_limits(y = 1) +
    expand_limits(y = c(0, 1)) +
    theme_ba2(legend_pos = "right")

plot_ml_3 <- ggarrange(ml_qk_3, ml_p_wert_3, ncol = 1, nrow = 2,
    labels = c("(A)", "(B)"), label.x = -0.01)

# Bestimmen der (maximalen) Lags für Korrelogramme
min(length(atx_xts_cont_ret_boxcox_diff[-1]) / 5, 10) # 10 Lags als Maximum

# Korrelogramm (Autokorrelation)
atx_xts_cont_ret_acf <- ggAcf(atx_xts_cont_ret_boxcox_diff[-1],
    type = "correlation", ci = NULL, lag.max = 10) +
    geom_hline(aes(yintercept = -0.6), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff[-1])))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff[-1])))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(1, 10, 1)) +
    scale_y_continuous(name = "ACF", breaks = round(seq(-0.6, 0.2, 0.2),
        digits = 2)) +
    expand_limits(y = -0.6) +
    expand_limits(y = c(-0.6, 0.2)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

# Korrelogramm (partielle Autokorrelation)
atx_xts_cont_ret_pacf <- ggAcf(atx_xts_cont_ret_boxcox_diff[-1],
    type = "partial", ci = NULL, lag.max = 10) +
    geom_hline(aes(yintercept = -0.6), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff[-1])))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff[-1])))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(1, 10, 1)) +
    scale_y_continuous(name = "PACF", breaks = round(seq(-0.6, 0.2, 0.2),
        digits = 2)) +
    expand_limits(y = -0.6) +
    expand_limits(y = c(-0.6, 0.2)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_acf_pacf <- ggarrange(atx_xts_cont_ret_acf,
    atx_xts_cont_ret_pacf, ncol = 1, nrow = 2, labels = c("(A)", "(B)"),
    label.x = -0.01)

# Verlauf der täglichen stetigen Renditen des ATX Box-Cox-transformiert und
# differenziert ohne Trend
atx_cont_ret_box_cox_diff_o_trend <- ggplot() +
    geom_line(data = atx_xts_cont_ret_boxcox_diff_detrend,
        aes(x = index(atx_xts_cont_ret_boxcox_diff_detrend),
        y = coredata(atx_xts_cont_ret_boxcox_diff_detrend),
        color = "Rendite")) +
    scale_color_manual(values = c("Rendite" = "black")) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = TeX("\\textbf{Rendite} $\\Delta R_{t}^{\\prime}(\\lambda)$"), breaks = seq(-1.5, 1.5, 0.5)) +
    expand_limits(y = -1.5) +
    expand_limits(y = c(-1.5, 1.5)) +
    theme_ba2(legend_pos = "none")

# McLeod-Li-Test 4
atx_xts_cont_ret_ml_matrix_4 <- matrix(nrow =
    length(atx_xts_cont_ret_boxcox_diff_detrend), ncol = 3)
for (i in 0:length(atx_xts_cont_ret_boxcox_diff_detrend)) {
    x1 <- Box.test(coredata(atx_xts_cont_ret_boxcox_diff_detrend) ^ 2,
        type = "Ljung-Box", lag = i)
    atx_xts_cont_ret_ml_matrix_4[i, 1] <- x1$statistic
    atx_xts_cont_ret_ml_matrix_4[i, 2] <- x1$p.value
    atx_xts_cont_ret_ml_matrix_4[i, 3] <- qchisq(1 - 0.05, i)
}

atx_xts_cont_ret_ml_matrix_4 <- as.data.frame(atx_xts_cont_ret_ml_matrix_4)

ml_qk_4 <- ggplot() +
    geom_vline(aes(xintercept = 10), color = "blue", linetype = "dashed",
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_ml_matrix_4, aes(x =
        seq(1, length(atx_xts_cont_ret_boxcox_diff_detrend), 1), y = V3,
        color = "A")) +
    geom_line(data = atx_xts_cont_ret_ml_matrix_4, aes(x =
        seq(1, length(atx_xts_cont_ret_boxcox_diff_detrend), 1), y = V1,
        color = "B")) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "Teststatistik", breaks = seq(0, 300, 100)) +
    scale_color_manual(values = c("A" = "black", "B" = "blue"),
        labels = unname(TeX(c("$\\chi_{k;1-\\alpha}^{2}$", "$Q(k)$")))) +
    expand_limits(y = 300) +
    expand_limits(y = c(0, 300)) +
    theme_ba2(legend_pos = "right")

ml_p_wert_4 <- ggplot() +
    geom_vline(aes(xintercept = 10), color = "blue", linetype = "dashed",
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_ml_matrix_4,
        aes(x = seq(1, length(atx_xts_cont_ret_boxcox_diff_detrend), 1),
        y = V2)) +
    geom_hline(aes(yintercept = 0.05), color = "blue", linetype = "dashed",
        linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "p-Wert", breaks = seq(0, 1, 0.2)) +
    expand_limits(y = 1) +
    expand_limits(y = c(0, 1)) +
    theme_ba2(legend_pos = "right")

plot_ml_4 <- ggarrange(ml_qk_4, ml_p_wert_4, ncol = 1, nrow = 2,
    labels = c("(A)", "(B)"), label.x = -0.01)

# Power Chi^{2}, k = 10
pwr.chisq.test(w = 0.1, df = 10,
    N = length(atx_xts_cont_ret_boxcox_diff_detrend), sig.level = 0.05)
pwr.chisq.test(w = 0.3, df = 10,
    N = length(atx_xts_cont_ret_boxcox_diff_detrend), sig.level = 0.05)
pwr.chisq.test(w = 0.5, df = 10,
    N = length(atx_xts_cont_ret_boxcox_diff_detrend), sig.level = 0.05)

# Power Chi^{2}, k = 234
pwr.chisq.test(w = 0.1, df = 234,
    N = length(atx_xts_cont_ret_boxcox_diff_detrend), sig.level = 0.05)
pwr.chisq.test(w = 0.3, df = 234,
    N = length(atx_xts_cont_ret_boxcox_diff_detrend), sig.level = 0.05)
pwr.chisq.test(w = 0.5, df = 234,
    N = length(atx_xts_cont_ret_boxcox_diff_detrend), sig.level = 0.05)

# Periodogramm (normiert auf [0;0,5])
perio_length <- length(coredata(atx_xts_cont_ret_boxcox_diff_detrend))
perio_fft <- (1 / perio_length) *
    abs(fft(coredata(atx_xts_cont_ret_boxcox_diff_detrend))) ^ 2
perio_x <- (0:floor(perio_length / 2)) / perio_length

# Maximum finden
temp1 <- data.frame(V1 = perio_x, V2 = perio_fft[1:((perio_length / 2) + 1)])
temp1$V1[which.max(temp1$V2)]
max(temp1$V2)

# In Tage umrechnen bzw. Zyklus/Saisonalität
1 / temp1$V1[which.max(temp1$V2)]

# Signifikanzniveaus
round(qchisq((0.05 / 2), 2), digits = 3) # 0.051
round(qchisq(1 - (0.05 / 2), 2), digits = 3) # 7.378
# Unteres Signifikanzniveau
2 * max(temp1$V2) / round(qchisq(1 - (0.05 / 2), 2), digits = 3)
# Oberes Signifikanzniveau
2 * max(temp1$V2) / round(qchisq((0.05 / 2), 2), digits = 3)

perio_norm <- ggplot() +
    geom_hline(aes(yintercept = (2 * max(temp1$V2) / round(qchisq(1 - (0.05 /
        2), 2), digits = 3))), color = "blue", linetype = "dashed",
        linewidth = 1) +
    geom_line(aes(x = perio_x, y = perio_fft[1:((perio_length / 2) + 1)])) +
    scale_x_continuous(name = TeX("$\\textbf{Frequenz}$")) +
    scale_y_continuous(name = "Spektrum", breaks = seq(0, 1.5, 0.5)) +
    expand_limits(y = 1.5) +
    expand_limits(y = c(0, 1.5)) +
    theme_ba2(legend_pos = "none")

# Periodogramm (1 / freq)
temp2 <- data.frame(V1 = 1 / perio_x, V2 = perio_fft[1:((perio_length / 2) +
    1)])
temp2$V1[which.max(temp2$V2)]
max(temp2$V2)

perio_freq <- ggplot() +
    geom_hline(aes(yintercept = (2 * max(temp2$V2) / round(qchisq(1 - (0.05 /
        2), 2), digits = 3))), color = "blue", linetype = "dashed",
        linewidth = 1) +
    geom_line(aes(x = 1 / perio_x, y = perio_fft[1:((perio_length / 2) + 1)])) +
    scale_x_continuous(name = TeX("$\\textbf{Frequenz^{-1}}$")) +
    scale_y_continuous(name = "Spektrum", breaks = seq(0, 1.5, 0.5)) +
    expand_limits(y = 1.5) +
    expand_limits(y = c(0, 1.5)) +
    theme_ba2(legend_pos = "none")

# Periodogramm (log(1 / freq))
temp3 <- data.frame(V1 = log(1 / perio_x),
    V2 = perio_fft[1:((perio_length / 2) + 1)])
exp(temp3$V1[which.max(temp3$V2)])
max(temp3$V2)

perio_log_freq <- ggplot() +
    geom_hline(aes(yintercept = (2 * max(temp3$V2) / round(qchisq(1 - (0.05 /
        2), 2), digits = 3))), color = "blue", linetype = "dashed",
        linewidth = 1) +
    geom_line(aes(x = log(1 / perio_x),
        y = perio_fft[1:((perio_length / 2) + 1)])) +
    scale_x_continuous(name = TeX("$\\textbf{log(Frequenz^{-1})}$")) +
    scale_y_continuous(name = "Spektrum", breaks = seq(0, 1.5, 0.5)) +
    expand_limits(y = 1.5) +
    expand_limits(y = c(0, 1.5)) +
    theme_ba2(legend_pos = "none")

perio <- ggarrange(perio_norm, perio_freq, perio_log_freq, ncol = 1, nrow = 3,
    labels = c("(A)", "(B)", "(C)"), label.x = -0.01)

# Ljung-Box-Test
atx_xts_cont_ret_lb_matrix <- matrix(nrow =
    length(atx_xts_cont_ret_boxcox_diff_detrend), ncol = 3)
for (i in 0:length(atx_xts_cont_ret_boxcox_diff_detrend)) {
    atx_xts_cont_ret_lb_stat <-
        Box.test(atx_xts_cont_ret_boxcox_diff_detrend, lag = i,
        type = "Ljung-Box")
    atx_xts_cont_ret_lb_matrix[i, 1] <- atx_xts_cont_ret_lb_stat$statistic
    atx_xts_cont_ret_lb_matrix[i, 2] <- qchisq(1 - 0.05, i)
    atx_xts_cont_ret_lb_matrix[i, 3] <- atx_xts_cont_ret_lb_stat$p.value
}

atx_xts_cont_ret_lb_matrix <- as.data.frame(atx_xts_cont_ret_lb_matrix)

lb_qk <- ggplot() +
    geom_vline(aes(xintercept = 10), color = "blue", linetype = "dashed",
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_lb_matrix,
        aes(x = seq(1, length(atx_xts_cont_ret_boxcox_diff_detrend), 1),
        y = V2, color = "A")) +
    geom_line(data = atx_xts_cont_ret_lb_matrix,
        aes(x = seq(1, length(atx_xts_cont_ret_boxcox_diff_detrend), 1),
        y = V1, color = "B")) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "Teststatistik", breaks = seq(0, 400, 100)) +
    scale_color_manual(values = c("A" = "black", "B" = "blue"),
        labels = unname(TeX(c("$\\chi_{k;1-\\alpha}^{2}$", "$Q(k)$")))) +
    expand_limits(y = 400) +
    expand_limits(y = c(0, 400)) +
    theme_ba2(legend_pos = "right")

lb_p_wert <- ggplot() +
    geom_hline(aes(yintercept = 0.05), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = 10), color = "blue", linetype = "dashed",
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_lb_matrix,
        aes(x = seq(1, length(atx_xts_cont_ret_boxcox_diff_detrend), 1),
        y = V3)) +
    scale_x_continuous(name = TeX("$\\textbf{k}$")) +
    scale_y_continuous(name = "p-Wert", breaks = seq(0, 0.6, 0.1)) +
    expand_limits(y = 0.6) +
    expand_limits(y = c(0, 0.6)) +
    theme_ba2(legend_pos = "none")

plot_lb <- ggarrange(lb_qk, lb_p_wert, ncol = 1, nrow = 2,
    labels = c("(A)", "(B)"), label.x = -0.01)

# Korrelogramm (Autokorrelation) 2
atx_xts_cont_ret_acf_2 <- ggAcf(atx_xts_cont_ret_boxcox_diff_detrend,
    type = "correlation", ci = NULL,
    lag.max = length(atx_xts_cont_ret_boxcox_diff_detrend)) +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff_detrend)))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff_detrend)))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"),
        breaks = seq(0, length(atx_xts_cont_ret_boxcox_diff_detrend), 20)) +
    scale_y_continuous(name = "ACF", breaks = round(seq(-0.2, 0.2, 0.1),
        digits = 2)) +
    expand_limits(y = 0.2) +
    expand_limits(y = c(-0.2, 0.2)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

# Korrelogramm (partielle Autokorrelation) 2
atx_xts_cont_ret_pacf_2 <- ggAcf(atx_xts_cont_ret_boxcox_diff_detrend,
    type = "partial", ci = NULL,
    lag.max = length(atx_xts_cont_ret_boxcox_diff_detrend)) +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.3), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff_detrend)))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff_detrend)))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"),
        breaks = seq(0, length(atx_xts_cont_ret_boxcox_diff_detrend), 20)) +
    scale_y_continuous(name = "PACF", breaks = round(seq(-0.2, 0.3, 0.1),
        digits = 2)) +
    expand_limits(y = 0.3) +
    expand_limits(y = c(-0.2, 0.3)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_acf_pacf_2 <- ggarrange(atx_xts_cont_ret_acf_2,
    atx_xts_cont_ret_pacf_2, ncol = 1, nrow = 2, labels = c("(A)", "(B)"),
    label.x = -0.01)

# Verlauf der Volatilität
atx_vola <- ggplot() +
    geom_line(data = atx_xts_cont_ret_vola,
        aes(x = index(atx_xts_cont_ret_vola),
        y = coredata(atx_xts_cont_ret_vola), color = "Vola.")) +
    scale_color_manual(values = c("Vola." = "black"),
        labels = unname(c("Vola."))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Volatilität", breaks = seq(0, 0.025, 0.005)) +
    expand_limits(x = as.Date("2019-07-01"), y = 0) +
    expand_limits(x = c(as.Date("2019-07-01"), as.Date("2020-07-01")),
        y = c(0, 0.025)) +
    theme_ba2(legend_pos = "none")

# Verlauf der Volatilität und Verlauf der Renditen
atx_vola_cont_ret <- ggplot() +
    geom_line(data = atx_xts_cont_ret[-1], aes(x = index(atx_xts_cont_ret[-1]),
        y = coredata(atx_xts_cont_ret[-1]), color = "Stetige Rendite")) +
    geom_line(data = atx_xts_cont_ret_vola,
        aes(x = index(atx_xts_cont_ret_vola),
        y = coredata(atx_xts_cont_ret_vola), color = "Vola."), linewidth = 1) +
    scale_color_manual(values = c("Stetige Rendite" = "black",
        "Vola." = "blue"), labels = unname(TeX(c("$R_{t}$", "Vola.")))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Volatilität bzw. Rendite",
        breaks = seq(-0.16, 0.12, 0.04)) +
    expand_limits(x = as.Date("2019-07-01"), y = -0.16) +
    expand_limits(x = c(as.Date("2019-07-01"), as.Date("2020-07-01")),
        y = c(-0.16, 0.12)) +
    theme_ba2(legend_pos = "right")

# Korrelogramm (Autokorrelation) 3
atx_xts_cont_ret_acf_3 <- ggAcf(atx_xts_cont_ret_boxcox_diff[-1],
    type = "correlation", ci = NULL,
    lag.max = length(atx_xts_cont_ret_boxcox_diff[-1])) +
    geom_hline(aes(yintercept = -0.6), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff[-1])))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff[-1])))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = "ACF", breaks = round(seq(-0.6, 0.2, 0.2),
        digits = 2)) +
    expand_limits(y = 0.2) +
    expand_limits(y = c(-0.6, 0.2)) +
    theme_ba2(legend_pos = "none") +
    theme(
        panel.grid.major.y = element_blank(),
    )

# Korrelogramm (partielle Autokorrelation) 3
atx_xts_cont_ret_pacf_3 <- ggAcf(atx_xts_cont_ret_boxcox_diff[-1],
    type = "partial", ci = NULL,
    lag.max = length(atx_xts_cont_ret_boxcox_diff[-1])) +
    geom_hline(aes(yintercept = -0.6), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff[-1])))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 /
        sqrt(length(atx_xts_cont_ret_boxcox_diff[-1])))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = "PACF", breaks = round(seq(-0.6, 0.2, 0.2),
        digits = 2)) +
    expand_limits(y = 0.2) +
    expand_limits(y = c(-0.6, 0.2)) +
    theme_ba2(legend_pos = "none") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_acf_pacf_3 <- ggarrange(atx_xts_cont_ret_acf_3,
    atx_xts_cont_ret_pacf_3, ncol = 1, nrow = 2, labels = c("(A)", "(B)"),
    label.x = -0.01)

# Inverse charakteristische Wurzel von ARIMA(0, 1, 1) im Einheitskreis
arima_011_char_roots <- ggplot() +
    geom_hline(aes(yintercept = 0), color = "black", linetype = "solid",
        linewidth = 0.5) +
    geom_vline(aes(xintercept = 0), color = "black", linetype = "solid",
        linewidth = 0.5) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
    coord_fixed() +
    geom_point(aes(x = arima_model_1_uc_point, y = 0), size = 3) +
    scale_x_continuous(name = TeX("$\\textbf{Re}(v^{-1})$")) +
    scale_y_continuous(name = TeX("$\\textbf{Im}(v^{-1})$")) +
    theme_ba2(legend_pos = "none") +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
    )

# Inverse charakteristische Wurzeln von ARIMA(0, 1, 3) im Einheitskreis
arima_013_char_roots <- ggplot() +
    geom_hline(aes(yintercept = 0), color = "black", linetype = "solid",
        linewidth = 0.5) +
    geom_vline(aes(xintercept = 0), color = "black", linetype = "solid",
        linewidth = 0.5) +
    geom_circle(aes(x0 = 0, y0 = 0, r = 1)) +
    coord_fixed() +
    geom_point(aes(x = as.numeric(arima_model_2_uc_point_1), y = 0), size = 3) +
    geom_point(aes(x = as.numeric(Re(arima_model_2_uc_point_2)),
        y = as.numeric(Im(arima_model_2_uc_point_2))), size = 3) +
    geom_point(aes(x = as.numeric(Re(arima_model_2_uc_point_3)),
        y = as.numeric(Im(arima_model_2_uc_point_3))), size = 3) +
    scale_x_continuous(name = TeX("$\\textbf{Re}(v_{n}^{-1})$")) +
    scale_y_continuous(name = TeX("$\\textbf{Im}(v_{n}^{-1})$")) +
    theme_ba2(legend_pos = "none") +
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
    )

# Residuen des ARIMA(0, 1, 3)-Modells
atx_xts_cont_ret_arima_res_plot <- ggplot() +
    geom_line(data = atx_xts_cont_ret_arima_res,
        aes(x = index(atx_xts_cont_ret_arima_res),
        y = coredata(atx_xts_cont_ret_arima_res))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = TeX("$\\textbf{Residuen}~(r)$"),
        breaks = seq(0, 0.025, 0.005)) +
    expand_limits(x = as.Date("2019-07-01"), y = 0) +
    expand_limits(x = c(as.Date("2019-07-01"), as.Date("2020-07-01")),
        y = c(0, 0.025)) +
    theme_ba2(legend_pos = "none")

# Quadrierte Residuen des ARIMA(0, 1, 3)-Modells
atx_xts_cont_ret_arima_res_sq_plot <- ggplot() +
    geom_line(data = atx_xts_cont_ret_arima_res,
        aes(x = index(atx_xts_cont_ret_arima_res),
        y = coredata(atx_xts_cont_ret_arima_res ^ 2))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = TeX("$\\textbf{Quad.~Residuen}~(r^{2})$"),
        breaks = seq(0, 0.0005, 0.0001),
        label = label_comma(decimal.mark = ",", big.mark = ".")) +
    expand_limits(x = as.Date("2019-07-01"), y = 0) +
    expand_limits(x = c(as.Date("2019-07-01"), as.Date("2020-07-01")),
        y = c(0, 0.0005)) +
    theme_ba2(legend_pos = "none")

atx_xts_cont_ret_arima_res_plot_arrange <- ggarrange(atx_xts_cont_ret_arima_res_plot, atx_xts_cont_ret_arima_res_sq_plot,
    ncol = 1, nrow = 2, labels = c("(A)", "(B)"), label.x = -0.01)

# Korrelogramm der Residuen des ARIMA(0, 1, 3)-Modells
atx_xts_cont_ret_arima_res_korr_plot <- ggAcf(atx_xts_cont_ret_arima_res,
    type = "correlation", ci = NULL,
    lag.max = length(atx_xts_cont_ret_arima_res)) +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.3), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 /
        sqrt(length(atx_xts_cont_ret_arima_res)))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 /
        sqrt(length(atx_xts_cont_ret_arima_res)))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = 10), color = "blue", linetype = "dashed",
        linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = TeX("$\\textbf{ACF}(r)$"),
        breaks = round(seq(-0.2, 0.3, 0.1), digits = 2)) +
    expand_limits(y = 0.3) +
    expand_limits(y = c(-0.2, 0.3)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

# Korrelogramm der quadrierten Residuen des ARIMA(0, 1, 3)-Modells
atx_xts_cont_ret_arima_res_sq_korr_plot <- ggAcf(atx_xts_cont_ret_arima_res ^ 2,
    type = "correlation", ci = NULL,
    lag.max = length(atx_xts_cont_ret_arima_res)) +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 /
        sqrt(length(atx_xts_cont_ret_arima_res)))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 /
        sqrt(length(atx_xts_cont_ret_arima_res)))), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = 10), color = "blue", linetype = "dashed",
        linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = TeX("$\\textbf{ACF}(r^{2})$"),
        breaks = round(seq(-0.2, 0.2, 0.1), digits = 2)) +
    expand_limits(y = 0.2) +
    expand_limits(y = c(-0.2, 0.2)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_arima_res_korr_plot_arrange <-
    ggarrange(atx_xts_cont_ret_arima_res_korr_plot,
        atx_xts_cont_ret_arima_res_sq_korr_plot, ncol = 1, nrow = 2,
        labels = c("(A)", "(B)"), label.x = -0.01)

# Fits von SMA, LWMA, HMA, EWMA, ARIMA, GARCH H1
vola_fits_h1_m_arima <- ggplot() +
    geom_line(data = atx_xts_cont_ret_vola[h1_date],
        aes(x = index(atx_xts_cont_ret_vola[h1_date]),
        y = coredata(atx_xts_cont_ret_vola[h1_date]), color = "Vola."),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_sma[h1_date],
        aes(x = index(atx_xts_cont_ret_vola30_sma[h1_date]),
        y = coredata(atx_xts_cont_ret_vola30_sma[h1_date]), color = "SMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_lwma[h1_date],
        aes(x = index(atx_xts_cont_ret_vola30_lwma[h1_date]),
        y = coredata(atx_xts_cont_ret_vola30_lwma[h1_date]), color = "LWMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_hma[h1_date],
        aes(x = index(atx_xts_cont_ret_vola30_hma[h1_date]),
        y = coredata(atx_xts_cont_ret_vola30_hma[h1_date]), color = "HMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola_ewma[h1_date],
        aes(x = index(atx_xts_cont_ret_vola_ewma[h1_date]),
        y = coredata(atx_xts_cont_ret_vola_ewma[h1_date]), color = "EWMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_arima_res[h1_date],
        aes(x = index(atx_xts_cont_ret_arima_res[h1_date]),
        y = coredata(atx_xts_cont_ret_arima_res[h1_date]), color = "ARIMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_garch_sig[h1_date],
        aes(x = index(atx_xts_cont_ret_garch_sig[h1_date]),
        y = coredata(atx_xts_cont_ret_garch_sig[h1_date]), color = "GARCH"),
        linewidth = 1) +
    geom_vline(aes(xintercept = as.Date("2019-08-16")), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_color_manual(values = c("Vola." = "black", "SMA" = "#D95F02",
        "LWMA" = "#7570B3", "HMA" = "#E7298A", "EWMA" = "#66A61E",
        "ARIMA" = "#E6AB02", "GARCH" = "#A6761D"),
        labels = unname(c("ARIMA", "EWMA", "GARCH", "HMA", "LWMA", "SMA",
        "Vola."))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020_1m[-1],
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Volatilität", breaks = seq(0, 0.006, 0.001)) +
    expand_limits(x = as.Date("2019-08-01"), y = 0) +
    expand_limits(x = c(as.Date("2019-08-01"), as.Date("2019-12-30")),
        y = c(0, 0.006)) +
    theme_ba2(legend_pos = "right")

# Fits von SMA, LWMA, HMA, EWMA, GARCH (ohne ARIMA) H1
vola_fits_h1_o_arima <- ggplot() +
    geom_line(data = atx_xts_cont_ret_vola[h1_date],
        aes(x = index(atx_xts_cont_ret_vola[h1_date]),
        y = coredata(atx_xts_cont_ret_vola[h1_date]), color = "Vola."),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_sma[h1_date],
        aes(x = index(atx_xts_cont_ret_vola30_sma[h1_date]),
        y = coredata(atx_xts_cont_ret_vola30_sma[h1_date]), color = "SMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_lwma[h1_date],
        aes(x = index(atx_xts_cont_ret_vola30_lwma[h1_date]),
        y = coredata(atx_xts_cont_ret_vola30_lwma[h1_date]), color = "LWMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_hma[h1_date],
        aes(x = index(atx_xts_cont_ret_vola30_hma[h1_date]),
        y = coredata(atx_xts_cont_ret_vola30_hma[h1_date]), color = "HMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola_ewma[h1_date],
        aes(x = index(atx_xts_cont_ret_vola_ewma[h1_date]),
        y = coredata(atx_xts_cont_ret_vola_ewma[h1_date]), color = "EWMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_garch_sig[h1_date],
        aes(x = index(atx_xts_cont_ret_garch_sig[h1_date]),
        y = coredata(atx_xts_cont_ret_garch_sig[h1_date]), color = "GARCH"),
        linewidth = 1) +
    geom_vline(aes(xintercept = as.Date("2019-08-16")), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_color_manual(values = c("Vola." = "black", "SMA" = "#D95F02",
        "LWMA" = "#7570B3", "HMA" = "#E7298A", "EWMA" = "#66A61E",
        "ARIMA" = "#E6AB02", "GARCH" = "#A6761D"),
        labels = unname(c("EWMA", "GARCH", "HMA", "LWMA", "SMA", "Vola."))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020_1m[-1],
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Volatilität", breaks = seq(0, 0.0006, 0.0001),
        label = label_comma(decimal.mark = ",", big.mark = ".")) +
    expand_limits(x = as.Date("2019-08-01"), y = 0) +
    expand_limits(x = c(as.Date("2019-08-01"), as.Date("2019-12-30")),
        y = c(0, 0.0006)) +
    theme_ba2(legend_pos = "right")

vola_fits_h1 <- ggarrange(vola_fits_h1_m_arima, vola_fits_h1_o_arima, ncol = 1,
    nrow = 2, labels = c("(A)", "(B)"), label.x = -0.01)

# Fits von SMA, LWMA, HMA, EWMA, ARIMA, GARCH H2
vola_fits_h2 <- ggplot() +
    geom_line(data = atx_xts_cont_ret_vola[h2_date],
        aes(x = index(atx_xts_cont_ret_vola[h2_date]),
        y = coredata(atx_xts_cont_ret_vola[h2_date]), color = "Vola."),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_sma[h2_date],
        aes(x = index(atx_xts_cont_ret_vola30_sma[h2_date]),
        y = coredata(atx_xts_cont_ret_vola30_sma[h2_date]), color = "SMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_lwma[h2_date],
        aes(x = index(atx_xts_cont_ret_vola30_lwma[h2_date]),
        y = coredata(atx_xts_cont_ret_vola30_lwma[h2_date]), color = "LWMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_hma[h2_date],
        aes(x = index(atx_xts_cont_ret_vola30_hma[h2_date]),
        y = coredata(atx_xts_cont_ret_vola30_hma[h2_date]), color = "HMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola_ewma[h2_date],
        aes(x = index(atx_xts_cont_ret_vola_ewma[h2_date]),
        y = coredata(atx_xts_cont_ret_vola_ewma[h2_date]), color = "EWMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_arima_res[h2_date],
        aes(x = index(atx_xts_cont_ret_arima_res[h2_date]),
        y = coredata(atx_xts_cont_ret_arima_res[h2_date]), color = "ARIMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_garch_sig[h2_date],
        aes(x = index(atx_xts_cont_ret_garch_sig[h2_date]),
        y = coredata(atx_xts_cont_ret_garch_sig[h2_date]), color = "GARCH"),
        linewidth = 1) +
    geom_vline(aes(xintercept = as.Date("2020-01-01")), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_color_manual(values = c("Vola." = "black", "SMA" = "#D95F02",
        "LWMA" = "#7570B3", "HMA" = "#E7298A", "EWMA" = "#66A61E",
        "ARIMA" = "#E6AB02", "GARCH" = "#A6761D"),
        labels = unname(c("ARIMA", "EWMA", "GARCH", "HMA", "LWMA", "SMA",
        "Vola."))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020_1m,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Volatilität", breaks = seq(0, 0.025, 0.005)) +
    expand_limits(x = as.Date("2020-01-01"), y = 0) +
    expand_limits(x = c(as.Date("2020-01-01"), as.Date("2020-06-30")),
        y = c(0, 0.025)) +
    theme_ba2(legend_pos = "right")

# Fits von SMA, LWMA, HMA, EWMA, ARIMA, GARCH Gesamt
vola_fits_ges <- ggplot() +
    geom_line(data = atx_xts_cont_ret_vola[ges_date],
        aes(x = index(atx_xts_cont_ret_vola[ges_date]),
        y = coredata(atx_xts_cont_ret_vola[ges_date]), color = "Vola."),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_sma[ges_date],
        aes(x = index(atx_xts_cont_ret_vola30_sma[ges_date]),
        y = coredata(atx_xts_cont_ret_vola30_sma[ges_date]), color = "SMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_lwma[ges_date],
        aes(x = index(atx_xts_cont_ret_vola30_lwma[ges_date]),
        y = coredata(atx_xts_cont_ret_vola30_lwma[ges_date]), color = "LWMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola30_hma[ges_date],
        aes(x = index(atx_xts_cont_ret_vola30_hma[ges_date]),
        y = coredata(atx_xts_cont_ret_vola30_hma[ges_date]), color = "HMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_vola_ewma[ges_date],
        aes(x = index(atx_xts_cont_ret_vola_ewma[ges_date]),
        y = coredata(atx_xts_cont_ret_vola_ewma[ges_date]), color = "EWMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_arima_res[ges_date],
        aes(x = index(atx_xts_cont_ret_arima_res[ges_date]),
        y = coredata(atx_xts_cont_ret_arima_res[ges_date]), color = "ARIMA"),
        linewidth = 1) +
    geom_line(data = atx_xts_cont_ret_garch_sig[ges_date],
        aes(x = index(atx_xts_cont_ret_garch_sig[ges_date]),
        y = coredata(atx_xts_cont_ret_garch_sig[ges_date]), color = "GARCH"),
        linewidth = 1) +
    geom_vline(aes(xintercept = as.Date("2019-08-16")), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_vline(aes(xintercept = as.Date("2020-01-01")), color = "blue",
        linetype = "dashed", linewidth = 1) +
    scale_color_manual(values = c("Vola." = "black", "SMA" = "#D95F02",
        "LWMA" = "#7570B3", "HMA" = "#E7298A", "EWMA" = "#66A61E",
        "ARIMA" = "#E6AB02", "GARCH" = "#A6761D"),
        labels = unname(c("ARIMA", "EWMA", "GARCH", "HMA", "LWMA", "SMA",
        "Vola."))) +
    scale_x_date(name = "Zeit", breaks = date_vector_atx_2019_2020,
        date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Volatilität", breaks = seq(0, 0.025, 0.005)) +
    expand_limits(x = as.Date("2019-08-01"), y = 0) +
    expand_limits(x = c(as.Date("2019-08-01"), as.Date("2020-06-30")),
        y = c(0, 0.025)) +
    theme_ba2(legend_pos = "right")

# Prognosen
vola_prognosen <- ggplot() +
    geom_vline(aes(xintercept = as.Date(forecast_sma$date[length(forecast_sma$date) - horizon + 1])), color = "blue",
        linetype = "dashed", linewidth = 1) +
    geom_line(data = forecast_sma, aes(x = date, y = data, color = "Vola."),
        linewidth = 1) +
    geom_line(data = forecast_naive, aes(x = index(forecast_naive),
        y = coredata(forecast_naive), color = "Naiv"), linetype = "dashed",
        linewidth = 1) +
    geom_line(data = forecast_sma, aes(x = date, y = yhat,
        color = "SMA"), linetype = "dashed", linewidth = 1) +
    geom_line(data = forecast_lwma, aes(x = date, y = yhat,
        color = "LWMA"), linetype = "dashed", linewidth = 1) +
    geom_line(data = forecast_hma, aes(x = date, y = yhat,
        color = "HMA"), linetype = "dashed", linewidth = 1) +
    geom_line(data = forecast_ewma$mean, aes(x = forecast_sma$date[(length(forecast_sma$date) - horizon + 1):length(forecast_sma$date)],
        y = forecast_ewma$mean, color = "EWMA"), linetype = "dashed",
        linewidth = 1) +
    geom_line(data = forecast_arima_xts, aes(x = index(forecast_arima_xts),
        y = coredata(forecast_arima_xts), color = "ARIMA"),
        linetype = "dashed", linewidth = 1) +
    geom_line(data = forecast_garch_xts, aes(x = index(forecast_garch_xts),
        y = coredata(forecast_garch_xts), color = "GARCH"),
        linetype = "dashed", linewidth = 1) +
    scale_color_manual(values = c("Vola." = "black",
        "Naiv" = "#1B9E77", "SMA" = "#D95F02", "LWMA" = "#7570B3",
        "HMA" = "#E7298A", "EWMA" = "#66A61E", "ARIMA" = "#E6AB02",
        "GARCH" = "#A6761D"), labels = unname(c("ARIMA", "EWMA", "GARCH",
        "HMA", "LWMA", "Naiv", "SMA", "Vola."))) +
    scale_x_date(name = "Zeit",
        limits = as.Date(c("2020-04-01", "2020-07-01")),
        breaks = date_vector_atx_2019_2020_1m, date_labels = "%d.%m.%Y") +
    scale_y_continuous(name = "Volatilität", limits = c(0, 0.005),
        breaks = seq(0, 0.005, 0.001)) +
    expand_limits(x = as.Date("2020-04-01")) +
    expand_limits(x = c(as.Date("2020-04-01"), as.Date("2020-07-01"))) +
    theme_ba2(legend_pos = "right")
