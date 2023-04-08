# Weißes Rauschen
wn_anhang <- arima.sim(model = list(order = c(0, 0, 0)), n = 240)

atx_xts_cont_ret_acf_wn <- ggAcf(wn_anhang, type = "correlation", ci = NULL,
    lag.max = length(wn_anhang)) +
    geom_hline(aes(yintercept = -0.15), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.05), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.05), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.15), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 / sqrt(length(wn_anhang)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 / sqrt(length(wn_anhang)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"),
        breaks = seq(0, length(wn_anhang), 20)) +
    scale_y_continuous(name = "ACF", breaks = round(seq(-0.15, 0.15, 0.05),
        digits = 2)) +
    expand_limits(y = 0.15) +
    expand_limits(y = c(-0.15, 0.15)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_pacf_wn <- ggAcf(wn_anhang, type = "partial", ci = NULL,
    lag.max = length(wn_anhang)) +
    geom_hline(aes(yintercept = -0.15), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.05), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.05), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.15), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 / sqrt(length(wn_anhang)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 / sqrt(length(wn_anhang)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"),
        breaks = seq(0, length(wn_anhang), 20)) +
    scale_y_continuous(name = "PACF", breaks = round(seq(-0.15, 0.15, 0.05),
        digits = 2)) +
    expand_limits(y = 0.15) +
    expand_limits(y = c(-0.15, 0.15)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_acf_pacf_wn <- ggarrange(atx_xts_cont_ret_acf_wn,
    atx_xts_cont_ret_pacf_wn, ncol = 1, nrow = 2, labels = c("(A)", "(B)"),
    label.x = -0.01)

# Random Walk
RW <- function(N, x0, mu, variance) {
    z <- cumsum(rnorm(n = N, mean = 0, sd = sqrt(variance)))
    t <- 1:N
    x <- x0 + t * mu + z

    return(x)
}

# Random Walk ohne Drift
rw_anhang_od <- RW(240, 10, 0, 0.0004)

atx_xts_cont_ret_acf_rw_od <- ggAcf(rw_anhang_od, type = "correlation",
    ci = NULL, lag.max = length(rw_anhang_od)) +
    geom_hline(aes(yintercept = -0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.6), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.8), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 / sqrt(length(rw_anhang_od)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 / sqrt(length(rw_anhang_od)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = "ACF", breaks = seq(-0.4, 1, 0.2)) +
    expand_limits(y = 1) +
    expand_limits(y = c(-0.4, 1)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_line(),
    )

atx_xts_cont_ret_pacf_rw_od <- ggAcf(rw_anhang_od, type = "partial", ci = NULL,
    lag.max = length(rw_anhang_od)) +
    geom_hline(aes(yintercept = -0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.6), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.8), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 / sqrt(length(rw_anhang_od)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 / sqrt(length(rw_anhang_od)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = "PACF", breaks = seq(-0.4, 1, 0.2)) +
    expand_limits(y = 1) +
    expand_limits(y = c(-0.4, 1)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_acf_pacf_rw_od <- ggarrange(atx_xts_cont_ret_acf_rw_od,
    atx_xts_cont_ret_pacf_rw_od, ncol = 1, nrow = 2, labels = c("(A)", "(B)"),
    label.x = -0.01)

# Random Walk (mit Drift)
rw_anhang_md <- RW(240, 10, 1, 0.04)

atx_xts_cont_ret_acf_rw_md <- ggAcf(rw_anhang_md, type = "correlation",
    ci = NULL, lag.max = length(rw_anhang_md)) +
    geom_hline(aes(yintercept = -0.5), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.25), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.25), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.5), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.75), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 / sqrt(length(rw_anhang_md)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 / sqrt(length(rw_anhang_md)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = "ACF", breaks = seq(-0.5, 1, 0.25)) +
    expand_limits(y = 1) +
    expand_limits(y = c(-0.5, 1)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_pacf_rw_md <- ggAcf(rw_anhang_md, type = "partial", ci = NULL,
    lag.max = length(rw_anhang_md)) +
    geom_hline(aes(yintercept = -0.5), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.25), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.25), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.5), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.75), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 / sqrt(length(rw_anhang_md)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 / sqrt(length(rw_anhang_md)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = "PACF", breaks = seq(-0.5, 1, 0.25)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_acf_pacf_rw_md <- ggarrange(atx_xts_cont_ret_acf_rw_md,
    atx_xts_cont_ret_pacf_rw_md, ncol = 1, nrow = 2, labels = c("(A)", "(B)"),
    label.x = -0.01)

# Brownsches Rauschen
brown_noise <- rnorm(n = length(0:240) - 1, sd = sqrt(0.01))
brown_noise <- c(0, cumsum(brown_noise))

atx_xts_cont_ret_acf_brown <- ggAcf(brown_noise, type = "correlation",
    ci = NULL, lag.max = length(brown_noise)) +
    geom_hline(aes(yintercept = -0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.6), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.8), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 / sqrt(length(rw_anhang_md)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 / sqrt(length(rw_anhang_md)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = "ACF", breaks = seq(-0.4, 1, 0.2)) +
    expand_limits(y = 1) +
    expand_limits(y = c(-0.4, 1)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_pacf_brown <- ggAcf(brown_noise, type = "partial", ci = NULL,
    lag.max = length(brown_noise)) +
    geom_hline(aes(yintercept = -0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.2), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.4), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.6), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 0.8), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = 1), color = "gray", linewidth = 1,
        linetype = "dashed") +
    geom_hline(aes(yintercept = (1.96 / sqrt(length(rw_anhang_md)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    geom_hline(aes(yintercept = (-1.96 / sqrt(length(rw_anhang_md)))),
        color = "blue", linetype = "dashed", linewidth = 1) +
    scale_x_continuous(name = TeX("$\\textbf{k}$"), breaks = seq(0, 240, 20)) +
    scale_y_continuous(name = "PACF", breaks = seq(-0.2, 1, 0.2)) +
    expand_limits(y = 1) +
    expand_limits(y = c(-0.2, 1)) +
    theme_ba2(legend_pos = "right") +
    theme(
        panel.grid.major.y = element_blank(),
    )

atx_xts_cont_ret_acf_pacf_brown <- ggarrange(atx_xts_cont_ret_acf_brown,
    atx_xts_cont_ret_pacf_brown, ncol = 1, nrow = 2, labels = c("(A)", "(B)"),
    label.x = -0.01)
