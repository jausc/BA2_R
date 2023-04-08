# Theme für ggplots basierend auf theme_hc()
theme_ba2 <- function(legend_pos = c("none", "right")) {
    theme_hc() +
    theme(
        plot.title = element_blank(),
        legend.position = legend_pos,
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.text.align = 0,
        axis.line.x = element_line(color = "black", linewidth = 0.5,
            linetype = "solid", arrow = arrow(length = unit(0.25, "cm"),
            type = "closed")),
        axis.line.y = element_line(color = "black", linewidth = 0.5,
            linetype = "solid", arrow = arrow(length = unit(0.25, "cm"),
            type = "closed")),
        axis.title = element_text(face = "bold", size = 20),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text = element_text(size = 18),
        axis.text.x.bottom = element_text(margin = margin(t = 10)),
        axis.text.y.left = element_text(margin = margin(r = 10)),
        axis.ticks.length = unit(0.25, "cm"),
        panel.grid.major.y = element_line(color = "gray", linewidth = 1,
            linetype = "dashed"),
        panel.grid.minor.y = element_line(color = "gray", linewidth = 0.5,
            linetype = "dashed"),
    )
}
