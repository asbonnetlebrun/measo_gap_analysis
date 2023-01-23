gc()

measo_fig4_figS3 <- function(all_measo_wd_sub) {
  records_measo_decade <- all_measo_wd_sub %>%
    group_by(mid_decade, QC_measo) %>%
    left_join(measo) %>%
    dplyr::summarise(N = n() / area) %>% # should be per unit area (i.e. divided by the area of each MEASO sector)
    unique()

  map_measo <- measo_map_fn(measo)

  plot_rec_fn <- function(measo_str) {
    ggplot(records_measo_decade %>%
      filter(str_detect(QC_measo, measo_str))) +
      geom_point(aes(mid_decade, N * 10^6, colour = QC_measo)) +
      geom_path(aes(mid_decade, N * 10^6, colour = QC_measo)) +
      scale_colour_manual(values = measo_colours) +
      theme_bw() +
      theme(
        plot.background = element_rect(colour = "black"),
        legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(size = 8)
      ) +
      ylim(0, max(records_measo_decade$N * 10^6)) +
      xlab("decade") +
      ylab("records per km2")
  }

  p_measo <- ggdraw() +
    draw_plot(map_measo, x = 0.07, y = 0.05, width = 0.9, height = 0.9) +
    draw_plot(plot_rec_fn("AO"),
      x = 0.24, y = 0.72, width = 0.245, height = 0.18
    ) +
    draw_plot(plot_rec_fn("CI"),
      x = 0.59, y = 0.53, width = 0.245, height = 0.18
    ) +
    draw_plot(plot_rec_fn("EI"),
      x = 0.55, y = 0.15, width = 0.245, height = 0.18
    ) +
    draw_plot(plot_rec_fn("WP"),
      x = 0.16, y = 0.09, width = 0.245, height = 0.18
    ) +
    draw_plot(plot_rec_fn("EP"),
      x = 0.005, y = 0.37, width = 0.245, height = 0.18
    ) +
    theme(plot.background = element_rect(fill = "white"))

  return(p_measo)
}
