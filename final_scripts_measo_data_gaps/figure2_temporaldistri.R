measo_fig2 <- function(all_measo_wd) {
  major_expe <- data.frame(
    expedition = c(
      "Discovery", # Investigations",
      "BIOMASS", # survey",
      "SO-CPR",
      "CAML"
    ), # "Census of Antarctic Marine Life"),
    year_start = c(1924, 1980, 1991, 2005),
    year_end = c(1951, 1985, max(all_measo_wd$year2, na.rm = TRUE), 2010),
    count = 10^6
  )

  ## Year plot
  year_plot <-
    all_measo_wd %>%
    ggplot() +
    geom_rect(
      data = major_expe,
      aes(xmin = year_start, xmax = year_end, ymin = 0, ymax = Inf, fill = expedition), alpha = 0.1
    ) +
    geom_text(
      data = major_expe,
      aes(year_start, count, colour = expedition, label = expedition), angle = 270, hjust = 0, vjust = 0, size = 3
    ) +
    scale_colour_discrete(guide = "none") +
    scale_fill_discrete(guide = "none") +
    new_scale_fill() +
    geom_histogram(aes(year2, fill = QC_basisOfRecord),
      position = "dodge",
      binwidth = 5
    ) +
    scale_fill_manual(
      name = "basis of record",
      values = c(
        "humanobservation" = "black",
        "machine" = "grey"
      ),
      labels = c(
        "human",
        "machine"
      )
    ) +
    xlab("year") +
    theme_bw() +
    theme(panel.grid = element_blank())

  ## Month plot
  all_measo_wd_bar <- all_measo_wd %>%
    mutate(month_trans = as.Date(paste0("2015-", month2, "-01"), "%Y-%m-%d")) %>%
    group_by(QC_basisOfRecord, mid_decade, decade, month_trans) %>%
    dplyr::summarise(N = n())

  type_labs <- c("human observations", "machine observations")
  names(type_labs) <- c("human", "machine")

  month_plot <- all_measo_wd_bar %>%
    na.omit() %>%
    ggplot() +
    geom_col(aes(month_trans, N, fill = decade), position = "dodge", width = 25) +
    scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") +
    scale_fill_discrete(name = "decade") +
    xlab("month") +
    ylab("count") +
    facet_wrap(~QC_basisOfRecord, ncol = 2, labeller = labeller(QC_basisOfRecord = type_labs)) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(
        colour = "black", face = "bold"
      )
    )

  ## Total plot
  p <- plot_grid(year_plot,
    month_plot,
    ncol = 1, labels = c("A", "B")
  )

  ggsave(paste0(fig_dir, "fig2_temporal_distri.png"), p, height = 6, width = 9)

  return(p)
}
