measo_figS_dna <- function(all_measo_wd, dna_reads = FALSE) {
  
  # Stats per group - coverage--------------------------------------------------------
  
  nb_records_per_gp <- nb_records_per_gp_fn(all_measo_wd)

  plot_rec <- nb_records_per_gp %>%
    ggplot() +
    geom_bar(aes(focalGroup, weight = N)) +
    ylab("% of records") +
    xlab("") +
    coord_flip() +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "none"
    )
  

# Temporal distribution ---------------------------------------------------

  ## Month plot
  all_measo_wd_bar <- all_measo_wd %>%
    mutate(month_trans = as.Date(paste0("2015-", month2, "-01"), "%Y-%m-%d")) %>%
    group_by(QC_basisOfRecord, mid_decade, decade, month_trans) %>%
    dplyr::summarise(N = n())
  
  month_plot <- all_measo_wd_bar %>%
    na.omit() %>%
    ggplot() +
    geom_col(aes(month_trans, N, fill = decade), 
             # position = "dodge", 
             position = position_dodge2(width = 0.9, preserve = "single"),
             width = 25) +
    scale_x_date(labels = scales::date_format("%b"), breaks = "1 month") +
    scale_fill_viridis_d(name = "decade", direction = -1) +
    xlab("month") +
    ylab("count") +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(
        colour = "black", face = "bold"
      )
    )
  
  ## Total plot
  p <- plot_grid(month_plot,
                 plot_rec,
                 ncol = 1#, labels = c("A", "C")
  )
  
  ggsave(paste0(fig_dir, "figS_dna_reads.png"), p, width = 5.5, height = 7)
  
  return(p)
}
