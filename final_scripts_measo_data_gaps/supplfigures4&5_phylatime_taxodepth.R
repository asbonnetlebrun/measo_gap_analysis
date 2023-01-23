# Number of records through time per phylum and taxonomic depth, for 10 most abundant phyla

abdt_phyla <- all_measo_wd %>%
  # filter(QC_basisOfRecord == "human") %>%
  group_by(phylum_final) %>%
  dplyr::summarise(
    N = n(),
    N_2015 = sum(mid_decade == 2015, na.rm = TRUE)
  ) %>%
  arrange(desc(N)) %>%
  na.omit() %>%
  dplyr::filter(row_number() <= 10) %>%
  arrange(desc(N_2015))

my_colours <- RColorBrewer::brewer.pal(10, "Paired")[c(seq(2, 10, 2), seq(1, 9, 2))]
names(my_colours) <- abdt_phyla$phylum_final

# Number of records through time per phylum -------------------------------

figS4_sub <- function(n_per_phylum, abdt_phyla) {
  labels_decades_df <- n_per_phylum %>%
    ungroup() %>%
    dplyr::select(decade, mid_decade) %>%
    unique()

  p_phyla <- n_per_phylum %>%
    ggplot() +
    geom_path(aes(mid_decade, N, colour = phylum_final)) +
    geom_point(aes(mid_decade, N, colour = phylum_final)) +
    ggrepel::geom_text_repel(
      data = n_per_phylum %>%
        filter(phylum_final %in% abdt_phyla$phylum_final) %>%
        group_by(phylum_final) %>%
        dplyr::summarise(
          N = N[mid_decade == 2015],
          group = 2015
        ),
      aes(group, N, colour = phylum_final, label = phylum_final),
      max.overlaps = Inf,
      nudge_x = 6,
      direction = "y",
      hjust = 0,
      alpha = 0.8,
      segment.linetype = 2
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0.05, 0.26)),
      breaks = labels_decades_df$mid_decade,
      labels = labels_decades_df$mid_decade # decade
    ) +
    scale_color_manual(values = my_colours) +
    ylab("number of records") +
    xlab("time") + # ("decade") +
    theme_bw() +
    theme(legend.position = "none")

  return(p_phyla)
}

measo_figS4 <- function(all_measo_wd) {
  n_per_phylum <- all_measo_wd %>%
    filter(
      QC_basisOfRecord == "human",
      phylum_final %in% abdt_phyla$phylum_final
    ) %>%
    group_by(phylum_final, mid_decade, decade) %>%
    dplyr::summarise(N = n())

  p_phyla <- figS4_sub(n_per_phylum, abdt_phyla)

  p_phyla_zoom <- n_per_phylum %>%
    filter(!phylum_final %in% c("Arthropoda", "Chordata")) %>%
    figS4_sub(., abdt_phyla) 

  figS4_plot <- cowplot::plot_grid(
    p_phyla, p_phyla_zoom,
    ncol = 1,
    rel_heights = c(1, 0.9),
    labels = c("A", "B")
  )

  ggsave(paste0(fig_dir, "figS4_phyla_through_time_withzoom.png"), figS4_plot, width = 8, height = 9)

  return(figS4_plot)
}


# Taxonomic depth ---------------------------------------------------------

measo_figS5 <- function(all_measo_wd) {
  taxo_depth <- all_measo_wd %>%
    group_by(phylum_final) %>%
    dplyr::summarise(
      phylum_perc = sum(!is.na(phylum_final)) / n() * 100,
      class_perc = sum(!is.na(class_final)) / n() * 100,
      order_perc = sum(!is.na(order_final)) / n() * 100,
      family_perc = sum(!is.na(family_final)) / n() * 100,
      genus_perc = sum(!is.na(genus_final)) / n() * 100,
      species_perc = sum(!is.na(species_final)) / n() * 100
    ) %>%
    pivot_longer(contains("_perc"),
      names_to = "group",
      values_to = "records"
    ) %>%
    dplyr::mutate(group = factor(group, levels = c("phylum_perc", "class_perc", "order_perc", "family_perc", "genus_perc", "species_perc")))

  p_taxdepth <- taxo_depth %>%
    filter(phylum_final %in% abdt_phyla$phylum_final) %>%
    ggplot() +
    geom_point(aes(group, records, colour = phylum_final)) +
    geom_path(aes(group, records, group = phylum_final, colour = phylum_final)) +
    ggrepel::geom_text_repel(
      data = taxo_depth %>%
        filter(phylum_final %in% abdt_phyla$phylum_final) %>%
        group_by(phylum_final) %>%
        dplyr::summarise(
          records = records[6],
          group = "species_perc"
        ),
      aes(group, records, colour = phylum_final, label = phylum_final),
      max.overlaps = Inf,
      nudge_x = 6,
      direction = "y",
      hjust = 0,
      alpha = 0.8,
      segment.linetype = 2
    ) +
    scale_color_manual(values = my_colours) +
    scale_x_discrete(expand = expansion(mult = c(0.05, 0.25))) +
    ylab("% of records") +
    theme_bw() +
    theme(legend.position = "none")

  ggsave(paste0(fig_dir, "figS5_taxdepth.png"), p_taxdepth, width = 8, height = 5)

  return(p_taxdepth)
}
