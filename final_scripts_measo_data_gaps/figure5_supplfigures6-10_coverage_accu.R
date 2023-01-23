
# Number of cells per MEASO region ----------------------------------------

ncells_measo <- data.frame(ID = st_intersects(eagrid_3deg %>% st_as_sf(), measo) %>% unlist()) %>%
  left_join(as.data.frame(measo)) %>%
  group_by(QC_measo) %>%
  dplyr::summarise(ncells = n()) %>%
  na.omit() %>%
  dplyr::mutate(zone = str_sub(QC_measo, 1, 2))

ncells_measo_zone <- ncells_measo %>%
  group_by(zone) %>%
  dplyr::summarise(ncells_zone = sum(ncells))

ncells_measo <- ncells_measo %>%
  left_join(ncells_measo_zone)

# Group info --------------------------------------------------------------

group_corresp <- data.frame(
  focalGroup = c("Macroalgae", "Tintinnid ciliates", "Protozoa", "Porifera", "Bryozoa", "Annelida", "Plantae", "Pisces", "Gelatinous zooplankton", "Eukaryote primary producers", "Bacteria", "Echinodermata", "Mollusca", "Tunicata", "Microzooheterotrophs", "Crustacea", "Birds and Mammals"),
  fG = c("Malg", "Tint", "Prot", "Pori", "Bryo", "Anne", "Plan", "Fish", "GelZoo", "EukPriProd", "Bact", "Echi", "Moll", "Tuni", "mZoo", "Crus", "Bi&Ma")
)

nb_records_per_gp_fn <- function(all_measo_wd) {
  
  all_measo_wd_hum <- all_measo_wd %>%
    filter(QC_basisOfRecord == "human") 
  
  all_measo_wd_hum <- all_measo_wd_hum %>%
    filter(!is.na(focalGroup)) %>% 
    group_by(focalGroup) %>%
    dplyr::summarise(N = n() / nrow(all_measo_wd_hum) * 100) %>%
    left_join(group_corresp) %>%
    na.omit() %>%
    arrange(N) %>%
    dplyr::mutate(
      focalGroup = factor(focalGroup, focalGroup),
      fG = factor(fG, fG),
      focalGroupID = row_number() %>% as.factor()
    ) %>% 
    return()
} 

# Figure 5 ---------------------------------------------------------

measo_fig5 <- function(all_measo_wd) {

  # Stats per group ---------------------------------------------------------

  ## focal groups
  nb_records_per_gp <- nb_records_per_gp_fn(all_measo_wd)

  nb_cells_per_gp <- all_measo_wd %>%
    filter(
      QC_basisOfRecord == "human",
      !is.na(focalGroup)
    ) %>%
    group_by(focalGroup) %>%
    dplyr::summarise(ext_3deg = length(unique(ea_cell_3deg)) / sum(ncells_measo$ncells) * 100) %>%
    left_join(group_corresp) %>%
    na.omit() %>%
    arrange(ext_3deg) %>%
    dplyr::mutate(
      focalGroup = factor(focalGroup, nb_records_per_gp$focalGroup),
      fG = factor(fG, nb_records_per_gp$fG)
    )

  ## benthic vs. pelagic
  nb_records_per_gp_benthic_pelagic <- all_measo_wd %>%
    group_by(benthic_pelagic) %>%
    dplyr::summarise(N = n() / nrow(all_measo_wd) * 100) %>%
    na.omit() %>%
    arrange(N) %>%
    dplyr::mutate(benthic_pelagic = factor(benthic_pelagic, benthic_pelagic))

  nb_cells_per_gp_benthic_pelagic <- all_measo_wd %>%
    group_by(benthic_pelagic) %>%
    dplyr::summarise(ext_3deg = length(unique(ea_cell_3deg)) / sum(ncells_measo$ncells) * 100) %>%
    na.omit() %>%
    arrange(ext_3deg) %>%
    dplyr::mutate(benthic_pelagic = factor(benthic_pelagic, benthic_pelagic))

  ## all combined
  nb_records_per_gp_all <- rbind(
    nb_records_per_gp %>%
      dplyr::select(focalGroup, N) %>%
      dplyr::mutate(group_type = "other focal groups"),
    nb_records_per_gp_benthic_pelagic %>%
      dplyr::select(benthic_pelagic, N) %>%
      dplyr::rename(focalGroup = benthic_pelagic) %>%
      dplyr::mutate(group_type = "benthic vs. pelagic") %>%
      filter(focalGroup != "neither")
  )

  nb_cells_per_gp_all <- rbind(
    nb_cells_per_gp %>%
      dplyr::select(focalGroup, ext_3deg) %>%
      dplyr::mutate(group_type = "other focal groups"),
    nb_cells_per_gp_benthic_pelagic %>%
      dplyr::select(benthic_pelagic, ext_3deg) %>%
      dplyr::rename(focalGroup = benthic_pelagic) %>%
      dplyr::mutate(group_type = "benthic vs. pelagic") %>%
      filter(focalGroup != "neither")
  )

  # Figures -----------------------------------------------------------------

  plot_rec <- nb_records_per_gp_all %>%
    ggplot() +
    geom_bar(aes(focalGroup, weight = N, fill = group_type)) +
    ylab("% of records") +
    xlab("") +
    coord_flip() +
    facet_grid(group_type ~ ., space = "free", scales = "free") +
    scale_fill_manual(values = c(
      "benthic vs. pelagic" = "midnightblue",
      "other focal groups" = "gray32"
    )) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "none"
    )

  plot_spat <- nb_cells_per_gp_all %>%
    ggplot() +
    geom_bar(aes(focalGroup, weight = ext_3deg, fill = group_type)) +
    ylab("% of grid cells sampled\n(3 degrees x 3 degrees)") +
    xlab("") +
    coord_flip() +
    facet_grid(group_type ~ ., space = "free", scales = "free") +
    scale_fill_manual(values = c(
      "benthic vs. pelagic" = "midnightblue",
      "other focal groups" = "gray32"
    )) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "none"
    )

  p <- cowplot::plot_grid(plot_rec, plot_spat, ncol = 2, labels = c("A", "B"))

  ggsave(paste0(fig_dir, "fig5_coverage.png"), width = 9, height = 5)

  return(p)
}


# Figures S6 - S7 - per MEASO region---------------------------------------

map_measo <- measo_map_fn(measo)

plot_N_fn <- function(measo_str, axis = "N", sdf = nb_records_per_gp_measo, labs = c("", ""), MM_fit_tot = NULL, accu_curves = NULL, lims = NULL) {
  if (axis == "N") {
    data_plot <- sdf %>%
      filter(
        str_detect(QC_measo, measo_str),
        !is.na(fG)
      )
    p <- ggplot(data_plot) +
      geom_bar(aes(fG, weight = N, fill = QC_measo)) +
      ylim(0, max(sdf$Nabs_zone / sdf$Ntot) * 100) +
      geom_text(y = 35, x = "Anne", label = paste(scales::scientific(data_plot$Ntot[1]), " records"), size = 1.6) +
      coord_flip()
  } else if (axis == "Ncell") {
    p <- ggplot(sdf %>%
      filter(
        str_detect(QC_measo, measo_str),
        !is.na(fG)
      )) +
      geom_bar(aes(fG, weight = ext_3deg, fill = QC_measo)) +
      ylim(0, 110) +
      coord_flip()
  } else if (axis == "accuNtax") {
    p <- ggplot(data = MM_fit_tot %>% filter(zone == measo_str)) +
      geom_line(aes(x = N / 1000, y = Ntax, colour = zone)) +
      geom_point(
        data = accu_curves %>%
          dplyr::mutate(zone = str_sub(QC_measo, 1, 2)) %>%
          filter(zone == measo_str),
        aes(x = N / 1000, y = Ntax, colour = zone), alpha = 0.5
      )
    if (!is.null(lims)) {
      p <- p + xlim(0, lims$xmax) + ylim(0, lims$ymax)
    }
  }


  p <- p +
    ylab(labs[1]) + xlab(labs[2]) +
    scale_fill_manual(values = measo_colours) +
    scale_colour_manual(values = c(
      "AO" = "#a6341c",
      "CI" = "#a9a320",
      "EI" = "#21aa55",
      "WP" = "#2651b5",
      "EP" = "#a72386"
    )) +
    theme_bw() +
    theme(
      plot.background = element_rect(colour = "black"),
      legend.position = "none",
      panel.grid = element_blank(),
      text = element_text(size = 7)
    )

  return(p)
}

map_measo_coverage <- function(axis = "Ncell", sdf = nb_records_per_gp_measo, labs = c("", ""), MM_fit_tot = NULL, accu_curves = NULL, lims = NULL) {
  final_map <- ggdraw() +
    draw_plot(map_measo + theme(legend.position = "none"), x = 0.0, y = 0.02, width = 1, height = 0.96) +
    draw_plot(plot_N_fn("AO",
      axis = axis, sdf = sdf, labs = labs,
      MM_fit_tot = MM_fit_tot, accu_curves = accu_curves, lims = lims
    ),
    x = 0.24, y = 0.72, width = 0.28, height = 0.26
    ) +
    draw_plot(plot_N_fn("CI",
      axis = axis, sdf = sdf, labs = labs,
      MM_fit_tot = MM_fit_tot, accu_curves = accu_curves, lims = lims
    ),
    x = 0.59, y = 0.53, width = 0.28, height = 0.26
    ) +
    draw_plot(plot_N_fn("EI",
      axis = axis, sdf = sdf, labs = labs,
      MM_fit_tot = MM_fit_tot, accu_curves = accu_curves, lims = lims
    ),
    x = 0.57, y = 0.18, width = 0.28, height = 0.26
    ) +
    draw_plot(plot_N_fn("WP",
      axis = axis, sdf = sdf, labs = labs,
      MM_fit_tot = MM_fit_tot, accu_curves = accu_curves, lims = lims
    ),
    x = 0.25, y = 0.09, width = 0.28, height = 0.26
    ) +
    draw_plot(plot_N_fn("EP",
      axis = axis, sdf = sdf, labs = labs,
      MM_fit_tot = MM_fit_tot, accu_curves = accu_curves, lims = lims
    ),
    x = 0.04, y = 0.37, width = 0.28, height = 0.26
    ) +
    theme(plot.background = element_rect(fill = "white"))

  return(final_map)
}

measo_recs <- function(all_measo_wd) {
  all_measo_wd <- all_measo_wd %>%
    dplyr::mutate(zone = str_sub(QC_measo, 1, 2))

  measo_stats <- all_measo_wd %>%
    group_by(zone) %>%
    dplyr::summarise(nrecs = n())
  return(measo_stats)
}

measo_figS6 <- function(all_measo_wd) {
  measo_stats <- measo_recs(all_measo_wd)
  nb_records_per_gp <- nb_records_per_gp_fn(all_measo_wd)

    ## Nb of records per group in each MEASO area
  nb_records_per_gp_measo <- all_measo_wd %>%
    dplyr::mutate(zone = str_sub(QC_measo, 1, 2)) %>%
    left_join(measo_stats) %>%
    group_by(focalGroup, QC_measo) %>%
    dplyr::summarise(
      Nabs = n(),
      Ntot = nrecs[1],
      N = n() / nrecs[1] * 100
    ) %>%
    na.omit() %>%
    dplyr::mutate(focalGroup = factor(focalGroup, nb_records_per_gp$focalGroup)) %>%
    left_join(nb_records_per_gp %>% dplyr::select(focalGroup, focalGroupID, fG)) %>%
    dplyr::mutate(QC_measo = factor(QC_measo, levels = rev(c("AOA", "AOS", "AON", "CIA", "CIS", "CIN", "EIA", "EIS", "EIN", "WPA", "WPS", "WPN", "EPA", "EPS", "EPN")))) %>%
    dplyr::mutate(zone = str_sub(QC_measo, 1, 2)) %>%
    na.omit()

  nb_records_per_gp_measo_zone <- nb_records_per_gp_measo %>%
    group_by(focalGroup, zone) %>%
    dplyr::summarise(Nabs_zone = sum(Nabs))
  nb_records_per_gp_measo <- nb_records_per_gp_measo %>%
    left_join(nb_records_per_gp_measo_zone)

  ## Mapping
  final_map_records <- map_measo_coverage("N", sdf = nb_records_per_gp_measo, labs = c("% of records", ""))
  ggsave(paste0(fig_dir, "figS6_measo_groups_N.png"), plot = final_map_records, width = 7, height = 7)
  return(plot)
}

measo_figS7 <- function(all_measo_wd) {
  measo_stats <- measo_recs(all_measo_wd)
  nb_records_per_gp <- nb_records_per_gp_fn(all_measo_wd)
  
  ## Nb of cells per group in each MEASO area
  nb_cells_per_gp_measo <- all_measo_wd %>%
    left_join(ncells_measo) %>%
    group_by(focalGroup, QC_measo, ncells, ncells_zone) %>%
    dplyr::summarise(ext_3deg = length(unique(ea_cell_3deg))) %>%
    dplyr::mutate(ext_3deg = ext_3deg / ncells_zone * 100) %>%
    na.omit() %>%
    dplyr::mutate(focalGroup = factor(focalGroup, nb_records_per_gp$focalGroup)) %>%
    left_join(nb_records_per_gp %>% dplyr::select(focalGroup, focalGroupID, fG)) %>%
    dplyr::mutate(QC_measo = factor(QC_measo, levels = rev(c("AOA", "AOS", "AON", "CIA", "CIS", "CIN", "EIA", "EIS", "EIN", "WPA", "WPS", "WPN", "EPA", "EPS", "EPN"))))

  ## Mapping
  final_map_cells <- map_measo_coverage("Ncell", sdf = nb_cells_per_gp_measo, labs = c("% of cells", ""))
  ggsave(paste0(fig_dir, "figS7_measo_groups_coverage.png"), plot = final_map_cells, width = 7, height = 7)
  return(plot)
}

# Michaelis-Menten --------------------------------------------------------

# install.packages("drc")
library(drc)

MM_fit <- function(df, fg = NA) {
  if (!is.na(fg)) {
    mm <- df %>%
      filter(focalGroup == fg)
  } else {
    mm <- df
  }
  
  mm$Ntax <- mm$Ngen
  
  model.drm <- drm(Ntax ~ N, data = mm, fct = MM.2())
  
  mml <- data.frame(N = seq(0, max(mm$N), length.out = 100))
  mml$Ntax <- predict(model.drm, newdata = mml)
  
  mmasym <- data.frame(N = max(mm$N) * 1000)
  mmasym$Ntax <- predict(model.drm, newdata = mmasym)
  
  id_min <- which.min(abs(mml$Ntax - mmasym$Ntax * 0.75))
  
  return(data.frame(mml,
                    focalGroup = fg,
                    asymptote = mmasym$Ntax,
                    threshold = mml$N[id_min]
  ))
}

accu_curves_fn <- function(all_measo_wd, basOfRec, benthopel) {
  if (benthopel %in% c("Benthic", "Pelagic")) {
    accu_curves_3deg_tot <- all_measo_wd %>%
      filter(str_detect(benthic_pelagic,benthopel))
  } else {
    accu_curves_3deg_tot <- all_measo_wd
  }
  
  accu_curves_3deg_tot <- accu_curves_3deg_tot %>%
    filter(QC_basisOfRecord == basOfRec) %>%
    group_by(ea_cell_3deg, QC_measo) %>%
    dplyr::summarise(
      N = n(),
      Nsp = length(unique(species_final)),
      Ngen = length(unique(genus_final)),
      Ntax = Ngen
    ) 
  
  MM_fit_tot <- c("EI", "AO", "WP", "CI", "EP") %>%
    plyr::ldply(., function(z) {
      data.frame(
        zone = z,
        MM_fit(accu_curves_3deg_tot %>% filter(str_sub(QC_measo, 1, 2) == z))
      )
    })
  
  lims <- data.frame(ymax = max(c(accu_curves_3deg_tot$Ntax, MM_fit_tot$Ntax), na.rm = TRUE),
                     xmax = max(accu_curves_3deg_tot$N, na.rm = TRUE)/1000)
  
  final_map_accuNtax <- map_measo_coverage("accuNtax", sdf = NULL, labs = c("N genera", "thousands of records"),
                                           MM_fit_tot = MM_fit_tot, accu_curves = accu_curves_3deg_tot, lims = lims) +
    ggtitle(benthopel)
  
  return(final_map_accuNtax)
}