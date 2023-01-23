gc()

drivers_map <- function(test_grid) {
  map_driver <- ggplot() +
    geom_sf(
      data = test_grid,
      aes(fill = log10(ext)), colour = NA
    ) +
    geom_sf(data = SOmap(trim = -35)$coastline[[1]]$plotargs$x) +
    geom_sf(
      data = SOmap(trim = -35)$border[[1]]$plotargs$x %>%
        st_as_sf(),
      fill = "grey"
    ) +
    geom_sf(
      data = SOmap(trim = -35)$border[[1]]$plotargs$x %>%
        st_as_sf() %>%
        filter((x / 2 - round(x / 2)) == 0),
      fill = "black"
    ) +
    coord_sf(crs = SOcrs()) +
    scale_fill_gradientn(colours = terrain.colors(80, rev = TRUE)) +
    labs(fill = "log10(N)") +
    theme_bw() +
    theme(
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank()
    )
  return(map_driver)
}

station_map <- function(all_measo_wd_obs, stations, eagrid) {
  test_grid <- obs2grid(all_measo_wd_obs, eagrid)

  map_stations <- drivers_map(test_grid) +
    geom_sf(
      data = stations %>%
        st_transform(SOcrs()) %>% st_union(),
      size = 2,
      shape = 8,
      colour = "red", alpha = 0.8
    )

  return(map_stations)
}

measo_figS1 <- function(all_measo_wd, stations, eagrid) {

  ## human
  all_measo_wd_obs <- all_measo_wd %>%
    filter(QC_basisOfRecord == "human")

  map_stations_human <- station_map(all_measo_wd_obs, stations, eagrid)

  ## machine
  all_measo_wd_obs <- all_measo_wd %>%
    filter(QC_basisOfRecord == "machine")

  map_stations_machine <- station_map(all_measo_wd_obs, stations, eagrid)

  ## combined
  map_stations_all <- cowplot::plot_grid(map_stations_human,
    map_stations_machine,
    ncol = 1,
    labels = c("A", "B")
  )

  ggsave(paste0(fig_dir, "figS1_researchstations.png"),
    map_stations_all,
    height = 9, width = 6, dpi = 300
  )

  return(map_stations_all)
}


fig_S2_sub <- function(bas_of_rec, season, ice) {
  all_measo_wd_obs <- all_measo_wd %>%
    filter(QC_basisOfRecord == bas_of_rec)

  if (season == "winter") {
    all_measo_wd_obs <- all_measo_wd_obs %>%
      filter(between(month2, 6, 9))
  } else if (season == "summer") {
    all_measo_wd_obs <- all_measo_wd_obs %>%
      filter(!between(month2, 6, 9))
  }

  test_grid <- obs2grid(all_measo_wd_obs, eagrid) ## SHOULD BE COMMON TO FIG. 1??
  map_ice <- drivers_map(test_grid) +
    geom_sf(
      data = ice %>%
        st_transform(SOcrs()) %>% st_union(),
      size = 0.4,
      fill = NA,
      colour = "blue"
    )
  return(map_ice)
}

measo_figS2 <- function(all_measo_wd, ice_min, ice_max, eagrid) {

  ## human winter
  map_ice_winter_human <- fig_S2_sub(bas_of_rec = "human", season = "winter", ice = ice_max)
  # ggsave("results/map_human_obs_winter_ice.png", plot = map_ice_winter_human, height = 7, width = 8, dpi = 300)

  ## human summer
  map_ice_summer_human <- fig_S2_sub(bas_of_rec = "human", season = "summer", ice = ice_min)
  # ggsave("results/map_human_obs_summer_ice.png", plot = map_ice_summer_human, height = 7, width = 8, dpi = 300)

  ## machine winter
  map_ice_winter_machine <- fig_S2_sub(bas_of_rec = "machine", season = "winter", ice = ice_max)
  # ggsave("results/map_human_obs_winter_ice.png", plot = map_ice_winter_machine, height = 7, width = 8, dpi = 300)

  ## machine summer
  map_ice_summer_machine <- fig_S2_sub(bas_of_rec = "machine", season = "summer", ice = ice_min)
  # ggsave("results/map_machine_obs_summer_ice.png", plot = map_ice_summer_machine, height = 7, width = 8, dpi = 300)

  map_stations_all <- cowplot::plot_grid(
    map_ice_winter_human,
    map_ice_summer_human,
    map_ice_winter_machine,
    map_ice_summer_machine,
    ncol = 2, labels = c("A", "B", "C", "D")
  )

  ggsave(paste0(fig_dir, "figS2_mapice.png"),
    plot = map_stations_all,
    height = 6, width = 8, dpi = 300
  )

  return(map_stations_all)
}
