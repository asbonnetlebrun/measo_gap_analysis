measo_figS4 <- function(all_measo_wd, ice_min, ice_max) {
  
  # Map winter records and ice ----------------------------------------------
  
  all_measo_wd_obs <- all_measo_wd %>% 
    filter(QC_basisOfRecord == "human") %>% 
    filter(between(month2, 6, 9))
  
  test_grid <- obs2grid(all_measo_wd_obs, eagrid) ## SHOULD BE COMMON TO FIG. 1??
  
  map_ice_winter <- drivers_map(test_grid) 
  
  map_ice_winter <- map_ice_winter +
    geom_sf(data =ice_max %>%
              st_transform(SOcrs()) %>% st_union(),
            size = 0.4,
            fill = NA,
            colour = "blue") 
  
  ggsave("results/map_human_obs_winter_ice.png", plot = map_ice_winter, height = 7, width = 8, dpi = 300)
  
  # Map summer records and ice ----------------------------------------------
  
  all_measo_wd_obs <- all_measo_wd %>% 
    filter(QC_basisOfRecord == "human") %>% 
    filter(!between(month2, 6, 9))
  
  rr <- obs2raster(all_measo_wd_obs, eagrid)
  test_grid <-st_as_sf(rr, as_points = FALSE, merge = FALSE) # converting a raster to a sf object requires dssatr to be loaded
  test_grid$ext <- exact_extract(rr, test_grid, "mean")
  
  
  map_ice_summer <- ggplot() + 
    geom_sf(data = test_grid, 
            aes(fill = log10(ext)), colour=NA) +
    geom_sf(data =ice_min %>%
              st_transform(SOcrs()) %>% st_union(),
            size = 0.4,
            fill = NA,
            colour = "blue") +
    geom_sf(data=SOmap(trim = -35)$coastline[[1]]$plotargs$x) + 
    geom_sf(data=SOmap(trim = -35)$border[[1]]$plotargs$x %>% 
              st_as_sf(),
            fill = "grey") +  
    geom_sf(data=SOmap(trim = -35)$border[[1]]$plotargs$x %>% 
              st_as_sf() %>% 
              filter((x/2 - round(x/2))==0),
            fill = "black") +
    coord_sf(crs=SOcrs()) +
    scale_fill_gradientn(colours = spirited) +
    # scale_fill_viridis_c() + #direction = -1) +
    labs(fill="log10(N)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  ggsave("results/map_human_obs_summer_ice.png", plot = map_ice_summer, height = 7, width = 8, dpi = 300)
  
  ggsave("results/map_human_obs_ice.svg", 
         plot = cowplot::plot_grid(map_ice_winter, map_ice_summer, ncol = 2), 
         height = 5, width = 9, dpi = 300)
  
  
  # Maps machine winter records and ice ---------------------------------------
  
  all_measo_wd_obs <- all_measo_wd %>% 
    filter(QC_basisOfRecord == "machine") %>% 
    filter(between(month2, 6, 9))
  
  rr <- obs2raster(all_measo_wd_obs, eagrid)
  test_grid <-st_as_sf(rr, as_points = FALSE, merge = FALSE) # converting a raster to a sf object requires dssatr to be loaded
  test_grid$ext <- exact_extract(rr, test_grid, "mean")
  
  map_ice_winter_machine <- ggplot() + 
    geom_sf(data = test_grid, 
            aes(fill = log10(ext)), colour=NA) +
    geom_sf(data =ice_max %>%
              st_transform(SOcrs()) %>% st_union(),
            size = 0.4,
            fill = NA,
            colour = "blue") +
    geom_sf(data=SOmap(trim = -35)$coastline[[1]]$plotargs$x) + 
    geom_sf(data=SOmap(trim = -35)$border[[1]]$plotargs$x %>% 
              st_as_sf(),
            fill = "grey") +  
    geom_sf(data=SOmap(trim = -35)$border[[1]]$plotargs$x %>% 
              st_as_sf() %>% 
              filter((x/2 - round(x/2))==0),
            fill = "black") +
    coord_sf(crs=SOcrs()) +
    scale_fill_gradientn(colours = spirited) +
    # scale_fill_viridis_c() + #direction = -1) +
    labs(fill="log10(N)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  ggsave("results/map_machine_obs_winter_ice.png", plot = map_ice_winter_machine, height = 7, width = 8, dpi = 300)
  
  # Map machine summer records and ice ----------------------------------------------
  
  all_measo_wd_obs <- all_measo_wd %>% 
    filter(QC_basisOfRecord == "machine") %>% 
    filter(!between(month2, 6, 9))
  
  rr <- obs2raster(all_measo_wd_obs, eagrid)
  test_grid <-st_as_sf(rr, as_points = FALSE, merge = FALSE) # converting a raster to a sf object requires dssatr to be loaded
  test_grid$ext <- exact_extract(rr, test_grid, "mean")
  
  
  map_ice_summer_machine <- ggplot() + 
    geom_sf(data = test_grid, 
            aes(fill = log10(ext)), colour=NA) +
    geom_sf(data =ice_min %>%
              st_transform(SOcrs()) %>% st_union(),
            size = 0.4,
            fill = NA,
            colour = "blue") +
    geom_sf(data=SOmap(trim = -35)$coastline[[1]]$plotargs$x) + 
    geom_sf(data=SOmap(trim = -35)$border[[1]]$plotargs$x %>% 
              st_as_sf(),
            fill = "grey") +  
    geom_sf(data=SOmap(trim = -35)$border[[1]]$plotargs$x %>% 
              st_as_sf() %>% 
              filter((x/2 - round(x/2))==0),
            fill = "black") +
    coord_sf(crs=SOcrs()) +
    scale_fill_gradientn(colours = spirited) +
    # scale_fill_viridis_c() + #direction = -1) +
    labs(fill="log10(N)") +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  
  ggsave("results/map_machine_obs_summer_ice.png", plot = map_ice_summer_machine, height = 7, width = 8, dpi = 300)
  
  ggsave("results/map_machine_obs_ice.svg", 
         plot = cowplot::plot_grid(map_ice_winter_machine, map_ice_summer_machine, ncol = 2), 
         height = 5, width = 9, dpi = 300)
  
  
  ggsave("results/map_machine_human_obs_ice.png", 
         plot = cowplot::plot_grid(map_ice_winter_machine,
                                   map_ice_summer_machine,
                                   map_ice_winter,
                                   map_ice_summer, 
                                   ncol = 2, labels = c("A","B","C","D")), 
         height = 6, width = 8, dpi = 300)
}

