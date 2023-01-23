measo_colours <- c("AOA" = "#ff5733",
  "AOS" = "#a6341c",
  "AON" = "#641e10",
  "CIA" = "#fff633",
  "CIS" =  "#a9a320",
  "CIN" =  "#757017",
  "EIA" = "#33ff80",
  "EIS" =  "#21aa55",
  "EIN" =  "#156b36",
  "WPA" = "#3371ff",
  "WPS" =  "#2651b5",
  "WPN" =  "#17316c",
  "EPA" = "#ff33cc",
  "EPS" =  "#a72386",
  "EPN" =  "#5c144a")

measo_map_fn <- function(measo) {
  SOmap()
  p <- ggplot() + 
    geom_sf(data = measo %>% smoothr::densify(n=40), #max_distance = 1),
            aes(fill = QC_measo), colour=NA) +
    scale_fill_manual(values = measo_colours,
                      name = "MEASO area") +
    geom_sf(data=SOmap(trim = -40)$coastline[[1]]$plotargs$x) + 
    geom_sf(data=SOmap(trim = -40)$border[[1]]$plotargs$x %>% st_as_sf()) + 
    coord_sf(crs=SOcrs()) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank())
  return(p)
}

obs2raster <- function(all_measo_wd_sub,eagrid) {
  coord_cols <- c("decimalLongitude", "decimalLatitude")
  all_measo_wd_sub$ea_cell <- cellFromXY(eagrid, rgdal::project(as.matrix(all_measo_wd_sub[,..coord_cols]), projection(eagrid)))
  rec_geo_N_samples <- group_by(all_measo_wd_sub, ea_cell) %>% dplyr::summarise(val = dplyr::n()) # number of samples per cell
  rr <- raster(eagrid)
  rr[rec_geo_N_samples$ea_cell] <- rec_geo_N_samples$val 
  return(rr)  
}

obs2grid <- function(all_measo_wd_sub, eagrid) {
  # require(dssatr)
  rr <- obs2raster(all_measo_wd_sub, eagrid)
  test_grid <-st_as_sf(rr, as_points = FALSE, merge = FALSE) # converting a raster to a sf object requires dssatr to be loaded
  test_grid$ext <- exact_extract(rr, test_grid, "mean")
  return(test_grid)
}