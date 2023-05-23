rm(list = ls())

library(tidyverse)
library(cowplot)
library(raster)
library(data.table)
library(ggnewscale)
library(sf)
library(measoshapes)
library(SOmap)
library(dssatr)
library(exactextractr)

## directories
fig_dir <- "../../results/ms1_figures/"
data_dir <- "../../data/"
results_dir <- "../../results/"

# Data loading ------------------------------------------------------------

## grids
dist <- geosphere::distGeo(c(0, -30), c(0, -90)) ## distance at 30 S
eagrid <- raster(extent(-dist, dist, -dist, dist),
  res = 100000, crs = "+proj=laea +lat_0=-90 +datum=WGS84"
) ## equal area grid
eagrid_3deg <- raster(extent(-180, 180, -90, -30),
  res = 3, crs = st_crs(4326)$proj4string
)

## observation data
all_measo <- fread(paste0(data_dir, "edited/all_measo_land_worms_date_dupli.csv") )
all_measo_wd <- data.table::fread(paste0(data_dir, "edited/all_measo_wd_land_worms_date_dupli_focalgps_cells.csv"))

## MEASO shapefile
measo <- measoshapes::measo_regions05_ll %>% 
  st_set_crs(4326) %>%
  filter(! str_detect(name,"T")) %>%
  dplyr::group_by(name) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "keep") %>%
  ungroup() %>% 
  dplyr::mutate(
    area = st_area(.) %>% as.numeric(),
    QC_measo = name,
    ID = 1:n()
  ) %>%
  dplyr::select(-name)

## MEASO basemap
source("map_helper.R")
# SOcrs(crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
SOmap()

## historic stations from qAntarctica
stations <- st_read(paste0(data_dir, "qAntarctica_histogric_stations.shp")) %>% 
  filter(is.na(YEAR_END) | YEAR_END > 1950) ## removing closed stations

## sea ice extent
ice_min <- st_read(paste0(data_dir, "qAntarctica_ice_feb_med.shp"))
ice_max <- st_read(paste0(data_dir, "qAntarctica_ice_sept_med.shp"))


# Figure 1 - Sankey diagram -----------------------------------------------

source("figure1_sankeyplot.R")
fig1 <- measo_fig1(all_measo, fig_name = "fig1_Sankey")
rm(all_measo)

# Figure 2 - temporal distribution ----------------------------------------

source("figure2_temporaldistri.R")
fig2 <- measo_fig2(all_measo_wd)

# Figure 3 - spatial distribution --------------------------------------

my_colors = terrain.colors(80, rev = TRUE)
## Human observations
rr <- obs2raster(all_measo_wd %>% filter(QC_basisOfRecord == "human"), eagrid)
png(paste0(fig_dir, "fig3A_map_n_human.png"),
    width = 1.2*480, height = 1.2*480)
SOmap(trim = -30)
SOplot(log10(rr), legend = FALSE, col = my_colors)
SOleg(log10(rr), position = "topright", col = my_colors,
      breaks = seq(0,4,1), tcex = 1.5,
      trim = -30, label = "log10(number of\nobservations)", type = "continuous")
dev.off()
## Machine observations
rr <- obs2raster(all_measo_wd %>% filter(QC_basisOfRecord == "machine"), eagrid)
png(paste0(fig_dir, "fig3B_map_n_machine.png"),
    width = 1.2*480, height = 1.2*480)
SOmap(trim = -30)
SOplot(log10(rr), legend = FALSE, col = my_colors)
SOleg(log10(rr), position = "topright", col = my_colors, 
      breaks = seq(0,4,1), tcex = 1.5,
      trim = -30, label = "log10(number of\nobservations)", type = "continuous")
dev.off()


# Figure 4 and Suppl Figure 5 - MEASO temporal distributions ---------------------------------

source("figure4_supplfigure3_measomap.R")

fig4 <- measo_fig4_figS3(all_measo_wd %>% filter(QC_basisOfRecord == "human"))
ggsave(paste0(fig_dir, "fig4_measo_temporal_human.png"), fig4, height = 7, width = 7, dpi = 300)

figS3 <- measo_fig4_figS3(all_measo_wd %>% filter(QC_basisOfRecord == "machine"))
ggsave(paste0(fig_dir, "figS3_measo_temporal_machine.png"), figS3, height = 7, width = 7)

# Figure 5 and Suppl Figures 6 and 7 - data + spatial coverage and Figures S8-S10 - accumulation curves --------------------------------------

source("figure5_supplfigures6-10_coverage_accu.R")
fig5 <- measo_fig5(all_measo_wd)
figS6 <- measo_figS6(all_measo_wd %>% filter(QC_basisOfRecord == "human"))
figS7 <- measo_figS7(all_measo_wd %>% filter(QC_basisOfRecord == "human"))

### !!! TO CORRECT - unit areas or total number of records should be common within a MEASO sector (i.e. three regions)

accu_benthic <- accu_curves_fn(all_measo_wd, "human", "Benthic")
accu_pelagic <- accu_curves_fn(all_measo_wd, "human", "Pelagic")
accu_human <- accu_curves_fn(all_measo_wd, "human", "All")

ggsave(paste0(fig_dir, "figS8_human.png"), accu_human, width = 7, height = 7)
ggsave(paste0(fig_dir, "figS9_human_benthic.png"), accu_benthic, width = 7, height = 7)
ggsave(paste0(fig_dir, "figS10_human_pelagic.png"), accu_pelagic, width = 7, height = 7)

# Figures S1 and S2 - research stations and ice coverage -------------------------------------------

source("supplfigures1&2_researchstations_ice.R")
figS1 <- measo_figS1(all_measo_wd, stations, eagrid)
figS2 <- measo_figS2(all_measo_wd, ice_min, ice_max, eagrid)

# Figures S4 and S5 - number of phyla through time and  taxonomic depth --------------------------------

source("supplfigures4&5_phylatime_taxodepth.R")
figS4 <- measo_figS4(all_measo_wd)
figS5 <- measo_figS5(all_measo_wd)



# Supplementary figure 1 -------------------------------------------------------

# % of data that are DNA reads
sum(all_measo_wd$organismQuantityType == "DNAsequencereads", na.rm=TRUE)/nrow(all_measo_wd)*100

# % of DNA reads identified at the species level
sum(all_measo_wd$organismQuantityType == "DNAsequencereads" & !is.na(all_measo_wd$species_final), na.rm=TRUE)/sum(all_measo_wd$organismQuantityType == "DNAsequencereads", na.rm=TRUE)*100

# % of all data identified at the species level
sum(!is.na(all_measo_wd$species_final))/nrow(all_measo_wd)*100

# DNA reads spatial distribution
rr <- obs2raster(all_measo_wd %>% filter(organismQuantityType == "DNAsequencereads"), eagrid)
png(paste0(fig_dir, "figS_map_n_dna.png"), width = 700, height = 700, units = "px")
SOmap(trim = -30)
SOplot(log10(rr), legend = FALSE, col = my_colors)
SOleg(log10(rr), position = "topright", col = my_colors,
      breaks = seq(0,4,1), tcex = 0.8,
      trim = -30, label = "log10(number of\nobservations)", type = "continuous")
dev.off()

source("figure_supp_dna_reads.R")
figS <- measo_figS_dna(all_measo_wd %>% filter(organismQuantityType == "DNAsequencereads"))



