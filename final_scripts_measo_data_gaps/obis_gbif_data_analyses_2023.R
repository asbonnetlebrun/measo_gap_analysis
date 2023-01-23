library(tidyverse)
library(data.table)
library(sf)
library(SOmap)

## directories
data_dir <- "../../data/"
results_dir <- "../../results/"

# All data after QC and working dataset -------------------------------------------------------

source("taxon_matching_final.R")

all_measo <- fread(paste0(data_dir, "edited/all_measo_land_worms_date_dupli.csv"))

all_measo_wd <- all_measo %>%
  filter(
    QC_onLand == FALSE,
    QC_notAbsent == TRUE,
    QC_hasScientificNameInWorms == "in WoRMS",
    QC_duplicatesCat_taxCheck == "unique",
    QC_basisOfRecord != "fossil"
  ) %>%
  filter(year2 >= 1773, year2 < 2023) %>%
  dplyr::select(-contains("QC"), QC_basisOfRecord, QC_measo)

all_measo_wd <- all_measo_wd %>%
  dplyr::mutate(
    # kingdom_final = ifelse(is.na(kingdom_worms), kingdom, kingdom_worms),
    phylum_final = ifelse(is.na(phylum_worms), phylum, phylum_worms),
    class_final = ifelse(is.na(class_worms), class, class_worms),
    order_final = ifelse(is.na(order_worms), order, order_worms),
    family_final = ifelse(is.na(family_worms), family, family_worms),
    genus_final = ifelse(is.na(genus_worms), genus, genus_worms),
    species_final = ifelse(tolower(taxonRank) == "species" | tolower(rank) == "species",
      ifelse(is.na(valid_name), species, valid_name), NA
    )
  )

# Matching with focal groups ----------------------------------------------

focal_gp <- read_csv(paste0(results_dir, "focal_groups_final.csv"))

# get aphiaIDs for those taxa in each group on WoRMS
aphiaIDs_simple <- taxon_match_fn(focal_gp$Taxa) %>%
  left_join(focal_gp %>% dplyr::rename(input_name = Taxa)) %>%
  dplyr::group_by(scientificname, valid_name, input_name, match_type, status, FocalGroup, TaxonomicLevel) %>%
  dplyr::summarise(AphiaID = AphiaID[1]) %>%
  ungroup()

## Match each taxon in the working dataset with a focal group based on valid_names from WoRMS
all_measo_wd$focalGroup <- NA

for (k in 1:nrow(aphiaIDs_simple)) {
  aphiaIDs_sub <- aphiaIDs_simple[k, ]

  # taxa that have an unusual taxonomic level associated -> fetch the corresponding classes belonging to this taxa
  if (aphiaIDs_sub$TaxonomicLevel %in% c("unranked?", "division")) {
    res <- taxize::worms_downstream(aphiaIDs_sub$AphiaID, "class", intermediate = TRUE)$target %>%
      dplyr::mutate(input = aphiaIDs_sub$scientificname)
    all_measo_wd$focalGroup[all_measo_wd$class_final %in% res$name] <- aphiaIDs_sub$FocalGroup
    rm(res)
  } else {
    # taxa identified at the kingdom level
    if (aphiaIDs_sub$TaxonomicLevel == "kingdom") {
      all_measo_wd$focalGroup[all_measo_wd$kingdom == aphiaIDs_sub$valid_name] <- aphiaIDs_sub$FocalGroup
    } else {
      # taxa identified at the phylum, class, order, family, genus or species level
      tx_f <- paste0(aphiaIDs_sub$TaxonomicLevel, "_final")
      tx_gp <- all_measo_wd[, ..tx_f] == aphiaIDs_sub$valid_name
      all_measo_wd$focalGroup[tx_gp] <- aphiaIDs_sub$FocalGroup
    }
  }
}

# Create grids and attribute data to grid cells ------------------------------------------------------------

## 100km * 100km grid
dist <- geosphere::distGeo(c(0, -30), c(0, -90)) ## distance at 30 S
eagrid <- raster(extent(-dist, dist, -dist, dist),
  res = 100000, crs = "+proj=laea +lat_0=-90 +datum=WGS84"
) ## equal area grid

## 3 degress * 3 degrees grid
eagrid_3deg <- raster(extent(-180, 180, -90, -30),
  res = 3, crs = st_crs(4326)$proj4string
)

coord_cols <- c("decimalLongitude", "decimalLatitude")

all_measo_wd$ea_cell_3deg <- raster::cellFromXY(eagrid_3deg, rgdal::project(as.matrix(all_measo_wd[, ..coord_cols]), projection(eagrid_3deg)))
all_measo_wd$ea_cell <- raster::cellFromXY(eagrid, rgdal::project(as.matrix(all_measo_wd[, ..coord_cols]), projection(eagrid_3deg)))


# Decade ------------------------------------------------------------------

mid_decade <- data.frame(
  mid_decade = rep(seq(1945, 2015, 10), each = 10),
  year = seq(1940, 2019, 1)
) %>%
  mutate(decade = paste(mid_decade - 5, mid_decade + 4, sep = "-"))

all_measo_wd <- all_measo_wd %>%
  dplyr::select(-contains("mid_decade")) %>%
  left_join(mid_decade)


# Benthic vs.  pelagic taxa -----------------------------------------------

pelagic_benthic <- read.csv(paste0(data_dir, "Pelagic_Benthic_atlas_Species_For_Maddie_56085.csv"), sep = ";") %>%
  arrange(ScientificName_accepted)

pelagic_benthic <- pelagic_benthic %>%
  dplyr::select(ScientificName_accepted, Group) %>%
  rename(
    valid_name = ScientificName_accepted,
    benthic_pelagic = Group
  ) %>%
  unique() %>%
  group_by(valid_name) %>%
  dplyr::summarise(benthic_pelagic = paste(unique(benthic_pelagic), collapse = "_"))

all_measo_wd <- all_measo_wd %>%
  left_join(pelagic_benthic)

# Functional groups from WoRMS --------------------------------------------

run_fungps <- FALSE

source("get_functional_groups.R")

if (run_fungps) {
  valid_ids <- read_csv(paste0(results_dir, "valid_ids_list.csv"))

  block_size <- 10000
  fg_all <- NULL

  for (m in 1:ceiling(n_taxa / block_size)) {
    # tic(paste("\n block", m*block_size))
    start <- (m - 1) * block_size + 1
    end <- min(c(m * block_size, n_taxa))
    fg_sub <- valid_ids[start:end, ] %>%
      group_by(valid_AphiaID) %>%
      do(get_worms_fgrp(AphiaID = .$valid_AphiaID))
    if (is.null(fg_all)) {
      fg_all <- fg_sub
    }
    fg_all <- rbind(fg_all_bis, fg_sub)
    # toc()
  }
  fg_all %>% write_csv(paste0(results_dir, "functionalgroups_keepall.csv"))
}

functional_groups <- read_csv(paste0(results_dir, "functionalgroups_keepall.csv"))

phyto <- functional_groups %>%
  filter(if_any(everything(), ~ (.x == "phytoplankton"))) %>%
  dplyr::select(valid_name) %>%
  mutate(phytoplankton = TRUE) %>%
  unique()
zoo <- functional_groups %>%
  filter(if_any(everything(), ~ (.x == "zooplankton"))) %>%
  dplyr::select(valid_name) %>%
  mutate(zooplankton = TRUE) %>%
  unique()

all_measo_wd <- all_measo_wd %>%
  left_join(phyto) %>%
  left_join(zoo)

all_measo_wd %>% data.table::fwrite(paste0(data_dir, "edited/all_measo_wd_land_worms_date_dupli_focalgps_cells.csv"), na = NA)
