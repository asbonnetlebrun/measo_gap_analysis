# ## install.packages("remotes")
# remotes::install_github("AustralianAntarcticDivision/measoshapes")
# # install.packages("devtools")
# devtools::install_github("iobis/obistools")
rm(list = ls())

library(robis)
library(rgbif)
library(obistools)
library(tidyverse)
library(finch)
library(data.table)
library(sf)
library(worrms)
library(SOmap)

library(measoshapes)

sf::sf_use_s2(FALSE) # to turn off s2 processing

do_downloads <- FALSE
run_worms <- FALSE

measo_simple <- measoshapes::measo_regions05_ll %>%
  st_set_crs(4326) %>%
  filter(!str_detect(name, "T")) %>%
  mutate(ID = 1:n())

data_dir <- "../../data/"
results_dir <- "../../results/"

# (down)loading OBIS/GBIF data --------------------------------------------

if (do_downloads) {

  ## MEASO shapes for the query
  measo_union_wkt <- measo_simple %>%
    st_union() %>%
    st_as_text()

  ## OBIS
  obis_measo <- occurrence(geometry = measo_union_wkt)
  fwrite(obis_measo, file = paste0(data_dir, "obis/obis_measo_", Sys.time() %>% format.Date("%Y-%m-%d"), ".csv"))

  ## GBIF
  gbif_key <- "0152294-220831081235567"
  # gbif_measo <- occ_download_get(gbif_key, overwrite = TRUE) %>%
  #   occ_download_import()
  gbif_measo <- fread(paste0(data_dir, "gbif/", gbif_key, "/occurrence.txt")) ## occ_download_get doesn't return all occurrences - so directly form the manual download...
  occ_download_meta(gbif_key) %>%
    gbif_citation()
  names(gbif_measo)
  # fwrite(gbif_measo, file=paste0(data_dir, "gbif/gbif_measo_", Sys.time() %>% format.Date("%Y-%m-%d"), ".csv"))
  fwrite(gbif_measo, file = paste0(data_dir, "gbif/gbif_measo_2022-11-10.csv"))
}



if (TRUE) {

  # Select columns of interest ------------

  select_columns <- function(dtb) {
    dtb %>%
      dplyr::select(
        occurrenceID,
        year, month, day, eventDate, # eventTime,
        scientificName, individualCount, organismQuantity,
        depth,
        organismQuantityType, # fieldNumber, modified,
        institutionCode,
        decimalLatitude, decimalLongitude,
        occurrenceStatus, taxonRank,
        species, kingdom, phylum, class, genus, family, order, # subgenus,
        basisOfRecord, datasetName,
        eventID, datasetID, samplingProtocol
      ) %>%
      return()
  }

  obis_measo <- fread(paste0(data_dir, "obis/obis_measo_2022-11-10.csv")) %>%
    select_columns()
  gbif_measo <- fread(paste0(data_dir, "gbif/gbif_measo_2022-11-10.csv")) %>%
    select_columns()

  # Flag terrestrial data points and match with MEASO regions ----------------

  # GSHHG data downloaded from https://www.soest.hawaii.edu/pwessel/gshhg/
  land_subantarctic <- st_read(paste0(data_dir, "basemap/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L1.shp")) %>%
    st_intersection(., measo_simple) %>%
    dplyr::select(-name, -ID)
  land_antarctica <- st_read(paste0(data_dir, "basemap/gshhg-shp-2.3.7/GSHHS_shp/f/GSHHS_f_L6.shp"))
  land <- rbind(land_subantarctic, land_antarctica)
  # f = full resolution
  # L1: boundary between land and ocean, except Antarctica. - for subantarctic islands
  # L5: boundary between Antarctica ice and ocean.
  # L6: boundary between Antarctica grounding-line and ocean.

  spatial_queries <- function(df_measo) {
    df_measo_sf <- df_measo[, .(occurrenceID, decimalLongitude, decimalLatitude)] %>%
      st_as_sf(., coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

    # flagging terrestrial data points
    res <- st_intersects(df_measo_sf, land)
    df_measo$QC_onLand <- (lengths(res) > 0)
    rm(res)

    # identify which MEASO area the record comes from
    df_measo$ID <- st_intersects(df_measo_sf, measo_simple) %>% # for points on the boundary between two regions, take the first region
      map_dbl(., 1) %>% unlist()
    df_measo <- df_measo %>%
      left_join(measo_simple %>% as.data.frame() %>%
        dplyr::select(name, ID)) %>%
      dplyr::select(-ID) %>%
      dplyr::rename(QC_measo = name)
    rm(df_measo_sf)

    return(df_measo)
  }

  obis_measo <- spatial_queries(obis_measo)
  gbif_measo <- spatial_queries(gbif_measo)

  all_measo <- rbind(
    data.frame(obis_measo,
      QC_database = "OBIS"
    ),
    data.frame(gbif_measo,
      QC_database = "GBIF"
    )
  )

  rm(obis_measo)
  rm(gbif_measo)
  gc()

  # Save the 1st QC steps -------------------------------------------------

  all_measo %>% fwrite(paste0(data_dir, "edited/all_measo_land.csv"), na = NA)
}


# Date QCs, counts, and taxon matching with WoRMS -----------------------------------------------

if (TRUE) {
  all_measo <- fread(paste0(data_dir, "edited/all_measo_land.csv"))

  all_measo <- all_measo[, `:=`(
    my_id = 1:.N,
    QC_eventDate = as.Date(eventDate),
    year = as.numeric(year),
    month = as.numeric(month),
    day = as.numeric(day)
  )][, `:=`(
    year_event = lubridate::year(QC_eventDate),
    month_event = lubridate::month(QC_eventDate),
    day_event = lubridate::day(QC_eventDate)
  )][, `:=`(
    year2 = ifelse(is.na(year), year_event, year),
    month2 = ifelse(is.na(month), month_event, month),
    day2 = ifelse(is.na(day), day_event, day),
    QC_individualCount = as.numeric(individualCount)
  )] %>%
    dplyr::mutate(
      across(where(is.character) & !c(scientificName, species), ~ str_replace_all(., " ", "")),
      across(where(is.character), ~ na_if(., ""))
    )

  # to deal with individualCounts recorded as xx + xx + xx...
  all_measo <- all_measo %>%
    group_by(individualCount) %>%
    dplyr::mutate(QC_individualCount = ifelse(
      is.na(QC_individualCount) &
        str_detect(individualCount, "\\+[:digit:]") &
        !str_detect(individualCount, "[:alpha:]"),
      eval(parse(text = paste(individualCount))),
      QC_individualCount
    )) %>%
    ungroup()

  # Merge with taxa matched with WoRMS (separate script) --------------------
  source("taxon_matching_final.R")

  if (run_worms) {
    all_names <- all_measo %>%
      dplyr::select(scientificName) %>%
      unique()

    all_names %>% write.table(paste0(data_dir, "scientific_names_obis_gbif.txt"))

    input_file <- paste0(data_dir, "scientific_names_obis_gbif.txt")
    output_file <- paste0(results_dir, "scientific_names_matching_2022.csv")
    tax_match_from_file(input_file, output_file)
  }

  tax_mtch_all <- read_csv(paste0(results_dir, "scientific_names_matching_2022.csv")) %>%
    filter(!is.na(valid_name)) %>%
    dplyr::mutate(taxonRank2 = "scientificName")

  if (run_worms) {
    missing_namesTMA <- setdiff(unique(all_measo$scientificName), unique(tax_mtch_all$input_name))

    to_check_species <- all_measo %>%
      filter(scientificName %in% missing_namesTMA) %>%
      filter(scientificName != "Biota") %>%
      dplyr::select(scientificName, species) %>%
      unique() %>%
      filter(!is.na(species)) %>%
      filter(scientificName != species)

    to_check_species %>% write_csv(paste0(results_dir, "additional_species_to_check_202211.csv"))
    all_names <- read_csv(paste0(results_dir, "additional_species_to_check_202211.csv"))$species %>%
      as.character()

    block_size <- 500
    tax_mtch_suppl <- taxon_match_iter(all_names, block_size)
    tax_mtch_suppl %>%
      write_csv(paste0(results_dir, "additional_species_checked_202211.csv"))
  }

  tax_mtch_suppl <- read_csv(paste0(results_dir, "additional_species_checked_202211.csv")) %>%
    filter(!is.na(valid_name)) %>%
    dplyr::mutate(taxonRank2 = "species")

  tax_mtch_all <- rbind(
    tax_mtch_all,
    tax_mtch_suppl
  ) %>%
    filter(
      match_type == "exact",
      !str_detect(input_name, "BOLD:")
    ) # to remove Barcode Index Numbers (BINs), as they cannot be matched with WoRMS.

  valid_ids <- tax_mtch_all %>%
    dplyr::select(valid_name, valid_AphiaID) %>%
    unique()
  valid_ids %>% write_csv(paste0(results_dir, "valid_ids_list.csv"))

  nb_matches <- tax_mtch_all %>%
    dplyr::select(input_name, scientificname, valid_name, valid_AphiaID, status, match_type, family, genus) %>%
    unique() %>%
    group_by(input_name) %>%
    dplyr::summarise(
      NVEA = length(unique(valid_name[match_type == "exact" & status == "accepted"]))
    )

  tax_mtch_final <- tax_mtch_all %>%
    filter(input_name %in% filter(nb_matches, NVEA == 1)$input_name & status == "accepted" | # accepted exact matching name for the input names that have this
      input_name %in% filter(nb_matches, NVEA == 0)$input_name) %>% # all non-accepted exact matching names for the input names that have exact matches but no accepted names
    group_by(input_name, phylum) %>%
    filter(row_number() == 1) %>%
    ungroup()

  levelJoin <- function(df, tax_mtch_final, level) {
    tmp <- tax_mtch_final %>%
      filter(taxonRank2 == level) %>%
      dplyr::select(input_name, valid_name, kingdom, phylum, class, order, family, genus, rank, taxonRank2) %>% # , species) %>%
      dplyr::rename(
        kingdom_worms = kingdom,
        phylum_worms = phylum,
        class_worms = class,
        order_worms = order,
        family_worms = family,
        genus_worms = genus
      )
    names(tmp)[names(tmp) == "input_name"] <- level

    if ("valid_name" %in% names(df)) {
      df_valid <- df %>%
        filter(!is.na(valid_name))
      df_no_valid <- df %>%
        filter(is.na(valid_name)) %>%
        dplyr::select(-valid_name, -contains("worms"), -rank, -taxonRank2) %>%
        left_join(tmp)
      df <- rbind(
        df_valid,
        df_no_valid # ,
        # fill=TRUE
      )
    } else {
      df <- df %>%
        left_join(tmp)
    }
    return(df)
  }

  for (l in unique(tax_mtch_final$taxonRank2)) {
    all_measo <- all_measo %>%
      levelJoin(tax_mtch_final, l)
  }

  all_measo <- as.data.table(all_measo)[, `:=`(rw = phylum == phylum_worms | .N <= 1),
    by = my_id # to keep only one match per input_name
  ][rw == TRUE, ][, `:=`(rw = NULL, my_id = NULL)][, `:=`(QC_hasScientificNameInWorms = ifelse(is.na(scientificName), FALSE,
    ifelse(!is.na(valid_name), "in WoRMS", "not in WoRMS")
  ))]

  ## Save intermediate QC steps
  all_measo %>%
    fwrite(paste0(data_dir, "edited/all_measo_land_worms.csv"), na = NA, sep2 = c("", "|", ""))
}

# Date --------------------------------------------------------------------

if (TRUE) {
  gc()
  all_measo <- fread(paste0(data_dir, "edited/all_measo_land_worms.csv"))

  all_measo <- all_measo %>%
    dplyr::mutate(
      QC_year = ifelse(is.na(year) & is.na(year_event), "missing", "ok"),
      QC_month = ifelse(is.na(month) & is.na(month_event), "missing", "ok"),
      QC_day = ifelse(is.na(day) & is.na(day_event), "missing", "ok")
    )

  all_measo <- all_measo %>%
    dplyr::mutate(
      QC_date = ifelse(QC_year == "missing", "missing",
        ifelse(QC_month == "missing" | QC_day == "missing", "incomplete", "ok")
      )
    ) %>%
    dplyr::select(-QC_year, -QC_month, -QC_day)

  all_measo <- all_measo %>%
    mutate(QC_hasScientificName = !is.na(scientificName))

  ## Save intermediate QC steps
  all_measo %>%
    fwrite(paste0(data_dir, "edited/all_measo_land_worms_date.csv"), na = NA, sep2 = c("", "|", ""))
}

# Flagging (potential) duplicates -----------------------------------------

if (TRUE) {
  gc()
  all_measo <- fread(paste0(data_dir, "edited/all_measo_land_worms_date.csv"))

  # Occurrence status -------------------------------------------------------

  # identifying absences
  all_measo <- all_measo %>%
    dplyr::mutate(occurrenceStatus = occurrenceStatus %>%
      tolower() %>%
      str_replace("presence", "present"))

  all_measo <- all_measo %>%
    mutate(QC_notAbsent = ifelse(QC_database == "OBIS",
      TRUE, # only presences in default downloads from OBIS
      occurrenceStatus != "absent"
    )) # in GBIF, occurrence status is a required field

  # Occurrence ID -----------------------------------------------------------

  all_measo <- all_measo %>%
    mutate(QC_hasOccurrenceID = !is.na(occurrenceID))

  # Potential duplicates ----------------------------------------------------

  dec_pres <- 4 # precision for rounding decimal coordinates
  all_measo <- all_measo[, `:=`(
    decimalLatitude_round = round(decimalLatitude, dec_pres),
    decimalLongitude_round = round(decimalLongitude, dec_pres)
  )]

  dupFields_taxCheck <- c(
    "valid_name",
    "decimalLatitude_round",
    "decimalLongitude_round",
    "year2", "month2", "day2"
  )

  all_measo$QC_potentialDuplicates_taxCheck <- duplicated(all_measo[, ..dupFields_taxCheck], na.rm = FALSE)


  # Duplicates between vs within database -----------------------------------

  all_measo <- all_measo[, combi_dupli_taxCheck := do.call(paste, c(.SD, sep = "_")), .SDcols = dupFields_taxCheck]

  all_measo$QC_potentialDuplicatesInter_taxCheck[all_measo$QC_database == "GBIF"] <- all_measo$combi_dupli_taxCheck[all_measo$QC_database == "GBIF"] %in% all_measo$combi_dupli_taxCheck[all_measo$QC_database == "OBIS"]
  all_measo$QC_potentialDuplicatesInter_taxCheck[all_measo$QC_database == "OBIS"] <- all_measo$combi_dupli_taxCheck[all_measo$QC_database == "OBIS"] %in% all_measo$combi_dupli_taxCheck[all_measo$QC_database == "GBIF"]
  all_measo$QC_potentialDuplicatesIntra_taxCheck[all_measo$QC_database == "GBIF"] <- duplicated(all_measo$combi_dupli_taxCheck[all_measo$QC_database == "GBIF"])
  all_measo$QC_potentialDuplicatesIntra_taxCheck[all_measo$QC_database == "OBIS"] <- duplicated(all_measo$combi_dupli_taxCheck[all_measo$QC_database == "OBIS"])

  all_measo$QC_duplicatesCat_taxCheck <- "unique"
  all_measo$QC_duplicatesCat_taxCheck[all_measo$QC_potentialDuplicates_taxCheck == TRUE] <- "potential duplicates"
  # all_measo$QC_duplicatesCat_taxCheck[all_measo$QC_trueDuplicates == TRUE] <- "true duplicates"

  ## split basisOfRecord
  all_measo <- all_measo %>%
    dplyr::mutate(
      QC_basisOfRecord = tolower(basisOfRecord) %>% str_replace("_", ""),
      QC_basisOfRecord = ifelse(QC_basisOfRecord == "machineobservation", "machine",
        ifelse(QC_basisOfRecord == "fossilspecimen", "fossil", "human")
      )
    )

  ## Save intermediate QC steps
  all_measo %>%
    dplyr::select(-contains("combi")) %>%
    fwrite(paste0(data_dir, "edited/all_measo_land_worms_date_dupli.csv"), na = NA)
}
