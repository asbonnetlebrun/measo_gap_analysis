library(tidyverse)
library(data.table)
library(networkD3)
library(webshot)
library(htmlwidgets)

measo_fig1 <- function(all_measo, fig_name) {
  my_cols <- c(
    "QC_database",
    "QC_onLand",
    "QC_hasScientificNameInWorms",
    "QC_date",
    "QC_notAbsent",
    "QC_duplicatesIntraCat_taxCheck",
    "QC_duplicatesCat_taxCheck",
    "QC_basisOfRecord"
  )
  
  dt <- copy(all_measo)
  dt <- dt[, `:=`(
    QC_hasScientificNameInWorms = ifelse(is.na(scientificName), FALSE,
                                         ifelse(!is.na(valid_name), "in WoRMS", "not in WoRMS")
    ),
    QC_duplicatesCat_taxCheck = ifelse(str_detect(QC_duplicatesCat_taxCheck, "OBIS-GBIF"),
                                       QC_duplicatesCat_taxCheck,
                                       paste("OBIS-GBIF", QC_duplicatesCat_taxCheck)
    ),
    QC_duplicatesIntraCat_taxCheck = ifelse(!is.na(QC_trueDuplicatesIntra),
                                            ifelse(QC_trueDuplicatesIntra == TRUE,
                                                   "true duplicates",
                                                   ifelse(QC_potentialDuplicatesIntra_taxCheck == TRUE,
                                                          "potential duplicates",
                                                          "I unique"
                                                   )
                                            ),
                                            "I unique"
    ),
    QC_basisOfRecord = ifelse(is.na(QC_basisOfRecord) | QC_basisOfRecord == "fossil",
                              "fossil or NA",
                              QC_basisOfRecord
    )
  )][, ..my_cols]
  
  # list of outputs to remove
  checks2keep <- data.frame(
    checks = my_cols[-1],
    val2keep = c(
      FALSE,
      "in WoRMS",
      "ok",
      "TRUE",
      "I unique",
      "OBIS-GBIF unique",
      "human"
    )
  )
  
  better_names <- function(name) {
    name <- str_replace(name, "QC_database==OBIS", "OBIS") %>%
      str_replace("QC_database==GBIF", "GBIF") %>%
      str_replace("QC_onLand==FALSE", "at sea") %>%
      str_replace("QC_onLand==TRUE", "on land") %>%
      str_replace("QC_hasScientificNameInWorms==FALSE", "no scientificName") %>%
      str_replace("QC_hasScientificNameInWorms==in WoRMS", "in WoRMS") %>%
      str_replace("QC_hasScientificNameInWorms==not in WoRMS", "not in WoRMS") %>%
      str_replace("QC_date==ok", "complete date") %>%
      str_replace("QC_date==missing", "missing date") %>%
      str_replace("QC_date==incomplete", "incomplete date") %>%
      str_replace("QC_date==conflicting", "conflicting date") %>% # separate the date info
      str_replace("QC_duplicatesIntraCat_taxCheck==I unique", "I unique records") %>%
      str_replace("QC_duplicatesIntraCat_taxCheck==potential duplicates", "potential duplicates") %>%
      str_replace("QC_duplicatesIntraCat_taxCheck==true duplicates", "true duplicates") %>%
      str_replace("QC_duplicatesCat_taxCheck==OBIS-GBIF unique", "OBIS-GBIF unique records") %>%
      str_replace("QC_duplicatesCat_taxCheck==OBIS-GBIF potential duplicates", "OBIS-GBIF pot. dupli.") %>%
      str_replace("QC_duplicatesCat_taxCheck==OBIS-GBIF true duplicates", "OBIS-GBIF true dupli.") %>%
      str_replace("QC_notAbsent==TRUE", "presences") %>%
      str_replace("QC_notAbsent==FALSE", "absences") %>%
      str_replace("QC_basisOfRecord==human", "human obs.") %>%
      str_replace("QC_basisOfRecord==fossil or NA", "fossil or NA") %>%
      str_replace("QC_basisOfRecord==machine", "machine obs.")
    return(name)
  }
  
  df2links_nodes <- function(df) {
    df2links <- function(df) {
      links_measo <- data.frame(
        source = character(),
        target = character(),
        value = numeric()
      )
      
      for (s_col in 1:(ncol(df) - 1)) {
        t_col <- s_col + 1
        
        sources <- unique(df[, ..s_col]) %>% sapply(., function(x) paste0(names(df)[s_col], "==", x))
        targets <- unique(df[, ..t_col]) %>% sapply(., function(x) paste0(names(df)[t_col], "==", x))
        
        combi <- expand.grid(source = sources, target = targets)
        combi$value <- numeric(nrow(combi))
        
        for (i in 1:nrow(combi)) {
          combi_i <- combi[i, ]
          combi$value[i] <- sum(df[, ..s_col] == str_split(combi_i$source, "==")[[1]][2] & df[, ..t_col] == str_split(combi_i$target, "==")[[1]][2])
        }
        
        df <- df[as.vector(df[, ..t_col] == filter(checks2keep, checks == names(df)[t_col])$val2keep), ]
        
        links_measo <- rbind(
          links_measo,
          combi
        )
      }
      
      return(links_measo)
    }
    
    links_measo <- df2links(df)
    
    nodes2keep <- plyr::ddply(checks2keep, ~checks, function(x) data.frame(name = paste(x$checks, x$val2keep, sep = "==")))$name
    
    links_measo <- links_measo %>%
      dplyr::mutate(
        group = ifelse(target %in% nodes2keep, "in", "out"),
        group = ifelse(str_detect(target, "human"), "human", group),
        group = ifelse(str_detect(target, "machine"), "machine", group)
      )
    
    links_measo <- links_measo %>%
      dplyr::mutate(across(c("source", "target"), ~ better_names(.x)))
    
    links_measo <- links_measo %>%
      left_join(links_measo %>%
                  group_by(target) %>%
                  dplyr::summarise(N = sum(value)) %>%
                  dplyr::mutate(target_N = paste(target, formatC(N, format = "e", digits = 1), sep = ": ")) %>%
                  dplyr::select(-N)) %>%
      left_join(links_measo %>%
                  group_by(source) %>%
                  dplyr::summarise(N = sum(value)) %>%
                  dplyr::mutate(source_N = paste(source, formatC(N, format = "e", digits = 1), sep = ": ")) %>%
                  dplyr::select(-N)) %>%
      dplyr::select(-source, -target) %>%
      dplyr::rename(
        source = source_N,
        target = target_N
      )
    
    # From these flows we need to create a node data frame: it lists every entities involved in the flow
    nodes_measo <- data.frame(
      name = c(
        as.character(links_measo$source),
        as.character(links_measo$target)
      ) %>% unique()
    )
    
    nodes_measo <- nodes_measo %>%
      left_join(links_measo %>%
                  dplyr::select(target, group) %>%
                  unique() %>%
                  dplyr::rename(name = target))
    
    # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    links_measo$IDsource <- match(links_measo$source, nodes_measo$name) - 1
    links_measo$IDtarget <- match(links_measo$target, nodes_measo$name) - 1
    
    nodes_measo <- nodes_measo %>%
      dplyr::mutate(
        group = ifelse(str_detect(name, "OBIS"), "OBIS", group),
        group = ifelse(str_detect(name, "GBIF"), "GBIF", group)
      )
    
    return(list(
      nodes_measo = nodes_measo,
      links_measo = links_measo
    ))
  }
  
  links_nodes_gbif <- df2links_nodes(df = dt %>% filter(QC_database == "GBIF"))
  links_nodes_obis <- df2links_nodes(df = dt %>% filter(QC_database == "OBIS"))
  
  n_nodes <- max(links_nodes_gbif$links_measo$IDtarget)
  
  group_modif <- function(df) {
    if ("target" %in% names(df)) {
      df <- df %>%
        mutate(group = ifelse(group %in% c("human", "machine", "out", "GBIF", "OBIS") | str_detect(target, "OBIS-GBIF unique records"),
                              group, paste(QC_database, group)
        ))
    } else {
      df <- df %>%
        mutate(group = ifelse(group %in% c("human", "machine", "out", "GBIF", "OBIS") | str_detect(name, "unique records"),
                              group, paste(QC_database, group)
        ))
    }
    
    return(df)
  }
  
  links_measo <- rbind(
    links_nodes_gbif$links_measo %>%
      dplyr::mutate(QC_database = "GBIF") %>%
      group_modif(),
    links_nodes_obis$links_measo %>%
      mutate(
        IDsource = IDsource + n_nodes + 1,
        IDtarget = IDtarget + n_nodes + 1
      ) %>%
      dplyr::mutate(QC_database = "OBIS") %>%
      group_modif()
  )
  
  nodes_measo <- rbind(
    links_nodes_gbif$nodes_measo %>%
      dplyr::mutate(QC_database = "GBIF") %>%
      group_modif(),
    links_nodes_obis$nodes_measo %>%
      dplyr::mutate(QC_database = "OBIS") %>%
      group_modif()
  )
  
  
  new_name_dupli_intra <- links_measo %>%
    filter(str_detect(target, "I unique")) %>%
    dplyr::summarise(
      new_value = sum(value),
      new_name = paste0("unique records: ", formatC(new_value, format = "e", digits = 1))
    )
  #
  new_name_dupli <- links_measo %>%
    filter(str_detect(target, "OBIS-GBIF unique")) %>%
    dplyr::summarise(
      new_value = sum(value),
      new_name = paste0("unique records: ", formatC(new_value, format = "e", digits = 1))
    )
  
  new_name_human <- links_measo %>%
    filter(str_detect(target, "human obs.")) %>%
    dplyr::summarise(
      new_value = sum(value),
      new_name = paste0("human obs.: ", formatC(new_value, format = "e", digits = 1))
    )
  
  new_name_machine <- links_measo %>%
    filter(str_detect(target, "machine obs.")) %>%
    dplyr::summarise(
      new_value = sum(value),
      new_name = paste0("machine obs.: ", formatC(new_value, format = "e", digits = 2))
    )
  
  new_name_potdupli <- links_measo %>%
    filter(str_detect(target, "OBIS-GBIF pot. dupli.")) %>%
    dplyr::summarise(
      new_value = sum(value),
      new_name = paste0("OBIS-GBIF pot. dupli.: ", formatC(new_value, format = "e", digits = 2))
    )
  
  links_str <- function(links_measo, old_str, new_str) {
    links_measo <- links_measo %>%
      dplyr::mutate(
        target = ifelse(str_detect(target, old_str),
                        new_str$new_name, target
        ),
        source = ifelse(str_detect(source, old_str),
                        new_str$new_name, source
        )
      )
  }
  
  links_measo <- links_measo %>%
    links_str(old_str = "I unique", new_str = new_name_dupli_intra) %>%
    links_str(old_str = "OBIS-GBIF unique", new_str = new_name_dupli) %>%
    links_str(old_str = "human", new_str = new_name_human) %>%
    links_str(old_str = "machine", new_str = new_name_machine) %>%
    links_str(old_str = "OBIS-GBIF pot. dupli.", new_str = new_name_potdupli) %>%
    dplyr::select(-QC_database)
  links_measo <- unique(links_measo)
  
  nodes_measo <- nodes_measo %>%
    dplyr::select(-contains("QC_database")) %>%
    dplyr::mutate(
      group = ifelse(str_detect(name, "OBIS-GBIF unique"), "in", group),
      group = ifelse(str_detect(name, "OBIS-GBIF") & str_detect(name, "dupli"), "out", group),
      name = ifelse(str_detect(name, "OBIS-GBIF unique"),
                    new_name_dupli$new_name, name
      ),
      name = ifelse(str_detect(name, "I unique"),
                    new_name_dupli_intra$new_name, name
      ),
      name = ifelse(str_detect(name, "human"),
                    new_name_human$new_name, name
      ),
      name = ifelse(str_detect(name, "machine"),
                    new_name_machine$new_name, name
      ),
      name = ifelse(str_detect(name, "OBIS-GBIF pot. dupli."),
                    new_name_potdupli$new_name, name
      )
    ) %>%
    unique()
  
  links_measo$IDsource <- match(links_measo$source, nodes_measo$name) - 1
  links_measo$IDtarget <- match(links_measo$target, nodes_measo$name) - 1
  
  # Give a color for each group:
  my_color <- 'd3.scaleOrdinal() .domain(["OBIS in", "GBIF in", "in", "out", "GBIF", "OBIS", "human", "machine"]) .range(["orange", "indianred", "#3b8795", "lightgrey", "orange", "indianred", "#3b8795", "#b0e0e6"])'
  
  # Make the Network
  p <- sankeyNetwork(
    Links = links_measo, Nodes = nodes_measo,
    Source = "IDsource",
    Target = "IDtarget",
    Value = "value",
    NodeID = "name",
    LinkGroup = "group",
    NodeGroup = "group",
    nodePadding = 25,
    colourScale = my_color,
    fontFamily = "Times",
    sinksRight = FALSE,
    fontSize = 19,
    height = 500, width = 1800,
    nodeWidth = 8
  )
  
  saveWidget(p, paste0(fig_dir, "fig1.html"))
  webshot(paste0(fig_dir, "fig1.html"), paste0(fig_dir, fig_name, ".png"), zoom = 1.4) # , vwidth = 441, vheight = 351)
  unlink(paste0(fig_dir, "fig1.html"))
  unlink(paste0(fig_dir, "fig1_files"), recursive = TRUE)
  
  return(p)
}
