library(tidyverse)
library(worrms)

taxon_match_fn <- function(unames) {
  
  pt <- proc.time()
  
  # split by blocks of 50
  pages <- split(unames, as.integer((seq_along(unames) - 1) / 50))
  
  # for each block of 50, use worrms
  taxon_match <- pages %>% 
    map_df(., function(unames_i) {
      
      tryCatch({ 
        result <- worrms::wm_records_taxamatch(unames_i) # Get records for one or more taxonomic name(s) using the TAXAMATCH fuzzy matching algorithm
      }, error=function(e){result<-NULL}) #print(paste(p, unames_i)); 
      
      if (!is.null(result)) {
        result_df <- map_df(1:length(result), function(i) {
          if (nrow (result[[i]]) > 0) {
            df <- result[[i]]
            df$input_name <- unames_i[i] # associate the input name to each match (because sometimes what is returned in the scientificname column is not exactly the input name)
            df$nb_match <- nrow(df) # to identify multiple matches
            df$nb_accepted_match <- sum(df$status == "accepted") #to identify multiple matches left after removing unaccepted/uncertain names
            return(df)
          }
        })
      } else {result_df = NULL}
      return(result_df)
    })
  
  print(proc.time() - pt)
  gc() 
  
  return(taxon_match)
}


# matching taxonomy -------------------------------------------------------

taxon_match_iter <- function(all_names) {
  n_taxa <- length(all_names)
  pt_match <- proc.time()
  for (m in 1:ceiling(n_taxa/block_size)) {
    start <- (m-1)*block_size+1
    end <- min(c(m*block_size, n_taxa))
    tax_mtch_sub <- taxon_match_fn(all_names[start:end])
    if (nrow(tax_mtch_all) == 0) {
      tax_mtch_all <- tax_mtch_sub
    }
    tax_mtch_all <- rbind(tax_mtch_all, tax_mtch_sub)
    print(m*block_size)
    print(proc.time() - pt_match)
  }
  return(tax_mtch_all)
}

tax_match_from_file <- function(input_file, output_file, block_size = 2000) {
  all_names <- read.table(input_file)$V1 %>% 
    as.character()
  
  tax_mtch_all <- NULL
  tax_mtch_all <- taxon_match_iter(all_names)
  
  tax_mtch_all %>% write_csv(output_file)
  
  for (k in 1:10) {
    failed_attempts <- setdiff(all_names, tax_mtch_all$input_name)
    length(failed_attempts)
    if (length(failed_attempts) > 0) { # identify the ones to retry in case we lost the internet connection along the way
      tax_mtch_all <- taxon_match_iter(failed_attempts)
    } 
  }
  tax_mtch_all %>% write_csv(output_file) 
}


