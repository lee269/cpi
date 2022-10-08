read_mm23 <- function(rawfile, path){
  require(dplyr)
  require(tibble)
  require(janitor)
  require(tidyr)
  require(readr)
  require(lubridate)
  
  if(missing(rawfile)){
    url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv"
    tmp <- tempfile()
    download.file(url, tmp)
    rawfile <- tmp
  }
  
  metadata <- readr::read_csv(rawfile, n_max = 6, show_col_types = FALSE,) %>% 
    t() %>% 
    tibble::as_tibble(rownames = "series", .name_repair = "unique") %>% 
    setNames( .[1,]) %>% 
    # rename(series = Title) %>%
    dplyr::filter(Title != "Title") %>% 
    janitor::clean_names() %>% 
    dplyr::relocate(cdid)
  
  message(paste0("Saving ",path, "/metadata.rds"))
  saveRDS(metadata, paste0(path, "/metadata.rds"))
  
  mm23_month <- readr::read_csv(rawfile, skip = 1, show_col_types = FALSE) %>% 
    dplyr::filter(nchar(CDID) == 8) %>% 
    dplyr::mutate(CDID = lubridate::ym(CDID)) %>%
    dplyr::rename(date = CDID) %>% 
    tidyr::pivot_longer(cols = 2:ncol(.), names_to = "cdid") %>% 
    dplyr::mutate(value = as.numeric(value)) %>% 
    dplyr::filter(!is.na(value))
  
  
  message(paste0("Saving ", path, "/mm23_month.rds"))
  saveRDS(mm23_month, paste0(path, "/mm23_month.rds"))  
  
  
  mm23_quarter <- readr::read_csv(rawfile, skip = 1, show_col_types = FALSE) %>% 
    dplyr::filter(nchar(CDID) == 7 & CDID != "PreUnit") %>% 
    dplyr::mutate(CDID = lubridate::yq(CDID)) %>% 
    dplyr::rename(date = CDID) %>% 
    tidyr::pivot_longer(cols = 2:ncol(.), names_to = "cdid") %>% 
    dplyr::mutate(value = as.numeric(value)) %>% 
    dplyr::filter(!is.na(value))
  
  message(paste0("Saving ", path, "/mm23_quarter.rds"))
  saveRDS(mm23_quarter, paste0(path, "/mm23_quarter.rds"))  
  
  mm23_year <- readr::read_csv(rawfile, skip = 1, show_col_types = FALSE) %>% 
    dplyr::filter(nchar(CDID) == 4 & CDID != "Unit") %>% 
    dplyr::mutate(CDID = as.numeric(CDID)) %>% 
    dplyr::rename(date = CDID) %>% 
    tidyr::pivot_longer(cols = 2:ncol(.), names_to = "cdid") %>% 
    dplyr::mutate(value = as.numeric(value)) %>% 
    dplyr::filter(!is.na(value))
  
  message(paste0("Saving ", path, "/mm23_year.rds"))
  saveRDS(mm23_year, paste0(path, "/mm23_year.rds"))  
  
  
}
