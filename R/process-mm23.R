process_mm23 <- function(rawfile, path){
  require(dplyr)
  require(tibble)
  require(janitor)
  require(tidyr)
  require(readr)
  require(lubridate)
  require(stringr)
  
  if(missing(rawfile)){
    url <- "https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv"
    tmp <- tempfile()
    download.file(url, tmp)
    rawfile <- tmp
  }

  message("Processing metadata")
  
  metadata <- readr::read_csv(rawfile, n_max = 6, show_col_types = FALSE,) %>% 
    t() %>% 
    tibble::as_tibble(rownames = "series", .name_repair = "unique") %>% 
    setNames( .[1,]) %>% 
    # rename(series = Title) %>%
    dplyr::filter(Title != "Title") %>% 
    janitor::clean_names() %>% 
    dplyr::relocate(cdid)

  release_date <- dmy(metadata$release_date[1])
  next_release <- dmy(metadata$next_release[1])
    
  message("Processing month")

  mm23_month <- readr::read_csv(rawfile, skip = 1, show_col_types = FALSE) %>% 
    dplyr::filter(nchar(CDID) == 8) %>% 
    dplyr::mutate(CDID = lubridate::ym(CDID)) %>%
    dplyr::rename(date = CDID) %>% 
    tidyr::pivot_longer(cols = 2:ncol(.), names_to = "cdid") %>% 
    dplyr::mutate(value = as.numeric(value),
                  period = "M") %>% 
    dplyr::filter(!is.na(value))
  
  
  message("Processing quarter")
  
  mm23_quarter <- readr::read_csv(rawfile, skip = 1, show_col_types = FALSE) %>% 
    dplyr::filter(nchar(CDID) == 7 & CDID != "PreUnit") %>% 
    dplyr::mutate(CDID = lubridate::yq(CDID)) %>% 
    dplyr::rename(date = CDID) %>% 
    tidyr::pivot_longer(cols = 2:ncol(.), names_to = "cdid") %>% 
    dplyr::mutate(value = as.numeric(value),
                  period = "Q") %>% 
    dplyr::filter(!is.na(value))
  
  
  message("Processing year")
  
  mm23_year <- readr::read_csv(rawfile, skip = 1, show_col_types = FALSE) %>% 
    dplyr::filter(nchar(CDID) == 4 & CDID != "Unit") %>% 
    dplyr::mutate(CDID = lubridate::ymd(paste(CDID, "-01-01"))) %>% 
    dplyr::rename(date = CDID) %>% 
    tidyr::pivot_longer(cols = 2:ncol(.), names_to = "cdid") %>% 
    dplyr::mutate(value = as.numeric(value),
                  period = "Y") %>% 
    dplyr::filter(!is.na(value))
  
  data <- dplyr::bind_rows(mm23_month, mm23_quarter, mm23_year) %>%
    dplyr::arrange(cdid, date)
  
  
  # CPIH annual rates ------------------------------------------------------------
  # Extract series and add a level flag to show hierarchy
  
  message("Processing CPIH annual rates")
  
  cpih_ann1 <- metadata %>% 
    filter(str_detect(title, "CPIH ANNUAL RATE [0-9][0-9] ?:")) %>% 
    mutate(title = str_remove(title, "CPIH ANNUAL RATE ")) %>% 
    mutate(level = 1)
  
  cpih_ann2 <- metadata %>% 
    filter(str_detect(title, "CPIH ANNUAL RATE [0-9][0-9]\\.[0-9] ?:")) %>% 
    mutate(title = str_remove(title, "CPIH ANNUAL RATE ")) %>% 
    mutate(level = 2)
  
  cpih_ann3 <- metadata %>%
    filter(str_detect(title, "CPIH ANN[A-Z]* RATE [0-9][0-9]\\.[0-9].[0-9] ?:")) %>% 
    mutate(title = str_remove(title, "CPIH ANN[A-Z]* RATE ")) %>% 
    mutate(level = 3)
  
  
  cpih_ann4 <- metadata %>% 
    filter(str_detect(title, "CPIH ANNUAL RATE [0-9][0-9]\\.[0-9].[0-9].[0-9]")) %>% 
    mutate(title = str_remove(title, "CPIH ANNUAL RATE ")) %>% 
    mutate(level = 4)
  
  
  cpih_ann_rate_cdids <- bind_rows(list(cpih_ann1, cpih_ann2, cpih_ann3, cpih_ann4)) %>% 
    mutate(title = str_to_title(str_remove(title, " 2015=100"))) %>% 
    mutate(category = "CPIH Annual rate (%)") %>% 
    select(cdid, title, category, level) %>% 
    arrange(title)
  
  
  
  # CPIH monthly rates -----------------------------------------------------------
  
  message("Processing CPIH monthly rates")
  
  cpih_mth1 <- metadata %>% 
    filter(str_detect(title, "CPIH MONTHLY RATE [0-9][0-9] ?:")) %>% 
    mutate(title = str_remove(title, "CPIH MONTHLY RATE ")) %>% 
    mutate(level = 1)
  
  cpih_mth2 <- metadata %>% 
    filter(str_detect(title, "CPIH MONTHLY RATE [0-9][0-9]\\.[0-9] ?:")) %>% 
    mutate(title = str_remove(title, "CPIH MONTHLY RATE ")) %>% 
    mutate(level = 2)
  
  cpih_mth3 <- metadata %>% 
    filter(str_detect(title, "CPIH MONTHLY RATE [0-9][0-9]\\.[0-9].[0-9] ?:")) %>% 
    mutate(title = str_remove(title, "CPIH MONTHLY RATE ")) %>% 
    mutate(level = 3)
  
  
  cpih_mth4 <- metadata %>% 
    filter(str_detect(title, "CPIH MONTHLY RATE [0-9][0-9]\\.[0-9].[0-9].[0-9]")) %>% 
    mutate(title = str_remove(title, "CPIH MONTHLY RATE ")) %>% 
    mutate(level = 4)
  
  
  cpih_mth_rate_cdids <- bind_rows(list(cpih_mth1, cpih_mth2, cpih_mth3, cpih_mth4)) %>% 
    mutate(title = str_to_title(str_remove(title, " 2015=100"))) %>% 
    mutate(category = "CPIH Monthly rate (%)") %>% 
    select(cdid, title, category, level) %>% 
    arrange(title)
  
  
  
  
  
  
  # RPI average prices -----------------------------------------------------------
  message("Processing RPI average prices")
  
  rpi_avg_price_cdids <- metadata %>% 
    filter(str_detect(title, "RPI? ?:? ?Ave")) %>% 
    mutate(title = str_remove(title, "RPI: Ave price - ")) %>%
    mutate(title = str_remove(title, "RPI: Ave Price - ")) %>%
    mutate(title = str_remove(title, "RPI: Ave Price: ")) %>%
    mutate(title = str_remove(title, "RPI :Ave price - ")) %>%
    mutate(title = str_remove(title, "RPI: Average [Pp]rice - ")) %>%
    mutate(title = str_remove(title, "RPI Average [Pp]rice- ")) %>%
    mutate(title = str_remove(title, "RPI Average [Pp]rice - ")) %>%
    mutate(title = str_to_title(title)) %>%
    mutate(category = "RPI Average price (pence)") %>% 
    select(cdid, title, category) %>% 
    arrange(title)
  
  
  
  
  # Contributions to CPIH annual rate -------------------------------------------
  message("Processing CPIH contributions to annual rate")
  
  cpih_cont_ann_cdids <- metadata %>% 
    filter(str_detect(title, "CPIH: Contribution to all items annual rate: ")) %>% 
    mutate(title = str_remove(title, "CPIH: Contribution to all items annual rate: ")) %>% 
    mutate(category = "CPIH contribution to all items annual rate") %>% 
    select(cdid, title, category) %>% 
    arrange(title)
  
  
  
  # Contributions to monthly change in rate -------------------------------
  message("Processing CPIH contributions to monthly change in rate")
  
  cpih_cont_mth_chg_cdids <- metadata %>% 
    filter(str_detect(title, "CPIH: Contribution to monthly change in all items index: ")) %>% 
    mutate(title = str_remove(title, "CPIH: Contribution to monthly change in all items index: ")) %>% 
    mutate(category = "CPIH contribution to monthly change in all items index") %>% 
    select(cdid, title, category) %>% 
    arrange(title)
  
  
  
  # Contributions to monthly change in annual rate -------------------------------
  message("Processing monthly change in annual rate")
  
  cpih_cont_ann_chg_cdids <- metadata %>% 
    filter(str_detect(title, "CPIH: % points change over previous month \\(12 month rate\\): ")) %>% 
    mutate(title = str_remove(title, "CPIH: % points change over previous month \\(12 month rate\\): ")) %>% 
    mutate(category = "CPIH % points change over previous months 12 month rate") %>% 
    select(cdid, title, category) %>% 
    arrange(title)
  
  
  # rebuild metadata
  
  series <- bind_rows(list(cpih_ann_rate_cdids, 
                         cpih_mth_rate_cdids, 
                         cpih_cont_ann_cdids, 
                         cpih_cont_ann_chg_cdids, 
                         cpih_cont_mth_chg_cdids,
                         rpi_avg_price_cdids))
  
  metadata <- metadata %>%
    left_join(series, by = "cdid") %>% 
    mutate(t2 = ifelse(is.na(title.y), title.x, title.y)) %>% 
    rename(title_original = title.x,
           title = t2) %>% 
    select(cdid, title, category, level, pre_unit, unit, release_date, next_release, important_notes)
  
  
  
  
   message("Writing mm23")
  mm23 <- list(
               release_date = release_date,
               next_release = next_release,
               metadata = metadata,
               data = data,
               cpih_ann_rate_cdids = cpih_ann_rate_cdids,
               cpih_mth_rate_cdids = cpih_mth_rate_cdids,
               cpih_cont_ann_cdids = cpih_cont_ann_cdids,
               cpih_cont_ann_chg_cdids = cpih_cont_ann_chg_cdids,
               cpih_cont_mth_chg_cdids = cpih_cont_mth_chg_cdids,
               rpi_avg_price_cdids = rpi_avg_price_cdids
               )
  
  saveRDS(mm23, paste0(path, "/mm23.rds"))
  
  
  message("Writing appdata"
          )
  appseries <- metadata %>% filter(!is.na(category))
  app <- data %>% 
    filter(cdid %in% appseries$cdid) %>% 
    left_join(appseries, by = "cdid") %>% 
    select(-release_date, -next_release)
  
  appdata <- list(
    release_date = release_date,
    next_release = next_release,
    data = app)
  
  
  saveRDS(appdata, paste0(path, "/appdata.rds"))
  
}

