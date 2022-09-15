library(here)
library(tidyverse)
library(tidyr)
library(lubridate)
library(ggplot2)
library(janitor)

# https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv
# read_csv("https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv")
# Not sure why this code seems to pull in a different version of mm23 than manually downloading.
# x <- read.csv("https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv")
# write_csv(x, here("data", "raw", "mm23.csv"))

download.file("https://www.ons.gov.uk/file?uri=/economy/inflationandpriceindices/datasets/consumerpriceindices/current/mm23.csv", here("data", "raw","mm23.csv"))

metadata <- read_csv(here("data", "raw", "mm23.csv"), n_max = 6) %>% 
            t() %>% 
            as_tibble(rownames = "series") %>% 
            setNames( .[1,]) %>% 
            # rename(series = Title) %>%
            filter(Title != "Title") %>% 
            clean_names() %>% 
            relocate(cdid)
  
            # pivot_longer(cols = 2:ncol(.), names_to = "series")

saveRDS(metadata, here("data", "tidy", "metadata.rds"))


mm23_month <- read_csv(here("data", "raw", "mm23.csv"), skip = 1) %>% 
        filter(nchar(CDID) == 8) %>% 
        mutate(CDID = ym(CDID)) %>%
        rename(date = CDID) %>% 
        pivot_longer(cols = 2:ncol(.), names_to = "cdid") %>% 
        mutate(value = as.numeric(value))

saveRDS(mm23_month, here("data", "tidy", "mm23_month.rds"))  


mm23_quarter <- read_csv(here("data", "raw", "mm23.csv"), skip = 1) %>% 
  filter(nchar(CDID) == 7 & CDID != "PreUnit") %>% 
  mutate(CDID = yq(CDID)) %>% 
  rename(date = CDID) %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "cdid") %>% 
  mutate(value = as.numeric(value))

saveRDS(mm23_quarter, here("data", "tidy", "mm23_quarter.rds"))  

mm23_year <- read_csv(here("data", "raw", "mm23.csv"), skip = 1) %>% 
  filter(nchar(CDID) == 4 & CDID != "Unit") %>% 
  mutate(CDID = as.numeric(CDID)) %>% 
  rename(date = CDID) %>% 
  pivot_longer(cols = 2:ncol(.), names_to = "cdid") %>% 
  mutate(value = as.numeric(value))

saveRDS(mm23_year, here("data", "tidy", "mm23_year.rds"))  
