library(here)
library(tidyverse)
library(countrycode)

# from https://data.oecd.org/price/inflation-cpi.htm
# downloaded, could perhaps use API

custom_codes <- c("G-7" = "G7 Countries",
                  "G-20" = "G20 Countries",
                  "OECD" = "OECD Countries",
                  "OECDE" = "OECDE",
                  "EU27_2020" = "EU27 (2020)",
                  "EA19" = "EA19")

# only annual CPI rate and monthly frequency pulled out here
oecd <- read_csv(here("data", "raw", "oecd.csv")) %>% 
  clean_names() %>% 
  filter(frequency == "M" & 
           measure == "AGRWTH" & 
           subject %in% c("FOOD", "TOT")) %>% 
  mutate(date = ym(time),
         country_code = location,
         country_name = countrycode(location,
                                   origin = "iso3c",
                                   destination = "country.name",
                                   custom_match = custom_codes)) %>% 
  select(country_code, country_name, date, subject, value, flag_codes)

saveRDS(oecd, here("data", "tidy", "oecd.rds"))
