library(here)
library(tidyverse)

metadata <- readRDS(here("data",  "tidy", "metadata.rds"))

# CPIH annual rates ------------------------------------------------------------
# Extract series and add a level flag to show hierarchy


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


cpih_ann <- bind_rows(list(cpih_ann1, cpih_ann2, cpih_ann3, cpih_ann4)) %>% 
            mutate(title = str_to_title(str_remove(title, " 2015=100"))) %>% 
            arrange(title)

saveRDS(cpih_ann, here("data", "tidy", "cpih_ann_rate_cdids.rds"))


# CPIH monthly rates -----------------------------------------------------------

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


cpih_mth <- bind_rows(list(cpih_mth1, cpih_mth2, cpih_mth3, cpih_mth4)) %>% 
            mutate(title = str_to_title(str_remove(title, " 2015=100"))) %>% 
            arrange(title)

saveRDS(cpih_mth, here("data", "tidy", "cpih_mth_rate_cdids.rds"))





# RPI average prices -----------------------------------------------------------

rpi <- metadata %>% 
  filter(str_detect(title, "RPI? ?:? ?Ave")) %>% 
  mutate(title = str_remove(title, "RPI: Ave price - ")) %>%
  mutate(title = str_remove(title, "RPI: Ave Price - ")) %>%
  mutate(title = str_remove(title, "RPI: Ave Price: ")) %>%
  mutate(title = str_remove(title, "RPI :Ave price - ")) %>%
  mutate(title = str_remove(title, "RPI: Average [Pp]rice - ")) %>%
  mutate(title = str_remove(title, "RPI Average [Pp]rice- ")) %>%
  mutate(title = str_remove(title, "RPI Average [Pp]rice - ")) %>%
  mutate(title = str_to_title(title)) %>% 
  arrange(title)

saveRDS(rpi, here("data", "tidy", "rpi_avg_price_cdids.rds"))


# Contributions to CPIH annual rate -------------------------------------------
cpih_cont_ann <- metadata %>% 
                filter(str_detect(title, "CPIH: Contribution to all items annual rate: ")) %>% 
                arrange(title)

saveRDS(cpih_cont_ann, here("data", "tidy", "cpih_cont_ann_cdids.rds"))

# Contributions to monthly change in rate -------------------------------
cpih_cont_mth_chg <- metadata %>% 
  filter(str_detect(title, "CPIH: Contribution to monthly change in all items index: ")) %>% 
  arrange(title)

saveRDS(cpih_cont_mth_chg, here("data", "tidy", "cpih_cont_mth_chg_cdids.rds"))

# Contributions to monthly change in annual rate -------------------------------
cpih_cont_ann_chg <- metadata %>% 
  filter(str_detect(title, "CPIH: % points change over previous month \\(12 month rate\\): ")) %>% 
  arrange(title)

saveRDS(cpih_cont_ann_chg, here("data", "tidy", "cpih_cont_ann_chg_cdids.rds"))


