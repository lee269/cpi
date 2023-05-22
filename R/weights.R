library(mm23)
library(dplyr)
library(tidyr)

mm23 <- acquire_mm23()
metadata <- get_mm23_metadata(mm23)
mth <- get_mm23_month(mm23)
wts <- get_cpih_weights()

unchain <- function(month, value) {
  case_when(
    month == 1 ~ value/lag(value) * 100,
    month == 2 ~ value/lag(value, 1) * 100,
    month == 3 ~ value/lag(value, 2) * 100,
    month == 4 ~ value/lag(value, 3) * 100,
    month == 5 ~ value/lag(value, 4) * 100,
    month == 6 ~ value/lag(value, 5) * 100,
    month == 7 ~ value/lag(value, 6) * 100,
    month == 8 ~ value/lag(value, 7) * 100,
    month == 9 ~ value/lag(value, 8) * 100,
    month == 10 ~ value/lag(value, 9) * 100,
    month == 11 ~ value/lag(value, 10) * 100,
    month == 12 ~ value/lag(value, 11) * 100
  )
}

# get weights for top level items
wt_meta <- metadata |> 
  filter(category == "CPIH Weights", 
         level == 1)

wt_cdids <- wt_meta$cdid |> unlist()

weights <- wts |> 
  filter(cdid %in% wt_cdids, date >= "2017-01-01", date <= "2023-03-01")

# get cpih for top level items
cpih_meta <- metadata |> 
  filter(category == "CPIH Index",
         level == 1)

cpih_cdids <- cpih_meta$cdid |> unlist()

cpih_series <- mth |> 
  filter(cdid %in% cpih_cdids, date >= "2017-01-01") |> 
  arrange(cdid, date) |> 
  group_by(cdid) |> 
  mutate(month = lubridate::month(date),
         unchained_value = unchain(month, value))

