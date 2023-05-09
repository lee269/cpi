library(unpivotr)
library(tidyxl)

file <- "~/Downloads/consumerpriceinflationdetailedreferencetables2.xlsx"

janweights <- xlsx_cells(file, sheets = "Table 11") |> 
  filter(row >=7 & row < 371 & col > 1) |> 
  behead("up", "year") |> 
  behead("left", "cdid") |> 
  behead("left", "title")  |> 
  filter(!is.na(numeric)) |> 
  filter(stringr::str_detect(year, "Jan") == TRUE) |> 
  mutate(date = lubridate::ym(year)) |> 
  select(date, cdid, value = numeric)
