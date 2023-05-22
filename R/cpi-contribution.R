library(mm23)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Get our data together
mm23 <- acquire_mm23()
data <- get_mm23_month(mm23)
wts <- get_cpi_weights()


# Select and unchain food indices
food_cdids <- c("D7BU",
                "D7D5",
                "D7D6",
                "D7D7",
                "D7D8",
                "D7D9",
                "D7DA",
                "D7DB",
                "D7DC",
                "D7DD",
                "D7DE",
                "D7DF")


food_unchained <- data |> 
  filter(cdid %in% food_cdids & date >= "2017-01-01") |> 
  group_by(cdid) |> 
  mutate(
    unchained_value = unchain(month(date), value)
  ) |> 
  select(date, cdid, value = unchained_value)


# Extract the relevant food weights
food_weights <- c("CHZR",
                  "CJWB",
                  "CJWC",
                  "CJWD",
                  "CJWE",
                  "CJWF",
                  "CJWG",
                  "CJWH",
                  "CJWI",
                  "CJWJ",
                  "CJWK",
                  "CJWL")


foodwts <- wts |> 
  filter(cdid %in% food_weights & date >= "2017-01-01" & date <= "2023-03-01") 


# Combine the indices and weights and calculate the contribution to annual rate
unchained <- food_unchained |> 
  bind_rows(foodwts) |> 
  pivot_wider(names_from = cdid)

contribution <- unchained |> 
  mutate(`Bread and cereals` = contribution(month = month(date),
                              all_items_index = D7BU,
                              all_items_weight = CHZR,
                              component_index = D7D5,
                              component_weight = CJWB),
         `Meat` = contribution(month = month(date),
                             all_items_index = D7BU,
                             all_items_weight = CHZR,
                             component_index = D7D6,
                             component_weight = CJWC),
         `Fish` = contribution(month = month(date),
                             all_items_index = D7BU,
                             all_items_weight = CHZR,
                             component_index = D7D7,
                             component_weight = CJWD),
         `Milk, cheese and eggs` = contribution(month = month(date),
                              all_items_index = D7BU,
                              all_items_weight = CHZR,
                              component_index = D7D8,
                              component_weight = CJWE),
         `Oils and fats` = contribution(month = month(date),
                             all_items_index = D7BU,
                             all_items_weight = CHZR,
                             component_index = D7D9,
                             component_weight = CJWF),
         `Fruit` = contribution(month = month(date),
                              all_items_index = D7BU,
                              all_items_weight = CHZR,
                              component_index = D7DA,
                              component_weight = CJWG),
         `Vegetables` = contribution(month = month(date),
                                   all_items_index = D7BU,
                                   all_items_weight = CHZR,
                                   component_index = D7DB,
                                   component_weight = CJWH),
         `Sugar, jam and confectionery` = contribution(month = month(date),
                              all_items_index = D7BU,
                              all_items_weight = CHZR,
                              component_index = D7DC,
                              component_weight = CJWI),
         `Food products (nec)` = contribution(month = month(date),
                                 all_items_index = D7BU,
                                 all_items_weight = CHZR,
                                 component_index = D7DD,
                                 component_weight = CJWJ),
         `Coffee and tea` = contribution(month = month(date),
                               all_items_index = D7BU,
                               all_items_weight = CHZR,
                               component_index = D7DE,
                               component_weight = CJWK),
         `Mineral waters, soft drinks` = contribution(month = month(date),
                              all_items_index = D7BU,
                              all_items_weight = CHZR,
                              component_index = D7DF,
                              component_weight = CJWL)
  )


# put the data back into tidy form and plot some results
cont <- contribution |> 
  select(date, `Bread and cereals`:`Mineral waters, soft drinks`) |> 
  pivot_longer(cols = `Bread and cereals`:`Mineral waters, soft drinks`) |> 
  filter(!is.na(value))

cont |> 
  filter(date >= "2020-01-01") |> 
  ggplot() +
  geom_col(aes(x = date, y = value, fill = name)) +
  labs(title = "Contribution to annual food and non-alc inflation rate")
