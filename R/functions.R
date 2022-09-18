


get_series_metadata <- function(df, seriesid){

 series <- df %>% 
           dplyr::filter(cdid == seriesid)
 # browser()
 if(nrow(series) != 1){
   stop("Error: CDID not found")
 }

 out <- list(
   cdid = series$cdid,
   title = series$title,
   pre_unit = series$pre_unit,
   unit = series$unit,
   release_date = series$release_date,
   next_release = series$next_release,
   important_notes = series$important_notes
 )
 
 return(out)
  
}



# text for up and down
eval_change <- function(num1, num2) {
  text <-  ""
  
  if (num1 == num2){
    text = "unchanged"
  }
  
  if(num1 < num2){
    text =  "down"
  }
  
  if(num1 > num2){
    text =  "up"
  }
  return(text)
}

# get the value for a series and date
get_val <- function(df, cdidcode, series_date){
  
  val <- df %>% 
    filter(cdid == cdidcode & date == series_date) %>% 
    pull(value)
  
  return(val)
  
}

get_val_month <- function(df, cdid, series_date){
  # require(lubridate)
  series_date <- lubridate::ymd(series_date)
  
  val <- get_val(df, cdid, series_date)
  prev_mth_date <- series_date %m-% months(1)
  prev_mth_val <- get_val(df, cdid, series_date %m-% months(1))
  prev_yr_date <- series_date %m-% years(1)
  prev_yr_val <- get_val(df, cdid, series_date %m-% years(1))
  
      
  out <- list(
    cdid = cdid,
    date = series_date,
    val = val,
    prev_mth_date = prev_mth_date,
    prev_mth_val = prev_mth_val,
    prev_yr_date = prev_yr_date,
    prev_yr_val = prev_yr_val
  )
  
  return(out)
}

get_prev_high <- function(df, cdid, series_date) {
  c <- cdid
  d <- series_date
  
  val <- get_val(df, c, d)
  
  date <- df %>% 
    filter(cdid == c & date < d & value >= val) %>% 
    pull(date) %>% 
    max
  prev_val <- get_val(df, c, date)
  
  out <- list(
    prev_high_mth = date,
    prev_high_val = prev_val
  )
  return(out)
}

# extract a useful title from the series name. Use a regex to strip stuff from
# beginning and end of string. defaults of "" will fail, need to fix this
tidy_series_name <- function(string, start = "", end = ""){
  string1 <- str_remove(string, start)
  string2 <- str_remove(string1, end)
  string2 <- str_to_lower(string2)
  return(string2)
}

# c("Overall inflation", "Food inflation")
# labs(title = "General and Food inflation",
#      subtitle = "year on year inflation",
#      caption = paste("All items series", cpih_all_yy, "food series", cpih_food_yy)) +

line_chart <- function(cdids, start_date = "2020-01-01", labels){
  if(missing(labels))(labels = cdids)
  
  data <- mm23_month %>% 
    filter(cdid %in% cdids & value != is.na(value) & date >= start_date) %>% 
  ggplot() +
    geom_line(aes(x = date, y = value, colour = cdid), size = 1) +
    scale_color_discrete(breaks = cdids, labels = labels) +
    scale_y_continuous(labels = label_percent(scale = 1,accuracy = 0.1),breaks = breaks_extended(10)) +
    scale_x_date(date_labels = "%b %Y") 
    # theme_fsr()
  
}

