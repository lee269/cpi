
cdid_chart <- function(data, cdids, freq = "M", start_date = "2020-01-01", labels){
  if(missing(labels))(labels = cdids)
  
  data <- data %>% 
    dplyr::filter(cdid %in% cdids & value != is.na(value) & date >= start_date) %>% 
    dplyr::filter(period == freq)
  
  chart <- data %>% 
    ggplot2::ggplot() +
    ggplot2::geom_line(aes(x = date, y = value, colour = cdid), size = 1) +
    ggplot2::scale_color_discrete(breaks = cdids, labels = labels) +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 0.1),
                                breaks = scales::breaks_extended(10)) +
    ggplot2::scale_x_date(date_labels = "%b %Y")
  
  out <- list(chart = chart, data = data)
  return(out)
}
