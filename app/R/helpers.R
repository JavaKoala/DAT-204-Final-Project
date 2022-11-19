# function to return the latest on time percent
latest_on_time_percent <- function(dataset, route, route_day_type) {
  route_name <- route
  latest_percent <- slice_head(
    arrange(
      filter(
        dataset,
        route_full_name == route_name & day_type == route_day_type),
      desc(month_start)
    )
  )['on_time_percent'] * 100
  
  if (toString(latest_percent) == "numeric(0)") {
    return("NA")
  } else {
    return(paste0(toString(latest_percent), "%"))
  }
}