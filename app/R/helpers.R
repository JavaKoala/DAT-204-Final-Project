# function to return the list of distinct routes
# used by the route-module for the route selector
distinct_routes <- function(dataset) {
  distinct(dataset['route_full_name'])[[1]]
}

# function used to return the distinct day types
# used by the route-module for the day type options
day_types <- function(dataset) {
  distinct(dataset['day_type'])[[1]]
}

# function to return the latest on time percent
# used by the route-module to find the last on time percent
latest_on_time_percent <- function(dataset, route, route_day_type) {
  route_name <- route
  latest_percent <- slice_head(
    dataset %>%
      filter(route_full_name == route_name & day_type == route_day_type) %>%
      arrange(desc(month_start))
  )['on_time_percent'] * 100
  
  if (toString(latest_percent) == "numeric(0)") {
    return("NA")
  } else {
    return(paste0(toString(latest_percent), "%"))
  }
}

# function to return the data used for the route module graphs
route_module_data <- function(dataset, route, day_types) {
  route_name <- route
  dataset %>%
    filter(route_full_name == route_name & day_type %in% day_types) %>%
    mutate(on_time_percent_100 = on_time_percent * 100)
}