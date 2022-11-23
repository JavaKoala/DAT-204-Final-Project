test_dataset <- read_csv('../test_data/dataset.csv')

# Test that the distinct_routes function returns a list of routes ordered
# by route number
test_that("a list of routes is returned", {
  test_routes <- distinct_routes(test_dataset)

  expect_equal(test_routes[1], "1 - FREEPORT ROAD")
  expect_equal(test_routes[104], "SLVR - Libary via Overbrook")
})

# Test that the day_types method returns the distinct day types
test_that("the route types are returned", {
  test_day_types <- day_types(test_dataset)

  expect_equal(test_day_types[1], "WEEKDAY")
  expect_equal(test_day_types[2], "SAT.")
  expect_equal(test_day_types[3], "SUN.")
})

# Test that the latest_on_time_percent function returns the latest percent
# for a given route and day type
test_that("the latest on time percent is returned", {
  expect_equal(
    latest_on_time_percent(test_dataset, '31 - BRIDGEVILLE', 'WEEKDAY'),
    "68.56%"
  )
  expect_equal(
    latest_on_time_percent(test_dataset, '31 - BRIDGEVILLE', 'SAT.'),
    "62.7%"
  )
})

test_that("NA is returned for an invalid route", {
  expect_equal(
    latest_on_time_percent(test_dataset, 'INVALID', 'WEEKDAY'),
    "NA"
  )
})

test_that("NA is returned for an invalid day type", {
  expect_equal(
    latest_on_time_percent(test_dataset, '31 - BRIDGEVILLE', 'INVALID'),
    "NA"
  )
})

# Test that the route_module_data function returns the data set filtered by
# route and day types
test_that("the route_module_data filters data by route", {
  filtered_data <- route_module_data(
    test_dataset,
    "6 - SPRING HILL",
    day_types(test_dataset)
  )
  distinct_route <- distinct(filtered_data['route_full_name'])[[1]]

  expect_equal(distinct_route, "6 - SPRING HILL")
})

test_that("the route_module_data filters data by day type", {
  filtered_data <- route_module_data(
    test_dataset,
    "8 - PERRYSVILLE",
    c("SAT.")
  )
  distinct_day_type <- distinct(filtered_data['day_type'])[[1]]

  expect_equal(distinct_day_type, "SAT.")
})

test_that("the route_module_data has the precent multiplied by 100", {
  filtered_data <- route_module_data(
    test_dataset,
    "11 - FINEVIEW",
    day_types(test_dataset)
  )
  test_percent <- slice_head(filtered_data)['on_time_percent_100'][[1]]

  expect_equal(test_percent, 74.38)
})

# Test that the mean_on_time_by_mode returns the mean on time percent by
# transportation mode
test_that("the data is aggregated by route and month start", {
  aggregate_data <- mean_on_time_by_mode(test_dataset)
  single_month <- aggregate_data %>%
    filter(month_start == "2019-05-01")
  start_month <- as.Date("2019-05-01")
  rounded_mean <- round(single_month$mean_on_time_percent, 2)

  expect_equal(single_month$mode, c("Bus", "Light Rail"))
  expect_equal(single_month$month_start, c(start_month, start_month))
  expect_equal(rounded_mean, c(66.25, 80.86))
})

