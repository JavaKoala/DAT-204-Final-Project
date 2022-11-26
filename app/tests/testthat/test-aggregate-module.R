test_dataset <- read_csv('../test_data/dataset.csv')

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

# Test that the mean_mode_on_time_percent_by_day_type
# returns the mean Bus on time percent by day type
test_that("the bus data is aggregated by day type and month start", {
  aggregate_data <- mean_mode_on_time_percent_by_day_type(test_dataset, "Bus")
  single_month <- aggregate_data %>%
    filter(month_start == "2019-05-01")
  start_month <- as.Date("2019-05-01")
  rounded_mean <- round(single_month$mean_on_time_percent, 2)

  expect_equal(single_month$day_type, c("SAT.", "SUN.", "WEEKDAY"))
  expect_equal(single_month$month_start, rep(start_month, 3))
  expect_equal(rounded_mean, c(65.61, 70.13, 64.37))
})

# Test that the mean_mode_on_time_percent_by_day_type
# returns the mean Light Rail on time percent by day type
test_that("the light rail data is aggregated by day type and month start", {
  aggregate_data <- mean_mode_on_time_percent_by_day_type(
    test_dataset, "Light Rail")
  single_month <- aggregate_data %>%
    filter(month_start == "2019-05-01")
  start_month <- as.Date("2019-05-01")
  rounded_mean <- round(single_month$mean_on_time_percent, 2)

  expect_equal(single_month$day_type, c("SAT.", "SUN.", "WEEKDAY"))
  expect_equal(single_month$month_start,  rep(start_month, 3))
  expect_equal(rounded_mean, c(79.74, 80.61, 82.24))
})

# Test that the mean_bus_on_time_percent_by_garage
# returns the mean bus on time percent by garage
test_that("the bus data is aggregated by garage and month start", {
  aggregate_data <- mean_bus_on_time_percent_by_garage(test_dataset)
  single_month <- aggregate_data %>%
    filter(month_start == "2019-05-01")
  start_month <- as.Date("2019-05-01")
  rounded_mean <- round(single_month$mean_on_time_percent, 2)
  current_garages <- c(
    "Collier",
    "East Liberty",
    "East Liberty/West Mifflin",
    "Ross",
    "West Mifflin"
  )

  expect_equal(single_month$current_garage, current_garages)
  expect_equal(
    single_month$month_start, rep(start_month, 5)
  )
  expect_equal(rounded_mean, c(70.52, 63.73, 66.95, 64.57, 67.17))
})

# Test server
test_that("on time percent by route plot can be accessed without error", {
  testServer(
    aggregateModuleServer,
    args = list(id = "aggregate-module", dataset = test_dataset),
    {
      expect_type(output$meanOnTimePercentByMode, "list")
    }
  )
})

test_that("the mean bus on time precent plot can be accessed without error", {
  testServer(
    aggregateModuleServer,
    args = list(id = "aggregate-module", dataset = test_dataset),
    {
      expect_type(output$meanBusOnTimePercentByDayType, "list")
    }
  )
})

test_that("the mean light rail precent plot can be accessed without error", {
  testServer(
    aggregateModuleServer,
    args = list(id = "aggregate-module", dataset = test_dataset),
    {
      expect_type(output$meanLightRailOnTimePercentByDayType, "list")
    }
  )
})

test_that("the mean bus by garage plot can be accessed without error", {
  testServer(
    aggregateModuleServer,
    args = list(id = "aggregate-module", dataset = test_dataset),
    {
      expect_type(output$meanBusOnTimePercentByGarage, "list")
    }
  )
})