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

# Test that the mean_bus_on_time_percent_by_day_type
# returns the mean Bus on time percent by day type
test_that("the data is aggregated by day type and month start", {
  aggregate_data <- mean_bus_on_time_percent_by_day_type(test_dataset)
  single_month <- aggregate_data %>%
    filter(month_start == "2019-05-01")
  start_month <- as.Date("2019-05-01")
  rounded_mean <- round(single_month$mean_on_time_percent, 2)

  print(rounded_mean)
  expect_equal(single_month$day_type, c("SAT.", "SUN.", "WEEKDAY"))
  expect_equal(
    single_month$month_start, c(start_month, start_month, start_month)
  )
  expect_equal(rounded_mean, c(65.61, 70.13, 64.37))
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