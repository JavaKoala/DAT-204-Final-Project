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
