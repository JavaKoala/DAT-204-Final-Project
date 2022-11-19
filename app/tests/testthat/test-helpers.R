test_dataset <- read_csv('../test_data/dataset.csv')

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
