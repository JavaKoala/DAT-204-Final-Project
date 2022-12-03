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
    day_types(test_dataset),
    1
  )
  distinct_route <- distinct(filtered_data['route_full_name'])[[1]]

  expect_equal(distinct_route, "6 - SPRING HILL")
})

test_that("the route_module_data filters data by day type", {
  filtered_data <- route_module_data(
    test_dataset,
    "8 - PERRYSVILLE",
    c("SAT."),
    1
  )
  distinct_day_type <- distinct(filtered_data['day_type'])[[1]]

  expect_equal(distinct_day_type, "SAT.")
})

test_that("the route_module_data has the precent multiplied by 100", {
  filtered_data <- route_module_data(
    test_dataset,
    "11 - FINEVIEW",
    day_types(test_dataset),
    1
  )
  test_percent <- slice_head(filtered_data)['on_time_percent_100'][[1]]

  expect_equal(test_percent, 74.38)
})

test_that("the route_module_data filters 0 on time percent", {
  filtered_data <- route_module_data(
    test_dataset,
    "G3 - MOON FLYER",
    day_types(test_dataset),
    0
  )
  lowest_percent <- slice_head(filtered_data %>%
                                 arrange(on_time_percent_100)
                               )['on_time_percent_100'][[1]]

  expect_equal(lowest_percent, 56.74)
})

test_that("the route_module_data includes 0 on time percent", {
  filtered_data <- route_module_data(
    test_dataset,
    "G3 - MOON FLYER",
    day_types(test_dataset),
    1
  )
  lowest_percent <- slice_head(filtered_data %>%
                                 arrange(on_time_percent_100)
                              )['on_time_percent_100'][[1]]

  expect_equal(lowest_percent, 0)
})

# Test server actions and plots are returned
test_that("the latest on time percents are returned", {
  testServer(
    routeModuleServer,
    args = list(id = "route-module", dataset = test_dataset),
    {
      # Set the `route` dropdown and check the output
      session$setInputs(route = "1 - FREEPORT ROAD")
      expect_equal(output$latest_weekday, "63.52%")
      expect_equal(output$latest_sat, "53.02%")
      expect_equal(output$latest_sun, "52.91%")
    }
  )
})


test_that("confirm the on time percent plot can be accessed without error", {
  testServer(
    routeModuleServer,
    args = list(id = "route-module", dataset = test_dataset),
    {
      # Set the route and the day type
      session$setInputs(route = "2 - MOUNT ROYAL")
      session$setInputs(day_types = day_types(test_dataset))
      session$setInputs(include_zero = 1)

      expect_type(output$onTimePercentPlot, "list")
    }
  )
})

test_that("on time percent by data source plot can be accessed without error", {
  testServer(
    routeModuleServer,
    args = list(id = "route-module", dataset = test_dataset),
    {
      # Set the route and the day type
      session$setInputs(route = "4 - TROY HILL")
      session$setInputs(day_types = day_types(test_dataset))
      session$setInputs(include_zero = 0)

      expect_type(output$onTimePercentByDataSourcePlot, "list")
    }
  )
})

test_that("when all the data sources are NA verify the plot can be accessed", {
  testServer(
    routeModuleServer,
    args = list(id = "route-module", dataset = test_dataset),
    {
      # Set the route and the day type
      session$setInputs(route = "BLUE - SouthHills Village via Overbrook")
      session$setInputs(day_types = day_types(test_dataset))
      session$setInputs(include_zero = 1)

      expect_type(output$onTimePercentByDataSourcePlot, "list")
    }
  )
})
