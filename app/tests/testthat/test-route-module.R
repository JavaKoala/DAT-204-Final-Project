test_dataset <- read_csv('../test_data/dataset.csv')
day_types <- distinct(test_dataset['day_type'])[[1]]

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
      session$setInputs(day_types = day_types)
      
      expect_type(output$onTimePercentPlot, "list")
    }
  )
})
