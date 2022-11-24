test_dataset <- read_csv('../test_data/dataset.csv')

test_that("on time percent by route plot can be accessed without error", {
  testServer(
    aggregateModuleServer,
    args = list(id = "aggregate-module", dataset = test_dataset),
    {
      expect_type(output$meanOnTimePercentByMode, "list")
    }
  )
})