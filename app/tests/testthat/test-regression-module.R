test_dataset <- read_csv('../test_data/dataset.csv')

# Test regression predictions
test_that("the linear model prediction is displayed", {
  testServer(
    regressionModuleServer,
    args = list(id = "regression-module", dataset = test_dataset),
    {
      # Set the date and check the linear_prediction result
      session$setInputs(date = as.Date("2022-12-05"))
      expect_match(output$linear_prediction, "^71")
    }
  )
})

test_that("the linear model R squared value is displayed", {
  testServer(
    regressionModuleServer,
    args = list(id = "regression-module", dataset = test_dataset),
    {
      # Set the date and check the linear_prediction result
      session$setInputs(date = as.Date("2022-12-05"))
      expect_match(output$linear_summary, "^0.01")
    }
  )
})

# Test plots
test_that("the on linear reg training plot can be accessed without error", {
  testServer(
    regressionModuleServer,
    args = list(id = "regression-module", dataset = test_dataset),
    {
      # Set the date
      session$setInputs(date = as.Date("2022-12-05"))
      
      expect_type(output$linearTrainingSet, "list")
    }
  )
})

test_that("the on linear reg test plot can be accessed without error", {
  testServer(
    regressionModuleServer,
    args = list(id = "regression-module", dataset = test_dataset),
    {
      # Set the date
      session$setInputs(date = as.Date("2022-12-05"))
      
      expect_type(output$linearTestSet, "list")
    }
  )
})