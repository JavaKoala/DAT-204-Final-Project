# This module is used to display regression data
regressionModuleUI <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    # Sidebar panel layout with input and output definitions
    sidebarPanel(
      fluidRow(
        column(12,
               dateInput(ns("date"),
                         label = "Date Input",
                         value = Sys.Date())
        )
      ),
      fluidRow(
        column(12,
               h4("Linear prediction"),
                 column(12,
                        h5("On Time Percent"),
                        p(textOutput(ns("linear_prediction"))),
                        h5("R Squared"),
                        p(textOutput(ns("linear_summary"))))
        )
      ),
    ),

    # Main panel for displaying outputs
    mainPanel(
      h4("Linear regression training set"),
      plotOutput(outputId = ns("linearTrainingSet")),

      h4("Linear regression test set"),
      plotOutput(outputId = ns("linearTestSet"))
    )
  )
}

# This is the server portion of the regression module
regressionModuleServer <- function(id, dataset) {
  # First filter the dataset by Bus and on time percent greater than 0
  # Add on_time_percent_100 for percent
  filtered_dataset <- dataset %>%
    filter(mode == "Bus" & on_time_percent > 0) %>%
    mutate(on_time_percent_100 = on_time_percent * 100)

  # split the filtered data set into training and test sets
  split <- sample.split(filtered_dataset$on_time_percent_100, SplitRatio = 2/3)
  training_set <- subset(filtered_dataset, split == TRUE)
  test_set <- subset(filtered_dataset, split == FALSE)

  # create linear regressor from on time percent and month start
  regressor <- lm(formula = on_time_percent_100 ~ month_start,
                 data = training_set)

  moduleServer(
    id,
    function(input, output, session) {
      # assign the linear_prediction using the linear model
      output$linear_prediction <- renderText({
        predict(regressor, newdata = list(month_start = input$date))
      })

      # assign the linear_summary using the linear model
      output$linear_summary <- renderText({
        summary(regressor)$r.squared
      })

      # plot for the linear model training set
      output$linearTrainingSet <- renderPlot({
        ggplot() +
          geom_point(aes(x = training_set$month_start,
                         y = training_set$on_time_percent_100),
                     color = '#00BA38') +
          geom_line(aes(x = training_set$month_start,
                        y = predict(regressor, newdata = training_set)),
                    color = '#F8766D',
                    linewidth = 1) +
          ggtitle('On Time Percet vs Month Start (Traning set)') +
          xlab('Month Start') +
          ylab('On Time Percent')
      })

      # plot for the linear model test set
      output$linearTestSet <- renderPlot({
        ggplot() +
          geom_point(aes(x = test_set$month_start,
                         y = test_set$on_time_percent_100),
                     color = '#00BA38') +
          geom_line(aes(x = training_set$month_start,
                        y = predict(regressor, newdata = training_set)),
                    color = '#F8766D',
                    linewidth = 1) +
          ggtitle('On Time Percet vs Month Start (Test set)') +
          xlab('Month Start') +
          ylab('On Time Percent')
      })
    }
  )
}