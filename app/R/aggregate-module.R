# This module is used to display aggregate data
aggregateModuleUI <- function(id) {
  ns <- NS(id)

  h4("Graph 3: Mean Bus and Light Rail On Time Percent Over Time")
  plotOutput(outputId = ns("meanOnTimePercentByMode"))
}

aggregateModuleServer <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {

      # Plot of the on-time percent over time
      output$meanOnTimePercentByMode <- renderPlot({
        ggplot(
          data = mean_on_time_by_mode(dataset)) +
          geom_point(mapping = aes(
            x = month_start,
            y = mean_on_time_percent,
            color = mode)
          )
      })
    }
  )
}