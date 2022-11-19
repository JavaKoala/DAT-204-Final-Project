# This module is used to select and display data for individual routes
# The routes are selected by name and day type
# The latest month's on time percentage for each day type is displayed
# A scatter plot of the on time percentage over time is displayed
#   with different colors for each day type
routeModuleUI <- function(id, distinct_routes, day_types) {
  # All uses of Shiny input/output IDs in the UI must be namespaced
  ns <- NS(id)

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      fluidRow(
        column(12,
               selectInput(
                 ns("route"),
                 h3("Select route"),
                 choices = distinct_routes
               )
        )
      ),
      fluidRow(
        column(6,
               checkboxGroupInput(ns("day_types"),
                                  h3("Day Type"),
                                  choices = day_types,
                                  selected = day_types))
      ),
      fluidRow(
        column(12,
               h4("Latest On Time Percent"),
               column(4,
                      h5("WEEKDAY"),
                      p(textOutput(ns("latest_weekday")))
               ),
               column(4,
                      h5("SAT."),
                      p(textOutput(ns("latest_sat")))
               ),
               column(4,
                      h5("SUN."),
                      p(textOutput(ns("latest_sun")))
               )
        )
      ),
    ),

    # Main panel for displaying outputs
    mainPanel(

      # Output: On time percent plot
      plotOutput(outputId = ns("onTimePercentPlot")),

      # Output: On time percent by data source
      plotOutput(outputId = ns("onTimePercentByDataSourcePlot"))
    )
  )
}

routeModuleServer <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      # Latest WEEKDAY
      output$latest_weekday <- renderText({
        latest_on_time_percent(dataset, input$route, "WEEKDAY")
      })

      # Latest SAT
      output$latest_sat <- renderText({
        latest_on_time_percent(dataset, input$route, "SAT.")
      })
      
      # Latest SUN
      output$latest_sun <- renderText({
        latest_on_time_percent(dataset, input$route, "SUN.")
      })

      # Plot of the on-time percent over time
      output$onTimePercentPlot <- renderPlot({
        ggplot(
          data = filter(
            dataset,
            route_full_name == input$route & day_type %in% input$day_types
          )) +
          geom_point(mapping = aes(
            x = month_start,
            y = on_time_percent,
            color = day_type)
          ) +
          ggtitle("On Time Percent Per Month") +
          xlab("Month") +
          scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") +
          ylab("On Time Percent") +
          labs(color = "Day Type") +
          theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 14)
          )
      })

      # Plot of the on-time percent by data source over time
      output$onTimePercentByDataSourcePlot <- renderPlot({
        ggplot(
          data = filter(
            dataset,
            route_full_name == input$route & day_type %in% input$day_types
          )) +
          geom_point(mapping = aes(
            x = month_start,
            y = on_time_percent,
            color = data_source)
          ) +
          geom_smooth(
            mapping = aes(
              x = month_start,
              y = on_time_percent,
              color = data_source),
            method = "loess",
            formula = "y ~ x"
          ) +
          ggtitle("On Time Percent By Data Source") +
          xlab("Month") +
          scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") +
          ylab("On Time Percent") +
          labs(color = "Data Source") +
          theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 14)
          )
      })
    }
  )
}