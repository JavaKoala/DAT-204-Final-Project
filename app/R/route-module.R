# This module is used to select and display data for individual routes
# The routes are selected by name and day type
# The latest month's on time percentage for each day type is displayed
# A scatter plot of the on time percentage over time is displayed
#   with different colors for each day type
routeModuleUI <- function(id, dataset) {
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
                 choices = distinct_routes(dataset)
               )
        )
      ),
      fluidRow(
        column(6,
               checkboxGroupInput(ns("day_types"),
                                  h3("Day Type"),
                                  choices = day_types(dataset),
                                  selected = day_types(dataset)))
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

      h4("Graph 1: On time percent vs month"),
      p("This is a graph of the selected route's on time percent by month."),
      p("Use the Day Type checkboxes to change the day type."),

      # Output: On time percent plot
      plotOutput(outputId = ns("onTimePercentPlot")),

      h4("Graph 2: On time percent vs data source"),
      p("From the data description:"),
      p("Starting in October 2018, Port Authority moved to a different OTP
           recording system called Clever.
         OTP data from the Clever system is more accurate because
           it uses more timepoints; the previous system excluded a large portion
           of data from OTP processing due to minor technical issues with rider
           counts on certain trips."
      ),
      p("This graph displays the different measuring systems by color and adds
           a smoothing function to the plot for each system.
         The grey portion around the lines is the confidence interval."),
      p("It is not clear from the data description what measuring system is used
           when it is not defined (NA)."),
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
          data = route_module_data(dataset, input$route, input$day_types)) +
          geom_point(mapping = aes(
            x = month_start,
            y = on_time_percent_100,
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
          data = route_module_data(dataset, input$route, input$day_types)) +
          geom_point(mapping = aes(
            x = month_start,
            y = on_time_percent_100,
            color = data_source)
          ) +
          geom_smooth(
            mapping = aes(
              x = month_start,
              y = on_time_percent_100,
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