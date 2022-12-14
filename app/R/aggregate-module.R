# function to return the mean on time percent by mode
mean_on_time_by_mode <- function(dataset, include_zero) {
  dataset %>%
    filter(mode != "NA") %>%
    filter(
      if (include_zero == 1) {
        on_time_percent >= 0
      } else {
        on_time_percent > 0
      }
    ) %>%
    mutate(on_time_percent_100 = on_time_percent * 100) %>%
    group_by(mode, month_start) %>%
    summarize(mean_on_time_percent = mean(on_time_percent_100, na.rm = TRUE))
}

# function to return the mean mode on time percent by day type
mean_mode_on_time_percent_by_day_type <- function(
    dataset, trans_mode, include_zero) {
  dataset %>%
    filter(mode == trans_mode) %>%
    filter(
      if (include_zero == 1) {
        on_time_percent >= 0
      } else {
        on_time_percent > 0
      }
    ) %>%
    mutate(on_time_percent_100 = on_time_percent * 100) %>%
    group_by(day_type, month_start) %>%
    summarize(mean_on_time_percent = mean(on_time_percent_100, na.rm = TRUE))
}

# function to return the mean bus on time percent by garage
mean_bus_on_time_percent_by_garage <- function(dataset, include_zero) {
  dataset %>%
    filter(mode == "Bus") %>%
    filter(
      if (include_zero == 1) {
        on_time_percent >= 0
      } else {
        on_time_percent > 0
      }
    ) %>%
    mutate(on_time_percent_100 = on_time_percent * 100) %>%
    group_by(current_garage, month_start) %>%
    summarize(mean_on_time_percent = mean(on_time_percent_100, na.rm = TRUE))
}


# This module is used to display aggregate data
aggregateModuleUI <- function(id) {
  ns <- NS(id)

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      fluidRow(
        column(12,
               radioButtons(ns("include_zero"),
                            h4("0 Percent"),
                            choices = list("Include" = 1, "Exclude" = 0),
                            selected = 1))
      ),
      width = 2
    ),

    # Main panel for displaying outputs
    mainPanel(
      h4("Graph 3: Mean Bus and Light Rail On Time Percent Over Time"),
      p("This graph is shows the difference between Bus and Light Rail on
          time percentages."),
      plotOutput(outputId = ns("meanOnTimePercentByMode")),

      h4("Graph 4: Mean Bus On Time Percent By Day Type"),
      p("This graph is shows the differences in on time percentages by day type"),
      p("The dip in SAT. and SUN. appear to correspond to the switch to Celver"),
      plotOutput(outputId = ns("meanBusOnTimePercentByDayType")),

      h4("Graph 5: Mean Light Rail On Time Percent By Day Type"),
      p("This graph is shows the differences in on time percentages by day type"),
      plotOutput(outputId = ns("meanLightRailOnTimePercentByDayType")),

      h4("Graph 6: Mean Bus On Time Percent By Current Garage"),
      p("This graph is shows the bus differences in on time percentages
          by garage"),
      plotOutput(outputId = ns("meanBusOnTimePercentByGarage")),
      width = 10
    )
  )
}

aggregateModuleServer <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {

      # Plot of the mean on-time percent over time vs mode
      output$meanOnTimePercentByMode <- renderPlot({
        ggplot(
          data = mean_on_time_by_mode(dataset, input$include_zero),
          aes(month_start, mean_on_time_percent, color = mode)) +
          geom_line(linewidth = 1) +
          ggtitle("Mean On Time Percent By Transportation Mode") +
          scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") +
          labs(color = "Mode") +
          xlab("Month") +
          ylab("Mean On Time Percent") +
          theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 14)
          ) +
          scale_color_manual(values = c("Bus" = "#F8766D",
                                        "Light Rail" = "#00BA38"))
      })

      # Plot of Bus mean on-time percent over time vs mode
      output$meanBusOnTimePercentByDayType <- renderPlot({
        ggplot(
          data = mean_mode_on_time_percent_by_day_type(
            dataset, "Bus", input$include_zero),
          aes(month_start, mean_on_time_percent, color = day_type)) +
          geom_line(linewidth = 1) +
          ggtitle("Mean Bus On Time Percent By Day Type") +
          scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") +
          labs(color = "Day Type") +
          xlab("Month") +
          ylab("Mean On Time Percent") +
          theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 14)
          )
      })

      # Plot of Light Rail mean on-time percent over time vs mode
      output$meanLightRailOnTimePercentByDayType <- renderPlot({
        ggplot(
          data = mean_mode_on_time_percent_by_day_type(
            dataset, "Light Rail", input$include_zero),
          aes(month_start, mean_on_time_percent, color = day_type)) +
          geom_line(linewidth = 1) +
          ggtitle("Mean Light Rail On Time Percent By Day Type") +
          scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") +
          labs(color = "Day Type") +
          xlab("Month") +
          ylab("Mean On Time Percent") +
          theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 14)
          )
      })

      # Plot of Bus mean on-time percent over time by garage
      output$meanBusOnTimePercentByGarage <- renderPlot({
        ggplot(
          data = mean_bus_on_time_percent_by_garage(
            dataset, input$include_zero),
          aes(month_start, mean_on_time_percent, color = current_garage)) +
          geom_line(linewidth = 1) +
          ggtitle("Mean Bus On Time Percent By Garage") +
          scale_x_date(date_breaks = "6 month", date_labels = "%m/%y") +
          labs(color = "Garage") +
          xlab("Month") +
          ylab("Mean On Time Percent") +
          theme(
            plot.title = element_text(hjust = 0.5, size = 16),
            axis.title = element_text(size = 14)
          )
      })
    }
  )
}