# Final Project ################################################################
# Autor: Ben Meyer
# Date Created: 2022-10-26
# Revision/Release: 0.1.0
# Purpose: Data analysis of on time percentage of Pittsburgh Port Authority
#          on time percentage
# Copyright: 2022 JavaKoala
#   Permission is hereby granted, free of charge, to any person obtaining a copy
#   of this software and associated documentation files (the "Software"), to
#   deal in the Software without restriction, including without limitation the
#   rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
#   sell copies of the Software, and to permit persons to whom the Software is
#   furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in
#   all copies or substantial portions of the Software.
#   
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
#   IN THE SOFTWARE.
# Author Contact Information: JavaKoala
# Notes:
#   Dataset link:
#   https://catalog.data.gov/dataset/port-authority-monthly-on-time-performance-by-route
#
#   Dockerizing shiny apps:
#   https://hosting.analythium.io/dockerizing-shiny-applications/
#   https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/
#   https://hosting.analythium.io/running-shiny-server-in-docker/
#
#   Time series plots
#   https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#
#   Build the application
#   docker build -t final-project-shiny-app .
#
#   Run the application
#   docker run --rm -p 3838:3838 final-project-shiny-app
################################################################################

# Set working directory
# setwd("/srv/shiny-server/")

# Install the tidyverse
# install.packages("tidyverse")
# install.packages("shiny")
library(shiny)
library(tidyverse)

# Set working directory to project then
dataset <- read_csv('dataset.csv')

# Create distinct routes for options
distinct_routes <- distinct(dataset['route_full_name'])[[1]]
day_types <- distinct(dataset['day_type'])[[1]]

# This is the start of the UI portion of the application
ui <- fluidPage(

  # Application title
  titlePanel("Final Project"),

  # Sidebar layout with input and output definitions
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      fluidRow(
        column(12,
          selectInput(
            "route",
            h3("Select route"),
            choices = distinct_routes
          )
        )
      ),
      fluidRow(
        column(6,
          checkboxGroupInput("day_types",
                             h3("Day Type"),
                             choices = day_types,
                             selected = day_types))
      ),
      fluidRow(
        column(12,
          h4("Latest On Time Percent"),
          column(4,
            h5("WEEKDAY"),
            p(textOutput("latest_weekday"))
          ),
          column(4,
            h5("SAT."),
            p(textOutput("latest_sat"))
          ),
          column(4,
            h5("SUN."),
            p(textOutput("latest_sun"))
          )
        )
      ),
    ),

    # Main panel for displaying outputs
    mainPanel(

      # Output: On time percent plot
      plotOutput(outputId = "onTimePercentPlot")

    )
  )
)

# Define server logic 
server <- function(input, output) {
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
        dataset, route_full_name == input$route & day_type %in% input$day_types
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
}

# run the server
shinyApp(
  ui = ui,
  server = server,
  options = list("port" = 3838, "host" = "0.0.0.0")
)
