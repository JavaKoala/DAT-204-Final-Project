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
#   Testing references:
#   https://shiny.rstudio.com/articles/testing-overview.html
#   https://shiny.rstudio.com/articles/server-function-testing.html
#   https://mastering-shiny.org/scaling-testing.html
#
#   Dockerizing shiny apps:
#   https://hosting.analythium.io/dockerizing-shiny-applications/
#   https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/
#   https://hosting.analythium.io/running-shiny-server-in-docker/
#
#   Build the application
#   docker build -t final-project-shiny-app .
#
#   Run the application
#   docker run --rm -p 3838:3838 final-project-shiny-app
#
#   Time series plots
#   https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
#
#   Scaling colors manually so they do not cycle when changing options
#   https://ggplot2.tidyverse.org/reference/scale_manual.html
#
#   ggplot2 default colors
#   https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette
#   library(scales)
#   show_col(hue_pal()(3))
#
#   Linear regression in R
#   https://www.udemy.com/course/machinelearning/ chapters 5 - 13
#
################################################################################

# Set working directory
# setwd("/srv/shiny-server/")

# Install the tidyverse
# install.packages("tidyverse")
# install.packages("shiny")
# install.packages("caTools")
library(shiny)
library(tidyverse)
library(caTools)

# Set working directory to project then
dataset <- read_csv('dataset.csv')

# This is the start of the UI portion of the application
ui <- fluidPage(

  # Application title
  titlePanel(
    "DAT-204 Final Project -
     Pittsburgh Port Authority On Time Percent Analysis"
  ),

  # Data set link
  h4("Dataset link"),
  a("https://catalog.data.gov/dataset/port-authority-monthly-on-time-performance-by-route",
    href = "https://catalog.data.gov/dataset/port-authority-monthly-on-time-performance-by-route",
    target = "_blank"),

  # Part 1: Analysis by route
  h3("Part 1: Analysis by Route"),
  p("The first analysis is by route.
     The route can be selected along with the day type, weekend, Saturday, or
     Sunday."),
  p("Some routes do not run on each day type."),

  # UI portion of the route module
  routeModuleUI("route-module", dataset),

  # Part 2: Aggregate analysis
  h3("Part 2: Aggregate analysis"),
  p("The second analysis is aggregated across multiple routes"),

  # UI portion of the aggregate module
  aggregateModuleUI("aggregate-module"),

  h3("Part 3: Regression analysis"),
  p("The third analysis is a regression analysis using different methods. The
       dataset is filtered by Bus routes and on time percent greater than 0.
       The dataset is then split into a training set and test set."),
  p("Use the date picker and the on-time percent will be predicted from
       the model."),
  regressionModuleUI("regression-module")
)

# Define server logic 
server <- function(input, output) {
  # Server portion of the route module
  routeModuleServer("route-module", dataset)

  # Server portion of the aggregate module
  aggregateModuleServer("aggregate-module", dataset)

  # Server portion of the regression module
  regressionModuleServer("regression-module", dataset)
}

# run the server
shinyApp(
  ui = ui,
  server = server,
  options = list("port" = 3838, "host" = "0.0.0.0")
)
