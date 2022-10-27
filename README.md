# DAT-204 Final Project

# Data analysis of on time percentage of Pittsburgh Port Authority on time percentage

## Purpose

The purpose of this project is to demonstrate data analysis techniques using R

---

## Running the application

### Running the application with RStudio

- Install RStudio https://posit.co/products/open-source/rstudio/
- Open `app/app.R` in RStudio
- Click the "Run App" button in RStudio

### Running the application with Docker

- Install Docker desktop docker.com/products/docker-desktop/
- Build the docker container from a terminal. This will take some time.
  - `docker build -t final-project-shiny-app .`
- Run the docker container
  - `docker run --rm -p 3838:3838 final-project-shiny-app`
- Navigate to `localhost:3838` in a web browser to view the application

## References

#### Location of the Port Authority data

https://catalog.data.gov/dataset/port-authority-monthly-on-time-performance-by-route
