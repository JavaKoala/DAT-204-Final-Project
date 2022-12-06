FROM rocker/r-base:latest
LABEL maintainer="bmeye32@acd.ccac.edu"
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*
RUN install2.r shiny tidyverse caTools --error --skipinstalled
RUN addgroup --system app \
    && adduser --system --ingroup app app
COPY ./app/ /srv/shiny-server/
RUN chown app:app -R /srv/shiny-server/
USER app
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server')"]

