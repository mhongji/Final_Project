FROM rocker/r-ver:4.4.0

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev && \
    rm -rf /var/lib/apt/lists/*

# Install all needed R packages in one single RUN
RUN R -e "install.packages(c('plumber','readr','dplyr','caret','yardstick','rsample','randomForest','rpart','here','tidyverse','tidymodels','forcats','knitr'), repos='https://cloud.r-project.org')"

WORKDIR /usr/src/app

COPY R/api.R    R/api.R
COPY model/     model/
COPY data/      data/

EXPOSE 8000