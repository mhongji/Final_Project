# ─── 1) Base image ───────────────────────────────────────────────────────────────
FROM rocker/r-ver:4.4.0

# ─── 2) System dependencies ────────────────────────────────────────────────────
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev && \
    rm -rf /var/lib/apt/lists/*

# ─── 3) Install R packages (including plumber) via install2.r ───────────────────
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')" && \
    R -e "remotes::install_cran(c('plumber','readr','dplyr','caret','yardstick','rsample','randomForest','rpart','here'))"

# ─── 4) App directory ───────────────────────────────────────────────────────────
WORKDIR /usr/src/app

# ─── 5) Copy API, model, and data ───────────────────────────────────────────────
COPY R/api.R    R/api.R
COPY model/     model/
COPY data/      data/

# ─── 6) Expose Plumber port ─────────────────────────────────────────────────────
EXPOSE 8000

# ─── 7) Run Plumber ────────────────────────────────────────────────────────────
CMD ["R", "-e", "library(plumber); pr <- plumber::plumb('R/api.R'); pr$run(host='0.0.0.0', port=8000)"]


