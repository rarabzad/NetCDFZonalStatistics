# Use the official R + Shiny + tidyverse image
FROM rocker/shiny-verse:4.3.1

# Install system dependencies for spatial packages
RUN apt-get update && apt-get install -y --no-install-recommends \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libjq-dev \
    jq \
    libprotobuf-dev \
    protobuf-compiler \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shinyjs', \
    'shinyWidgets', \
    'DT', \
    'lubridate', \
    'sf', \
    'ncdf4', \
    'plotly', \
    'reshape2', \
    'ggplot2', \
    'geosphere', \
    'sp', \
    'lwgeom', \
    'rmapshaper' \
    ), repos='https://cloud.r-project.org/')"

# Copy app files
COPY . /srv/shiny-server/

# Set permissions
RUN chmod -R 755 /srv/shiny-server/ && \
    chown -R shiny:shiny /srv/shiny-server

# Expose Shiny port
EXPOSE 3838

# Run app
CMD ["/usr/bin/shiny-server"]
