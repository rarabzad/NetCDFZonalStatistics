# Base image with R and Shiny
FROM rocker/shiny:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libglpk-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libnetcdf-dev \
    libv8-dev \
    libjq-dev \
    libprotobuf-dev \
    protobuf-compiler \
    cargo \
    gdal-bin \
    unzip \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c(\
    'shiny', 'shinyjs', 'shinyWidgets', 'DT', 'lubridate', 'sf', 'ncdf4', \
    'plotly', 'reshape2', 'ggplot2', 'geosphere', 'sp', 'lwgeom', 'rmapshaper', 'htmltools'\
    ), repos='https://cloud.r-project.org')"

# Copy app files to image
COPY . /srv/shiny-server/

# Fix permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=3838)"]
