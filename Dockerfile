# Use the official R image with Shiny Server pre-installed
FROM rocker/shiny:latest

# System libraries needed for your R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libnetcdf-dev \
    unzip \
    wget \
    pandoc \
    pandoc-citeproc \
    libv8-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Copy your app into the image
COPY . /srv/shiny-server/

# Install required R packages
RUN R -e "install.packages(c( \
    'shiny', 'shinyjs', 'shinyWidgets', 'DT', 'lubridate', 'sf', \
    'ncdf4', 'plotly', 'reshape2', 'ggplot2', 'htmltools' \
  ), repos='https://cloud.r-project.org')"

# Set permissions for shiny user
RUN chown -R shiny:shiny /srv/shiny-server

# Expose default Shiny Server port
EXPOSE 3838

# Run the Shiny Server
CMD ["/usr/bin/shiny-server"]
