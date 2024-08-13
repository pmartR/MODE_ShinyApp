## MODE Application: Dockerfile
## Last Updated: 2024_05_13

## Build with the "--no-cache" flag

# Install latest version of rocker image: base image for MODE
# Standalone version: code-registry.emsl.pnl.gov/multiomics-analyses/mode-app/standalone:1.1.0
FROM code-registry.emsl.pnl.gov/multiomics-analyses/mode-app/base:1.0.3

# Add dependency for new trelliscope
RUN apt-get update
RUN apt-get install -y libmagick++-6.q16-dev

RUN Rscript -e "devtools::install_github('trelliscope/trelliscope')"
RUN Rscript -e "devtools::install_github('yang-tang/shinyjqui')"
RUN Rscript -e "devtools::install_github('daattali/shinycssloaders')"

# Install latest pmartR
RUN Rscript -e "remotes::install_github('pmartR/pmartR@latest_trelliscope_package')"

# Copy directories into /srv/shiny-server
WORKDIR /srv/shiny-server
COPY . .

EXPOSE 5600

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 5600, launch.browser = FALSE)"]
