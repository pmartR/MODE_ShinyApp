## MODE Application: Dockerfile
## Last Updated: 2021_07_13

## Build with the "--no-cache" flag

# Install latest version of rocker image: base image for MODE
# Standalone version: code-registry.emsl.pnl.gov/multiomics-analyses/mode-app/standalone:1.1.0
FROM code-registry.emsl.pnl.gov/multiomics-analyses/mode-app/base:1.0.3

RUN Rscript -e "devtools::install_github('yang-tang/shinyjqui')"
RUN Rscript -e "devtools::install_github('hafen/trelliscopejs')"

# Install latest pmartR
RUN Rscript -e "remotes::install_github('pmartR/pmartR@develop')"

# Copy directories into /srv/shiny-server
WORKDIR /srv/shiny-server
COPY . .

EXPOSE 5600

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 5600, launch.browser = FALSE)"]
