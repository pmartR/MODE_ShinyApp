## MODE Application: Dockerfile
## Last Updated: 2021_07_13

## Build with the "--no-cache" flag

# Install latest version of rocker image: base image for MODE
FROM code-registry.emsl.pnl.gov/multiomics-analyses/mode-app/base:1.0.3

# Copy directories into /srv/shiny-server
COPY . .

EXPOSE 5600

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 5600, launch.browser = FALSE)"]
