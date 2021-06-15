## MODE Application: Dockerfile
## Last Updated: 2021_06_09

## Build with the "--no-cache" flag

# Install latest version of rocker image: base image for MODE
FROM code-registry.emsl.pnl.gov/multiomics-analyses/mode-app:base1.0.0

# Add jsonp to the mimetype list
RUN sed -i 's@\(application/javascript.*\)@\1 jsonp@' /etc/mime.types

# Copy directories into /srv/shiny-server
COPY . .

EXPOSE 5600

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 5600, launch.browser = FALSE)"]
