## MODE Application: BASE IMAGE dockerfile
## Last Updated: 2021_06_09

## Use:
## export DOCKER_BUILDKIT=0
## docker build --no-cache -t code-registry.emsl.pnl.gov/multiomics-analyses/mode-app:base1.0.0 .
## docker push code-registry.emsl.pnl.gov/multiomics-analyses/mode-app:base1.0.0

# Install latest version of rocker image
FROM rocker/shiny:4.0.3

# Load general use libraries
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libssl-dev \
    libcurl4-openssl-dev \
    libcairo2-dev \
    libxt-dev \
    libssh2-1-dev \
    libhiredis-dev \
    libzmq3-dev \
    libxml2-dev \
    vim python3-venv

## restore from renv.lock ##

WORKDIR /srv/shiny-server/

# only need these three to restore
COPY renv.lock .

# pre-install renv
RUN R -e "install.packages('renv', repos = 'https://cran.rstudio.com')"

# 
RUN R -e 'renv::restore()'

## Setup Python venv ##
USER root
COPY requirements.txt .
RUN python3 -m venv /venv
RUN /venv/bin/pip install --upgrade pip
RUN /venv/bin/pip install -r requirements.txt

# Install rworker from source
RUN R -e "install.packages('devtools', repos = 'https://cran.rstudio.com')"
RUN R -e "devtools::install_github('https://github.com/lecardozo/rworker', repos = 'https://cran.rstudio.com', upgrade = 'always')"

# Install map data access package
COPY mapDataAccess /srv/shiny-server/mapDataAccess
RUN Rscript -e "devtools::install_local('/srv/shiny-server/mapDataAccess')"
