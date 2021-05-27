## MODE 

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

# load package to be installed from source
COPY local_packages ./local_packages

# set path to local packages for renv
ENV RENV_PATHS_LOCAL=/srv/shiny-server/local_packages

# 
RUN R -e 'renv::restore()'

# install rworker from source
RUN R -e "options(repos='https://cran.rstudio.com');renv::install('./local_packages/rworker_0.1.3.tar.gz', repos=NULL, type='source')"

## Setup Python venv ##
USER root
COPY modeapp_requirements.txt .
RUN python3 -m venv /venv
RUN /venv/bin/pip install --upgrade pip
RUN /venv/bin/pip install -r modeapp_requirements.txt

COPY . .

EXPOSE 5600

# Launch App
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 5600, launch.browser = FALSE)"]
