### MODE:  An application to visualize omics data in trelliscope.
****

Omics-data objects from the pmartR R package are ingested and sent to a separate R process to create trelliscope displays, which are then retrieved and visualized in the app.

****

#### App structure

- global.R, ui.R and server.R (Holds minimal, highest level variables and reactive/UI elements, sourcing other resources from sub-folders)
- observers
- reactive_variables
- ui_templates (mid-level UI definitions)
- reactive_ui (mostly renderUI elements referenced in ui.R and ui_templates)

#### Docker containers

There are two containers:
1. The base image that installs system libraries and R/Python dependencies. 
2. The code image, which extends the base image and includes the shiny app R code.

This makes it easier to make code changes and rebuild the container, since installing dependencies in 1. takes a long time.

**To build container 1.**  You need to put the following binaries next to the docker file.
- mapDataAccess ** (mapDataAccess_**.tar.gz, ** is the version #)

1.'s has a separate Docker and .dockerignore file (Dockerfile-base, Dockerfile-base.dockerignore).  To build the container:

docker build -f Dockerfile-base -t code-registry.emsl.pnl.gov/multiomics-analyses/mode-app:base\<version\>
docker push code-registry.emsl.pnl.gov/multiomics-analyses/mode-app:base\<version\>

To run the container, we mount two config files, one for minio and one for redis.  Examples can be found in the cfg folder of this repo.

Run command with config mounting looks like:

docker run -p 5601:5601 -v /some/path/minio_config_svc.yml:/minio_config.yml -v /some/path/redis_config.yml:/redis_config.yml

**To build container 2.**

Simply run

Docker build -t code-registry.emsl.pnl.gov/multiomics-analyses/mode-app:\<version\>

****

#### renv

Included is an renv.lock file to build the environment to run locally.  See https://rstudio.github.io/renv/articles/renv.html.