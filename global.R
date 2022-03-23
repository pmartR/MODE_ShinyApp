library(jsonlite)
library(plotly)
library(purrr)
library(readr)
library(reticulate)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(stringr)
library(shinyjqui)
library(pmartR)
library(DT)

#  source all UI 
for (f in Sys.glob("./ui_templates/*.R")) source(f, local = TRUE)

# First, check if this is the MAP Version, or local version of the application
MAP <- ifelse(Sys.getenv("MAP_VERSION") == "1", TRUE, FALSE)

if (MAP) {
  
  # Create a minio connection
  miniocon = map_data_connection(config_file='minio_config.yml')
  
  # Register url, this is running in another docker container alongside this one
  if(!file.exists("redis_config.yml")){
    warning("No redis configuration found, attempting connection to default url: redis://redis1:6379")
    redis_url <- "redis://redis1:6379"
  } else {
    redis_cfg = yaml::read_yaml("redis_config.yml")
    redis_host = if(Sys.getenv("SHINY_LOCAL_OR_NETWORK")=="local") "0.0.0.0" else redis_cfg[['host']]
    redis_url <- sprintf('redis://%s:%s/%s', 
                         redis_host, 
                         redis_cfg[['port']],
                         redis_cfg[['db']])
  }
  
  message("Setting up redis connection at:  ", redis_url)
  
  # Import celery package from the virtual environment
  clry <- reticulate::import('celery')
  celery_app = clry$Celery('app', broker=redis_url, backend=redis_url)
  
}

# use this for all "None selected" options where applicable
NOSELECT_ = "__nullselect__"

# reference warning messages here to keep code clean
WARN_TEXT <- list(
  "BAD_GROUP_LENGTH" = "Specified group vector does not have the right number of elements.  Needs to have as many columns as your data minus one (the id column)",
  "SPECIFY_GROUPS" = "Either you have not correctly specified groups, or your data has not loaded properly"
)

source("UI_helper_functions.R", local=TRUE)

sym_diff <- function(a,b) setdiff(union(a,b), intersect(a,b))
