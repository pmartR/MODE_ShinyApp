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
library(trelliscopejs)
library(colourpicker)

#  source all UI 
for (f in Sys.glob("./ui_templates/*.R")) source(f, local = TRUE)

# First, check if this is the MAP Version, or local version of the application
MAP <- ifelse(Sys.getenv("MAP_VERSION") == "1", TRUE, FALSE)

# Second, check if this is a local test version with minio
Minio_Test <- ifelse(Sys.getenv("MINIO_TEST") == "1", TRUE, FALSE)

# Third, check if this is a local test version with redis
Redis_Test <- ifelse(Sys.getenv("REDIS_TEST") == "1", TRUE, FALSE)

# Check for minio if minio version or MAP is enabled 
if (Minio_Test | MAP) {
  
  # Load map data access
  library(mapDataAccess)
  
  # Connect to a local minio run 
  source("./MAP_Functions.R", local = TRUE)
  
  # Create a reactive values to hold MAP specific objects
  if (Minio_Test) {
    MapConnect <- reactiveValues(MapConnect = map_data_connection(config_file = "./cfg/minio_config_local.yml"), Data = NULL)
  } else if (MAP) {
    MapConnect <- reactiveValues(MapConnect = map_data_connection(config_file = "minio_config.yml"), Data = NULL)
  }
    
} else {
  hide(id = "loading-gray-overlay")
}

# Add the redis container
if (MAP | Redis_Test) {
  
  if (Redis_Test) {
    redis_cfg = yaml::read_yaml("./cfg/redis_config.yml")
    redis_host = "0.0.0.0" 
    redis_url <- sprintf('redis://%s:%s/%s', 
                         redis_host, 
                         redis_cfg[['port']],
                         redis_cfg[['db']])
    
    
  } else if (MAP) {
  
    # Register url, this is running in another docker container alongside this one
    if(!file.exists("redis_config.yml")){
      warning("No redis configuration found, attempting connection to default url: redis://redis1:6379")
      redis_url <- "redis://redis1:6379"
    } else {
      redis_cfg = yaml::read_yaml("redis_config.yml")
      redis_host = redis_cfg[['host']]
      redis_url <- sprintf('redis://%s:%s/%s', 
                           redis_host, 
                           redis_cfg[['port']],
                           redis_cfg[['db']])
    }
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
