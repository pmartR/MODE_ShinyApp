library(plotly)
library(readr)
library(reticulate)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(stringr)
library(trelliscopejs)
library(mapDataAccess)

#  source all UI 
for (f in Sys.glob("./ui_templates/*.R")) source(f, local = TRUE)

# 
miniocon = map_data_connection(config_file='minio_config.yml')

url_prefix = if(Sys.getenv("SHINY_LOCAL_OR_NETWORK")=="local") "0.0.0.0" else "redis1"
redis_url <- sprintf('redis://%s:6379', url_prefix)

sym_diff <- function(a,b) setdiff(union(a,b), intersect(a,b))