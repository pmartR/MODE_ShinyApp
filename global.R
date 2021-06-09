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
miniocon = map_data_connection(config_file='./cfg/minio_config_app.yml')
# Local: './cfg/minio_config_app.yml'
# Compose: '/srv/shiny-server/cfg/minio_config_app.yml'

# Register url, this is running in another docker container alongside this one
if(!file.exists("redis_config.yml")){
  warning("No redis configuration found, attempting connection to default url: redis://redis1:6379")
  redis_url <- "redis://redis1:6379"
} else {
  redis_cfg = yaml::read_yaml("redis_config.yml")
  redis_host = if(Sys.getenv("SHINY_LOCAL_OR_NETWORK")=="local") "0.0.0.0" else redis_cfg[['host']]
  redis_url <- sprintf('redis://%s:%s@%s:%s/%s', 
                       redis_cfg[["username"]], 
                       redis_cfg[["password"]], 
                       redis_host, 
                       redis_cfg[['port']],
                       redis_cfg[['db']])
}
message("Setting up redis connection at:  ", redis_url)

sym_diff <- function(a,b) setdiff(union(a,b), intersect(a,b))
