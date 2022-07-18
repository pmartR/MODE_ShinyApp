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

# Fourth, have an option for a docker compose local test
Compose_Test <- ifelse(Sys.getenv("COMPOSE_TEST") == "1", TRUE, FALSE)

# use this for all "None selected" options where applicable
NOSELECT_ = "__nullselect__"

# reference warning messages here to keep code clean
WARN_TEXT <- list(
  "BAD_GROUP_LENGTH" = "Specified group vector does not have the right number of elements.  Needs to have as many columns as your data minus one (the id column)",
  "SPECIFY_GROUPS" = "Either you have not correctly specified groups, or your data has not loaded properly"
)

source("UI_helper_functions.R", local=TRUE)

sym_diff <- function(a,b) setdiff(union(a,b), intersect(a,b))
