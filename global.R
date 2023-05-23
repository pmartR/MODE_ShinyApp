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
library(processx)
library(markdown)
library(devtools)

# Load bioconductor separately
library(limma)

options(shiny.maxRequestSize = 10 * 1024^2) 

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

# Define the filtering function for trelliData here: 
trelli_pvalue_filter <- function(trelliData, 
                                 p_value_test = "anova", 
                                 p_value_thresh = 0.05, 
                                 comparison = NULL) {
  
  if (any(class(trelliData) %in% c("trelliData")) == FALSE) {
    stop("trelliData must be of the class trelliData.")
  }
  if (is.null(trelliData$statRes)) {
    stop("trelliData must contain a statRes object.")
  }
  if (p_value_test %in% c("anova", "gtest", "combined") == FALSE) {
    stop("p_value_test must be anova, gtest, or combined.")
  }
  if (!is.numeric(p_value_thresh)) {
    stop("p_value_threshold must be a number.")
  }
  p_value_thresh <- abs(p_value_thresh)
  if (!is.null(comparison)) {
    if (!is.character(comparison) | length(comparison) > 
        1) {
      stop("comparison must be a string of length 1.")
    }
    Comparisons <- attr(trelliData$statRes, "comparisons")
    if (comparison %in% Comparisons == FALSE) {
      stop(paste0(comparison, "is not an acceptable comparison"))
    }
  }
  biomolecule <- attr(trelliData$statRes, "cnames")$edata_cname
  if (!is.null(comparison)) {
    trelliData$trelliData.stat <- trelliData$trelliData.stat[trelliData$trelliData.stat$Comparison == 
                                                               comparison, ]
  }
  if (p_value_test == "anova") {
    trelliData$trelliData.stat <- trelliData$trelliData.stat[trelliData$trelliData.stat$p_value_anova <= 
                                                               p_value_thresh, ]
  }
  else if (p_value_test == "gtest") {
    trelliData$trelliData.stat <- trelliData$trelliData.stat[trelliData$trelliData.stat$p_value_gtest <= 
                                                               p_value_thresh, ]
  }
  if (!is.null(trelliData$trelliData.omics)) {
    biomolecule_subset <- trelliData$trelliData.stat[, biomolecule] %>% 
      unlist()
    trelliData$trelliData.omics <- trelliData$trelliData.omics[trelliData$trelliData.omics[[biomolecule]] %in% 
                                                                 biomolecule_subset, ]
  }
  return(trelliData)
  
}
