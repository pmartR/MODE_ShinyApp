# Source all reactive ui elements, observers, reactive()
# manually instantiate reactiveValues here 
server <- function(input, output, session) {
  # source all 'modules'
  for(folder in c('reactive_variables', 'preprocessing', 'reactive_ui',
                  'plot_functions', 'observers')){
    for(f in Sys.glob(sprintf('./%s/*.R', folder))) source(f, local=TRUE)
  }
  display_objects <- reactiveValues(saved_displays=list())
  file_tags <- reactiveValues(file_dictionary=list(), from_map=F, local_files=list())
  queryTags <- reactiveValues(query1 = NULL)
  projectObject <- reactiveValues(object1 = NULL)
  
  # If they came here from MAP, then we can load an object directly from the UUID provided in the URL.
  observe({
    # Parse the query, which should be /?data=minio_object_name
    query <- parseQueryString(session$clientData$url_search)
    cond = length(query) != 0 && "data" %in% names(query) & isTRUE(input$minio_choose_file == NOSELECT_)
    
    # Confirm that "data" is in the query, and if so extract the project object. 
    if (cond) {
      queryTags$query1 <- query$data
      projectObject$object1 <- get_data(miniocon, query$data) 
      shinyBS::updateCollapse(session, "trelli_collapse", close = "main_trelli_upload")
    } else {
      projectObject$object1 <- NULL
    }
    
    shinyjs::toggle(
      id = "main_trelli_upload_from_map_indicator", 
      condition = cond
    )

  }, priority = 10)
  
  # Run the minio test version
  if (Minio_Test | MAP) {
    
    # Load map data access
    library(mapDataAccess)
    
    # Connect to a local minio run 
    source("./MAP_Functions.R", local = TRUE)
    
    # Create a reactive values to hold MAP specific objects
    MapConnect <- reactiveValues(MapConnect = map_data_connection(config_file = "./cfg/minio_config_local.yml"), 
                                 Data = NULL)
    
  } else {
    hide(id = "loading-gray-overlay")
  }
  
}
