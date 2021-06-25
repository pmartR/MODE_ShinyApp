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
    # Parse the query, which should be /?edata=uuid
    query <- parseQueryString(session$clientData$url_search)
    
    # Confirm that "data" is in the query, and if so extract the project object. 
    if (length(query) != 0 && "data" %in% names(query)) {
      queryTags$query1 <- query$data
      projectObject$object1 <- get_data(miniocon, query$data) 
      shinyBS::updateCollapse(session, "trelli_collapse", close = "main_trelli_upload")
      # TODO:  Show indicator that data is already uploaded
    } 
    
  }, priority = 10)
  
}
