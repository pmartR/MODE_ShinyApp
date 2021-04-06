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
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    for(key in names(query)){
      if(key == "from_map"){
        file_tags[[key]] <- query[[key]]
      }
      else {
        file_tags$file_dictionary[[key]] <- query[[key]] 
      }
      message(sprintf("INFO: stored parameter %s: %s", key, query[[key]]))
    }
  }, priority = 10)
}