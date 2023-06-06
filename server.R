# Source all reactive ui elements, observers, reactive()
# manually instantiate reactiveValues here 
server <- function(input, output, session) {
  
  # Check for minio if minio version or MAP is enabled 
  if (Minio_Test | MAP | Compose_Test) {
    
    # Load map data access
    library(mapDataAccess)
    
    # Connect to a local minio run 
    source("./MAP_Functions.R", local = TRUE)
    
    # Create a reactive values to hold MAP specific objects
    if (Minio_Test) {
      MapConnect <- reactiveValues(MapConnect = map_data_connection(config_file = "./cfg/minio_config_local.yml"), Data = NULL)
    } else if (Compose_Test) {
      MapConnect <- reactiveValues(MapConnect = map_data_connection(config_file = "./cfg/minio_config_compose.yml"), Data = NULL, Trelliscope = NULL, Job = NULL)
    } else if (MAP) {
      MapConnect <- reactiveValues(MapConnect = map_data_connection(config_file = "minio_config.yml"), Data = NULL, Trelliscope = NULL, Job = NULL)
    }
    
  } else {
    hide(id = "loading-gray-overlay")
  }
  
  # Add the redis container
  if (MAP | Redis_Test | Compose_Test) {
    
    if (Redis_Test) {
      
      redis_url <- "redis://redis1:6379/0"
      
    } else if (MAP | Compose_Test) {
      
      # Register url, this is running in another docker container alongside this one
      if(!file.exists("./cfg/redis_config.yml")){
        warning("No redis configuration found, attempting connection to default url: redis://redis1:6379")
        redis_url <- "redis://redis1:6379/0"
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
  
  # source all 'modules'
  for(folder in c('reactive_variables', 'preprocessing', 'reactive_ui',
                  'plot_functions', 'observers')){
    for(f in Sys.glob(sprintf('./%s/*.R', folder))) source(f, local=TRUE)
  }

  
}
