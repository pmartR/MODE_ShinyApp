## Functionality that was added for MAP
## Last Updated: June 28th, 2022

list(
  
  observeEvent(input$`__startup__`, {
    
    # Parse the query string at the url header
    query <- parseQueryString(session$clientData$url_search)
    
    # Set a conditional test. We only care if the "data" parameter exists. 
    cond <- length(query) != 0 && "data" %in% names(query) 
    
    # Continue if condition is met 
    if (cond) {
      
      # Get the data that was uploaded, and determine whether it is a project object,
      # or a midpoint object.
      pullData <- get_data(MapConnect$MapConnect, query$data)
      
      # If the class is a project object 
      if (class(pullData) == "project edata") {
        
        Project <- pullData
        
        # Create a loading screen
        html(
          "loading-gray-overlay", 
          paste("<div class='fadein-out busy relative-centered', style='font-size:xx-large'>", "Loading expression data of the", 
                Project$Project$DataType, "type...</div>")
        )
        
        # Add data to MapConnect
        MapConnect$Data <- Project
        
        
      } else if (class(pullData) %in% c("midpoint pmart", "midpoint ipmart")) {
        
        # If the object isn't a project, then it's a midpoint
        MidPointFile <- pullData
        
        # Create a loading screen
        html(
          "loading-gray-overlay", 
          paste("<div class='fadein-out busy relative-centered', style='font-size:xx-large'>", "Loading midpoint", 
                MidPointFile$Tracking$`Original Files`$Project$DataType, "data from the", 
                MidPointFile$Tracking$Tab %>% gsub(pattern = "_", replacement = " "), "of the",
                class(pullData) %>% strsplit(" ") %>% unlist() %>% tail(1), "application...</div>")
        )
        
        # Add data to MapConnect
        MapConnect$Data <- MidPointFile
        
      }
      
    }
    
    # Exit loading screen
    on.exit({
      Sys.sleep(2)
      hide("loading-gray-overlay")
    })
    
  }, priority = -10, ignoreNULL = FALSE, once = TRUE)
  
  
)