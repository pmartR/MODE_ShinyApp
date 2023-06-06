#'@details Holds the edata for which the trelliscope object will be constructed from.
#' If they came to MODE from MAP, then there should be a project object automatically loaded.
#' If there is not, then either we load from previously stored MAP project objects
#' OR we allow them to upload a file.
uploaded_data <- reactive({
  
  if (Minio_Test | MAP | Compose_Test) {
    
    if (!is.null(MapConnect$Data)) {return(MapConnect$Data)} else {return(NULL)}
    
    # Get the file's class
    if (class(file) %in% c("project edata", "midpoint pmart", "midpoint ipmart")) {
      return(file)
    } else {
      sendSweetAlert(session, "Upload file is incorrect", 
                     "MODE currently accepts edata projects, and midpoints from pmart and ipmart")
      return(NULL)
    }
    
  } else {
    
    # Require an upload of a file
    req(input$UploadConfirm)
    
    # First check that edata is included 
    if (is.null(input$EdataFile)) {
      sendModalAlert("Please upload an 'Expression Data' file")
      return(NULL)
    }
    
    # Now, double check edata 
    edata <- get_edata()
    edata_test <- is.edata(edata)
    if (edata_test != "Valid") {
      sendModalAlert(edata_test)
      return(NULL)
    }
    
    if (is.null(input$FdataFile)) {
      
      if (!is.null(input$EmetaFile) & !is.null(input$StatisticsFile)) {
        sendModalAlert("Please upload a 'Sample Information' file to use data from 'Biomolecule Information' or 'Differential Statisitics' files")
      }
      
      return(project.edata(
        projectname = "MODE_Generated",
        datatype = "Unknown",
        edata = edata,
        edata_filename = input$EdataFile$name
      ))
      
    } else {
      
      # Check fdata 
      fdata <- get_fdata()
    
      fdata_test <- is.fdata(edata, fdata)
      if (fdata_test != "Valid") {
        sendModalAlert(fdata_test)
        return(NULL)
      }
      
      # Optional check emeta 
      if (!is.null(input$EmetaFile)) {
        emeta <- get_emeta()
        emeta_test <- is.emeta(edata, emeta)
        if (emeta_test != "Valid") {
          sendModalAlert(emeta_test)
          return(NULL)
        }
      }
      
      # Optional check statistics
      if (!is.null(input$StatisticsFile)) {
        stats <- get_stats()
        stats_test <- is.statistics(edata, fdata, stats)
        if (stats_test != "Valid") {
          sendModalAlert(stats_test)
          return(NULL)
        }
      }
      
      # Return an edata file. We will generate a pmart midpoint elsewhere 
      return(project.edata(
        projectname = "MODE_Generated",
        datatype = "Unknown",
        edata = edata,
        edata_filename = input$EdataFile$name
      ))
      
    }
    
  }
  
})

# Create special readers for e_data, f_data, e_meta, and statistics when uploaded sepearately 
get_edata <- reactive({
  
  if (is.null(input$EdataFile)) {
    return(NULL)
  } else {
    return(read.csv(input$EdataFile$datapath))
  }
  
})

get_fdata <- reactive({
  
  if (is.null(input$FdataFile)) {
    return(NULL)
  } else {
    return(read.csv(input$FdataFile$datapath))
  }
  
})

get_emeta <- reactive({
  
  if (is.null(input$EmetaFile)) {
    return(NULL)
  } else {
    return(read.csv(input$EmetaFile$datapath))
  }
  
})

get_stats <- reactive({
  
  if (is.null(input$StatisticsFile)) {
    return(NULL)
  } else {
    return(read.csv(input$StatisticsFile$datapath))
  }
  
})




