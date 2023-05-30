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
    edata <- read.csv(input$EdataFile$datapath)
    edata_test <- is.edata(edata)
    if (edata_test != "Valid") {
      sendModalAlert(edata_test)
      return(NULL)
    }
    
    if (is.null(input$FdataFile)) {
      
      if (!is.null(input$EmetaFile) & !is.null(input$StatisticsFile)) {
        sendModalAlert("Please upload a 'Sample Information' file to use data from 'Biomolecule Information' or 'Differential Statisitics' files")
      }
      
      browser()
      
      return(project.edata(
        projectname = "MODE_Generated",
        datatype = "Unknown",
        edata = edata,
        
      ))
      
    } else {
      
      browser()
      
    }
    
    
  }
  
})

# turn uploaded e_data into dataframe suitable for trelliscope
nested_edata <- reactive({
  
  req(uploaded_data(), input$edata_idcname_picker)
  
  out_df = edata_to_plot_df(
    uploaded_data()$Data$e_data,
    panel_column = input$edata_idcname_picker,
    names_to = "Sample",
    values_to = "Value"
  )
  
  return(out_df)
  
})




