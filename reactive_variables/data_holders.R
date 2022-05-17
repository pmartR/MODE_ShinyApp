#'@details Holds the edata for which the trelliscope object will be constructed from.
#' If they came to MODE from MAP, then there should be a project object automatically loaded.
#' If there is not, then either we load from previously stored MAP project objects
#' OR we allow them to upload a file.
uploaded_data <- reactive({
  
  # Require an upload of a file
  req(input$UploadFile)
  
  # Check that the file is an RDS object
  tryCatch(
    file <- readRDS(input$UploadFile$datapath),
    error = function(e) {
      sendSweetAlert(session, "Upload file is incorrect", 
                     "MODE currently accepts RDS files from MAP")
      return(NULL)
    }
  )
  
  # Get the file's class
  if (class(file) %in% c("project edata", "midpoint pmart", "midpoint ipmart")) {
    return(file)
  } else {
    sendSweetAlert(session, "Upload file is incorrect", 
                   "MODE currently accepts edata projects, and midpoints from pmart and ipmart")
    return(NULL)
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




