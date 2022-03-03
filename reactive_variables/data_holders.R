#'@details Holds the edata for which the trelliscope object will be constructed from.
#' If they came to MODE from MAP, then there should be a project object automatically loaded.
#' If there is not, then either we load from previously stored MAP project objects
#' OR we allow them to upload a file.
uploaded_data <- reactive({
  
  # Require an upload of a file
  req(input$UploadFile)
  
  # Read RDS file 
  # TODO: Add all the checks and different formats of data, etc 
  edata <- readRDS(input$UploadFile$datapath)
  
  return(edata)
  
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
