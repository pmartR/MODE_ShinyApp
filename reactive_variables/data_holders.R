#'@details Holds the edata for which the trelliscope object will be constructed from.
#' If they came to MODE from MAP, then there should be a project object automatically loaded.
#' If there is not, then either we load from previously stored MAP project objects
#' OR we allow them to upload a file.
uploaded_edata <- reactive({
  if (is.null(projectObject$object1) == FALSE) {
    return(projectObject$object1$Data$e_data)
  } else {
    if(input$local_or_minio == "local") {
      req(input$raw_data_upload)
      read_csv(input$raw_data_upload$datapath)
    } else if(input$local_or_minio == "minio") {
      req(input$minio_choose_file != NOSELECT_)
      projectObject$object1 <-  mapDataAccess::get_data(
        miniocon, 
        input$minio_choose_file
      )
      projectObject$object1$Data$e_data
    }
  }
})

# turn uploaded e_data into dataframe suitable for trelliscope
nested_edata <- reactive({
  req(uploaded_edata(), input$edata_idcname_picker)
  
  out_df = edata_to_plot_df(
    uploaded_edata(),
    panel_column = input$edata_idcname_picker,
    names_to = "Sample",
    values_to = "Value"
  )
  
  return(out_df)
})
