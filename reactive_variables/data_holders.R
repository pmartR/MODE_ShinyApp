# Much of this functionality is mirrored in the service, kept here for preview

# TODO:  Download from minio
uploaded_edata <- reactive({
  if(input$local_or_minio == "local"){
    req(input$raw_data_upload)
    edata_out = read_csv(input$raw_data_upload$datapath)
  }
  else if(input$local_or_minio == "minio")
    req(input$minio_choose_file)
    projectObject = mapDataAccess::get_data(miniocon, input$minio_choose_file)
    edata_out = projectObject$e_data
  return(edata_out)
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
