# Much of this functionality is mirrored in the service, kept here for preview

# TODO:  Download from minio
uploaded_edata <- reactive({
  if (is.null(projectObject$object1) == FALSE) {
    return(projectObject$object1$Data$e_data)
  } else {return(NULL)}
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
