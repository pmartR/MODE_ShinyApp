#' @details tell users what they've uploaded 
output$UploadedFileType <- renderUI({
  
  req(uploaded_data())
  
  # TODO: Get class information 
  HTML(paste("Uploaded", uploaded_data()$Project$DataType, "expression data"))
  
})

#' @details picker for the column names of the uploaded data
output$choose_edata_colname <- renderUI({
  
  req(uploaded_data())
  
  pickerInput(
    'edata_idcname_picker',
    label = "Which is the ID column?",
    choices = uploaded_data()$Data$e_data %>% colnames()
  )
})

#' @details Allow users to generate groups 

