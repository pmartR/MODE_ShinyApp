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
output$GroupDesignationUI <- renderUI({
  
  req(uploaded_data())
  
  if (input$WantGroups == "Yes") {
    tagList(
      textInput("GroupName", HTML("<strong>Enter Group Name</strong>")),
      actionButton("GroupAdd", "Add", icon = icon("plus")),
      br(), br()
    )
  }
  
})

