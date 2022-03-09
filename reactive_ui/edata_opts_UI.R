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

#' @details Allow user to decide whether they want groups 
output$WantGroupsUI <- renderUI({
  req(uploaded_data())
  radioGroupButtons("WantGroups", "Would you like to assign groups?", c("Yes", "No"), "No")
})

#' @details Allow users to enter group names 
output$GroupDesignationUI <- renderUI({
  
  req(uploaded_data())
  
  if (!is.null(input$WantGroups) && input$WantGroups == "Yes") {
    tagList(
      textInput("GroupName", HTML("<strong>Enter Group Name</strong>")),
      actionButton("GroupAdd", "Add", icon = icon("plus")),
      br(), br()
    )
  }
  
})

#' @details Allow users to indicate whether their data is log transformed or not
output$IsTransformedUI <- renderUI({
  req(uploaded_data())
  radioGroupButtons("IsTransformed", "Is your data log transformed?", c("Yes", "No"), "No")
})

#' @details Allow user to indicate whether their data is normalized or not
output$IsNormalizedUI <- renderUI({
  req(uploaded_data())
  radioGroupButtons("IsNormalized", "Is your data normalized?", c("Yes", "No"), "No")
})

