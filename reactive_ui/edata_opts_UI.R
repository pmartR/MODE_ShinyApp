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

#' @details Get NA values 
output$EnterNAValuesUI <- renderUI({
  req(uploaded_data())
  textInput("NASymbol", "What value denotes missing data?", value = NA)
})

#' @details Allow users to select a transformation
output$SelectTransformationUI <- renderUI({
  req(uploaded_data())
  tagList(
    pickerInput("OrigDataScale", "On what scale is your data?",
                choices = list("Raw intensity" = "abundance", "Log base 2" = "log2", "Log base 10" = "log10", "Natural log" = "log"), 
                selected = "abundance"),
    pickerInput("NewDataScale", "What scale do you want to transform to?",
                choices = list("Raw intensity" = "abundance", "Log base 2" = "log2", "Log base 10" = "log10", "Natural log" = "log"), 
                selected = "abundance")
  )
})

#' @details Allow users to confirm selection
output$MoveToNormalizationUI <- renderUI({
  req(uploaded_data())
  actionButton("MoveToNormalization", "Check Inputs", icon = icon("check"))
})

#' @details Allow user to indicate whether their data is normalized or not
output$IsNormalizedUI <- renderUI({
  if (edata_groups$ToNormalization) {
    radioGroupButtons("IsNormalized", "Is your data normalized?", c("Yes", "No"), "No")
  }
})

#' @detail Select normalization parameters
output$SelectNormalizationUI <- renderUI({
  if (edata_groups$ToNormalization & !is.null(input$IsNormalized) && input$IsNormalized == "No") {
    tagList(
      pickerInput("NormSubsetFun", "Select Subset Function", c(
        "Everything" = "all", "Top L order statistics (los)" = "los", "Percentage present (ppp)" = "ppp",
        "Complete" = "complete", "Rank invariant (rip)" = "rip", "Percentage present and rank invariant (ppp+rip)" = "ppp_rip"
      ), selected = "Everything"),
      pickerInput("NormFun", "Select Noramlization Function", c(
        "Mean" = "mean", "Median" = "median", "Z-norm" = "zscore", "Median Absolute Distance" = "mad"
      ), selected = "Median"),
      uiOutput("MoreNormalizationUI")
    )
  }
})

#' @detail Expanded normalization parameters 
output$MoreNormalizationUI <- renderUI({
  if (edata_groups$ToNormalization) {
    browser()
  }
})
  
  
  
  
