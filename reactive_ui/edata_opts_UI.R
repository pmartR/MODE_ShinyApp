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
      list(actionButton("GroupAdd", "Add", icon = icon("plus")),
           actionButton("LockGroups", "Confirm", icon = icon("hand-spock"))
      ),
      uiOutput("GroupText"),
      br(), br()
    )
  }
  
})

#' @details Add group text
output$GroupText <- renderUI({
  HTML(paste("Groups:", unlist(edata_groups$Group) %>% paste0(collapse = "<p></p>")))
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
      pickerInput("NormFun", "Select Normalization Function", c(
        "Mean" = "mean", "Median" = "median", "Z-norm" = "zscore", "Median Absolute Distance" = "mad"
      ), selected = "Median"),
      uiOutput("MoreNormalizationUI")
    )
  }
})

#' @detail Expanded normalization parameters 
output$MoreNormalizationUI <- renderUI({
  
  if (edata_groups$ToNormalization) {
    
    # Make a switch function for the additional normalization parameters
    additional_parameters <- switch(input$NormSubsetFun,
      "los" = numericInput("NormalLOS", "Top L order statistics (los)", "0.05"),
      "ppp" = numericInput("NormalPPP", "Percentage Present (ppp)", "0.5"),
      "rip" = numericInput("NormalRIP", "Rank Invariant (rip)", "0.2"),
      "ppp_rip" = tagList(
        numericInput("NormalPPP", "Percentage Present (ppp)", "0.5"),
        numericInput("NormalRIP", "Rank Invariant (rip)", "0.2")
      )
    )
    
    additional_parameters

  }
})

#' @details Check normalization choices
output$CheckNormalizationUI <- renderUI({
  if (edata_groups$ToNormalization) {
    if (length(unique(edata_groups$LockedGroupOrder)) == 1) {
      tagList(
        actionButton("ConfirmNormalization", "Confirm"),
        uiOutput("NormalizationTest")
      )
    } else {
      tagList(
        list(
          actionButton("CheckNormalization", "Test Normalization"),
          actionButton("ConfirmNormalization", "Confirm")
        ),
        uiOutput("NormalizationTest")
      )
    }
  }
})
  
#' @details Display results of normalization test
output$NormalizationTest <- renderUI({
  HTML(edata_groups$NormalizationText)
})

#' @details Generate panel option variable choices
output$TrelliPanelVariableUI <- renderUI({
  req(final_data$TrelliData)
  
  div(
    id = "TrelliPanelDiv",
    pickerInput("TrelliPanelVariable", "Panel By Choice", 
                choices = attr(final_data$TrelliData, "panel_by_options"))
    )
  
})

#' @details Generate panel option variable choices
output$TrelliPlottingVariableUI <- renderUI({
  
  req(final_data$TrelliData)
  variable_choices <- summary(final_data$TrelliData)$Plot %>% 
    strsplit(" ") %>% 
    lapply(function(x) {head(x, 1)}) %>% 
    unlist() %>% 
    unique()

  div(
    id = "TrelliPlottingDiv", 
    pickerInput("TrelliPlottingVariable", "What data would you like to plot?", 
                choices = variable_choices)
  )



})

#' @details Select a panel to see
output$PlotOptionsPanelUI <- renderUI({
  req(final_data$PlotOptions)
  choices <- final_data$TrelliData$trelliData.omics[[input$TrelliPanelVariable]] %>% unique() %>% as.character()
  
  div(
    id = "TrelliPlotOptDiv",
    pickerInput("PlotOptionsPanel", "Select Panel Variable", choices = choices, selected = choices[1])
  )
 
})
  
#' @details Confirm plot selection for the trelliscope
output$PlotOptionsConfirmUI <- renderUI({
  req(final_data$PlotOptions)
  list(
    actionButton("PlotOptionsConfirm", "Confirm Selection", icon = icon("lock")),
    actionButton("PlotOptionsUnconfirm", "Make Another Selection", icon = icon("lock-open"))
  )
})
  
  
