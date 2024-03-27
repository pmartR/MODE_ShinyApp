#' @details tell users what they've uploaded 
output$UploadedFileType <- renderUI({
  
  req(uploaded_data())
  
  if (class(uploaded_data()) == "project edata") {
    HTML(paste("Uploaded", uploaded_data()$Project$DataType, "expression data"))
  } else if (class(uploaded_data()) == "midpoint pmart") {
    HTML(paste("The midpoint file named <strong>", uploaded_data()$Tracking$Name, "</strong> has",
               uploaded_data()$Tracking$`Original Files`$Project$DataType, 
               "data exported from pmart", uploaded_data()$Tracking$Tab
    ))
  } else if (class(uploaded_data()) == "midpoint ipmart") {
    HTML(paste("The midpoint file named <strong>", uploaded_data()$Tracking$Name, "</strong> has",
               "multiple files uploaded from ipmart", uploaded_data()$Tracking$Tab))
  } else {
    HTML("Unknown file type")
  }

  
})

#' @details allow users to select a specific omics dataset from an ipmart midpoint
output$SelectOmicsUI <- renderUI({
  
  req(uploaded_data())
  
  if (class(uploaded_data()) == "midpoint ipmart") {
    tagList(
      hr(),
      pickerInput("SelectOmics", "Choose Omics Dataset", names(uploaded_data()$`Data Objects`),
        names(uploaded_data()$`Data Objects`)[1])
    )
  } else {
    return(NULL)
  }
  
})

#' @details picker for the column names of the uploaded data
output$choose_edata_colname <- renderUI({
  
  req(uploaded_data())
  
  if (class(uploaded_data()) == "project edata") {
    pickerInput(
      'edata_idcname_picker',
      label = "Which is the ID column?",
      choices = uploaded_data()$Data$e_data %>% colnames()
    )
  } else {
    HTML("Input data was formatted appropriately in a different application.")
  }
  

})

#' @details Allow user to decide whether they want groups 
output$WantGroupsUI <- renderUI({
  req(uploaded_data())
  if (class(uploaded_data()) == "project edata") {
    
    if (is.null(input$FdataFile)) {
      radioGroupButtons("WantGroups", "Would you like to assign groups?", c("Yes", "No"), "No")
    } else {
      list(
        pickerInput("FDataColumn", "Which column in your Sample Information indicates sample names?", colnames(get_fdata())),
        uiOutput("FDataGroupUI")
      )
    }
    
  }
})

#' @details If users upload fdata (local version), they need to select groups
output$FDataGroupUI <- renderUI({
  req(input$FDataColumn)
  pickerInput("FDataGroup", "Which column in your Sample Information indicates groups?", colnames(get_fdata())[colnames(get_fdata()) != input$FDataColumn])
})

#' @details Allow users to enter group names 
output$GroupDesignationUI <- renderUI({
  
  req(uploaded_data())
  
  if (class(uploaded_data()) == "project edata") {
  
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
  }
    
})

#' @details Add group text
output$GroupText <- renderUI({
  HTML(paste("Groups:", unlist(edata_groups$Group) %>% paste0(collapse = "<p></p>")))
})

#' @details Get NA values 
output$EnterNAValuesUI <- renderUI({
  req(uploaded_data())
  if (class(uploaded_data()) == "project edata") {
    textInput("NASymbol", "What value denotes missing data?", value = NA)
  }
})

#' @details Allow users to select a transformation
output$SelectTransformationUI <- renderUI({
  req(uploaded_data())
  if (class(uploaded_data()) == "project edata") {
    tagList(
      uiOutput("OrigDataScaleUI"),
      uiOutput("NewDataScaleUI")
    )
  }
  
})

#' @details Render original datascale dropdown if the data is not transcriptomics.
output$OrigDataScaleUI <- renderUI({
  
  if (input$input_datatype == "MS/NMR") {
    pickerInput("OrigDataScale", "On what scale is your data?",
                choices = list("Raw intensity" = "abundance", "Log base 2" = "log2", "Log base 10" = "log10", "Natural log" = "log"), 
                selected = "abundance")
  } else {
    return(NULL)
  }

  
})

#' @details Render transformed datascale dropdown if hte data is not transcriptomics.
#' Add a "No Transformation" option if anything but raw intensity is selected as the
#' original scale. 
output$NewDataScaleUI <- renderUI({
  
  # Original data scale should be rendered. It will not be rendered if the data is transcriptomics. 
  if (is.null(input$OrigDataScale)) {return(NULL)}
  
  # If data scale is not the raw intensity, allow for a no transformation option. Otherwise,
  # don't allow it. 
  if (input$input_datatype == "MS/NMR") {
    if (input$OrigDataScale == "abundance") {
      theChoices = list("Raw intensity" = "abundance", "Log base 2" = "log2", "Log base 10" = "log10", "Natural log" = "log")
      pickerInput("NewDataScale", "What scale do you want to transform to?",
                  choices = list("Raw intensity" = "abundance", "Log base 2" = "log2", "Log base 10" = "log10", "Natural log" = "log"), 
                  selected = "abundance")
    } else {return(NULL)}
  } else {
    return(NULL)
  }
  
})

#' @details Allow users to confirm selection
output$MoveToNormalizationUI <- renderUI({
  req(uploaded_data())
  if (class(uploaded_data()) == "project edata") {
    actionButton("MoveToNormalization", "Check Inputs", icon = icon("check"))
  }
})

#' @details Allow user to indicate whether their data is normalized or not
output$IsNormalizedUI <- renderUI({
  
  if (class(uploaded_data()) == "project edata") {
    if (edata_groups$ToNormalization) {
      radioGroupButtons("IsNormalized", "Is your data normalized?", c("Yes", "No"), "No")
    }
  } else {
    HTML("Input data was normalized in a different application.")
  }

})

#' @detail Select normalization parameters
output$SelectNormalizationUI <- renderUI({
  
  if (class(uploaded_data()) == "project edata") {
  
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
    
  }
  
})

#' @detail Expanded normalization parameters 
output$MoreNormalizationUI <- renderUI({
  
  if (class(uploaded_data()) == "project edata") {
  
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
  
  }
  
})

#' @details Check normalization choices
output$CheckNormalizationUI <- renderUI({
  if (edata_groups$ToNormalization) {
    if ((!is.null(input$IsNormalized) && input$IsNormalized == "Yes") | (is.null(get_fdata()) & length(unique(edata_groups$LockedGroupOrder)) == 1)) {
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
  if (edata_groups$ToNormalization) {
    HTML(edata_groups$NormalizationText)
  }
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
  if (is.null(input$TrelliPanelVariable)) {return(NULL)}
  
  # Get all plot options and create a list of variable choices 
  all_plot_opts <- summary(final_data$TrelliData)$Plot 
  variable_choices <- c()
  
  if (lapply(all_plot_opts, function(x) {grepl("abundance", x)}) %>% unlist() %>% any()) {variable_choices <- c(variable_choices, "abundance")}
  if (lapply(all_plot_opts, function(x) {grepl("missingness", x)}) %>% unlist() %>% any()) {variable_choices <- c(variable_choices, "missingness")}
  if (lapply(all_plot_opts, function(x) {grepl("fold change", x)}) %>% unlist() %>% any()) {variable_choices <- c(variable_choices, "fold change")}

  # Remove foldchange when the panel by choice is fdata_cname
  fdata_cname <- pmartR::get_fdata_cname(final_data$TrelliData$omicsData) 
  if (input$TrelliPanelVariable == fdata_cname & "fold change" %in% variable_choices) {
    variable_choices <- variable_choices[variable_choices != "fold change"]
  }
  
  div(
    id = "TrelliPlottingDiv", 
    pickerInput("TrelliPlottingVariable", "What data would you like to plot?", 
                choices = variable_choices)
  )

})

#' @details Select a panel to see
output$PlotOptionsPanelUI <- renderUI({
  
  req(final_data$PlotOptions)
  
  choices <- final_data$TrelliData$trelliData[[input$TrelliPanelVariable]] %>% unique() %>% as.character() 
  
  div(
    id = "TrelliPlotOptDiv",
    pickerInput("PlotOptionsPanel", "Select Panel Variable", choices = choices, selected = choices[1], options = list(`live-search` = TRUE))
  )
 
})

#' @detail Plot foldchange options
output$PlotFoldchangeOptsUI <- renderUI({
  
  # TrelliRow and trelli plotting variable is required
  if (is.null(input$TrelliPlottingVariable)) {
    return(NULL)
  }

  # Require TrelliPlottingVariable to be foldchange
  if (input$TrelliPlottingVariable == "fold change") {
     
    # Get selected row
    theRow <- input$PlotOptionsTable_rows_selected
    
    # Add the required additional ui if the plot if a fold change bar 
    if (grepl("fold change bar|fold change boxplot|fold change heatmap", 
              unlist(final_data$PlotOptions[theRow, "Plot"]))) {
      
      # Add the statistical test
      return(tagList(
        numericInput("PValueThresh", "Significance (P-Value) Plotting Threshold", 0.05, 0, 1, 0.001)
      ))
      
    } else if (grepl("fold change volcano", unlist(final_data$PlotOptions[theRow, "Plot"]))) {
      
      # Get comparisons
      theComparisons <- attr(final_data$TrelliData$statRes, "comparisons")
      
      # Add the statistical test
      return(tagList(
        pickerInput("SelectComparison", "Select Comparison", theComparisons, theComparisons[1]),
        numericInput("PValueThresh", "P Value Threshold", 0.05, 0, 1, 0.001)
      ))
      
    }
    
  } else {
    return(NULL)
  }
  
})
  
#' @details Confirm plot selection for the trelliscope
output$PlotOptionsConfirmUI <- renderUI({
  req(final_data$PlotOptions)
  list(
    actionButton("PlotOptionsConfirm", "Confirm Selection", icon = icon("lock")),
    actionButton("PlotOptionsUnconfirm", "Make Another Selection", icon = icon("lock-open"))
  )
})
  
#' @details Add the large number of input widgets for plot modifications
output$RenderPlotModsUI <- renderUI({
  
  req(final_data$TrelliRow)

  tagList(
    fluidRow(
      column(3, textInput("XLab", "X-axis label")),
      column(3, textInput("YLab", "Y-axis label")),
      column(3, numericInput("XAxisSize", "X-axis Font Size", 10, min = 1, max = 20, step = 1)),
      column(3, numericInput("YAxisSize", "Y-axis Font Size", 10, min = 1, max = 20, step = 1)),
      column(3, numericInput("XAxisTickAngle", "X-axis Tick Angle", 90, min = 0, max = 360, step = 1)),
      column(3, numericInput("YAxisTickAngle", "Y-axis Tick Angle", 0, min = 0, max = 360, step = 1)),
      column(3, numericInput("XAxisTickSize", "X-axis Tick Font Size", 8, min = 1, max = 20, step = 1)),
      column(3, numericInput("YAxisTickSize", "Y-axis Tick Font Size", 8, min = 1, max = 20, step = 1)),
      column(3, textInput("PlotTitle", "Title")),
      column(3, numericInput("PlotTitleSize", "Plot Title Size", 12, min = 1, max = 100, step = 1)),
      column(3, textInput("LegendTitle", "Legend Title")),
      column(3, pickerInput("SelectColor", "Select Color", choices = c("Original Colors", "YlOrRd", "YlOrBr", 
                  "YlGnBu", "YlGn", "Reds", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges", 
                  "Greys", "Greens", "GnBu", "BuPu", "BuGn", "Blues", "Set3", "Set2", "Set1", "Pastel2", 
                  "Pastel1", "Paired", "Dark2", "Accent", "Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", 
                  "PuOr", "PRGn", "PiYG", "BrBG"), selected = "Standard")),
      column(12, 
        column(3, materialSwitch("AxisFlip", HTML("<strong>Flip X and Y-Axis</strong>"), inline = TRUE, status = "primary")),
        column(3, materialSwitch("RemoveLegend", HTML("<strong>Remove Legend</strong>"), inline = TRUE, status = "primary")),
        column(6, actionButton("PlotRedraw", "Redraw Plot", icon = icon("pencil")))
      )
    )
  )
  
  
})

#' @details Add potential cognostics 
output$ChooseCognosticsUI <- renderUI({
  
  req(final_data$PlotOptions)
  
  if (nrow(final_data$PlotOptions[final_data$TrelliRow, "Plot"]) < 1) {
    return(NULL)
  }
  
  # Determine function of interest 
  theFun <- paste0("trelli_", final_data$PlotOptions[final_data$TrelliRow, "Plot"] %>% unlist() %>%
           strsplit(" ") %>% unlist() %>% paste0(collapse = "_"))
  
  # If foldchange has an underscore in it, change it
  if (grepl("fold_change", theFun)) {theFun <- gsub("fold_change", "foldchange", theFun)}
  
  # Get cognostic defaults
  allCogs <- return_cognostic(the_function = theFun, the_object = final_data$TrelliData)

  # Get cognostic options 
  pickerInput("ChooseCognostics", "Choose Cognostics", allCogs, allCogs, multiple = T)
  
})

#' @details Filter All Data By PValue, if applicable 
output$FilterByPValueUI <- renderUI({
  
  # Check that there is a final data frame 
  if (is.null(final_data$TrelliData)) {
    HTML("There are no panel filtering options for this data.")
  } else if (is.null(final_data$TrelliRow)) {
    HTML("Click 'Confirm Selection' to trigger this feature.")
  } else {
    
    # Get all plot options and create a list of variable choices 
    all_plot_opts <- summary(final_data$TrelliData)$Plot 
    
    # Add widget if filtering by p-value is possible 
    if (grepl("fold change", all_plot_opts) %>% any()) {
      
      # Get comparisons
      theComparisons <- c(attr(final_data$TrelliData$statRes, "comparisons"), "None")
      
      tagList(
        numericInput("PValueFilterPanel", "Filter data by P-value", 1, 0, 1, 0.001),
        pickerInput("PValueFilterTest", "Select test to filter by", c("ANOVA" = "anova", "G-Test" = "gtest"), selected = "ANOVA"),
        pickerInput("PValueFilterComparisons", "Select a comparison to filter by", theComparisons, selected = "None")
      )
    } else {
      return(HTML("There are no panel filtering options for this data."))
    }
  
  }
  
})

#' @details Filter PValue consequences 
output$FilterByPValueTextUI <- renderUI({
  
  # Return NULL if no PValuePanel
  if (is.null(input$PValueFilterPanel) | is.null(final_data$TrelliRow)) {return(NULL)}
  
  # Get total 
  total <- final_data$PlotOptions[final_data$TrelliRow, "Number of Plots"] %>% unlist() %>% as.numeric()
  
  # Convert comparisons appropriately
  Comparisons <- input$PValueFilterComparisons
  if (Comparisons == "None") {Comparisons <- NULL}
  
  # P Value test
  PValTest <- input$PValueFilterTest 
  if (PValTest == "none") {PValTest <- NULL}
  
  # Get filtered amount
  filtered <- trelli_pvalue_filter(
    trelliData = final_data$TrelliData, 
    p_value_test = PValTest,
    p_value_thresh = input$PValueFilterPanel, 
    comparison = Comparisons
  )
  filt_summary <- filtered %>% summary()
  nonfilt_amt <- filt_summary[filt_summary$Plot == unlist(final_data$PlotOptions[final_data$TrelliRow, "Plot"]) & 
                           filt_summary$`Panel By Choice` == unlist(final_data$PlotOptions[final_data$TrelliRow, "Panel By Choice"]), "Number of Plots"] %>%
    unlist() %>% as.numeric()
  
  # Return text
  HTML(paste0("<p># Plots Pre-Filter: ", total, "</p>",
              "<p># Plots Post-Filter: ", nonfilt_amt, "</p>",
              "<p>Percentage of Plots Retained: ", round(nonfilt_amt/total * 100), "%</p>"))
  
})

#' @details Download the trelliscope display
output$download <- downloadHandler(
  filename = function() {
    if (MAP | Compose_Test) {
      paste0(MapConnect$Trelliscope, ".zip")
    } else {
      paste0(.scrub_clean(input$trelliscope_name), ".zip")
    }
  },
  content = function(file) {
    if (MAP | Compose_Test) {
      message(paste0("./www/trelliscope/", MapConnect$Trelliscope, "/"))
      zip(file, paste0("./www/trelliscope/", MapConnect$Trelliscope, "/"))
    } else {
      zip(file, "./www/trelli")
    }
  }
)

#' @details If the app is MODE classic, render the UI to select fold change columns
output$Select_FC_UI <- renderUI({
  
  if (!is.null(get_stats())) {
    
    theselected <- colnames(get_stats()) 
    
    # Try to identify fold change columns
    if (any(grepl("Fold_change", colnames(get_stats())))) {
      theselected <- colnames(get_stats())[grepl("Fold_change", colnames(get_stats()))]
    } 
    
    pickerInput(inputId = "Select_FC", label = "Select Fold Change Columns", 
                choices = colnames(get_stats()), selected = theselected, multiple = T)
    
  }
  
})
  
