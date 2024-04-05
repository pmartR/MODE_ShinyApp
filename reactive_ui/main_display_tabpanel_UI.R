#########################
## PREVIEW TABLES PAGE ##
#########################

# Preview the edata in the file
output$edata_preview <- DT::renderDT({
  
  if (Minio_Test | MAP | Compose_Test) {
  
    # Require uploaded data
    req(uploaded_data())
    
    # Extract edata
    if (class(uploaded_data()) == "project edata") {
      if (is.null(final_data$TrelliData)) {
        edata <- uploaded_data()$Data$e_data
      } else {
        edata <- final_data$TrelliData$omicsData$e_data
      }
    } else if (class(uploaded_data()) == "midpoint pmart") {
      edata <- uploaded_data()$`Data Objects`$OmicsData$e_data
    } else if (class(uploaded_data()) == "midpoint ipmart") {
      req(input$SelectOmics)
      edata <- uploaded_data()$`Data Objects`[[input$SelectOmics]]$`Data Objects`$OmicsData$e_data
    }
  
  } else {
    
    # Load and display CSV - files are checked when "confirm" is clicked 
    req(input$EdataFile)
    edata <- read.csv(input$EdataFile$datapath)
  
  }

  # Visualize in an interactive table
  DT::datatable(edata, selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
  options = list(pageLength = 10, scrollX = T))
  
})

# Create the input for Group Input within the fdata_preview table 
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

# Obtain the value of the Group Input within the fdata_preview table
shinyValue <- function(id, len, input) {
  unlist(lapply(seq_len(len), function(i) {
    value <- input[[paste0(id, i)]]
    if (is.null(value)) {""} else {value}
  }))
}

# Create reactive for fdata
fdata_table <- reactive({
  
  # Require uploaded data 
  req(uploaded_data())

  if (is.null(input$edata_idcname_picker)) {return(NULL)}
  
  # Get column names and groups
  Columns <- colnames(uploaded_data()$Data$e_data)
  Edata_Col <- input$edata_idcname_picker
  
  # Generate sample f data 
  edata_groups$Table <- data.frame(
    "Sample" = Columns[Columns %in% Edata_Col == FALSE],
    "Group" = shinyInput(selectizeInput, length(uploaded_data()$Data$e_data) - 1, 
                         "GroupSelector", label = NULL, choices = c("NA", unlist(edata_groups$Group))),
    check.names = FALSE
  )

})

# Preview the fdata in the file
output$fdata_preview <- DT::renderDT({
  
  if (Minio_Test | MAP | Compose_Test) {
  
  # Require uploaded data 
  req(uploaded_data())
  
  # Create an fdata file if uploaded object is project edata
  if (class(uploaded_data()) == "project edata") {
  
    if (is.null(input$FdataFile)) {
    
      session$sendCustomMessage("unbind-DT", "fdata_preview")
      
      if (edata_groups$ToNormalization == FALSE) {fdata <- edata_groups$Table} else {
        Columns <- colnames(uploaded_data()$Data$e_data)
        Edata_Col <- input$edata_idcname_picker
        fdata <- data.frame(
          "Sample" = Columns[Columns %in% Edata_Col == FALSE],
          "Group" = edata_groups$LockedGroupOrder,
          check.names = FALSE
        )
        edata_groups$fdata <- fdata
        fdata
      }
      
      # Visualize in an interactive table
      DT::datatable(fdata, escape = FALSE, selection = "single", rownames = FALSE, 
                options = list(pageLength = nrow(fdata), dom = "t", scrollX = T, ordering = FALSE,
                initComplete = JS("function(settings){",
                                  "  $('#Group').selectize()",
                                  "}"),
                preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } '))
      )
    
    } else {
      
      fdata <- read.csv(input$FdataFile$datapath)
      DT::datatable(fdata,
                    selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                    options = list(pageLength = 10, scrollX = T))
      
      
   }
  
  } else if (class(uploaded_data()) == "midpoint pmart") {
    
    # Visualize in an interactive table
    DT::datatable(uploaded_data()$`Data Objects`$OmicsData$f_data, selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                  options = list(pageLength = 10, scrollX = T))
    
  } else if (class(uploaded_data()) == "midpoint ipmart") {
    
    req(input$SelectOmics)
    DT::datatable(uploaded_data()$`Data Objects`[[input$SelectOmics]]$`Data Objects`$OmicsData$f_data,
                  selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                  options = list(pageLength = 10, scrollX = T))
    
  }
    
  } else {
    
    # Load and display CSV - files are checked when "confirm" is clicked 
    if (is.null(input$FdataFile)) {
      
      session$sendCustomMessage("unbind-DT", "fdata_preview")
      
      if (edata_groups$ToNormalization == FALSE) {fdata <- edata_groups$Table} else {
        Columns <- colnames(uploaded_data()$Data$e_data)
        Edata_Col <- input$edata_idcname_picker
        fdata <- data.frame(
          "Sample" = Columns[Columns %in% Edata_Col == FALSE],
          "Group" = edata_groups$LockedGroupOrder,
          check.names = FALSE
        )
        edata_groups$fdata <- fdata
        fdata
      }
      
      # Visualize in an interactive table
      DT::datatable(fdata, escape = FALSE, selection = "single", rownames = FALSE, 
                    options = list(pageLength = nrow(fdata), dom = "t", scrollX = T, ordering = FALSE,
                                   initComplete = JS("function(settings){",
                                                     "  $('#Group').selectize()",
                                                     "}"),
                                   preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
                                   drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')))

    } else {
      
      
      fdata <- read.csv(input$FdataFile$datapath)
      DT::datatable(fdata,
                    selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                    options = list(pageLength = 10, scrollX = T))
      
    }

    
  }
    
})

# Preview the emeta in the file
output$emeta_preview <- DT::renderDT({
  
  if (Minio_Test | MAP | Compose_Test) {
  
    req(uploaded_data())
    
    if (class(uploaded_data()) == "midpoint pmart") {
      DT::datatable(uploaded_data()$`Data Objects`$OmicsData$e_meta, selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                   options = list(pageLength = 10, scrollX = T))
    } else if (class(uploaded_data()) == "midpoint ipmart") {
      req(input$SelectOmics)
      DT::datatable(uploaded_data()$`Data Objects`[[input$SelectOmics]]$`Data Objects`$OmicsData$e_meta,
                    selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                    options = list(pageLength = 10, scrollX = T))
    }
    
  } else {
    
    # Load and display CSV - files are checked when "confirm" is clicked 
    req(input$EmetaFile)
    emeta <- read.csv(input$EmetaFile$datapath)
    
    DT::datatable(emeta,
                  selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                  options = list(pageLength = 10, scrollX = T))
    
  }
  
})

# Preview the statistics in the file
output$stat_preview <- DT::renderDT({
  
  if (Minio_Test | MAP | Compose_Test) {
  
    req(uploaded_data())
    
    if (class(uploaded_data()) == "midpoint pmart") {
      DT::datatable(uploaded_data()$`Data Objects`$OmicsStats, selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                    options = list(pageLength = 10, scrollX = T))
    } else if (class(uploaded_data()) == "midpoint ipmart") {
      req(input$SelectOmics)
      DT::datatable(uploaded_data()$`Data Objects`[[input$SelectOmics]]$`Data Objects`$OmicsStats,
                    selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                    options = list(pageLength = 10, scrollX = T))
    }
    
  } else {
    
    # Load and display CSV - files are checked when "confirm" is clicked 
    req(input$StatisticsFile)
    stats <- read.csv(input$StatisticsFile$datapath)
    
    DT::datatable(stats,
                  selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                  options = list(pageLength = 10, scrollX = T))
    
    
  }
  
})

# Extract the comparison table 
output$comparison_table <- DT::renderDT({
  
  if (!is.null(input$FoldChangeCols)) {
  
    if (input$input_datatype == "MS/NMR") {
      
      fc_cols <- input$FoldChangeCols
      pa_cols <- input$PValueACols
      pg_cols <- input$PValueGCols
      
      df <- tryCatch(data.frame(
        "Fold Change Columns" = fc_cols,
        "P Value ANOVA Columns" = pa_cols,
        "P Value G-Test Columns" = pg_cols
      ) %>%
        dplyr::mutate(Comparison = gsub("Fold_change_", "", Fold.Change.Columns)) %>%
        dplyr::relocate(Comparison), 
                     error = function(e) {return(data.frame())})
      
    } else {
      
      fc_cols <- input$FoldChangeCols
      p_cols <- input$PValueCols
      
      df <- tryCatch(data.frame(
        "Fold Change Columns" = fc_cols,
        "P Value Columns" = p_cols
      ) %>%
        dplyr::mutate(Comparison = gsub("Fold_change_", "", Fold.Change.Columns)) %>%
        dplyr::relocate(Comparison), 
      error = function(e) {return(data.frame())})
      
    }
    
    edata_groups$ComparisonTable <- df
    
    if (nrow(df) != 0) {
      DT::datatable(df,
                    selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                    options = list(pageLength = 10, scrollX = T))

    }
    
  }
  
})

######################
## SELECT PLOT PAGE ##
######################

# Render plot 
output$PlotOptionsPlot <- renderPlot({
  
  # Require the PlotOptionsTable to be rendered first
  if (is.null(final_data$PlotOptions) | is.null(input$PlotOptionsPanel)) {return(NULL)}
  
  # If no row clicked, assume it's the first
  if (is.null(input$PlotOptionsTable_row_last_clicked)) {row <- 1} else {
    row <- input$PlotOptionsTable_row_last_clicked
  }
  
  req(input$TrelliPanelVariable)
  
  # If selected row is larger than the number of entries, convert to 1
  if (row > nrow(final_data$PlotOptions)) {row <- 1}
  
  # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
  paneled <- trelli_panel_by(final_data$TrelliData, input$TrelliPanelVariable)
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  if (theFun %in% c("trelli_", "trelli_NA")) {return(NULL)}
  
  # foldchange is written without the underscore
  if (grepl("fold_change", theFun)) {
    theFun <- gsub("fold_change", "foldchange", theFun)
  }
  
  # Determine test example number
  choices <- final_data$TrelliData$trelliData[[input$TrelliPanelVariable]] %>% unique() %>% as.character()
  test_example_num <- match(input$PlotOptionsPanel, choices)
  
  # If no test_example_num, return NULL
  if (is.na(test_example_num)) {return(NULL)}
  
  if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
    
    if (is.null(input$PValueThresh)) {return(NULL)}
    
    pvaluethresh <- input$PValueThresh
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T, p_value_thresh = pvaluethresh)")))
    
  } else if (theFun == "trelli_foldchange_volcano") {
    
    if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}

    pvaluethresh <- input$PValueThresh
    comparison <- input$SelectComparison
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T, p_value_thresh = pvaluethresh, comparison = comparison)")))
    
  } else {
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=TRUE)")))
  }
  
})

# Render data table
output$PlotOptionsTable <- DT::renderDT({
  
  # Require TrelliData object 
  if (is.null(final_data$TrelliData) | 
      is.null(input$TrelliPanelVariable) | 
      is.null(input$TrelliPlottingVariable)) {return(NULL)}
  
  # Get the summary of all possible plots
  PlotOptions <- summary(final_data$TrelliData)
  
  # Save resulting table
  final_data$PlotOptions <- PlotOptions[PlotOptions$`Panel By Choice` == input$TrelliPanelVariable & 
                                        grepl(input$TrelliPlottingVariable, PlotOptions$Plot),]
  
  # Visualize in an interactive table
  DT::datatable(final_data$PlotOptions, selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                options = list(pageLength = 10, scrollX = T))
  
})

######################
## MODIFY PLOT PAGE ##
######################
  
# Make static plot - render modified plot only if final_data$Trelli_Row is not NULL (the "Confirm Selection" button has been clicked)
output$OnePlotPreview <- renderPlot({
  
  if (is.null(final_data$TrelliRow)) {return(NULL)}
  
  # Get the row 
  row <- final_data$TrelliRow
  
  # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
  paneled <- trelli_panel_by(final_data$TrelliData, input$TrelliPanelVariable)
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  if (theFun %in% c("trelli_", "trelli_NA")) {return(NULL)}
  
  # foldchange is written without the underscore
  if (grepl("fold_change", theFun)) {
    theFun <- gsub("fold_change", "foldchange", theFun)
  }
  
  # Determine test example number
  choices <- final_data$TrelliData$trelliData[[input$TrelliPanelVariable]] %>% unique() %>% as.character()
  test_example_num <- match(input$PlotOptionsPanel, choices)
  
  # Add additional values if plot inputs are not null 
  if (is.null(final_data$PlotInputs)) {
    
    # If no test_example_num, return NULL
    if (is.na(test_example_num)) {return(NULL)}
    
    if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
      
      if (is.null(input$PValueThresh)) {return(NULL)}
      
      pvaluethresh <- input$PValueThresh
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T, p_value_thresh = pvaluethresh)")))
      
    } else if (theFun == "trelli_foldchange_volcano") {
      
      if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
      
      pvaluethresh <- input$PValueThresh
      comparison <- input$SelectComparison
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T, p_value_thresh = pvaluethresh, comparison = comparison)")))
      
    } else {
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T)")))
    } 
    
  } else {

    # Add list of ggplot commands
    gg_params <- final_data$PlotInputs$Code
    
    # If no test_example_num, return NULL
    if (is.na(test_example_num)) {return(NULL)}
    
    if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
      
      if (is.null(input$PValueThresh)) {return(NULL)}
      
      pvaluethresh <- input$PValueThresh
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T, p_value_thresh = pvaluethresh, ggplot_params=gg_params)")))
      
    } else if (theFun == "trelli_foldchange_volcano") {
      
      if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
      
      pvaluethresh <- input$PValueThresh
      comparison <- input$SelectComparison
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T, p_value_thresh = pvaluethresh, comparison = comparison, ggplot_params=gg_params)")))
      
    } else {
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T, ggplot_params=gg_params)")))
    }
    
  }
  
})

# Make interactive plot - render modified plot only if final_data$Trelli_Row is not NULL (the "Confirm Selection" button has been clicked)
output$OnePlotlyPreview <- renderPlotly({
  
  # Get the row 
  row <- final_data$TrelliRow
  
  # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
  paneled <- trelli_panel_by(final_data$TrelliData, input$TrelliPanelVariable)
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  if (theFun %in% c("trelli_", "trelli_NA")) {return(NULL)}
  
  # Determine test example number
  choices <- final_data$TrelliData$trelliData[[input$TrelliPanelVariable]] %>% unique() %>% as.character()
  test_example_num <- match(input$PlotOptionsPanel, choices)
  
  # Add additional values if plot inputs are not null 
  if (is.null(final_data$PlotInputs)) {
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=TRUE, jsonp=FALSE, interactive=TRUE)"))) 
  } else {
    
    # Add list of ggplot commands
    gg_params <- final_data$PlotInputs$Code
    
    # Make updated plot with parameters 
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=TRUE, ggplot_params=gg_params, jsonp=FALSE, interactive=TRUE)"))) 
    
  }
  
})

#############################
## RENDER TRELLISCOPE PAGE ##
#############################

# Create specific refresh options for MAP version 
observeEvent(input$refresh, {
  
  if (Compose_Test | MAP) {
    
    # List files 
    IDs <- get_all_data_ids(MapConnect$MapConnect)
    
    # Subset down to only trelliscope displays
    IDs <- IDs[lapply(IDs, function(id) {get_tags(MapConnect$MapConnect, id)$ObjectType == "trelliscope-display"}) %>% unlist()]
    
    # Pull their names 
    Names <- lapply(IDs, function(id) {get_tags(MapConnect$MapConnect, id)$ProjectName}) %>% unlist()
    
    showModal(modalDialog(
      pickerInput("refreshSelect", "Select trelliscope display", Names),
      title = HTML('<p style="text-align: center;">Choose a display to load</p>'),
      footer = list(actionButton("TrelliscopeLoadAccept", "Confirm"), modalButton("Close")), size = "m", easyClose = T
    ))
    
  }
  
})

# Allow for the user to see the selected trelliscope
observeEvent(input$TrelliscopeLoadAccept, {
  MapConnect$Trelliscope <- input$refreshSelect
  removeModal()
})

output$trelliscope <- renderUI({
  
  if (Compose_Test | MAP) {

    if (!is.null(MapConnect$Trelliscope)) {
      
      # List files and names
      IDs <- get_all_data_ids(MapConnect$MapConnect)
      Names <- lapply(IDs, function(id) {get_tags(MapConnect$MapConnect, id)$ProjectName}) %>% unlist()
      
      # Get the proper ID
      theID <- IDs[Names == MapConnect$Trelliscope]
      
      withProgress(
        message = "Pulling files from Minio...", value = 1, 
        {
          Sys.sleep(0.5)
          tmp_file <- mapDataAccess::get_file(MapConnect$MapConnect, theID, filename=tempfile())
          unzip(tmp_file, exdir = "www")
          file.remove(tmp_file)
        }
      )
      
      tags$iframe(src = "")
      tags$iframe(src = paste0("trelliscope/", MapConnect$Trelliscope, "/index.html"), width = "1000px", height = "600px")
      
      # Rename the file 
      
      
      #id = gsub("[/]+$", "", theID) # remove trailing slashes to prevent shinyproxy error.
      
      # Current janky solution to prevent trelliscope from loading jquery again
      #displaycfg_path <- Sys.glob(
      #  file.path(
      #    "www", "trelliscope", "*", "appfiles", "displays", "common", "*", "displayObj.json"
      #  )
      #)[1]
      
      #shiny::validate(need(isTruthy(displaycfg_path), "Display needs to be retrieved, or was corrupted on download.  Try retrieving again."))
      #displaycfg <- jsonlite::fromJSON(displaycfg_path)
      
      # remove jquery from the config to prevent conflicts
      #idx = which(grepl("jquery", displaycfg$panelInterface$deps$assets$url))
      #if(length(idx) > 0) {
      #  displaycfg$panelInterface$deps$assets <- displaycfg$panelInterface$deps$assets[-idx,] 
      #  jsonlite::write_json(displaycfg, displaycfg_path, auto_unbox = T)
      #}
      
      # reference the json file through trelliscope-app
      #tagList(
      #  tags$div(id="trelli-display-wrapper", class="trelliscope-not-spa", style="width:800px; height:500px;"),
      #  tags$script(src="https://unpkg.com/trelliscopejs-lib/dist/trelliscope.min.js"),
      #  tags$script(sprintf("(function() {
      #  trelliscopeApp('trelli-display-wrapper',
      #    '%s/appfiles/config.json');
      #})();", id))
      #)
    
      
    } else {
      return(NULL)
    }
    
    
  } else if (!MAP) {
    if (file.exists("www/trelli/index.html") | input$refresh == 1) {
      tags$iframe(src = "")
      tags$iframe(src = "trelli/index.html", width = "1000px", height = "600px")
    }
  } else if (MAP) {
    tags$iframe(src = "trelliscope/CrazyName/index.html", width = "1000px", height = "600px")
  }
  
})
