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
        
        if (!is.null(input$NewDataScale) && input$NewDataScale != "abundance") {
          
          # Separate out the id column 
          theID <- edata %>% select_at(input$edata_idcname_picker)
          theNames <- colnames(edata)[colnames(edata) %in% input$edata_idcname_picker == FALSE]
          theRest <- edata %>% select_at(theNames)
          
          if (input$NewDataScale == "log2") {theRest <- log2(theRest)}
          if (input$NewDataScale == "log10") {theRest <- log10(theRest)}
          if (input$NewDataScale == "log") {theRest <- log(theRest)}
          
          edata <- cbind(theID, theRest)
          
        }
        
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
    
    # Log transform if necessary 
    if (!is.null(input$NewDataScale)) {
      
      if (input$NewDataScale != "abundance") {
        
        # Separate out the id column 
        theID <- edata %>% select_at(input$edata_idcname_picker)
        theRest <- edata[,colnames(edata) %in% input$edata_idcname_picker == FALSE]
        
        if (input$NewDataScale == "log2") {theRest <- log2(theRest)}
        if (input$NewDataScale == "log10") {theRest <- log10(theRest)}
        if (input$NewDataScale == "log") {theRest <- log(theRest)}
        
        edata <- cbind(theID, theRest)
        
      }
      
    }
    
    # Normalize if necessary
    if (!is.null(final_data$TrelliData)) {
      edata <- final_data$TrelliData$omicsData$e_data
    }
    
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
  
  # Get groups
  theGroups <- input$GroupName %>% strsplit(",") %>% unlist()
  
  # Get target length
  targetLength <- Columns[Columns %in% Edata_Col == FALSE] %>% length()
  
  # Send a warning
  if (length(theGroups) != targetLength) {
    sendSweetAlert(session, 
                   "Group length warning", 
                   paste0("Expected ", targetLength, " values and received ", length(theGroups),
                         ". If the number is less than expected, the remaining samples will be labeled NA.", 
                         " If the number is more than expected, extra values will be tossed"), "warning"
                   )
  }
  
  
  # Fill blanks if necessary  
  if (length(theGroups) != targetLength) {
    if (length(theGroups) > targetLength) {theGroups <- theGroups[1:targetLength]}
    if (length(theGroups) < targetLength) {theGroups <- c(theGroups, rep(NA, targetLength - length(theGroups)))}
  }

  

  
  # Generate sample f data 
  edata_groups$Table <- data.frame(
    "Sample" = Columns[Columns %in% Edata_Col == FALSE],
    "Condition" = theGroups,
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
      
      if (is.null(get_fdata())) {return(NULL)} else {
        DT::datatable(get_fdata(),
                      selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                      options = list(pageLength = 10, scrollX = T))
      }

    
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
      
      if (is.null(get_fdata())) {return(NULL)} else {
        DT::datatable(get_fdata(),
                      selection = list(mode = 'single', selected = 1), rownames = F, filter = 'top', 
                      options = list(pageLength = 10, scrollX = T))
      }

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

# Render data table
output$PlotOptionsTable <- DT::renderDT({
  
  # Require TrelliData object 
  if (is.null(final_data$TrelliData) | 
      is.null(input$TrelliPanelVariable) | 
      is.null(input$TrelliPlottingVariable)) {return(NULL)}
  
  # Get the summary of all possible plots
  PlotOptions <- summary(final_data$TrelliData)
  
  # Save resulting table
  subPlotTable <- PlotOptions[grepl(input$TrelliPanelVariable, PlotOptions$`Panel By Choice`) & 
                                grepl(input$TrelliPlottingVariable, PlotOptions$Plot),]
  
  # If there's a comma in the Number of Plots, fix it 
  if (grepl(", ", subPlotTable$`Number of Plots`[1], fixed = T)) {
    pos <- which(unlist(strsplit(unlist(subPlotTable$`Panel By Choice`[1]), ", ")) == input$TrelliPanelVariable)
    subPlotTable$`Panel By Choice` <- input$TrelliPanelVariable
    subPlotTable$`Number of Plots` <- strsplit(subPlotTable$`Number of Plots`[1], ", ") %>% unlist() %>% .[pos]
  }
  
  # If data type is rna-seq, make sure to remove the non-zero option when not appropriate
  if (get_data_type() == "RNA-Seq") {
    if (!is.null(input$TrelliPlottingVariable)) {
      if (input$TrelliPlottingVariable == "rnaseq") {
        subPlotTable <- subPlotTable %>% filter(Plot != "rnaseq nonzero bar")
      } else if (input$TrelliPlottingVariable == "nonzero") {
        subPlotTable <- subPlotTable %>% filter(Plot == "rnaseq nonzero bar")
      }
    }
  }
  
  final_data$PlotOptions <- subPlotTable
  
  # Visualize in an interactive table
  return(NULL)
  
})

# Render plot 
output$PlotOptionsPlot <- renderPlot({
  
  # Require the PlotOptionsTable to be rendered first
  if (is.null(final_data$PlotOptions) | is.null(input$PlotOptionsPanel)) {return(NULL)}
  
  # If no row clicked, assume it's the first
  if (is.null(input$ChoosePlotType)) {row <- 1} else {
    row <- match(input$ChoosePlotType, final_data$PlotOptions$Plot)
  }
  
  req(input$TrelliPanelVariable)
  
  # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
  paneled <- trelli_panel_by(final_data$TrelliData, input$TrelliPanelVariable)
  
  # If no selected plot, return NULL
  if (is.null(input$PlotOptionsPanel)) {return(NULL)}
  
  # Filter down to just the selected plot
  paneled$trelliData <- paneled$trelliData[paneled$trelliData[[input$TrelliPanelVariable]] == input$PlotOptionsPanel,]
  
  # Attempt the trelliscope build function
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  if (theFun %in% c("trelli_", "trelli_NA")) {return(NULL)}
  
  # foldchange is written without the underscore
  if (grepl("fold_change", theFun)) {
    theFun <- gsub("fold_change", "foldchange", theFun)
  }
  
  if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
    
    if (is.null(input$PValueThresh)) {return(NULL)}
    
    pvaluethresh <- input$PValueThresh
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=T, p_value_thresh = pvaluethresh)")))
    
  } else if (theFun == "trelli_foldchange_volcano") {
    
    if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}

    pvaluethresh <- input$PValueThresh
    comparison <- input$SelectComparison
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=T, p_value_thresh = pvaluethresh, comparison = comparison)")))
    
  } else {
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=TRUE)")))
  }
  
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
  
  # If no selected plot, return NULL
  if (is.null(input$PlotOptionsPanel)) {return(NULL)}
  
  # Filter down to just the selected plot
  paneled$trelliData <- paneled$trelliData[paneled$trelliData[[input$TrelliPanelVariable]] == input$PlotOptionsPanel,]
  
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  if (theFun %in% c("trelli_", "trelli_NA")) {return(NULL)}
  
  # foldchange is written without the underscore
  if (grepl("fold_change", theFun)) {
    theFun <- gsub("fold_change", "foldchange", theFun)
  }
  
  # Add additional values if plot inputs are not null 
  if (is.null(final_data$PlotInputs)) {
    
    if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
      
      if (is.null(input$PValueThresh)) {return(NULL)}
      
      pvaluethresh <- input$PValueThresh
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=T, p_value_thresh = pvaluethresh)")))
      
    } else if (theFun == "trelli_foldchange_volcano") {
      
      if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
      
      pvaluethresh <- input$PValueThresh
      comparison <- input$SelectComparison
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=T, p_value_thresh = pvaluethresh, comparison = comparison)")))
      
    } else {
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=T)")))
    } 
    
  } else {

    # Add list of ggplot commands
    gg_params <- final_data$PlotInputs$Code
    
    if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
      
      if (is.null(input$PValueThresh)) {return(NULL)}
      
      pvaluethresh <- input$PValueThresh
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=T, p_value_thresh = pvaluethresh, ggplot_params=gg_params)")))
      
    } else if (theFun == "trelli_foldchange_volcano") {
      
      if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
      
      pvaluethresh <- input$PValueThresh
      comparison <- input$SelectComparison
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=T, p_value_thresh = pvaluethresh, comparison = comparison, ggplot_params=gg_params)")))
      
    } else {
      eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=T, ggplot_params=gg_params)")))
    }
    
  }
  
})

# Make interactive plot - render modified plot only if final_data$Trelli_Row is not NULL (the "Confirm Selection" button has been clicked)
output$OnePlotlyPreview <- renderPlotly({
  
  # Get the row 
  row <- final_data$TrelliRow
  
  # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
  paneled <- trelli_panel_by(final_data$TrelliData, input$TrelliPanelVariable)
  
  # If no selected plot, return NULL
  if (is.null(input$PlotOptionsPanel)) {return(NULL)}
  
  # Filter down to just the selected plot
  paneled$trelliData <- paneled$trelliData[paneled$trelliData[[input$TrelliPanelVariable]] == input$PlotOptionsPanel,]
  
  
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  if (theFun %in% c("trelli_", "trelli_NA")) {return(NULL)}
  
  # Add additional values if plot inputs are not null 
  if (is.null(final_data$PlotInputs)) {
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=TRUE, jsonp=FALSE, interactive=TRUE)"))) 
  } else {
    
    # Add list of ggplot commands
    gg_params <- final_data$PlotInputs$Code
    
    # Make updated plot with parameters 
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=1, single_plot=TRUE, ggplot_params=gg_params, jsonp=FALSE, interactive=TRUE)"))) 
    
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
    IDs <- IDs[IDs != "Jobs"]
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

# Build out trelliscope load accept 
observeEvent(input$TrelliscopeLoadAccept, {
  MapConnect$Trelliscope <- input$refreshSelect
  removeModal()
})

output$trelliscope <- renderUI({
  
  if (Compose_Test | MAP) {

    if (!is.null(MapConnect$Trelliscope)) {
      
      # List files and names
      IDs <- get_all_data_ids(MapConnect$MapConnect)
      IDs <- IDs[IDs != "Jobs"]
      IDs <- IDs[lapply(IDs, function(id) {get_tags(MapConnect$MapConnect, id)$ObjectType == "trelliscope-display"}) %>% unlist()]
      Names <- lapply(IDs, function(id) {get_tags(MapConnect$MapConnect, id)$ProjectName}) %>% unlist()
      
      # Get the proper ID
      theID <- IDs[Names == MapConnect$Trelliscope]
      
      # If people name the trelliscope the same thing, that's on them
      if (length(theID) > 1) {theID <- theID[1]}
      
      # Remove www/www if it already exists
      if (dir.exists("./www/www")) {
        unlink("./www/www", recursive = TRUE, force = TRUE)
      }
       
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
      tags$iframe(src = paste0("www/trelli_json/index.html"), width = "1000px", height = "600px")
      
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
    if (file.exists("./www/trelli_json/index.html") | input$refresh == 1) {
      tags$iframe(src = "")
      tags$iframe(src = "./trelli_json/index.html", width = "1000px", height = "600px")
    }
  } else if (MAP) {
    tags$iframe(src = "trelliscope/CrazyName/index.html", width = "1000px", height = "600px")
  }
  
})
