#########################
## PREVIEW TABLES PAGE ##
#########################

# Preview the edata in the file
output$edata_preview <- DT::renderDT({
  
  # Require uploaded data
  req(uploaded_data())
  
  # Extract edata
  if (is.null(final_data$TrelliData)) {
    edata <- uploaded_data()$Data$e_data
  } else {
    edata <- final_data$TrelliData$omicsData$e_data
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
  
  # Require uploaded data 
  req(uploaded_data())
  
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

})

# Preview the emeta in the file
output$emeta_preview <- DT::renderDT({
  req(uploaded_data())
  return(NULL)
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
  
  # Determine test example number
  choices <- final_data$TrelliData$trelliData.omics[[input$TrelliPanelVariable]] %>% unique() %>% as.character()
  test_example_num <- match(input$PlotOptionsPanel, choices)
  eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T)")))
  
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

# Render modified plot only if final_data$Trelli_Row is not NULL (the "Confirm Selection" button has been clicked)
output$OnePlotPreview <- renderPlot({
  
  req(final_data$TrelliRow)
  
  # Get the row 
  row <- final_data$TrelliRow
  
  # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
  paneled <- trelli_panel_by(final_data$TrelliData, input$TrelliPanelVariable)
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  # Determine test example number
  choices <- final_data$TrelliData$trelliData.omics[[input$TrelliPanelVariable]] %>% unique() %>% as.character()
  test_example_num <- match(input$PlotOptionsPanel, choices)
  
  # Add additional values if plot inputs are not null 
  if (is.null(final_data$PlotInputs)) {
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T, jsonp=FALSE)"))) 
  } else {

    # Add list of ggplot commands
    gg_params <- final_data$PlotInputs$Code
    
    # Make updated plot with parameters 
    eval(parse(text = paste0(theFun, "(trelliData=paneled, test_example=test_example_num, single_plot=T, ggplot_params=gg_params, jsonp=FALSE)"))) 
    
  }
  
})

#############################
## RENDER TRELLISCOPE PAGE ##
#############################

output$trelliscope <- renderUI({
  
  if (final_data$MakeTrelliscope) {
  
    # Make it false again
    final_data$MakeTrelliscope <- FALSE
    
    # Get the row 
    row <- final_data$TrelliRow
    
    # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
    paneled <- trelli_panel_by(final_data$TrelliData, input$TrelliPanelVariable)
    theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
    
    # Determine test example number
    choices <- final_data$TrelliData$trelliData.omics[[input$TrelliPanelVariable]] %>% unique() %>% as.character()
    test_example_num <- match(input$PlotOptionsPanel, choices)
    
    # Delete the trellifolder
    unlink("www/MODE", recursive = TRUE)
    
    withProgress({
      
      incProgress(0.5, "Building Trelliscope...")
      
      # Add additional values if plot inputs are not null 
      if (is.null(final_data$PlotInputs)) {
        eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', self_contained = TRUE, jsonp = FALSE) %>% print(view = FALSE)"))) 
      } else {
        
        # Add list of ggplot commands
        gg_params <- final_data$PlotInputs$Code
        
        # Make updated plot with parameters 
        eval(parse(text = paste0(theFun, "(trelliData=paneled, ggplot_params=gg_params, path = 'www/trelli', self_contained = TRUE, jsonp = FALSE) %>% print(view = FALSE)"))) 
        
      }
      
      incProgress(0.5, "Finished!")
      
    })
    
  }

  if (file.exists("www/trelli/index.html")) {
    test <- tags$iframe(src = "trelli/index.html", width = "1000px", height = "600px")
    test
  }
  
      
})
    
    # Fix html 
    #trelli_path <- "./www/MODE"
    #index_path <- file.path(trelli_path, 'index.html')
    #temp_index <- readLines(index_path)
    #
    ## Leading slashes mess things up, so included [/|\\]* regex, please change if there is a better way to do this substitution
    #temp_index <- gsub(file.path(paste0('[/|\\]*', "MODE"), 'appfiles'), 'appfiles', temp_index)
    #fileConn <- file(index_path)
    #writeLines(temp_index, fileConn)
    #close(fileConn)
    #
    ## Get path to display json and read json file
    #displaycfg_path <- file.path("./www", "MODE", "appfiles", "displays", "common", "Trelliscope", "displayObj.json")
    #displaycfg <- jsonlite::fromJSON(displaycfg_path)
    #
    ## remove jquery from the config to prevent conflicts
    #idx = which(grepl("jquery", displaycfg$panelInterface$deps$assets$url))
    #if(length(idx) > 0) {
    #  displaycfg$panelInterface$deps$assets <- displaycfg$panelInterface$deps$assets[-idx,] 
    #  jsonlite::write_json(displaycfg, displaycfg_path, auto_unbox = T)
    #}
    #
    #tagList(
    #  tags$div(id='trelli-display-wrapper', class='trelliscope-not-spa', style='width:500px; height:500px;'),
    #  tags$script(src='https://unpkg.com/trelliscopejs-lib/dist/trelliscope.min.js'),
    #  tags$script("(function() {
    #    trelliscopeApp('trelli-display-wrapper', 'MODE/appfiles/config.json');
    #   })();")
    #)
    
    
  #}
  
#})


##############
## OLD CODE ##
##############

output$one_plot_preview <- renderPlotly({
  #' TODO:  This will eventually be the UI which shows different kinds of plots.
  #' Will most likely be superseded by a renderUI with conditional output for
  #' each panel type.
  input$refresh_panel_preview
  one_df = nested_edata()$data[[1]]
  
  isolate({
    shiny::validate(
      need(
        length(edata_groups()) == nrow(one_df), 
        WARN_TEXT[["BAD_GROUP_LENGTH"]]
      )
    )
    one_df[['__GROUP_COL__']] <- edata_groups()
  })
  
  p <- simple_boxplots(one_df, "__GROUP_COL__", "Value")
  
  p
})

# trelliscope displays will be pulled to local filesystem from minio and 
# displayed by simply calling an iframe on the index object
output$trelliscope_from_iframe<- renderUI({
  input$reload_trelliscope_iframe
  input$pull_trelliscope
  # req(length(display_objects$saved_displays) > 0)
  # id = display_objects$saved_displays[[1]]$id
  id = input$minio_trelli_picker
  id = gsub("[/]+$", "", id) # remove trailing slashes to prevent shinyproxy error.
  
  # Current janky solution to prevent trelliscope from loading jquery again
  displaycfg_path <- Sys.glob(
    file.path(
      "www", id, "appfiles", "displays", "common", "*", "displayObj.json"
    )
  )[1]
  
  shiny::validate(need(isTruthy(displaycfg_path), "Display needs to be retrieved, or was corrupted on download.  Try retrieving again."))
  displaycfg <- jsonlite::fromJSON(displaycfg_path)
  
  # remove jquery from the config to prevent conflicts
  idx = which(grepl("jquery", displaycfg$panelInterface$deps$assets$url))
  if(length(idx) > 0) {
    displaycfg$panelInterface$deps$assets <- displaycfg$panelInterface$deps$assets[-idx,] 
    jsonlite::write_json(displaycfg, displaycfg_path, auto_unbox = T)
  }
  
  # reference the json file through trelliscope-app
  tagList(
    tags$div(id="trelli-display-wrapper", class="trelliscope-not-spa", style="width:800px; height:500px;"),
    tags$script(src="https://unpkg.com/trelliscopejs-lib/dist/trelliscope.min.js"),
    tags$script(sprintf("(function() {
        trelliscopeApp('trelli-display-wrapper',
          '%s/appfiles/config.json');
      })();", id))
  )
  # tags$iframe(src= file.path(id, "index.html"), width = "100%", height = "500px")
})
