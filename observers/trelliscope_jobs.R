# Define function for modal warnings, since "sendSweetAlert" was messing with the UI
sendModalAlert <- function(message = "") {
  showModal(modalDialog(
    HTML(paste0('<span style="font-size: 22px;">', message, '</span>')),
    title = "", size = "s", easyClose = TRUE
  ))
}

# Add build stats 
output$BuildStats <- renderUI({
  
  if (is.null(final_data$TrelliRow)) {return(HTML("Build time statistics will appear here."))}

  # Apply filtering if there is that option
  if (!is.null(final_data$TrelliData_Filtered)) {
    
    Sys.sleep(1)
    
    # Pull selected row information 
    ThePlot <- final_data$PlotOptions[final_data$TrelliRow, "Plot"] %>% unlist()
    PanelByChoice <- final_data$PlotOptions[final_data$TrelliRow, "Panel By Choice"] %>% unlist()
    PlotNum <- final_data$TrelliData_Filtered %>% select(!!input$TrelliPanelVariable) %>% unique() %>% unlist() %>% length()
    
  } else {
    PlotNum <- final_data$PlotOptions[final_data$TrelliRow, "Number of Plots"] %>% unlist() %>% as.numeric()
  }
  
  BuildTime <- ceiling(PlotNum / 60)
  
  if (BuildTime <= 10) {
    HTML(paste("The estimated build time is", BuildTime, "minutes"))
  } else {
    HTML(paste("The estimated build time is", BuildTime, "minutes. Consider filtering down the number of plots with the 'Filter Plots' sidebar"))
  }

  
})

# Make the trelliscope display locally (non-docker), or in a redis container
observeEvent(input$make_trelliscope, {
  
  updateTabsetPanel(session, "trelliscope_mainpanel", "trelliscope_display")
  
  # Get the row 
  row <- final_data$TrelliRow
  
  # Apply filtering if there is that option
  if (!is.null(final_data$TrelliData_Filtered)) {
    trelliData <- final_data$TrelliData
    trelliData$trelliData <- final_data$TrelliData_Filtered[!is.na(final_data$TrelliData_Filtered[[input$TrelliPanelVariable]]),]
  } else {
    trelliData <- final_data$TrelliData
  }
  
  # If nothing to plot, trigger warning
  if (nrow(trelliData$trelliData) == 0) {
    sendSweetAlert(session, "No plots error", "You have filtered out all plots. Try less strict filters in the 'Filter Plots' menu.",
                   "error")
    return(NULL)
  }
  
  # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
  paneled <- trelli_panel_by(trelliData, input$TrelliPanelVariable)
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  # Determine test example number
  choices <- trelliData$trelliData[[input$TrelliPanelVariable]] %>% unique() %>% as.character() 
  
  # foldchange is written without the underscore
  if (grepl("fold_change", theFun)) {
    theFun <- gsub("fold_change", "foldchange", theFun)
  }
  
  # Name the trelliscope display
  scrub_clean2 <- function(string) {gsub("[^[:alnum:]]|[[:space:]]|:|-", " ", as.character(string))}
  trelliName <- scrub_clean2(input$trelliscope_name)
  
  # If MAP or REDIS_VERSION or Compose version 
  if (Redis_Test | MAP | Compose_Test) {
    
    if (is.null(final_data$PlotInputs)) {
      ggparams <- NULL
    } else {ggparams <- final_data$PlotInputs$Code}
    
    sendModalAlert(paste0("The trelliscope display titled ", trelliName, " has been submitted as a job.", 
                          " Note that only one job processes at a time.", 
                          " See job status under the 'Job Status' tab and click 'Refresh Display' to",
                          " view it when it's finished. Displays can also be downloaded from MAP, as well as job statuses checked."))
    
    # Grab tags
    theTags <- mapDataAccess::pull_tags_from_object(MapConnect$Data)
    
    # Save the paneled trelliscope data 
    new_file <- mapDataAccess::put_data(MapConnect$MapConnect, paneled)
    
    set_tags(MapConnect$MapConnect, new_file, list(
      "ObjectType" = "MODE-File",
      "DataType" = theTags$DataType,
      "ProjectName" = trelliName
    ))
    
    # Needed parameters: Username, whether this is the Compose or MAP version, 
    # uuid to the trelliData file, selected cognostics, ggparameters, pValue threshold (if necessary),
    # and comparison (if necessary)
    MapConnect$Job = celery_app$send_task(
      "trelliscope_builder",
      kwargs = list(
        username = Sys.getenv("SHINYPROXY_USERNAME"),
        Compose_Test = Compose_Test,
        MAP = MAP,
        uuid = new_file,
        theFun = theFun,
        trelliPath = file.path("/trelliscope", trelliName), 
        cogs = input$ChooseCognostics,
        ggplotParams = ggparams,
        pValueThresh = input$PValueThresh,
        compare = input$SelectComparison
      )
    )
    
  } else {
  
    # Delete the trellifolder
    unlink("www/trelli_json", recursive = TRUE)
    unlink("www/trelli", recursive = TRUE) 

    
    withProgress({
      
      incProgress(0.5, "Building Trelliscope...")
      
      # Add additional values if plot inputs are not null 
      if (is.null(final_data$PlotInputs)) {
        
        if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
          
          if (is.null(input$PValueThresh)) {return(NULL)}
          
          pvaluethresh <- input$PValueThresh
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli_json', name = trelliName, p_value_thresh = pvaluethresh, cognostics = input$ChooseCognostics, jsonp = FALSE) %>% print(view = FALSE)")))
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, p_value_thresh = pvaluethresh, cognostics = input$ChooseCognostics, jsonp = TRUE) %>% print(view = FALSE)")))
          
        } else if (theFun == "trelli_foldchange_volcano") {
          
          if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
          
          pvaluethresh <- input$PValueThresh
          comparison <- input$SelectComparison
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli_json', name = trelliName, p_value_thresh = pvaluethresh, comparison = comparison, cognostics = input$ChooseCognostics, jsonp = FALSE) %>% print(view = FALSE)")))
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, p_value_thresh = pvaluethresh, comparison = comparison, cognostics = input$ChooseCognostics, jsonp = TRUE) %>% print(view = FALSE)")))
          
        } else {
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli_json', name = trelliName, cognostics = input$ChooseCognostics, jsonp = FALSE) %>% print(view = FALSE)")))
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, cognostics = input$ChooseCognostics, jsonp = TRUE) %>% print(view = FALSE)")))
        } 
        
      } else {
        
        # Add list of ggplot commands
        gg_params <- final_data$PlotInputs$Code
        
        if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
          
          if (is.null(input$PValueThresh)) {return(NULL)}
          
          pvaluethresh <- input$PValueThresh
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli_json', name = trelliName, p_value_thresh = pvaluethresh, ggplot_params=gg_params, jsonp = FALSE) %>% print(view = FALSE)")))
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, p_value_thresh = pvaluethresh, ggplot_params=gg_params, jsonp = TRUE) %>% print(view = FALSE)")))
          
        } else if (theFun == "trelli_foldchange_volcano") {
          
          if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
          
          pvaluethresh <- input$PValueThresh
          comparison <- input$SelectComparison
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli_json', name = trelliName, p_value_thresh = pvaluethresh, comparison = comparison, ggplot_params=gg_params, cognostics = input$ChooseCognostics, jsonp = FALSE) %>% print(view = FALSE)")))
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, p_value_thresh = pvaluethresh, comparison = comparison, ggplot_params=gg_params, cognostics = input$ChooseCognostics, jsonp = TRUE) %>% print(view = FALSE)")))
          
        } else {
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli_json', name = trelliName, ggplot_params=gg_params, cognostics = input$ChooseCognostics, jsonp = FALSE) %>% print(view = FALSE)")))
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, ggplot_params=gg_params, cognostics = input$ChooseCognostics, jsonp = TRUE) %>% print(view = FALSE)")))
        }
        
      }
  
      incProgress(0.5, "Finished!")
      
    })
  }
  
})

