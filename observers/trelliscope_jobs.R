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
  if (!is.null(input$PValueFilterTest)) {
    
    # Pull comparisons
    Comparisons <- input$PValueFilterComparisons
    if (Comparisons == "None") {Comparisons <- NULL}
    
    # P Value test
    PValTest <- input$PValueFilterTest 
    if (PValTest == "none") {PValTest <- NULL}
    
    # Pull selected row information 
    ThePlot <- final_data$PlotOptions[final_data$TrelliRow, "Plot"] %>% unlist()
    PanelByChoice <- final_data$PlotOptions[final_data$TrelliRow, "Panel By Choice"] %>% unlist()
    
    # Calculate number of plots
    FiltSummary <- trelli_pvalue_filter(
      trelliData = final_data$TrelliData, 
      p_value_test = PValTest,
      p_value_thresh = input$PValueFilterPanel, 
      comparison = Comparisons
    ) %>% summary()
    PlotNum <- FiltSummary %>%
      dplyr::filter(`Panel By Choice` == PanelByChoice & Plot == ThePlot) %>%
      dplyr::select(`Number of Plots`) %>%
      unlist() %>%
      as.numeric()
    
  } else {
    PlotNum <- final_data$PlotOptions[final_data$TrelliRow, "Number of Plots"] %>% unlist() %>% as.numeric()
  }
  
  BuildTime <- ceiling(PlotNum / 87.6)
  
  if (BuildTime < 12) {
    HTML(paste("The estimated build time is", BuildTime, "minutes"))
  } else {
    HTML(paste("The estimated build time is", BuildTime, "minutes. Try filtering the data by p-values in MODE. If there are no p-values to filter, they can be calculated in PMart and iPMart."))
  }

  
})

# Create job status if MAP version
output$job_status_ui <- renderUI({
  if (MAP) {actionButton("job_status", "Check Job Status", icon = icon("clipboard-check"))}
})

# Return job status
observeEvent(input$job_status, {
  if (!is.null(MapConnect$Job)) {
    if ("NULL" %in% MapConnect$Job$info) {sendModalAlert("Job finished!")} else {
      sendModalAlert(gsub("INFO: ", "", MapConnect$Job$info))}
  } else {
    sendModalAlert("No jobs are currently running. Try the 'Refresh Display' button. If the display isn't there, try 'Create Trelliscope Display'")
  }
})

# Make the trelliscope display locally (non-docker), or in a redis container
observeEvent(input$make_trelliscope, {
  
  updateTabsetPanel(session, "trelliscope_mainpanel", "trelliscope_display")
  
  # Get the row 
  row <- final_data$TrelliRow
  
  # Apply filtering if there is that option
  if (!is.null(input$PValueFilterTest)) {
    
    # Pull comparisons
    Comparisons <- input$PValueFilterComparisons
    if (Comparisons == "None") {Comparisons <- NULL}
    
    # P Value test
    PValTest <- input$PValueFilterTest 
    if (PValTest == "none") {PValTest <- NULL}
    
    trelliData <- trelli_pvalue_filter(
      trelliData = final_data$TrelliData, 
      p_value_test = PValTest,
      p_value_thresh = input$PValueFilterPanel, 
      comparison = Comparisons
    )
    
  } else {
    trelliData <- final_data$TrelliData
  }
  
  # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
  paneled <- trelli_panel_by(trelliData, input$TrelliPanelVariable)
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  # Determine test example number
  choices <- trelliData$trelliData.omics[[input$TrelliPanelVariable]] %>% unique() %>% as.character() 
  test_example_num <- match(input$PlotOptionsPanel, choices)
  
  # foldchange is written without the underscore
  if (grepl("fold_change", theFun)) {
    theFun <- gsub("fold_change", "foldchange", theFun)
  }
  
  # Name the trelliscope display
  trelliName <- .scrub_clean(input$trelliscope_name)
  
  # If MAP or REDIS_VERSION or Compose version 
  if (Redis_Test | MAP | Compose_Test) {
    
    if (is.null(final_data$PlotInputs)) {
      ggparams <- NULL
    } else {ggparams <- final_data$PlotInputs$Code}
    
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(input$NormSubsetFun)) {
      normalParams <- switch(input$NormSubsetFun,
       "all" = list(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, apply_norm = TRUE, backtransform = TRUE),
       "complete" = list(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, apply_norm = TRUE, backtransform = TRUE),
       "los" = list(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, params = list("los" = input$NormalLOS), apply_norm = TRUE, backtransform = TRUE),
       "ppp" = list(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, params = list("ppp" = input$NormalPPP), apply_norm = TRUE, backtransform = TRUE),
       "rip" = list(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, params = list("rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE),
       "ppp_rip" = list(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, params = list("ppp" = input$NormalPPP, "rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE)
      ) 
    } else {
      normalParams <- NULL
    }
    
    sendModalAlert(paste0("The trelliscope display titled ", trelliName, " has been submitted as a job.", 
                          " Click 'Check Status' to see the status of the job and 'Refresh Display' to",
                          " view it when it's finished."))
    
    sc <- input$self_contained == "Yes"
    
    MapConnect$Job = celery_app$send_task(
      "trelliscope_builder",
      kwargs = list(
        username = Sys.getenv("SHINYPROXY_USERNAME"),
        Compose_Test = Compose_Test,
        MAP = MAP,
        uuid = query$data,
        panelVar = input$TrelliPanelVariable,
        theFun = theFun,
        trelliPath = file.path("/trelliscope", trelliName), 
        cogs = input$ChooseCognostics,
        ggplotParams = ggparams,
        pValueTest = input$PValueTest, # We will need to fix when updating MAP's MODE 
        pValueThresh = input$PValueThresh,
        compare = input$SelectComparison,
        ipmart_sub = input$SelectOmics,
        edata_args = list(
          edata_cname = input$edata_idcname_picker,
          data_scale_original = input$OrigDataScale, 
          data_scale = input$NewDataScale,
          normalization_fun = "global",
          normalization_params = normalParams
        )
      )
    )
    
  } else {
  
    # Delete the trellifolder
    unlink("www/trelli", recursive = TRUE)
    
    withProgress({
      
      incProgress(0.5, "Building Trelliscope...")
      
      # Add additional values if plot inputs are not null 
      if (is.null(final_data$PlotInputs)) {
        
        # If no test_example_num, return NULL
        if (is.na(test_example_num)) {return(NULL)}
        
        if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
          
          if (is.null(input$PValueThresh)) {return(NULL)}
          
          pvaluethresh <- input$PValueThresh
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, self_contained = TRUE, jsonp = FALSE, p_value_thresh = pvaluethresh, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
          
        } else if (theFun == "trelli_foldchange_volcano") {
          
          if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
          
          pvaluethresh <- input$PValueThresh
          comparison <- input$SelectComparison
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, self_contained = TRUE, jsonp = FALSE, p_value_thresh = pvaluethresh, comparison = comparison, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
          
        } else {
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, self_contained = TRUE, jsonp = FALSE, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
        } 
        
      } else {
        
        # Add list of ggplot commands
        gg_params <- final_data$PlotInputs$Code
        
        # If no test_example_num, return NULL
        if (is.na(test_example_num)) {return(NULL)}
        
        if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
          
          if (is.null(input$PValueThresh)) {return(NULL)}
          
          pvaluethresh <- input$PValueThresh
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, self_contained = TRUE, jsonp = FALSE, p_value_thresh = pvaluethresh, ggplot_params=gg_params) %>% print(view = FALSE)")))
          
        } else if (theFun == "trelli_foldchange_volcano") {
          
          if (is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
          
          pvaluethresh <- input$PValueThresh
          comparison <- input$SelectComparison
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, self_contained = TRUE, jsonp = FALSE, p_value_thresh = pvaluethresh, comparison = comparison, ggplot_params=gg_params, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
          
        } else {
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', name = trelliName, self_contained = TRUE, jsonp = FALSE, ggplot_params=gg_params, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
        }
        
      }
  
      incProgress(0.5, "Finished!")
      
    })
  }
  
})
