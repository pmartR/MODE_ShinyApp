#' ping redis and send the selected task
#' TODO:  This observer will eventually if-else over several options that 
#' specify which type of display is to be created.
observeEvent(input$make_trelliscope, {
  
  updateTabsetPanel(session, "trelliscope_mainpanel", "trelliscope_display")
  
  # Get the row 
  row <- final_data$TrelliRow
  
  # Make plot. Paneled = trelli_panel_by run on trelliData. theFun = name of the plotting fun.
  paneled <- trelli_panel_by(final_data$TrelliData, input$TrelliPanelVariable)
  theFun <- paste0("trelli_", final_data$PlotOptions[row, "Plot"] %>% unlist() %>% gsub(pattern = " ", replacement = "_"))
  
  # Determine test example number
  choices <- final_data$TrelliData$trelliData.omics[[input$TrelliPanelVariable]] %>% unique() %>% as.character()
  test_example_num <- match(input$PlotOptionsPanel, choices)
  
  # foldchange is written without the underscore
  if (grepl("fold_change", theFun)) {
    theFun <- gsub("fold_change", "foldchange", theFun)
  }
  
  # If MAP or REDIS_VERSION
  if (Redis_Test | MAP) {
    
    if (is.null(final_data$PlotInputs)) {
      ggparams <- NULL
    } else {ggparams <- final_data$PlotInputs$Code}
    
    celery_app$send_task(
      "trelliscope_builder",
      kwargs = list(
        username = "test",
        paneled = paneled, 
        theFun = theFun,
        trelliPath = "/trelliscope",
        cogs = input$ChooseCognostics,
        ggplotParams = ggparams,
        pValueTest = input$PValueTest,
        pValueThresh =input$PValueThresh,
        compare = input$SelectComparison
      )
    )
    
  } else {
  
    # Delete the trellifolder
    unlink("www/MODE", recursive = TRUE)
    
    withProgress({
      
      incProgress(0.5, "Building Trelliscope...")
      
      # Add additional values if plot inputs are not null 
      if (is.null(final_data$PlotInputs)) {
        
        # If no test_example_num, return NULL
        if (is.na(test_example_num)) {return(NULL)}
        
        if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
          
          if (is.null(input$PValueTest) | is.null(input$PValueThresh)) {return(NULL)}
          
          pvaluetest <- input$PValueTest
          pvaluethresh <- input$PValueThresh
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', self_contained = TRUE, jsonp = FALSE, p_value_test = pvaluetest, p_value_thresh = pvaluethresh, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
          
        } else if (theFun == "trelli_foldchange_volcano") {
          
          if (is.null(input$PValueTest) | is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
          
          pvaluetest <- input$PValueTest
          pvaluethresh <- input$PValueThresh
          comparison <- input$SelectComparison
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', self_contained = TRUE, jsonp = FALSE, p_value_test = pvaluetest, p_value_thresh = pvaluethresh, comparison = comparison, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
          
        } else {
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', self_contained = TRUE, jsonp = FALSE, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
        } 
        
      } else {
        
        # Add list of ggplot commands
        gg_params <- final_data$PlotInputs$Code
        
        # If no test_example_num, return NULL
        if (is.na(test_example_num)) {return(NULL)}
        
        if (theFun %in% c("trelli_foldchange_bar", "trelli_foldchange_boxplot", "trelli_foldchange_heatmap")) {
          
          if (is.null(input$PValueTest) | is.null(input$PValueThresh)) {return(NULL)}
          
          pvaluetest <- input$PValueTest
          pvaluethresh <- input$PValueThresh
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', self_contained = TRUE, jsonp = FALSE, p_value_test = pvaluetest, p_value_thresh = pvaluethresh, ggplot_params=gg_params) %>% print(view = FALSE)")))
          
        } else if (theFun == "trelli_foldchange_volcano") {
          
          if (is.null(input$PValueTest) | is.null(input$PValueThresh) | is.null(input$SelectComparison)) {return(NULL)}
          
          pvaluetest <- input$PValueTest
          pvaluethresh <- input$PValueThresh
          comparison <- input$SelectComparison
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', self_contained = TRUE, jsonp = FALSE, p_value_test = pvaluetest, p_value_thresh = pvaluethresh, comparison = comparison, ggplot_params=gg_params, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
          
        } else {
          eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', self_contained = TRUE, jsonp = FALSE, ggplot_params=gg_params, cognostics = input$ChooseCognostics) %>% print(view = FALSE)")))
        }
        
      }
  
      incProgress(0.5, "Finished!")
      
    })
  }
  
})
    
  # TODO:  Smarter check that groups are valid
  #req(edata_groups(), input$edata_idcname_picker)
  #
  #if(input$local_or_minio=="local"){
  #  req(input$raw_data_upload)
  #  object_name = uuid::UUIDgenerate()
  #  refname = paste0(object_name, "_trelliscope")
  #  mapDataAccess::put_file(miniocon, 
  #                          id = object_name, 
  #                          filename = input$raw_data_upload$datapath)
  #}
  #else if(input$local_or_minio == "minio"){
  #  if(isTRUE(input$minio_choose_file != NOSELECT_)){
  #    object_name = input$minio_choose_file
  #    refname = paste0(get_data(miniocon, input$minio_choose_file)$Project$Name, "_trelliscope")
  #  } else {
  #    object_name = queryTags$query1
  #    refname = paste0(get_data(miniocon, queryTags$query1)$Project$Name, "_trelliscope") 
  #  }
  #}
  #
  #celery_app$send_task(
  #  "edata_simple_boxplots", 
  #   kwargs=list(
  #     object_name=object_name,
  #     trelli_path=refname,
  #     username=Sys.getenv("SHINYPROXY_USERNAME"),
  #     panel_column=input$edata_idcname_picker,
  #     groups=edata_groups()
  #    )
  #)

# grab finished trelliscope display from minio
observeEvent(input$pull_trelliscope, {
  req(input$minio_trelli_picker)
  
  withProgress(
    message = "Pulling files from Minio...", value = 1, 
    {
      tmp_file <- mapDataAccess::get_file(miniocon, input$minio_trelli_picker, filename=tempfile())
      unzip(tmp_file, exdir = "www")
      file.remove(tmp_file)
    }
  )
}, priority=10)
