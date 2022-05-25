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
  
  # Delete the trellifolder
  unlink("www/MODE", recursive = TRUE)
  
  withProgress({
    
    incProgress(0.5, "Building Trelliscope...")
    
    # Add additional values if plot inputs are not null 
    if (is.null(final_data$PlotInputs)) {
      
      eval(parse(text = paste0(theFun, "(trelliData=paneled, path='www/trelli', self_contained = TRUE, jsonp = FALSE, interactive=FALSE) %>% print(view = FALSE)"))) 
      
    } else {
      
      # Add list of ggplot commands
      gg_params <- final_data$PlotInputs$Code
      
      eval(parse(text = paste0(theFun, "(trelliData=paneled, ggplot_params=gg_params, path = 'www/trelli', self_contained = TRUE, jsonp = FALSE, interactive=FALSE) %>% print(view = FALSE)")))
      
    }

    incProgress(0.5, "Finished!")
    
  })
  
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
