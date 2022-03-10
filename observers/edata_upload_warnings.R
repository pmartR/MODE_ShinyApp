#' @details Allow users to add groups 
observeEvent(input$GroupAdd, {
  
  edata_groups$Group <- append(edata_groups$Group, input$GroupName)
  
})

#' @details Run basic checks before proceeding to normalization
observeEvent(input$MoveToNormalization, {
  
  # Edata has been checked to be an edata_file in MAP
  
  # Check that the final transformation is at least log transformed 
  if (input$NewDataScale == "abundance") {
    sendSweetAlert(session, "Expression Data Transformation Error", 
      "Please choose a log transformation for the Expression Data.", "error")
    return(NULL)
  }
  
  # We are ready for normalization
  edata_groups$ToNormalization <- TRUE
  
  # Collapse above tabs and open next one
  updateCollapse(session, "trelli_collapse", open = "front_page_normalize_data",
                 close = c("front_page_upload_opts", "front_page_data_process_opts"))
  
  
})