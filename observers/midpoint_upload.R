#' @detail Create an observer to fill the final_data reactive value holders when 
#' the input data is a midpoint file 
observe({
  
  req(uploaded_data())
  
  # Update collapses 
  if (class(uploaded_data()) %in% c("midpoint pmart", "midpoint ipmart")) {
    
    # Immediately open the make plot 
    updateCollapse(session, id = "trelli_collapse", 
                   open = "make_plot_opts",
                   close = c("front_page_data_process_opts", "front_page_normalize_data"))
    
    # Open a tab 
    updateTabsetPanel(
      session, "trelliscope_mainpanel", "select_plot"
    )
    
  }
  
  # Pass objects to final_data
  if (class(uploaded_data()) %in% c("midpoint pmart")) {

    final_data$OmicsData <- uploaded_data()$`Data Objects`$OmicsData
    final_data$TrelliData <- as.trelliData(
      omicsData = uploaded_data()$`Data Objects`$OmicsData,
      statRes = uploaded_data()$`Data Objects`$OmicsStats
    )
    
  } 
  
  if (class(uploaded_data()) %in% c("midpoint ipmart")) {
    
    req(input$SelectOmics)
    
    final_data$OmicsData <- uploaded_data()$`Data Objects`[[input$SelectOmics]]$`Data Objects`$OmicsData
    final_data$TrelliData <- as.trelliData(
      omicsData = uploaded_data()$`Data Objects`[[input$SelectOmics]]$`Data Objects`$OmicsData,
      statRes = uploaded_data()$`Data Objects`[[input$SelectOmics]]$`Data Objects`$OmicsStats
    )
    
  } 
  
  
})