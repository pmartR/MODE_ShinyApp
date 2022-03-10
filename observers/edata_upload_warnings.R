#' @details Allow users to add groups 
observeEvent(input$GroupAdd, {
  edata_groups$Group <- append(edata_groups$Group, input$GroupName)
})

#' @detail Allow user to lock input groups
observeEvent(input$LockGroups, {
  
  # Make table
  fdata_table()
  
  # Disable widgets
  disable("GroupAdd")
  disable("LockGroups")
  disable("GroupName")
  
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
  edata_groups$LockedGroupOrder <- shinyValue("GroupSelector", ncol(uploaded_data()$Data$e_data) - 1, input)
  
  # Collapse above tabs and open next one
  updateCollapse(session, "trelli_collapse", open = "front_page_normalize_data",
                 close = c("front_page_upload_opts", "front_page_data_process_opts"))
  
  
})

#' @details Run normalization check 
observeEvent(input$CheckNormalization, {
  
  browser()
  
  # Create an edata object to test 
  omicFUN <- switch(uploaded_data()$Project$DataType,
    "Peptide-level Label Free" = "as.pepData", 
    "Peptide-level Isobaric" = "as.isobaricpepData", 
    "Protein-level Label Free" = "as.proData", 
    "Protein-level Isobaric" = "as.proData", 
    "Lipidomics-Negative" = "as.lipidData", 
    "Lipidomics-Positive" = "as.lipidData", 
    "Metabolomics-GC/LC-MS" = "as.metabData", 
    "Metabolomics-NMR" = "as.nmrData"
  )
  
  # Create omicData object
  omicData <- eval(parse(text = paste0(omicFUN, 
       "(e_data = uploaded_data()$Data$e_data, 
       edata_cname = input$edata_idcname_picker, 
       f_data = edata_groups$fdata, 
       fdata_cname = 'Sample',
       data_scale = input$OrigDataScale)")))
  
  # Log transform if necessary
  if (input$OrigDataScale != input$NewDataScale) {
    omicData <- edata_transform(omicData, input$NewDataScale)
  }
  
  # Add grouping
  omicData <- group_designation(omicData, "group")
  
  # Run normalization
  switch(input$NormSubsetFun,
    "all"
    
    
  )
  normalize_global(omicData, input$NormSubsetFun, input$NormFun) %>% normRes_tests()
  
})


