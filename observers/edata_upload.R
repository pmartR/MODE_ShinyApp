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
  
  # Check that the final transformation is at least log transformed, unless it is 
  # isobaric peptide data or nmr data.
  if (input$NewDataScale == "abundance" & uploaded_data()$Project$DataType %in% c("Peptide-level Isobaric", "Protein-level Isobaric", "Metabolomics-NMR") == FALSE) {
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
  if (is.null(omicFUN)) {omicFUN <- "as.pepData"}
  
  if (is.null(get_fdata())) {
    
    # Create omicData object
    omicData <- eval(parse(text = paste0(omicFUN, 
      "(e_data = uploaded_data()$Data$e_data, 
       edata_cname = input$edata_idcname_picker, 
       f_data = edata_groups$fdata, 
       fdata_cname = 'Sample',
       data_scale = input$OrigDataScale)")))
    
    # Add grouping
    omicData <- group_designation(omicData, "group")
    
  } else {

    # Create omicData object
    omicData <- as.pepData(
      e_data = get_edata(),
      edata_cname = input$edata_idcname_picker,
      f_data = get_fdata(), 
      fdata_cname = input$FDataColumn,
      data_scale = input$OrigDataScale
    )
    
    # Add grouping
    omicData <- group_designation(omicData, input$FDataGroup)
    
  }
  
  # Log transform if necessary
  if (input$OrigDataScale != input$NewDataScale) {
    omicData <- edata_transform(omicData, input$NewDataScale)
  }
  
  # Run normalization
  pval <- switch(input$NormSubsetFun,
    "all" = normalize_global(omicData, input$NormSubsetFun, input$NormFun),
    "complete" = normalize_global(omicData, input$NormSubsetFun, input$NormFun),
    "los" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("los" = input$NormalLOS)),
    "ppp" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP)),
    "rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("rip" = input$NormalRIP)),
    "ppp_rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP, "rip" = input$NormalRIP))
  ) %>% normRes_tests()
  pval_test <- pval$p_location
  pval_test <- round(pval_test, 4)
  
  # Save text results
  edata_groups$NormalizationText <- ifelse(pval_test >= 0.1, paste("P-Value:", pval_test), 
                                           paste("P-Value:", pval_test, "Consider another normalization"))
  
})

#' @details Apply normalization
observeEvent(input$ConfirmNormalization, {
  
  # If no f_data is uploaded, then
  if (!is.null(get_fdata())) {
    
    # Pick a random e_meta cname since it doesn't matter at all for MODE
    if (!is.null(get_emeta())) {
      emeta_cnames <- colnames(get_emeta())
      theEMETAcname <- emeta_cnames[emeta_cnames != input$edata_idcname_picker][1]
    } else {
      theEMETAcname <- NULL
    }
    
    # Make the omicData object
    omicData <- as.pepData(
      e_data = get_edata(),
      edata_cname = input$edata_idcname_picker,
      f_data = get_fdata(),
      fdata_cname = input$FDataColumn,
      e_meta = get_emeta(),
      emeta_cname = theEMETAcname,
      data_scale = input$OrigDataScale
    )
    
    # Log transform if necessary
    if (input$OrigDataScale != input$NewDataScale) {
      omicData <- edata_transform(omicData, input$NewDataScale)
    }
    
    # Group object
    omicData <- group_designation(omicData, input$FDataGroup)
    
    # Normalize if necessary
    if (input$IsNormalized == "No") {
      
      omicData <- switch(input$NormSubsetFun,
                         "all" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
                         "complete" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
                         "los" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("los" = input$NormalLOS), apply_norm = TRUE, backtransform = TRUE),
                         "ppp" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP), apply_norm = TRUE, backtransform = TRUE),
                         "rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE),
                         "ppp_rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP, "rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE)) 
      
      
    } else {
      attributes(omicData)$data_info$norm_info$is_normalized <- TRUE
    }
    
    # Make statistics object
    if (is.null(get_stats())) {
      statsObj <- NULL
    } else {
      statsObj <- get_stats()
      class(statsObj) <- c(class(statsObj), "statRes")
      attr(statsObj, "cnames")$edata_cname <- input$edata_idcname_picker
      attr(statsObj, "comparisons") <- colnames(statsObj)[grepl("P_value_A", colnames(statsObj))] %>% gsub(pattern = "P_value_A_", replacement = "") %>% unique()
    }
    
    # Now, we can finally create the trelliData object
    final_data$TrelliData <- as.trelliData(
      omicsData = omicData,
      statRes = statsObj
    )
    
  } else {
    
    # If more than one group, then this needs to become an omicsData object 
    if (unique(length(edata_groups$LockedGroupOrder)) > 1) {
      
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
      
      if (is.null(omicFUN)) {omicFUN <- "as.pepData"}
      
      if (!is.null(edata_groups$fdata)) {
        
        # Create omicData object
        omicData <- eval(parse(text = paste0(omicFUN, 
        "(e_data = uploaded_data()$Data$e_data, 
         edata_cname = input$edata_idcname_picker, 
         f_data = edata_groups$fdata, 
         fdata_cname = 'Sample',
         data_scale = input$OrigDataScale)")))
        
        
      } else {
        
        fdata <- data.frame(
          "Sample" = colnames(get_edata())[colnames(get_edata()) != input$edata_idcname_picker],
          "group" = "Group1"
        )
        
        # Create omicData object
        omicData <- eval(parse(text = paste0(omicFUN, 
                                             "(e_data = uploaded_data()$Data$e_data, 
         edata_cname = input$edata_idcname_picker, 
         f_data = fdata, 
         fdata_cname = 'Sample',
         data_scale = input$OrigDataScale)")))
        
      }
      
      # Log transform if necessary
      if (input$OrigDataScale != input$NewDataScale) {
        omicData <- edata_transform(omicData, input$NewDataScale)
      }
      
      # Add grouping
      omicData <- group_designation(omicData, "group")
      
      # Apply normalization
      omicData <- switch(input$NormSubsetFun,
        "all" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
        "complete" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
        "los" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("los" = input$NormalLOS), apply_norm = TRUE, backtransform = TRUE),
        "ppp" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP), apply_norm = TRUE, backtransform = TRUE),
        "rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE),
        "ppp_rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP, "rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE)
      ) 
      
      # Save omicData 
      final_data$OmicsData <- omicData
      
      # Create trelliData object 
      final_data$TrelliData <- as.trelliData(omicsData = omicData)
      
    } else {
      
      # Create an edata object to test 
      omic_type <- switch(uploaded_data()$Project$DataType,
                        "Peptide-level Label Free" = "pepData", 
                        "Peptide-level Isobaric" = "isobaricpepData", 
                        "Protein-level Label Free" = "proData", 
                        "Protein-level Isobaric" = "proData", 
                        "Lipidomics-Negative" = "lipidData", 
                        "Lipidomics-Positive" = "lipidData", 
                        "Metabolomics-GC/LC-MS" = "metabData", 
                        "Metabolomics-NMR" = "nmrData"
      )
      
      # Pull normalization parameters
      normalParams <- switch(input$NormSubsetFun,
        "all" = list(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, apply_norm = TRUE, backtransform = TRUE),
        "complete" = list(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, apply_norm = TRUE, backtransform = TRUE),
        "los" = list(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, params = list("los" = input$NormalLOS), apply_norm = TRUE, backtransform = TRUE),
        "ppp" = normalize_global(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, params = list("ppp" = input$NormalPPP), apply_norm = TRUE, backtransform = TRUE),
        "rip" = normalize_global(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, params = list("rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE),
        "ppp_rip" = normalize_global(subset_fn = input$NormSubsetFun, norm_fn = input$NormFun, params = list("ppp" = input$NormalPPP, "rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE)
      ) 
      
      
      # Otherwise, create trelliData.edata object
      final_data$TrelliData <- as.trelliData.edata(
        e_data = uploaded_data()$Data$e_data,
        edata_cname = input$edata_idcname_picker,
        omics_type = omic_type,
        data_scale_original = input$OrigDataScale,
        data_scale = input$NewDataScale,
        normalization_fun = "global",
        normalization_params = normalParams
      )
      
    }
    
  }
    
  # Close and update side bar
  updateCollapse(session, "trelli_collapse", open = "make_plot_opts",
                 close = c("front_page_upload_opts", "front_page_data_process_opts", "front_page_normalize_data"))
  
  # Open new tab
  updateTabsetPanel(session, "trelliscope_mainpanel", "select_plot")
  
})
