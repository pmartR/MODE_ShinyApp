#' @details Allow users to add groups 
observeEvent(input$GroupAdd, {
  edata_groups$Group <- append(edata_groups$Group, input$GroupName)
})

#' @detail Allow user to lock input groups
observeEvent(input$LockGroups, {
  
  # Make table
  fdata_table()
  
  # Disable widgets
  disable("LockGroups")
  
})

#' @details Run basic checks before proceeding to normalization
observeEvent(input$MoveToNormalization, {
  
  # Edata has been checked to be an edata_file in MAP
  if (!(MAP | Minio_Test | Redis_Test | Compose_Test)) {
  
    # Check that the final transformation is at least log transformed, unless it is 
    # isobaric peptide data or nmr data.
    if (input$input_datatype == "MS/NMR") {
      
      if (input$OrigDataScale == "abundance" & input$NewDataScale == "abundance" & 
          uploaded_data()$Project$DataType %in% c("Peptide-level Isobaric", "Protein-level Isobaric", "Metabolomics-NMR") == FALSE) {
        sendSweetAlert(session, "Expression Data Transformation Error", 
                       "Please choose a log transformation for the Expression Data.", "error")
        return(NULL)
      } 
      
    }
    
    ## Differential Statistics Check
    if (!is.null(edata_groups$ComparisonTable)) {
      
      # If the data.frame is 0 rows, then there is an issue
      if (nrow(edata_groups$ComparisonTable) == 0) {
        sendModalAlert(paste0("Differential Statistics Error! 1) Make sure this is ",
          "a unique column name per fold change. 2) Make sure there is a unique column ",
          "name per p-value or that it is entirely 'NA'. The 'Comparison Table' will ",
          "appear if this step is done correctly. See documentation for more help."))
        return(NULL)
      } 
      
      # Fold change cannot be empty
      if (any("NA" %in% edata_groups$ComparisonTable$Fold.Change.Columns)) {
        sendModalAlert("Differential Statistics Error! Fold Change Columns cannot be NA.")
        return(NULL)
      }
      
      # Fold change must have unique entries 
      if (length(edata_groups$ComparisonTable$Fold.Change.Columns) !=
          length(unique(edata_groups$ComparisonTable$Fold.Change.Columns))) {
        sendModalAlert("Differential Statistics Error! Fold Change Columns must be unique.")
        return(NULL)
      }
      
    }
  
    # We are ready for normalization
    edata_groups$ToNormalization <- TRUE
    
  } else {
    
    # Check that the final transformation is at least log transformed, unless it is 
    # isobaric peptide data or nmr data.
    if (MapConnect$Data$Project$DataType != "RNA-seq") {
      
      if (input$OrigDataScale == "abundance" & input$NewDataScale == "abundance" & 
          uploaded_data()$Project$DataType %in% c("Peptide-level Isobaric", "Protein-level Isobaric", "Metabolomics-NMR") == FALSE) {
        sendSweetAlert(session, "Expression Data Transformation Error", 
                       "Please choose a log transformation for the Expression Data.", "error")
        return(NULL)
      } 
      
    }
    
    edata_groups$ToNormalization <- TRUE
    
  }
    
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
    
    fdata_fake <- data.frame(
      Sample = colnames(get_edata()) %>% .[. != input$edata_idcname_picker],
      Condition = "A"
    )
    
    # Create omicData object
    omicData <- eval(parse(text = paste0(omicFUN, 
      "(e_data = uploaded_data()$Data$e_data, 
       edata_cname = input$edata_idcname_picker, 
       f_data = fdata_fake, 
       fdata_cname = 'Sample',
       data_scale = input$OrigDataScale)")))
    
    # Add grouping
    omicData <- group_designation(omicData, "Condition")
    
  } else {
    
    fdata_col <- input$FDataColumn
    if (is.null(fdata_col)) {
      fdata_col <- "Sample"
    }
    
    groups <- input$FDataGroup
    if (is.null(groups)) {
      groups <- "Condition"
    }
    
    # Create omicData object
    omicData <- as.pepData(
      e_data = get_edata(),
      edata_cname = input$edata_idcname_picker,
      f_data = get_fdata(), 
      fdata_cname = fdata_col,
      data_scale = input$OrigDataScale
    )
    
    # Add grouping
    omicData <- group_designation(omicData, groups)
    
  }
  
  # Log transform if necessary
  if (input$OrigDataScale == "abundance" & input$OrigDataScale != input$NewDataScale) {
    omicData <- edata_transform(omicData, input$NewDataScale)
  }
  
  # Run normalization
  pval <- tryCatch({
    pval_test <- switch(input$NormSubsetFun,
      "all" = normalize_global(omicData, input$NormSubsetFun, input$NormFun),
      "complete" = normalize_global(omicData, input$NormSubsetFun, input$NormFun),
      "los" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("los" = input$NormalLOS)),
      "ppp" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP)),
      "rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("rip" = input$NormalRIP)),
      "ppp_rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP, "rip" = input$NormalRIP))
    ) %>% normRes_tests()
    pval_test <- pval_test$p_location
    pval_test <- round(pval_test, 4)
    pval_test <- ifelse(pval_test >= 0.1, paste0("Test for subset & normalization: ", input$NormSubsetFun, " & ",
        input$NormFun, ". P-Value: ", pval_test, ". You may proceed with this normalization approach"), 
        paste0("Test for subset & normalization: ", input$NormSubsetFun, " & ",
          input$NormFun, ". P-Value: ", pval_test, ". Consider another normalization approach"))
    pval_test}, 
    error = function(e) return("This normalization approach is not possible with your current data. Try another approach."))

  if (is.na(pval)) {pval <- "Add groups to test normalization"}
  
  # Save text results
  edata_groups$NormalizationText <- pval
  
})

#' @details Apply normalization
observeEvent(input$ConfirmNormalization, {
  
  if (!MAP) {

    # If no fdata, generate a fdata file 
    if (!is.null(get_emeta())) {
      emeta_cnames <- colnames(get_emeta())
      theEMETAcname <- emeta_cnames[emeta_cnames != input$edata_idcname_picker][1]
    } else {
      theEMETAcname <- NULL
    }
        
    # Pick a random e_meta cname since it doesn't matter at all for MODE
    if (!is.null(get_emeta())) {
      emeta_cnames <- colnames(get_emeta())
      theEMETAcname <- emeta_cnames[emeta_cnames != input$edata_idcname_picker][1]
    } else {
      theEMETAcname <- NULL
    }
    
    # Make an exception for when there's no f data 
    fdata <- get_fdata()
    if (is.null(fdata)) {
      fdata <- pmartR::as.trelliData.edata(e_data = get_edata(), edata_cname = input$edata_idcname_picker, omics_type = "pepData")$omicsData$f_data
      fdata$Condition <- 1
    }
      
    fdata_col <- input$FDataColumn
    if (is.null(fdata_col)) {
      fdata_col <- "Sample"
    } 
      
    # Make the omicData object
    if (get_data_type() == "MS/NMR") {
      omicData <- as.pepData(
        e_data = get_edata(),
        edata_cname = input$edata_idcname_picker,
        f_data = fdata,
        fdata_cname = fdata_col,
        e_meta = get_emeta(),
        emeta_cname = theEMETAcname,
        data_scale = input$OrigDataScale
      )
    } else {
      omicData <- as.seqData(
        e_data = get_edata(),
        edata_cname = input$edata_idcname_picker,
        f_data = fdata,
        fdata_cname = fdata_col,
        e_meta = get_emeta(), 
        emeta_cname = theEMETAcname
      )
    }
    
    # Log transform if necessary
    if (get_data_type() == "MS/NMR" & !is.null(input$OrigDataScale) && input$OrigDataScale == "abundance") {
      omicData <- edata_transform(omicData, input$NewDataScale)
    }
    
    # Group object
    if (!is.null(input$FDataGroup)) {
      omicData <- group_designation(omicData, input$FDataGroup)
    } else {
      omicData <- group_designation(omicData, "Condition")
    }
    
    # Normalize if necessary
    if (get_data_type() == "MS/NMR" & !is.null(input$IsNormalized) && input$IsNormalized == "No") {
      
      omicData <- tryCatch({
        switch(input$NormSubsetFun,
               "all" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
               "complete" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
               "los" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("los" = input$NormalLOS), apply_norm = TRUE, backtransform = TRUE),
               "ppp" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP), apply_norm = TRUE, backtransform = TRUE),
               "rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE),
               "ppp_rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP, "rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE))
        },
        error = function(e) {
          sendSweetAlert(session,
                         "Normalization not possible",
                         paste(input$NormSubsetFun, "is not possible with your current data. Try another approach."),
                         type = "error")
          return(NULL)
        })
    
      if (is.null(omicData)) {return(NULL)}
      
      
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
      attr(statsObj, "comparisons") <- colnames(statsObj)[grepl("P_value_A_|P_value_G_|P_value_", colnames(statsObj))] %>% 
        gsub(pattern = "P_value_A_|P_value_G_|P_value_", replacement = "") %>% unique()
    }
    
    if (get_data_type() == "MS/NMR" & !is.null(input$OrigDataScale) && input$OrigDataScale != "abundance") {
      attr(omicData, "data_info")$data_scale_orig <- input$OrigDataScale
      attr(omicData, "data_info")$data_scale <- input$OrigDataScale
    }
    
    # Now, we can finally create the trelliData object
    final_data$TrelliData <- as.trelliData(
      omicsData = omicData,
      statRes = statsObj
    )
    
  } else {
    
    # If less than one group, then this needs to become an omicsData object 
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
      
      if (!is.null(get_fdata())) {
        
        fdata_fake <- data.frame(
          Sample = colnames(get_edata()) %>% .[. != input$edata_idcname_picker],
          Condition = "A"
        )
        
        # Create omicData object
        omicData <- eval(parse(text = paste0(omicFUN, 
        "(e_data = uploaded_data()$Data$e_data, 
         edata_cname = input$edata_idcname_picker, 
         f_data = fdata_fake, 
         fdata_cname = 'Sample',
         data_scale = input$OrigDataScale)")))
        
        # Add grouping
        omicData <- group_designation(omicData, "Condition")
        
        
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
      
      # Apply normalization if possible
      omicData <- tryCatch({
        switch(input$NormSubsetFun,
               "all" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
               "complete" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
               "los" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("los" = input$NormalLOS), apply_norm = TRUE, backtransform = TRUE),
               "ppp" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP), apply_norm = TRUE, backtransform = TRUE),
               "rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE),
               "ppp_rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP, "rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE))
      },
      error = function(e) {
        sendSweetAlert(session,
                       "Normalization not possible",
                       paste(input$NormSubsetFun, "is not possible with your current data. Try another approach."),
                       type = "error")
        return(NULL)
      })
      
      # Return NULL if it does not work
      if (is.null(omicData)) {return(NULL)}
      
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
      normalParams <- tryCatch({
        switch(input$NormSubsetFun,
               "all" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
               "complete" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, apply_norm = TRUE, backtransform = TRUE),
               "los" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("los" = input$NormalLOS), apply_norm = TRUE, backtransform = TRUE),
               "ppp" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP), apply_norm = TRUE, backtransform = TRUE),
               "rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE),
               "ppp_rip" = normalize_global(omicData, input$NormSubsetFun, input$NormFun, params = list("ppp" = input$NormalPPP, "rip" = input$NormalRIP), apply_norm = TRUE, backtransform = TRUE))
      },
      error = function(e) {
        sendSweetAlert(session,
                       "Normalization not possible",
                       paste(input$NormSubsetFun, "is not possible with your current data. Try another approach."),
                       type = "error")
        return(NULL)
      })
      
      # Return NULL if not possible
      if (is.null(normalParams)) {return(NULL)}
      
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

#' @details Download example data
observeEvent(input$ExampleDataPopUp, {
  showModal(modalDialog(fluidPage(fluidRow( 
    downloadButton("ExampleFiles", "Download Normalized NMR Example Files (Small)"),
    hr(),
    downloadButton("ExampleFiles2", "Download Normalized MS Example Files (Large)"),
    hr(),
    downloadButton("ExampleFiles3", "Download RNA-Seq Example Files"))),
    title = HTML('<p style="text-align: center;"><strong>Select an Example Dataset</strong></p>'),
    footer = modalButton("Exit"),
    size = "m", easyClose = T))
})

