#' @description Returns the acceptable cognostics given a function and the trelliData object. 
#'    Since there are several rules regulating what cognostics can be returned in which instances,
#'    this function keeps track of that information
#' 
#' @param the_function The specific trelli plot building function 
#' @param the_object The trelliData object that will be passed to the trelli plot building function
return_cognostic <- function(the_function, the_object) {
  
  # Pull metas
  metas <- c()
  
  # Add emeta columns 
  if (!is.null(attr(final_data$TrelliData, "emeta_col"))) {
    metas <- c(metas, attr(final_data$TrelliData, "emeta_col"))
  }
  
  ## TRELLI ABUNDANCE BOXPLOT ##
  if (the_function == "trelli_abundance_boxplot") {
    
    metas <- c(metas, "count", "mean abundance")
    
    if (is.null(the_object$statRes)) {
      return(metas)
    } else {
      return(c(metas, "anova p-value", "fold change"))
    }
    
  }
  
  ## TRELLI ABUNDANCE HISTOGRAM ## 
  if (the_function == "trelli_abundance_histogram") {
    return(c(metas, "sample count", "mean abundance", "median abundance", "cv abundance",
             "skew abundance"))
  }
  
  ## TRELLI ABUNDANCE HEATMAP ##
  if (the_function == "trelli_abundance_heatmap") {
    return(c("sample count", "mean abundance", "biomolecule count"))
  }
  
  ## MISSINGNESS BAR PLOT ## 
  if (the_function == "trelli_missingness_bar") {
    
    metas <- c(metas, "total count", "observed count", "observed proportion")
    
    if (is.null(the_object$statRes)) {
      return(metas)
    } else {
      return(c(metas, "g-test p-value"))
    }
    
  }
  
  ## FOLD CHANGE BAR PLOT ## 
  if (the_function == "trelli_foldchange_bar") {
    return(c(metas, "fold change", "anova p-value"))
  }
  
  ## FOLD CHANGE BOXPLOT ##
  if (the_function == "trelli_foldchange_boxplot") {
    return(c("biomolecule count", "proportion significant", "mean fold change", 
             "sd fold change"))
  }
  
  ## FOLD CHANGE VOLCANO ##
  if (the_function == "trelli_foldchange_volcano") {
    return(c("biomolecule count", "proportion significant", "proportion significant up", 
             "proportion significant down"))
  }
  
  ## FOLD CHANGE HEATMAP ## 
  if (the_function == "trelli_foldchange_heatmap") {
    return(c("biomolecule count", "proportion significant", "mean fold change", 
             "sd fold change"))
  }
  
  ## TRELLI RNASEQ BOXPLOT ##
  if (the_function == "trelli_rnaseq_boxplot") {
    
    metas <- c(metas, "count", "mean lcpm")
    
    if (is.null(the_object$statRes)) {
      return(metas)
    } else {
      return(c(metas, "p-value", "fold change"))
    }
    
  }
  
  ## TRELLI RNASEQ HISTOGRAM ## 
  if (the_function == "trelli_rnaseq_histogram") {
    return(c(metas, "sample count", "mean lcpm", "median lcpm", "cv lcpm", "skew lcpm"))
  }
  
  ## TRELLI RNASEQ HEATMAP ##
  if (the_function == "trelli_rnaseq_heatmap") {
    return(c("sample count", "mean lcpm", "biomolecule count"))
  }
  
  ## RNASEQ NONZERO BAR PLOT ## 
  if (the_function == "trelli_missingness_bar") {
    return(c(metas, "total count", "non-zero count", "non-zero proportion"))
  }
  
  
}
