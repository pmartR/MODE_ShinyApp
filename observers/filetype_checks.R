# A modified version of the mapDataAccess is_edata function that returns incorrect formatting if any
is.edata <- function(edata) {
  
  # If edata is NULL, return FALSE
  if (is.null(edata) || is.data.frame(edata) == FALSE) {
    return("Expression Data must be a data.frame or data.table")
  }
  
  # edata must have at least 2 samples 
  if (ncol(edata) < 3) {
    return("Expression Data must have at least 3 columns, a 'descriptor' column and at least 2 samples.")
  }
  
  ## All columns with the exception of one column MUST be numeric
  
  # Get counts of the number of numeric columns
  LogicCounts <- lapply(1:ncol(edata), function(col) {is.numeric(edata[,col])}) %>% 
    unlist() %>%
    table() 
  
  # If more than one column is not numeric, return error 
  if ("FALSE" %in% names(LogicCounts) && LogicCounts[["FALSE"]] > 1) {
    return("All columns in Expression Data must be numeric, with exception of a 'descriptor' column.")
  }
  
  # Otherwise, return True
  return("Valid")
  
}

# A modified version of the mapDataAccess is_fdata function that returns incorrect formatting if any
is.fdata <- function(edata, fdata) {
  
  # Make sure neither file is NULL 
  if (is.null(edata) || is.data.frame(edata) == FALSE) {
    return("Expression Data must be a data.frame or data.table.")
  }
  if (is.null(fdata) || is.data.frame(fdata) == FALSE) {
    return("Sample Information must be a data.frame or data.table.")
  }
  
  # The number of rows in fdata must be the number of columns in edata minus 1
  if (nrow(fdata) != (ncol(edata) - 1)) {
    return("The number of rows in Sample Information must match the number of columns in Expression Data minus 1.")
  }
  
  # Make sure the column names of edata[-1] match the column values of a f_data
  ColumnNameCheck <- lapply(1:ncol(fdata), function(col) {
    NamesToCheck <- colnames(edata)
    fdata[,col] %in% NamesToCheck %>% all()
  }) %>% unlist() %>% any()
  if (ColumnNameCheck == FALSE) {
    return("One column of Sample Information must match the sample names at the header (top) of the Expression Data edata.")
  }
  
  # Otherwise, return true
  return("Valid")
  
} 

# A modified version of the mapDataAccess is_emeta function that returns incorrect formatting if any
is.emeta <- function(edata, emeta) {
  
  # Make sure neither file is NULL 
  if (is.null(edata) || is.data.frame(edata) == FALSE) {
    return("Expression data must be a data.frame or data.table.")
  }
  if (is.null(emeta) || is.data.frame(emeta) == FALSE) {
    return("Biomolecule Information must be a data.frame or data.table.")
  }
  
  # The number of rows must match
  if (nrow(emeta) < nrow(edata)) {
    return("The number of rows in Biomolecule Information must be equal to or greater than the number of rows in Expression Data.")
  }
  
  # Identifiers in edata must exist in emeta 
  IdentifierCheck <- lapply(1:ncol(emeta), function(col) {
    all(edata[,1] %in% emeta[,col])
  }) %>% unlist() %>% any()
  if (IdentifierCheck == FALSE) {
    return("All identifiers in the Expression Data must exist in the Biomolecule Information file.")
  }
  
  # Otherwise, return True
  return("Valid")
  
}

is.statistics <- function(edata, fdata, statistics) {
  
  # Check for P_value_
  if (!any(grepl("P_value_", colnames(statistics)))) {
    return("'P_value_' must be included in at least one column of the Differential Statistics File.")
  }
  
  # Check for Fold_change_
  if (!any(grepl("Fold_change_", colnames(statistics)))) {
    return("'Fold_change_' must be included in at least one column of the Differential Statistics File.")
  }
  
  # Check comparisons 
  comparisons <- gsub("P_value_A_", "", colnames(statistics)[grepl("P_value_A_", colnames(statistics))])
  theLevels <- lapply(comparisons, function(x) {strsplit(x, "_vs_")}) %>% unlist() %>% unique()
  levelCheck <- lapply(theLevels, function(x) {any(fdata==x)}) %>% unlist()
  
  if (!all(levelCheck)) {
    return(
      paste("The following data is missing from the Sample Information:", paste0(theLevels[!levelCheck], collapse = ", "))
    )
  }
  
  # Check file sizes
  if (!(nrow(edata) == nrow(statistics))) {
    return("Expression Data and Differential Statistics should have the same number of rows.")
  }
  
  return("Valid")
  
}

# A scaled down version of the mapDataAccess project_edata function without input checking
project.edata <- function(projectname, datatype, edata, edata_filename = NULL) {
  
  # Construct project object
  ProjectObject <- list(
    "Project" = list(
      "Name" = projectname,
      "DataType" = datatype 
    ),
    "Data" = list(
      "e_data" = edata,
      "e_data_filename" = edata_filename
    )
  )
  
  # Assign the class attribute
  class(ProjectObject) <- "project edata"
  
  # Return
  return(ProjectObject)
  
}

# A scaled down version of .scrub_clean from mapDataAccess
.scrub_clean <- function(string) {
  return(gsub("[^[:alnum:]]|[[:space:]]", " ", as.character(string)))
}

