#' @details Make sure that the column id and group specifications are valid to 
#' create the trelliscope boxplots
#' TODO:  When there are multiple plots, will require that we have selected the
#' boxplots option to perform this check.
observe({
  #' req(<we have selected boxplots as our panel>)
  
  #' Get T/F of whether we pass certain conditions for making display, it should
  #' always be T -> everything is good to go
  cond_valid_groups <- tryCatch({
    length(edata_groups()) == nrow(nested_edata()$data[[1]])
  }, error = function(e) {
    "__no_groups__"
  })
  
  disable_message = character(0)
  
  if(cond_valid_groups == "__no_groups__") {
    disable_message <- c(disable_message, WARN_TEXT[["SPECIFY_GROUPS"]])
    cond_valid_groups <- FALSE
  } else if(!cond_valid_groups) {
    disable_message <- c(disable_message, WARN_TEXT[["BAD_GROUP_LENGTH"]])
  }
  
  # accumulate different conditions
  cond = cond_valid_groups
  
  disable_message = paste(disable_message, collapse = "\n")
  
  toggleState("make_trelliscope", condition = cond)
  
  show_add_tooltip(
    session, 
    "make_trelliscope_disable_info", 
    condition = !cond,
    tooltip_text = disable_message
  )
  
})