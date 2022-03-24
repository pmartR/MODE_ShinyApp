#' @details Enable the make trelliscope button when the plot is ready 
observe({
  
  # Accumulate different conditions
  cond = TRUE
  message = ""
  
  # If no trelliRow, let the user know
  if (is.null(final_data$TrelliRow)) {
    cond = FALSE
    message = "This will be enabled once 'Confirm Plot' is clicked in Make Plot."
  }
  
  toggleState("make_trelliscope", condition = cond)
  
  show_add_tooltip(
    session, 
    "make_trelliscope_disable_info", 
    condition = !cond,
    tooltip_text = message
  )
  
})