output$enter_debugger <- renderUI({
  if(isTRUE(getOption("shiny.testmode"))){
    tagList(
      actionButton("debugger", "whats wrong!?!?")
    )
  }
  else return(NULL)
})
