output$one_dataset_preview <- renderDataTable({
  nested_edata()$data[[1]]
})

output$one_plot_preview <- renderPlotly({
  #' TODO:  This will eventually be the UI which shows different kinds of plots.
  #' Will most likely be superseded by a renderUI with conditional output for
  #' each panel type.
  input$refresh_panel_preview
  one_df = nested_edata()$data[[1]]
  
  isolate({
    shiny::validate(
      need(
        length(edata_groups()) == nrow(one_df), 
        WARN_TEXT[["BAD_GROUP_LENGTH"]]
      )
    )
    one_df[['__GROUP_COL__']] <- edata_groups()
  })
  
  p <- simple_boxplots(one_df, "__GROUP_COL__", "Value")
  
  p
})

# trelliscope displays will be pulled to local filesystem from minio and 
# displayed by simply calling an iframe on the index object
output$trelliscope_from_iframe<- renderUI({
  input$reload_trelliscope_iframe
  input$pull_trelliscope
  # req(length(display_objects$saved_displays) > 0)
  # id = display_objects$saved_displays[[1]]$id
  id = input$minio_trelli_picker
  id = gsub("[/]+$", "", id) # remove trailing slashes to prevent shinyproxy error.
  
  # Current janky solution to prevent trelliscope from loading jquery again
  displaycfg_path <- Sys.glob(
    file.path(
      "www", id, "appfiles", "displays", "common", "*", "displayObj.json"
    )
  )[1]
  
  shiny::validate(need(isTruthy(displaycfg_path), "Display needs to be retrieved, or was corrupted on download.  Try retrieving again."))
  displaycfg <- jsonlite::fromJSON(displaycfg_path)
  
  # remove jquery from the config to prevent conflicts
  idx = which(grepl("jquery", displaycfg$panelInterface$deps$assets$url))
  if(length(idx) > 0) {
    displaycfg$panelInterface$deps$assets <- displaycfg$panelInterface$deps$assets[-idx,] 
    jsonlite::write_json(displaycfg, displaycfg_path, auto_unbox = T)
  }
  
  # reference the json file through trelliscope-app
  tagList(
    tags$div(id="trelli-display-wrapper", class="trelliscope-not-spa", style="width:800px; height:500px;"),
    tags$script(src="https://unpkg.com/trelliscopejs-lib/dist/trelliscope.min.js"),
    tags$script(sprintf("(function() {
        trelliscopeApp('trelli-display-wrapper',
          '%s/appfiles/config.json');
      })();", id))
  )
  # tags$iframe(src= file.path(id, "index.html"), width = "100%", height = "500px")
})
