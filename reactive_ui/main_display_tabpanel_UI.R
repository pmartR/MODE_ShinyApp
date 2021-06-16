output$one_dataset_preview <- renderDataTable({
  nested_edata()$data[[5]]
})

output$one_plot_preview <- renderPlotly({
  one_df = nested_edata()$data[[5]]
  req(length(edata_groups()) == nrow(one_df))
  
  one_df[['__GROUP_COL__']] <- edata_groups()
  
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
  
  # reference the json file through trelliscope-app
  tags$div(id="trelli-display-wrapper", class="trelliscope-not-spa", style="width:800px; height:500px;")
  tags$script(src="https://unpkg.com/trelliscopejs-lib/dist/trelliscope.min.js")
  tags$script(sprintf("(function() {
      trelliscopeApp('trelli-display-wrapper',
        '%s/appfiles/config.json');
    })();", id))
  # tags$iframe(src= file.path(id, "index.html"), width = "100%", height = "500px")
})
