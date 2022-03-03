#'@details Disable second dropdown until data is uploaded
observe({
  cond = tryCatch({
    !is.null(uploaded_data())
  }, error = function(e){
    FALSE
  })
  
  toggle("front_page_plot_opts", condition = cond)
  show_add_tooltip(
    session, 
    id = "main_trelli_upload_icon",
    condition = !cond,
    tooltip_text = "No object uploaded or error uploading..."
  )
  
})