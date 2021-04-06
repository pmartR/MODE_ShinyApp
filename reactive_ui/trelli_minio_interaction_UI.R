# conditional action button to retrieve trelliscope displays from minio
output$pull_trelliscope_ui <- renderUI({
  # TODO:  disable this button if the currently selected trelli display already exists?
  # or maybe a softer option to warn that a display with this name already exists....yea 
  # thats better...
  actionButton("pull_trelliscope", "Retrieve finished trelliscope display")
})

# # a list of displays saved in minio, they should be labeled "trelli-display"
output$trelli_download_picker <- renderUI({
  pickerInput("minio_trelli_picker",
              "Choose a saved display to visualize",
              choices = display_objects$saved_displays)
})

# OBSERVER for the above picker, modifies the choices periodically, if there are
# new displays in minio
observe({
  trelli_paths = list(
    reticulate::iterate(
      miniocon$client$list_objects(miniocon$bucket, prefix="trelli-display-"),
      function(x) x$object_name, simplify=TRUE)
  )[[1]]
  
  # trelli_paths = trelli_paths[[sapply(trelli_paths, function(x) grepl("^trelli-display-", x))]]
  new_fpaths = sym_diff(trelli_paths, display_objects$saved_displays)

  if(length(new_fpaths) > 0){
    display_objects$saved_displays = trelli_paths
  }

  invalidateLater(3000)
})
