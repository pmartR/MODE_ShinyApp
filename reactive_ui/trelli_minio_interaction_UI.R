#'@details conditional action button to retrieve trelliscope displays from minio
output$pull_trelliscope_ui <- renderUI({
  # TODO:  disable this button if the currently selected trelli display already exists?
  # or maybe a softer option to warn that a display with this name already exists....yea 
  # thats better...
  actionButton("pull_trelliscope", "Retrieve finished trelliscope display")
})

#'@details A list of displays saved in minio, they are detected by an observer
#'which scans for a minio object tag:  "ObjectType" = "trelliscope-display" 
output$trelli_download_picker <- renderUI({
  pickerInput("minio_trelli_picker",
              "Choose a saved display to visualize",
              choices = display_objects$saved_displays)
})

#'@details OBSERVER which controls the choices for 'minio_trelli_picker', 
#' modifies the choices periodically, picking up all objects in minio with the
#' "ObjectType" = "trelliscope-display" tag
#'
#if (MAP) { 
#
#  observe({
#    # this will get the full paths of all objects in minio
#    object_ids <- suppressMessages({mapDataAccess::get_all_data_ids(miniocon)})
#    
#    # iterate over objects and get list of trelliscope object prefixes
#    trelli_paths <- purrr::map(object_ids, function(oid) {
#      suppressMessages({
#        if(isTRUE(mapDataAccess::get_tags(miniocon, oid)[['ObjectType']] == "trelliscope-display")) {
#          oid
#        } else NULL
#      })
#    }) %>% unlist()
#    
#    new_fpaths = sym_diff(trelli_paths, display_objects$saved_displays)
#    
#    if(length(new_fpaths) > 0){
#      display_objects$saved_displays = trelli_paths
#    }
#    
#    invalidateLater(3000)
#  })
#  
#}
