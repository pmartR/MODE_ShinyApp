#output$conditional_data_upload <- renderUI({
#  req(input$local_or_minio)
#  input$minio_upload_refresh
#  
#  if (input$local_or_minio == "local") {
#    fileInput("raw_data_upload", "Upload Compatible File",
#              accept = c(
#                "text/csv",
#                "text/comma-separated-values,text/plain",
#                ".csv"
#              )
#    )
#  }
#  else if (input$local_or_minio == "minio") {
#    #' Currently polling minio for objects with e_data.  Add more as they become
#    #' relevant to making plots.
#    object_ids <- suppressMessages(
#      {
#        mapDataAccess::get_all_data_ids(miniocon, filter_tags = "e_data_filename")
#      }
#    )
#  
#    tagList(
#      pickerInput(
#        "minio_choose_file", 
#        "Choose file from cloud storage",
#        choices = c("Select one" = NOSELECT_, object_ids)
#      ),
#      actionButton("minio_upload_refresh", "Refresh file list")
#    )
#  }
#})


