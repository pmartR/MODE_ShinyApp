output$conditional_data_upload <- renderUI({
  req(input$local_or_minio)
  
  if (input$local_or_minio == "local") {
    fileInput("raw_data_upload", "Upload Compatible File",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv"
              )
    )
  }
  else if (input$local_or_minio == "minio") {
    # if ((length(file_tags[["file_dictionary"]]) > 0) & file_tags[["from_map"]]){
    #   choices = file_tags[["file_dictionary"]]
    # }
    # else {
    #   # list files in minio bucket
    #   fpaths = list(
    #     reticulate::iterate(
    #       miniocon$client$list_objects(miniocon$bucket, recursive = TRUE), 
    #       function(x) x$object_name, simplify=TRUE)
    #     )[[1]]
    #   
    #   for(fpath in new_fpaths){
    #     id = uuid::UUIDgenerate()
    #     file_tags[["file_dictionary"]][[fpath]] = id
    #   }
    #   choices = file_tags[["file_dictionary"]] 
    # }
    # 
    # choices = file_tags[["file_dictionary"]]
    
    tagList(
      pickerInput("minio_choose_file", 
                  "Choose file from cloud storage",
                  choices = file_tags[["file_dictionary"]]),
      actionButton("minio_upload_pull", "Use this file")
    )
  }
})

# TODO:  Option to continuously upload files into MODE
# observe({
#   fpaths = list(
#     reticulate::iterate(
#       miniocon$client$list_objects(miniocon$bucket, recursive = TRUE),
#       function(x) x$object_name, simplify=TRUE)
#   )[[1]]
# 
#   new_fpaths = sym_diff(fpaths, names(file_tags[["file_dictionary"]]))
# 
#   if(length(new_fpaths) > 0){
#     for(fpath in new_fpaths){
#       id = uuid::UUIDgenerate()
#       file_tags[["file_dictionary"]][[fpath]] = id
#     }
#     choices = file_tags[["file_dictionary"]]
#   }
#   invalidateLater(3000)
# })
