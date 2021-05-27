## ping redis and send the selected task
observeEvent(input$make_trelliscope, {
  # TODO:  Smarter check that groups are valid
  req(edata_groups(), input$edata_idcname_picker)
  
  # Import celery package from the virtual environment
  clry <- reticulate::import('celery')
  celery_app = clry$Celery('app', broker=redis_url, backend=redis_url)
  
  if(input$local_or_minio=="local"){
    req(input$raw_data_upload)
    object_name = uuid::UUIDgenerate()
    file_tags$local_files[[input$raw_data_upload$name]] = object_name
    mapDataAccess::put_file(miniocon, 
                            object_name = object_name, 
                            file_path = input$raw_data_upload$datapath)
  }
  else if(input$local_or_minio == "minio"){
    object_name = queryTags$query1
    id = paste0(get_data(miniocon, queryTags$query1)$Project$Name, "_trelliscope")
  }
  
  celery_app$send_task("edata_simple_boxplots", 
                       kwargs=list(
                         object_name=object_name,
                         id=id,
                         panel_column=input$edata_idcname_picker,
                         groups=edata_groups()
                        )
  )
})

# grab finished trelliscope display from minio
observeEvent(input$pull_trelliscope, {
  # TODO: move this to mapDataAccess
  # TODO: function takes minio url as argument
  # id = display_objects$saved_displays[[1]]$id
  # cmdstring = paste(
  # c(
  #   "from minio import Minio",
  #   "client = Minio('%s:9000', access_key='minioadmin', secret_key='minioadmin', secure=False)",
  #   "all_objs = list(client.list_objects(bucket_name = 'map', prefix = '%s/', recursive = True))",
  #   "out_list = [obj.object_name for obj in all_objs]"
  # ),
  # collapse="\n"
  # )
  
  trelli_objects = list(
    reticulate::iterate(
      miniocon$client$list_objects(miniocon$bucket, prefix="trelli-display-", recursive=TRUE),
      function(x) x$object_name, simplify=TRUE)
  )[[1]]
  
  url_prefix = if(Sys.getenv("SHINY_LOCAL_OR_NETWORK")=="local") "0.0.0.0" else "minio-map"
  #cmdstring = sprintf(cmdstring, url_prefix, input$minio_trelli_picker)
  #reticulate::py_run_string(cmdstring)
  
  withProgress(message = "Pulling files from Minio...",
    for(fpath in trelli_objects){
      incProgress(1/length(trelli_objects))
      miniocon$client$fget_object("map", object_name = fpath, file_path = file.path("www", fpath))
    }
  )
  
  
}, priority=10)
