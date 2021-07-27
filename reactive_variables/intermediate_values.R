# stores the values of grouping vector to be passed to the job
edata_groups <- reactive({
  
  req(input$edata_how_make_groups)
  
  if(input$edata_how_make_groups == "manual") {
    req(input$manual_group_values)
    return(strsplit(input$manual_group_values, ";")[[1]])
  }
  else if (input$edata_how_make_groups == "sampnames"){
    req(input$edata_how_extract_groups)
    if (input$edata_how_extract_groups == "split") {
      req(input$group_creation_delimiter)
      column_values <- as.character(
        colnames(uploaded_edata() %>% select(-one_of(input$edata_idcname_picker)))
      )
      split_result <- tryCatch({
        str_split(column_values, input$group_creation_delimiter)
      }, error = function(e){
        NULL
      })
      keep_which <- sapply(split_result, function(x) {
        if (!isTruthy(x)) {
          return(NA)
        }
        paste(x[as.numeric(input$groups_split_keep_which)],
              collapse = input$group_creation_delimiter
        )
      })
      return(keep_which)
    }
    # ... extracts the text corresponding to a regex input
    else if (input$edata_how_extract_groups == "regex") {
      req(input$groups_regex)
      column_values <- as.character(
        colnames(uploaded_edata() %>% select(-one_of(input$edata_idcname_picker)))
      )
      extracted_values <- tryCatch({
        str_extract(column_values, input$groups_regex)
      }, error= function(e){
        NULL
      })
      return(extracted_values)
    }
  }
  return(NULL)
})
