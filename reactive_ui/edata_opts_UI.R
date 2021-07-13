# conditional option to make groups manually or by splitting column names
output$edata_how_make_groups_UI <- renderUI({
  req(input$edata_how_make_groups)
  if (input$edata_how_make_groups == "manual") {
    textInput("manual_group_values", "Enter group values separated by ';'")
  }
  else if (input$edata_how_make_groups == "sampnames") {
    radioGroupButtons('edata_how_extract_groups', 'Extract groups by:', 
                      choices = c('Delimiter split' = 'split', 'Regex search' = 'regex'))
  }
})

# making new column values by regex or delimiter
output$edata_extract_groups <- renderUI({
  req(input$edata_how_extract_groups, input$edata_how_make_groups == "sampnames")
  if(input$edata_how_extract_groups == 'split'){
    tagList(
      textInput('group_creation_delimiter', 'Split sample name values by:'),
      uiOutput('split_position_picker')
    )
  }
  else if(input$edata_how_extract_groups == 'regex'){
    textInput('groups_regex', 'Extract values by regex string:')
  }
})

#'@details conditional picker depending on what column and delimiter is used
output$split_position_picker <- renderUI({
  req(input$edata_idcname_picker)
  values_to_split = colnames(uploaded_edata() %>% select(-one_of(input$edata_idcname_picker)))
  
  # 
  split_result <- tryCatch({
    str_split(values_to_split, input$group_creation_delimiter)
  }, error = function(e){
    NULL
  })
  
  validate(need(!is.null(split_result), "Invalid regex..."))
  
  # let them split up to the maximum number of split elements.  
  # If a value is selected that exceeds the length of one of the split results, that result will become NA in the new column
  truncate_length <- max(sapply(split_result, length))
  for (i in which(map_int(split_result, length) < truncate_length)) {
    length(split_result[[i]]) <- truncate_length
  }
  subtext <- map_chr(1:truncate_length, function(n) toString(unique(map_chr(split_result, n))))
  
  if(isTruthy(input$group_creation_delimiter)){
    pickerInput(
      'groups_split_keep_which', 
      'Keep value in which positions', 
      choices = 1:truncate_length, 
      choicesOpt = list(subtext = subtext),
      multiple = TRUE
    )
  } else{
    tipify(
      disabled(
        pickerInput(
          'groups_split_keep_which',
          'Keep value in which positions',
          choices = 1:truncate_length,
          multiple = TRUE
        )
      ), 
      title = "no split value chosen")
  }
})

#'@details picker for the column names of the uploaded data
output$choose_edata_colname <- renderUI({
  req(uploaded_edata())
  
  pickerInput(
    'edata_idcname_picker',
    label = "Which is the ID column?",
    choices = colnames(uploaded_edata())
  )
})

#'@details display for group values
output$edata_groups_preview <- renderUI({
  input$edata_how_make_groups
  validate(need(edata_groups(), "Current group/regex specification is invalid."))
  div(
    tags$b("New column values preview:"),
    paste(edata_groups(), collapse = " | ")
  )
})
