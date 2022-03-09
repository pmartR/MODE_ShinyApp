#'@details Allow users to add groups 
observeEvent(input$GroupAdd, {
  
  edata_groups$Group <- append(edata_groups$Group, input$GroupName)
  
})