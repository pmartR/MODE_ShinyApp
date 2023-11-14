# Trigger pop ups for data type descriptors

# Expression Data 
observeEvent(input$EdataFileHelp, {
  
  showModal(modalDialog(
    HTML("<center><img src='datatype_descriptors/Edata_Description.png' width=90%></center>"),
    title = HTML("<center>Expression Data File Help</center>"), size = "l", easyClose = TRUE
  ))
  
})

# Sample Information
observeEvent(input$FdataFileHelp, {
  
  showModal(modalDialog(
    HTML("<center><img src='datatype_descriptors/Fdata_Description.png' width=90%></center>"),
    title = HTML("<center>Sample Information File Help</center>"), size = "l", easyClose = TRUE
  ))
  
})

# Biomolecule Information
observeEvent(input$EmetaFileHelp, {
  
  showModal(modalDialog(
    HTML("<center><img src='datatype_descriptors/Emeta_Description.png' width=90%></center>"),
    title = HTML("<center>Biomolecule Information File Help</center>"), size = "l", easyClose = TRUE
  ))
  
})

# Differential Statistics
observeEvent(input$StatisticsFileHelp, {
  
  showModal(modalDialog(
    HTML("<center><img src='datatype_descriptors/Statistics_Description.png' width=90%></center>"),
    title = HTML("<center>Differential Statistics File Help</center>"), size = "l", easyClose = TRUE
  ))
  
})

# Trigger download
output$ExampleFiles <- downloadHandler(
  filename = function() {"MODE_Example_Data.zip"},
  content = function(file) {
    zip(file, "./www/example_files/")
  }
)

# Close upload file 
observeEvent(input$UploadConfirm, {
  
  req(uploaded_data())
  updateCollapse(session, id = "trelli_collapse", close = "front_page_upload_opts", open = "front_page_data_process_opts")

})



