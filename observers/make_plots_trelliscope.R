#' @details Confirm plot choice to lock the "Panel By" and "Data" options
observeEvent(input$PlotOptionsConfirm, {
  
  # Change to the modify plot tab
  updateTabsetPanel(session, "trelliscope_mainpanel", "modify_plot")
  
  # Indicate that plots should be locked and save variable selections 
  disable("TrelliPanelDiv")
  disable("TrelliPlottingDiv")
  
  # Store row 
  row <- input$PlotOptionsTable_row_last_clicked
  if (is.null(row)) {row <- 1}
  final_data$TrelliRow <- row
  
  # Open trelliscope tab
  updateCollapse(session, "trelli_collapse", open = c("make_plot_opts", "make_trelli_opts"),
                 close = c("front_page_upload_opts", "front_page_data_process_opts", "front_page_normalize_data"))
  
  
})

#' @details Choose another plot choice 
observeEvent(input$PlotOptionsUnconfirm, {
  
  # Change to the modify plot tab
  updateTabsetPanel(session, "trelliscope_mainpanel", "select_plot")
  
  # Indicate that plots should be locked and save variable selections 
  enable("TrelliPanelDiv")
  enable("TrelliPlottingDiv")
  
  # Store row 
  final_data$TrelliRow <- NULL
  
  # Close trelliscope tab
  updateCollapse(session, "trelli_collapse", open = c("make_plot_opts"),
                 close = c("front_page_upload_opts", "front_page_data_process_opts", "front_page_normalize_data", "make_trelli_opts"))
  
})

#' @details Gather all plot inputs
observeEvent(input$PlotRedraw, {
  
  req(final_data$TrelliRow)
  
  # Determine if this plot is a heatmap
  isHeatmap <- grepl("heatmap", unlist(final_data$PlotOptions[final_data$TrelliRow, "Plot"])) & input$SelectColor != "Original Colors"
  if (isHeatmap) {
    suppressWarnings({theColors <- RColorBrewer::brewer.pal(n = 12, name = input$SelectColor)})
  }
  
  # Collect all inputs
  Collected <- data.frame(
    Inputs = c(input$XLab, input$YLab, input$XAxisSize, input$YAxisSize, input$XAxisTickAngle,
               input$YAxisTickAngle, input$XAxisTickSize, input$YAxisTickSize, input$PlotTitle,
               input$PlotTitleSize, input$AxisFlip, input$LegendTitle, 
               input$RemoveLegend, input$SelectColor),
    Code = c(paste0("xlab('", input$XLab, "')"), 
             paste0("ylab('", input$YLab, "')"), 
             paste0("theme(axis.title.x = ggplot2::element_text(size=", abs(round(input$XAxisSize)), "))"),
             paste0("theme(axis.title.y = ggplot2::element_text(size=", abs(round(input$YAxisSize)), "))"),
             paste0("theme(axis.text.x = ggplot2::element_text(angle=", abs(round(input$XAxisTickAngle)), "))"),
             paste0("theme(axis.text.y = ggplot2::element_text(angle=", abs(round(input$YAxisTickAngle)), "))"),
             paste0("theme(axis.text.x = ggplot2::element_text(size=", abs(round(input$XAxisTickSize)), "))"),
             paste0("theme(axis.text.y = ggplot2::element_text(size=", abs(round(input$YAxisTickSize)), "))"),
             paste0("ggtitle('", input$PlotTitle, "')"),
             paste0("theme(plot.title = ggplot2::element_text(size=", input$PlotTitleSize, "))"),
             paste0("coord_flip()"),
             paste0("guides(fill=ggplot2::guide_legend(title='", input$LegendTitle, "'))"),
             paste0("theme(legend.position='none')"),
             ifelse(isHeatmap, 
              paste0("scale_fill_gradient2(low='", theColors[1], "', mid='", theColors[round(length(theColors) / 2)], "', high='", theColors[length(theColors)],"', na.value='white')"),
              paste0("scale_fill_brewer(palette='", input$SelectColor, "', na.value='white')")
             )
    )
             
  )
  
  # Determine if NULL 
  IsNULL <- lapply(Collected$Inputs, function(x) {
    if (is.na(x) || x == "" || x == "FALSE" || x == "Original Colors") {return("Yes")} else {return("No")}
  }) %>% unlist()
  
  if (all(IsNULL == "Yes")) {final_data$PlotInputs <- NULL} else {final_data$PlotInputs <- Collected[IsNULL == "No",]} 
  
})