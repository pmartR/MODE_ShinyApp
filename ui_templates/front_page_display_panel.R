front_page_display_panel <- function(){
  tabsetPanel(
    id = "trelliscope_mainpanel",
    tabPanel(
      title = "Preview Tables", 
      value = "preview_tables",
        br(),
        HTML("<strong>Expression Data</strong>"), hr(),
        DT::DTOutput("edata_preview"), br(), br(),
        HTML("<strong>Sample Data</strong>"), hr(),
        DT::DTOutput("fdata_preview"), br(), br(),
        HTML("<strong>Expression Metadata</strong>"), hr(),
        DT::DTOutput("emeta_preview"), br(), br()
    ),
    tabPanel(
      title = "Select Plot",
      value = "select_plot",
      jqui_resizable(plotOutput("PlotOptionsPlot")),
      DT::DTOutput("PlotOptionsTable")
    ),
    tabPanel(
      title = "Modify Plot",
      value = "modify_plot",
      jqui_resizable(uiOutput("OnePlotPreviewUI")),
      uiOutput("RenderPlotModsUI")
    ),
    tabPanel(
      title = "Trelliscope Display",
      value = "trelliscope_display",
        div(class = "horizontal-aligned",
          #uiOutput("trelli_download_picker"),
          #uiOutput("pull_trelliscope_ui")
          # actionButton("reload_trelliscope_iframe", "Reload Display")
          uiOutput("trelliscope")
        ),
        #uiOutput("trelliscope_from_iframe")
    )
  )
}