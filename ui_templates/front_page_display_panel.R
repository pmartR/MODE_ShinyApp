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
        DT::DTOutput("emeta_preview"), br(), br(),
        HTML("<strong>Statistics</strong>"), hr(),
        DT::DTOutput("stat_preview"), br(), br(),
      
      if (!(Minio_Test | MAP | Compose_Test)) {
        list(
          HTML("<strong>Comparison Table</strong>"), hr(),
          DT::DTOutput("comparison_table")
        )
      }
      
    ),
    tabPanel(
      title = "Select Plot",
      value = "select_plot",
      jqui_resizable(plotOutput("PlotOptionsPlot") %>% withSpinner(type = 5)),
      DT::DTOutput("PlotOptionsTable")
    ),
    tabPanel(
      title = "Modify Plot",
      value = "modify_plot",
      jqui_resizable(plotOutput("OnePlotPreview") %>% withSpinner(type = 5)),
      uiOutput("RenderPlotModsUI")
    ),
    tabPanel(
      title = "Trelliscope Display",
      value = "trelliscope_display",
        div(class = "horizontal-aligned",
          #uiOutput("trelli_download_picker"),
          #uiOutput("pull_trelliscope_ui")
          # actionButton("reload_trelliscope_iframe", "Reload Display")
          uiOutput("trelliscope"),
          br(),
          HTML("If the trelliscope display does not look as expected, click 'Refresh Display' in the 'Create Trelliscope' menu.")
        ),
        #uiOutput("trelliscope_from_iframe")
    )
  )
}