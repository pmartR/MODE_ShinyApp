front_page_display_panel <- function(){
  tabsetPanel(
    id = "trelliscope_mainpanel",
    tabPanel(
      title = "Table Preview",
      uiOutput("groups_preview"),
      dataTableOutput("one_dataset_preview"),
    ),
    tabPanel(
      title = "Plot Preview",
      plotlyOutput("one_plot_preview"),
      bsButton("refresh_panel_preview", "Update plot", style = "primary")
    ),
    tabPanel(
      title = "Trelliscope Display",
        div(class = "horizontal-aligned",
          uiOutput("trelli_download_picker"),
          uiOutput("pull_trelliscope_ui")
          # actionButton("reload_trelliscope_iframe", "Reload Display")
        ),
        uiOutput("trelliscope_from_iframe")
    )
  )
}