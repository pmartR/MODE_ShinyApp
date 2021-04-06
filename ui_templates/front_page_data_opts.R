make_front_page_data_opts <- function(){
  tagList(
    uiOutput('choose_edata_colname'),
    radioGroupButtons("edata_how_make_groups", 
                      "Make new groups manually or by splitting sample names?",
                      choices = c("Manually" = "manual", "Split sample names" = "sampnames")),
    uiOutput('edata_how_make_groups_UI'),
    uiOutput('edata_extract_groups'),
    uiOutput('edata_groups_preview')
  )
}

make_front_page_upload_opts <- function(){
  tagList(
    radioGroupButtons(
      "local_or_minio",
      "Where is your data coming from?",
      choices = c("MAP project directory" = "minio", 
                 "From my computer" = "local")
    ),
    uiOutput("conditional_data_upload")
  )
}

front_page_left_collapse <- function(){
  bsCollapse(
    id = "trelli_collapse", multiple = TRUE, 
    open = c("main_trelli_upload", "main_trelli_plot_opts"),
    bsCollapsePanel("Specify grouping variable and plot type.",
                    value = "main_trelli_upload",
                    make_front_page_upload_opts()
    ),
    bsCollapsePanel(title = "Plot Options", value='main_trelli_plot_opts',
                    make_front_page_data_opts(),
                    
                    tags$hr(),
                    div(class = "centered-buttons",
                      actionButton("make_trelliscope", "Create Trelliscope Display")
                    )
    )
  )
}
