make_front_page_plot_opts <- function(){
  div(id = "front_page_plot_opts",
    uiOutput('choose_edata_colname'),
    radioGroupButtons("WantGroups", "Would you like to assign groups?", c("Yes", "No"), "No"),
    uiOutput("GroupDesignationUI"),
    radioGroupButtons("IsTransformed", "Is your data log transformed?", c("Yes", "No"), "No"),
    uiOutput("SelectTransformationUI"),
    radioGroupButtons("IsNormalized", "Is your data normalized?", c("Yes", "No"), "No"),
    uiOutput("SelectNormalizationUI"),
    uiOutput("SelectNormalizationButton"),
    uiOutput("TrelliPanelVariable"),
    uiOutput("TrelliPlottingVariable")
  )
}

make_front_page_upload_opts <- function(){
  
  if (MAP) {
    # TODO: Change to a text bar and freeze 
  } else {
    tagList(
      fileInput("UploadFile", "Select an edata project or midpoint file"),
      uiOutput("UploadedFileType")
    )
  }
  
}

front_page_left_collapse <- function(){
  bsCollapse(
    id = "trelli_collapse", multiple = TRUE, 
    open = c("main_trelli_upload", "main_trelli_plot_opts"),
    bsCollapsePanel(
      div(
        subsection_header(
          "Upload File",
          id = "main_trelli_upload_icon",
          style = "color:red;display:inline-block",
          icon = icon("exclamation-sign", lib = "glyphicon"),
          hidden = T
        )
      ),
      value = "main_trelli_upload",
      make_front_page_upload_opts()
    ),
    bsCollapsePanel(
      title = "Format Data", 
      value='main_trelli_plot_opts',
      make_front_page_plot_opts()
    ),
    bsCollapsePanel(
      title = "Make Trelliscope",
      value = 'make_trelli_plot_opts',
      tags$hr(),
      div(
        class = "flex-baseline",
        bsButton(
          "make_trelliscope", 
          "Create Trelliscope Display", 
          style = "primary"
        ),
        div(
          id = "make_trelliscope_disable_info",
          style = "color:red;margin-left:3px;",
          icon("exclamation-sign", lib = "glyphicon")  
        )
      )
    )
  )
}
