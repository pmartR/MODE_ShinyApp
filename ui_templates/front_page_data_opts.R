make_front_page_upload_opts <- function(){
  
  if (Minio_Test | MAP | Compose_Test) {
    tagList(
      uiOutput("UploadedFileType"),
      uiOutput("SelectOmicsUI")
    )
  } else {
   div(
      HTML('<p><span style="font-size: 16px;"><strong>Required Inputs.</strong></span></p><p>Click the question mark for more details on filetypes.</p>'),
      splitLayout(
        fileInput("EdataFile", "Expression Data", accept = ".csv"), 
        actionButton("EdataFileHelp", "", icon("question"), style = "margin-top:25px")
      ),
      HTML('<p><span style="font-size: 16px;"><strong>Optional Inputs.</strong></span></p>'),
      splitLayout(
        fileInput("FdataFile", "Sample Information", accept = ".csv"),
        actionButton("FdataFileHelp", "", icon("question"), style = "margin-top:25px")
      ),
      splitLayout(
        fileInput("EmetaFile", "Biomolecule Information", accept = ".csv"),
        actionButton("EmetaFileHelp", "", icon("question"), style = "margin-top:25px")
      ),
      splitLayout(
        fileInput("StatisticsFile", "Differential Statistics", accept = ".csv"),
        actionButton("StatisticsFileHelp", "", icon("question"), style = "margin-top:25px")
      ),
      actionButton("UploadConfirm", "Confirm", icon("check")),
      hr(),
      downloadButton("ExampleFiles", "Download Normalized Example Files")
    )
  }
}

make_front_page_data_process_opts <- function(){
  div(id = "front_page_plot_opts",
    uiOutput('choose_edata_colname'),
    uiOutput("WantGroupsUI"),
    uiOutput("GroupDesignationUI"),
    uiOutput("EnterNAValuesUI"),
    uiOutput("SelectTransformationUI"),
    uiOutput("MoveToNormalizationUI")
  )
}

make_front_page_normalize_data <- function() {
  tagList(
    uiOutput("IsNormalizedUI"),
    uiOutput("SelectNormalizationUI"),
    uiOutput("CheckNormalizationUI")
  )
}

make_plot_variable_options <- function() {
  tagList(
    uiOutput("TrelliPanelVariableUI"),
    uiOutput("TrelliPlottingVariableUI"),
    uiOutput("PlotOptionsPanelUI"),
    uiOutput("PlotFoldchangeOptsUI"),
    uiOutput("PlotOptionsConfirmUI")
  )
}

make_trelliscope_plotting_options <- function() {
  tagList(
    uiOutput("ChooseCognosticsUI"),
    uiOutput("SubsetTrelliscopeUI")
  )
}

make_data_filtering_options <- function() {
  tagList(
    uiOutput("FilterByPValueUI"),
    uiOutput("FilterByPValueTextUI")
  )
}

front_page_left_collapse <- function(){
  bsCollapse(
    id = "trelli_collapse", multiple = TRUE, 
    open = c("front_page_upload_opts", "front_page_data_process_opts"),
    bsCollapsePanel(
      div(
        subsection_header(
          "Upload Files",
          id = "main_trelli_upload_icon",
          style = "color:red;display:inline-block",
          icon = icon("exclamation-sign", lib = "glyphicon"),
          hidden = T
        )
      ),
      value = "front_page_upload_opts",
      make_front_page_upload_opts()
    ),
    bsCollapsePanel(
      title = "Format Data", 
      value = "front_page_data_process_opts",
      make_front_page_data_process_opts()
    ),
    bsCollapsePanel(
      title = "Normalize Data",
      value = "front_page_normalize_data",
      make_front_page_normalize_data()
    ),
    bsCollapsePanel(
      title = "Make Plot",
      value = "make_plot_opts",
      make_plot_variable_options()
    ),
    bsCollapsePanel(
      title = "Data Filtering Options",
      value = "Data_filtering",
      make_data_filtering_options()
    ),
    bsCollapsePanel(
      title = "Make Trelliscope",
      value = "make_trelli_opts",
      make_trelliscope_plotting_options(),
      hr(),
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
      ),
      hr(),
      textInput("trelliscope_name", "Name Trelliscope", value = "NewTrelliscope"),
      actionButton("refresh", "Refresh Display", icon = icon("pencil-alt")),
      downloadButton("download", "Download Display"),
      uiOutput("job_status_ui"),
      hr(),
      uiOutput("BuildStats")
    )
  )
}
