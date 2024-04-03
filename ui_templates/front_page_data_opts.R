make_front_page_upload_opts <- function(){
  
  if (Minio_Test | MAP | Compose_Test) {
    tagList(
      uiOutput("UploadedFileType"),
      uiOutput("SelectOmicsUI")
    )
  } else {
    
   div(
     
     # Input data selector
     pickerInput("input_datatype", "Is the input data MS/NMR or RNA-Seq?", c("MS/NMR", "RNA-Seq"), "MS/NMR"),
    
     fluidRow(
       
       # E Data
       column(10, fileInput("EdataFile", "Expression Data - Required", accept = ".csv"), div(style = "margin-top: -15px")),
       column(2, actionButton("EdataFileHelp", "", icon("question"), style = "margin-top:25px")),
              
       # F Data       
       column(10, fileInput("FdataFile", "Sample Information - Optional", accept = ".csv"), div(style = "margin-top: -15px")),
       column(2, actionButton("FdataFileHelp", "", icon("question"), style = "margin-top:25px")),
     
       # E Meta
       column(10, fileInput("EmetaFile", "Biomolecule Information - Optional", accept = ".csv"), div(style = "margin-top: -15px")),
       column(2, actionButton("EmetaFileHelp", "", icon("question"), style = "margin-top:25px")),
    
       # Statistics File
       column(10, fileInput("StatisticsFile", "Differential Statistics - Optional", accept = ".csv"), div(style = "margin-top: -15px")),
       column(2, actionButton("StatisticsFileHelp", "", icon("question"), style = "margin-top:25px"))
      
      ),
     
     # Statistics samples 
     uiOutput("Select_FC_UI"),
     uiOutput("Select_PVAL_A_UI"),
     uiOutput("Select_PVAL_G_UI"),
     
     # Confirm
     list(
       actionButton("UploadConfirm", "Confirm", icon("check")),
       actionButton("ExampleDataPopUp", "Click Here for Examples", icon("handshake"))
     )
     
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
      title = "Pre-Process Data", 
      value = "front_page_data_process_opts",
      make_front_page_data_process_opts()
    ),
    bsCollapsePanel(
      title = "Normalize Data",
      value = "front_page_normalize_data",
      make_front_page_normalize_data()
    ),
    bsCollapsePanel(
      title = "Design Plot",
      value = "make_plot_opts",
      make_plot_variable_options()
    ),
    bsCollapsePanel(
      title = "Filter Plots (Optional)",
      value = "Data_filtering",
      make_data_filtering_options()
    ),
    bsCollapsePanel(
      title = "Create Trelliscope",
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
