# Define UI for application that draws a histogram
ui <- fluidPage(
  htmltools::htmlDependency("jquery", "3.5.1",
                            src = c(href = "https://code.jquery.com/"),
                            script = "jquery-3.5.1.min.js"),
  useShinyjs(),
  useShinyalert(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "MODEstyle.css")
  ),
  div(class = "title-panel",
      HTML('<p><img src="mode_icon.png" width=150></p><p><span style="font-size: 22px;">  Trelliscope visualization of omics data and statistics</span></p>')
  ),
  div(
    id = "loading-gray-overlay",
    class = "loading-mask",
    div(class = "fadein-out busy relative-centered", style = "font-size:xx-large", "Loading app resources...")
  ), 
  fluidRow(
    id = "trelliscope_fluidRow",
    column(
      width = 4,
      front_page_left_collapse(),
      uiOutput("enter_debugger") # hidden if in debug mode
    ),
    column(
      width = 8,
      front_page_display_panel(),
    )
  )
)