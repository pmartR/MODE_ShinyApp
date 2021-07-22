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
      tags$h1("MODE"),
      tags$p("omics trelliscope visualization")
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