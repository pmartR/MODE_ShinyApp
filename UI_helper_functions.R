#'@details Creates a sequence of inputs as html, usually to be added the column
#'of a table.
buttonInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

subsection_header <- function(titletext, id, style, icon, hidden = T, tooltip_text = NULL) {
  if (!is.null(tooltip_text)) {
    icon <- tipify(icon, tooltip_text)
  }
  if (hidden == T) {
    div(titletext, hidden(div(id = id, style = style, icon)))
  }
  else {
    div(titletext, div(id = id, style = style, icon))
  }
}

fluidSplitLayout <- function(col1_content, col2_content, width1 = 6, width2 = 6) {
  fluidRow(
    column(
      width1,
      col1_content
    ),
    column(
      width2,
      col2_content
    )
  )
}

# dont know why this doesn't exist...
toggleTooltip <- function(session, id, condition, tooltip_text, position="bottom") {
  if (condition) {
    # addTooltip(session, id, tooltip_text) # tooltip('destroy') is awful
    shinyjs::runjs(
      sprintf("$('#%s').attr('data-original-title', '%s')", id, tooltip_text)
    )
    shinyjs::runjs(
      sprintf("$('#%s').tooltip({placement:'%s'})", id, position)
    )
  }
  else {
    removeTooltip(session, id)
  }
}

# show an element and add a tooltip if condition is met
show_add_tooltip <- function(session, id, condition, tooltip_text) {
  toggleElement(id = id, condition = condition)
  toggleTooltip(session, id, condition, tooltip_text)
}

# disable/enable sub-elements of a div and display a tooltip based on condition.
### NOTE:  THIS CANNOT ADD A TOOLTIP TO THE SAME ELEMENT IT DISABLES
### DISABLED ELEMENTS DO NOT LIKE HAVING TOOLTIPS ADDED TO THEM FOR SOME REASON.
togglestate_add_tooltip <- function(session, id, condition, tooltip_text) {
  toggleState(id = id, condition = condition)
  toggleTooltip(session, id, !condition, tooltip_text)
}

# a version of req that returns a value instead of 'cancelling execution'
reqNull <- function(..., returnvalue = NULL) {
  args <- list(...)
  for (el in args) {
    if (!isTruthy(el)) {
      shiny:::reactiveStop(class = "validation")
      return(returnvalue)
    }
  }
  invisible()
}

style_UI <- function(pagename) {
  tagList(
    splitLayout(textInput(paste0(pagename, "_xlab"), "X-axis label"),
      textInput(paste0(pagename, "_ylab"), "Y-axis label"),
      numericInput(paste0(pagename, "_x_fontsize"), "X-axis font size", value = 11),
      numericInput(paste0(pagename, "_y_fontsize"), "Y-axis font size", value = 11),
      cellWidths = rep("25%", 4)
    ),
    splitLayout(numericInput(paste0(pagename, "_xangle"), "X-axis tick angle", value = NULL),
      numericInput(paste0(pagename, "_yangle"), "Y-axis tick angle", value = NULL),
      numericInput(paste0(pagename, "_x_ticksize"), "X-axis tick size", value = NULL),
      numericInput(paste0(pagename, "_y_ticksize"), "Y-axis tick size", value = NULL),
      cellWidths = rep("25%", 4)
    ),
    splitLayout(textInput(paste0(pagename, "_title"), "Title"),
      numericInput(paste0(pagename, "_title_fontsize"), "Title font size", value = 14),
      cellWidths = c("25%", "25%")
    )
  )
}

####
# Enable/disable tabs css #####
css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

###


# Create a popup that requires input to close
bsModalNoClose <- function(...) {
  b <- bsModal(...)
  b[[2]]$`data-backdrop` <- "static"
  b[[2]]$`data-keyboard` <- "false"
  return(b)
}


# return the input value for a particular set of inputs that are replicated across all datatypes
get_inputs <- function(session, names, prefix = "", postfix = "") {
  sapply(names, function(name) {
    get("input", envir = session)[[sprintf("%s%s%s", prefix, name, postfix)]]
  })
}

# Enable/disable tabs java #####  .classlist.contain
jscode <- "

shinyjs.isTabdisabled = function(el) {
    var tab = $('.nav li a[data-value=' + el + ']');
    var outstring = 'jscatch_disabled_' + el;
    Shiny.setInputValue(outstring, tab.hasClass('disabled'));
}

shinyjs.isIconhidden = function(el) {
  var icon = $('#' + el);
  var outstring = 'jscatch_icon_' + el;
  Shiny.setInputValue(outstring, icon.css('display') == 'none');
}

shinyjs.disableTab = function(params) {
  var defaultParams = {
    name: null,
    class:'disabled'
  };
  params = shinyjs.getParams(params, defaultParams)
  
  var tab = $('.nav li a[data-value=' + params.name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass(params.class);
}

shinyjs.enableTab = function(params) {
  var defaultParams = {
    name: null,
    class:'disabled'
  };
  params = shinyjs.getParams(params, defaultParams)
  
  var tab = $('.nav li a[data-value=' + params.name + ']');
  tab.unbind('click.tab');
  tab.removeClass(params.class);
}

shinyjs.disableBtn = function(selector, onoff) {
  $(selector).prop('disabled', onoff)
}

shinyjs.toggleTabInputs = function(params){
  var defaultParams = {
    tab_selector: null,
    sub_elements:'*',
    exclude_elements:null,
    disabled:true
  };
  params = shinyjs.getParams(params, defaultParams)

  $(params.tab_selector).find(params.sub_elements).not(params.exclude_elements).prop('disabled', params.disabled)
}
"
