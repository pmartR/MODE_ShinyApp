#' Helper function to turn edata into long (tall) format for plotting purposes
#' 
#' @param e_data data.frame as defined in the `pmartR` package.
#' @param panel_column The column which will get a unique row for every column that is pivoted
#' @param names_to see ?tidyr::pivot_longer
#' @param names_from see ?tidyr::pivot_longer
#' @param exlude_columns Columns to ignore when performing the transformation from wide to tall
#' @param ... extra arguments passed to tidyr::pivot_longer
#'
#' @return nested tibble, grouped by the panel column
edata_to_plot_df <- function(e_data, panel_column, names_to, values_to, exclude_columns=NULL, ...){
  out_df <- e_data %>% 
    group_by(!!rlang::sym(panel_column)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    tidyr::pivot_longer(cols = c(-one_of(c(panel_column, exclude_columns))), 
                        names_to = names_to, 
                        values_to = values_to, 
                        ...) %>% 
    dplyr::group_by(!!rlang::sym(panel_column)) %>% 
    tidyr::nest()
  out_df
}
