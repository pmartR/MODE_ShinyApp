simple_boxplots <- function(df, group_cname, value_cname){
  df %>%
    plot_ly(x = purrr::pluck(df, group_cname), 
            y = purrr::pluck(df, value_cname), 
            type = 'box', 
            color = purrr::pluck(df, group_cname))
}