#' @title plotly_xts
#' @description Plots time series in xts
#' @param x xts
#' @details xx
#' @export
plotly_xts <- function(x, ...){
  x %<>%
    as.data.frame() %>%
    tibble::rownames_to_column("date") %>%
    mutate(date = as.Date(date)) %>%
    gather(key, value, -date)

  p <- x %>%
    plot_ly(x = ~date,
            y = ~value,
            color = ~key, ...) %>%
    add_lines()

  p
}
