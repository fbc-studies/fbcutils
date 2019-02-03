#' Calculate counts and percentages
#'
#' Wraps [dplyr::count()] to calculate counts and group percentages for a given
#' combination of variables.
#'
#' @param df  a data frame
#' @param x   a variable whose levels are counted
#' @param ... additional variables to count by, defining also the grouping that
#'   is used to calculate percentages
#'
#' @export
np <- function(df, x, ...) {
  count_var <- enquo(x)
  group_var <- quos(...)

  df %>%
    group_by(!!!group_var) %>%
    count(!!count_var) %>%
    mutate(p = n / sum(n), n_ryhma = sum(n)) %>%
    ungroup()
}
