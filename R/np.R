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

#' Create an np table and display it with gt
#' @inheritParams np
#' @param .fmt_p a gt formatter applied to the `p` column
#' @export
#' @importFrom magrittr %>%
#' @examples
#' np_gt(mtcars, cyl)
#' np_gt(mtcars, cyl, am)
#' np_gt(mtcars, cyl, am, vs)
np_gt <- function(df, x, ..., .fmt_p = gt::fmt_percent) {
  if (!requireNamespace("gt", quietly = TRUE)) {
    stop("the gt package must be installed to use np_gt().", call. = FALSE)
  }

  quo_x <- rlang::enquo(x)
  quos_dots <- rlang::quos(...)

  np_tbl <- np(df, !!quo_x, !!!quos_dots)

  np_tbl %>%
    dplyr::select(-n_ryhma) %>%
    dplyr::group_by(!!!quos_dots) %>%
    gt::gt(rowname_col = rlang::as_label(quo_x)) %>%
    .fmt_p("p") %>%
    gt::summary_rows(TRUE, "n", fns = "sum", decimal = 0) %>%
    gt::summary_rows(TRUE, "p", fns = "sum", formatter = .fmt_p) %>%
    gt::tab_options(row.padding = "10px", summary_row.padding = "10px")
}
