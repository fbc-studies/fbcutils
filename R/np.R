#' @export
np <- function(df, x, ...) {
  count_var <- enquo(x)
  group_var <- quos(...)

  df %>%
    group_by(!!!group_var) %>%
    count(!!count_var) %>%
    mutate(p = n / sum(n), n_ryhma = sum(n)) %>%
    ungroup
}
