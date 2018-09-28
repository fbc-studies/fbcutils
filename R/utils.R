#' @export
fixed_text_size <- function(size) {
  new_elements <- theme_get() %>%
    keep(inherits, "element_text") %>%
    modify(~ element_text(size = size))
  invoke(theme, new_elements)
}
#' @export
not_distinct <- function(.data, ...) {
  ungroup(filter(group_by(.data, ...), n() > 1L))
}
#' @export
pcomma <- function(x) paste(x * 100, "%")
#' @export
gcommap <- function(x) {
  format(x * 100, big.mark = " ", decimal.mark = ",", scientific = FALSE) %>%
    paste0(" %")
}
#' @export
np <- function(df, x, ...) {
  count_var <- enquo(x)
  group_var <- quos(...)

  df %>%
    group_by(!!!group_var) %>%
    count(!!count_var) %>%
    mutate(p = n / sum(n), n_ryhma = paste0('', sum(n), '')) %>%
    ungroup
}
#' @export
kylla_ei <- partial(factor, levels = c(T, F), labels = c("Kyllä", "Ei"))
#' @export
to_na <- function(x, value) {
  x[x %in% value] <- NA
  x
}
#' @export
tidy_text <- function(x) {
  x <- tolower(x)
  x <- gsub("-|\r|\n", "", x)
  x <- gsub(" ", "_", x)
  x
}
