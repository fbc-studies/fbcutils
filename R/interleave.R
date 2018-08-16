#' Interleave elements of vectors
#'
#' Use to interleave elements in multiple vectors to a single vector
#'
#' @param ... vectors to be interleaved
#' @return result of `c(...)` ordered to interleave elements of `...`
#' @examples
#' x <- paste0("x", 1:5)
#' y <- paste0("y", 1:2)
#' z <- paste0("z", 1:3)
#' interleave(x, y, z)
#' @export
interleave <- function(...) {
  i <- purrr::map(list(...), seq_along)
  c(...)[order(purrr::flatten_int(i))]
}
