#' @export
setxor <- function(x, y) {
  setdiff(union(x, y), intersect(x, y))
}
