#' Align a vector of character strings
#'
#' Align a vector of character strings by padding appropriately with spaces.
#' Naturally fails hopelessly if the text is rendered in a non-monospace font.
#'
#' @param x A character vector.
#' @param align A character vector specifying alignment. Valid values are
#'   `"left"`, `"right"`, and `"center"`.
#' @return `x` with all elements padded to the maximum width and aligned
#' @examples
#' x <- c("short", "medium length", "and a rather long string")
#' str_align(x, "left")
#' str_align(x, "right")
#' str_align(x, "center")
#'
#' # can also use a vector of alignments
#' str_align(x, c("center", "right", "left"))
#' @export
str_align <- function(x, align = "left") {
  x <- as.character(x)
  n <- max(nchar(x))
  side <- align_to_side(align)

  # probably kinda slow to pmap but shouldn't matter most of the time
  purrr::pmap_chr(list(x, n, side), stringr::str_pad)
}

align_to_side <- function(x) {
  c(left = "right", right = "left", center = "both")[x]
}
