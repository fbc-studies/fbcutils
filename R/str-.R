#' Align a vector of character strings
#'
#' Align a vector of character strings by padding appropriately with spaces.
#' Naturally fails hopelessly if the text is rendered in a non-monospace font.
#'
#' @param x A character vector.
#' @param align A string specifying alignment.
#' @return `x` with all elements padded to the maximum width and aligned
#' @examples
#' x <- c("short", "medium length", "and a rather long string")
#' str_align(x, "left")
#' str_align(x, "right")
#' str_align(x, "center")
#' @export
str_align <- function(x, align = c("left", "right", "center")) {
  align <- match.arg(align)

  x <- as.character(x)
  n <- max(nchar(x))
  side <- align_to_side(align)

  # vectorizing align via pmap was 5x slower when align is scalar
  stringr::str_pad(x, n, side)
}

align_to_side <- function(x) {
  switch(x, left = "right", right = "left", center = "both")
}
