#' Align a vector of character strings
#' @param x a character vector
#' @param align string specifying alignment
#' @return `x` with all elements padded to the maximum width and aligned
#' @examples
#' x <- c("short", "medium length", "and a rather long string")
#' str_align(x, "left")
#' str_align(x, "right")
#' str_align(x, "center")
#' @export
str_align <- function(x, align = c("left", "right", "center")) {
  x <- as.character(x)
  n <- max(nchar(x))
  side <- align_to_side(align)

  ## TODO: vectorize align
  stringr::str_pad(x, n, side)
}

align_to_side <- function(x) {
  c(left = "left", right = "right", center = "both")[x]
}
