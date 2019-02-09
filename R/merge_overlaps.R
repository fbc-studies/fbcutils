#' Merge overlapping intervals in a data frame
#' @export
merge_overlaps <- function(tbl, start, end, ..., max_gap = 0) {
  UseMethod("merge_overlaps")
}

#' @export
merge_overlaps.data.frame <- function(tbl, start, end, ..., max_gap = 0) {
  grps <- dplyr::group_vars(tbl)
  res <- merge_overlaps(as.data.table(tbl), start, end, ...,
                        maxp_gap = max_gap, by = grps)
  dplyr::group_by_at(setDF(res), grps)
}

#' @export
#' @import data.table
merge_overlaps.data.table <- function(tbl, start, end, ..., max_gap = 0,
                                      by = character()) {
  s <- rlang::enexpr(start)
  e <- rlang::enexpr(end)

  s_nm <- rlang::as_label(s)
  e_nm <- rlang::as_label(e)

  setkeyv(tbl, c(by, s_nm, e_nm))

  tbl[, pe := shift(eval(e), fill = eval(s)[1L]) + max_gap, keyby = by]
  tbl[, g := 1L + cumsum(eval(s) > cummax(as.numeric(pe))), keyby = by]

  res <- tbl[, .(
    s = min(eval(s)),
    e = max(eval(e))
  ), by = c(by, "g")]

  setnames(res, c("g", "s", "e"), c("seq", s_nm, e_nm))

  res
}
