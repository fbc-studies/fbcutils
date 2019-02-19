#' Merge overlapping intervals
#' @param .data A data frame. All variables are evaluated in this context.
#' @param .start An expression that gives the start of the interval.
#' @param .end An expression that gives the end of the interval.
#' @param ... Additional arguments passed on to [dplyr::summarize()]. Use these
#'   to specify how additional columns should be preserved during the merge.
#' @param .max_gap Mximum distance between the star and end of consecutive
#'   intervals that are still counted as overlapping.
#' @export
merge_overlaps <- function(.data, ...) {
  UseMethod("merge_overlaps")
}

#' @export
merge_overlaps.data.frame <- function(.data, ...) {
  as.data.frame(merge_overlaps(tibble::as_tibble(.data), ...))
}

#' @export
merge_overlaps.tbl_df <- function(.data, .start, .end, ..., .max_gap = 0) {
  start <- rlang::enquo(.start)
  end <- rlang::enquo(.end)

  overlaps_found <- .data %>%
    dplyr::arrange(!!start, !!end) %>%
    dplyr::mutate(
      lag_end = dplyr::lag(!!end, default = first(!!start)) + !!.max_gap,
      .seq = 1L + cumsum(!!start > cummax(as.numeric(lag_end)))
    )

  overlaps_found %>%
    dplyr::group_by(.seq, add = TRUE) %>%
    dplyr::summarise(
      !!rlang::as_label(start) := min(!!start),
      !!rlang::as_label(end)   := max(!!end),
      ...
    )
}
