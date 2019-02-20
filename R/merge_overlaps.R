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
      .lag_end = dplyr::lag(!!end, default = -Inf) + !!.max_gap,
      .seq = cumsum(!!start > cummax(as.numeric(.lag_end)))
    )

  overlaps_found %>%
    dplyr::group_by(.seq, add = TRUE) %>%
    dplyr::summarise(
      !!rlang::as_label(start) := min(!!start),
      !!rlang::as_label(end)   := max(!!end),
      ...
    )
}

#' @export
identify_overlaps <- function(data, start, end, max_gap = 0) {
  UseMethod("identify_overlaps")
}

identify_overlaps.tbl_df <- function(data, start, end, max_gap = 0) {
  start <- rlang::enquo(start)
  end <- rlang::enquo(end)

  original_cols <- names(data)

  data %>%
    dplyr::arrange(!!start, !!end) %>%
    dplyr::mutate(
      .lag_end = dplyr::lag(!!end, default = -Inf) + !!max_gap,
      .seq = cumsum(!!start > cummax(as.numeric(.lag_end)))
    ) %>%
    dplyr::select(.seq, one_of(original_cols))
}

identify_overlaps.data.frame <- function(data, start, end, max_gap = 0) {
  as.data.frame(identify_overlaps(tibble::as_tibble(data), start, end, max_gap))
}
