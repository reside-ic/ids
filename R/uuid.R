#' Generate UUIDs using the uuid package
#' @title Generate UUIDs
#'
#' @inheritParams ids
#'
#' @param drop_hyphens Drop the hyphens from the UUID?
#'
#' @param use_time Passed through to \code{UUIDgenerate} as \code{use.time}.
#'
#' @export
#'
#' @importFrom uuid UUIDgenerate
uuid <- function(n = 1, drop_hyphens = FALSE, use_time = NA) {
  if (is.null(n)) {
    force(drop_hyphens)
    force(use_time)
    function(n = 1) {
      uuid(n, drop_hyphens, use_time)
    }
  } else {
    res <- vapply(seq_len(n), function(i) uuid::UUIDgenerate(use_time),
                  character(1))
    if (drop_hyphens) gsub("-", "", res, fixed = TRUE) else res
  }
}
