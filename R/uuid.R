#' Generate UUIDs.  In previous versions this
#' was simply a thin wrapper around \code{uuid::UUIDgenerate}, however
#' this was subject to collisions on windows where relatively small
#' numbers of UUIDs generated at the same time could return values
#' that were identical. We now generate only version 4 UUIDs (i.e.,
#' random though with particular bits set).
#'
#' @title Generate UUIDs
#'
#' @inheritParams ids
#'
#' @param drop_hyphens Drop the hyphens from the UUID?
#'
#' @param use_time Unused.
#'
#' @export
#'
#' @author Rich FitzJohn
#' @examples
#' # Generate one id
#' uuid()
#'
#' # Or a bunch
#' uuid(10)
#'
#' # More in the style of random_id()
#' uuid(drop_hyphens = TRUE)
uuid <- function(n = 1, drop_hyphens = FALSE, use_time = NA) {
  if (!missing(use_time)) {
    warning("The 'use_time' argument is now ignored")
  }
  if (is.null(n)) {
    force(drop_hyphens)
    function(n = 1) {
      uuid(n, drop_hyphens)
    }
  } else {
    bytes <- matrix(random_bytes(n * 16), 16, n)
    ## (a) set the high nibble of the 7th byte equal to 4 and
    bytes[7, ] <- as.raw(0x40) | (bytes[7, ] & as.raw(0xf))
    ## (b) set the two most significant bits of the 9th byte to 10'B,
    ##     so the high nibble will be one of {8,9,A,B}.
    bytes[9, ] <- as.raw(0x80) | (bytes[9, ] & as.raw(0x3f))

    if (drop_hyphens) {
      apply(bytes, 2, paste, collapse = "")
    } else {
      a <- apply(bytes[1:4, , drop = FALSE], 2, paste, collapse = "")
      b <- apply(bytes[5:6, , drop = FALSE], 2, paste, collapse = "")
      c <- apply(bytes[7:8, , drop = FALSE], 2, paste, collapse = "")
      d <- apply(bytes[9:10, , drop = FALSE], 2, paste, collapse = "")
      e <- apply(bytes[11:16, , drop = FALSE], 2, paste, collapse = "")
      paste(a, b, c, d, e, sep = "-")
    }
  }
}
