#' Cryptopgraphically random identifiers.  This uses the
#' \code{openssl} package to produce a random set of bytes, and
#' expresses that as a hex character string.  This does not affect
#' R's random number stream.
#'
#' @title Cryptographically random identifiers
#' @inheritParams ids
#'
#' @param bytes The number of bytes to include for each identifier.
#'   The length of the returned identifiers will be twice this long
#'   with each pair of characters representing a single byte.
#'
#' @param use_openssl A logical, indicating if we should use the
#'   openssl for generating the random identifiers.  The openssl
#'   random bytestream is not affected by the state of the R random
#'   number generator (e.g., via \code{\link{set.seed}}) so may not be
#'   suitable for use where reproducibility is important.  The speed
#'   should be very similar for both approaches.
#'
#' @export
#' @author Rich FitzJohn
random_id <- function(n = 1, bytes = 16, use_openssl = TRUE) {
  if (is.null(n)) {
    force(bytes)
    force(use_openssl)
    function(n = 1) {
      random_id(n, bytes, use_openssl)
    }
  } else {
    rand_bytes <- if (use_openssl) openssl::rand_bytes else r_rand_bytes
    apply(matrix(as.character(rand_bytes(n * bytes)), n), 1, paste,
          collapse = "")
  }
}

r_rand_bytes <- function(n) {
  as.raw(sample(256L, n, TRUE) - 1L)
}
