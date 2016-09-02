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
#' @export
#' @author Rich FitzJohn
random_id <- function(n = 1, bytes = 16) {
  if (is.null(n)) {
    force(bytes)
    function(n = 1) {
      random_id(n, bytes)
    }
  } else {
    apply(matrix(as.character(openssl::rand_bytes(n * bytes)), n), 1, paste,
          collapse = "")
  }
}
