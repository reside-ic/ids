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
#' @examples
#' # Generate a random id:
#' random_id()
#'
#' # Generate 10 of them!
#' random_id(10)
#'
#' # Different length ids
#' random_id(bytes = 8)
#' # (note that the number of characters is twice the number of bytes)
#'
#' # The ids are not affected by R's RNG state:
#' set.seed(1)
#' (id1 <- random_id())
#' set.seed(1)
#' (id2 <- random_id())
#' # The generated identifiers are different, despite the seed being the same:
#' id1 == id2
#'
#' # If you need these identifiers to be reproducible, pass use_openssl = FALSE
#' set.seed(1)
#' (id1 <- random_id(use_openssl = FALSE))
#' set.seed(1)
#' (id2 <- random_id(use_openssl = FALSE))
#' # This time they are the same:
#' id1 == id2
#'
#' # Pass \code{n = NULL} to generate a function that binds your arguments:
#' id8 <- random_id(NULL, bytes = 8)
#' id8(10)
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
