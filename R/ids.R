## A future version of this will keep a pool so that duplicates are
## excluded.  For now we'll just rely on the reasonably large pool.

#' Generic id generating function
#' @title Generic id generating function
#'
#' @param n number of ids to return.  If `NULL`, it instead returns
#'   the generating function
#'
#' @param ... A number of character vectors
#'
#' @param vals A list of character vectors, *instead* of `...`
#'
#' @param style Style to join words with.  Can be one of "Pascal",
#'   "camel", "snake", "kebab", "dot", "title", "sentence", "lower",
#'   "upper", "constant" or "spongemock"
#'
#' @param global Use global random number generator that responds to
#'   `set.seed` (see [ids::random_id] for details, but
#'   note that the default here is different).
#'
#' @param use_openssl Use openssl for random number generation when
#'   using a non-global generator (see [ids::random_id] for
#'   details)
#'
#' @return Either a character vector of length `n`, or a function of
#'   one argument if `n` is `NULL`
#'
#' @export
#' @author Rich FitzJohn
#' @examples
#' # For an example, please see the vignette
ids <- function(n, ..., vals = list(...), style = "snake",
                global = TRUE, use_openssl = FALSE) {
  combine <- make_combine(style)
  force(vals)
  force(style)
  force(global)
  force(use_openssl)
  gen <- function(n = 1) {
    combine(vapply(vals, sample_str, character(n), n, global, use_openssl))
  }
  if (is.null(n)) gen else gen(n)
}
