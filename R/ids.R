## A future version of this will keep a pool so that duplicates are
## excluded.  For now we'll just rely on the reasonably large pool.

##' Generic id generating function
##' @title Generic id generating function
##'
##' @param n number of ids to return.  If \code{NULL}, it instead
##'   returns the generating function
##'
##' @param ... A number of character vectors
##'
##' @param vals A list of character vectors, \emph{instead} of \code{...}
##'
##' @param style Style to join words with.  Can be one of "Pascal",
##'   "camel", "snake", "kebab", "title", "sentence", "lower",
##'   "upper", and "constant".
##'
##' @return Either a character vector of length \code{n}, or a
##'   function of one argument if \code{n} is \code{NULL}
##'
##' @export
ids <- function(n, ..., vals=list(...), style="snake") {
  nvals <- length(vals)
  combine <- make_combine(style)
  gen <- function(n=1) {
    combine(vapply(vals, sample, character(n), n))
  }
  if (is.null(n)) gen else gen(n)
}
