## A future version of this will keep a pool so that duplicates are
## excluded.  For now we'll just rely on the reasonably large pool.

##' Generic id generating function
##' @title Generic id generating function
##' @param ... A number of character vectors
##' @param vals A list of character vectors, \emph{instead} of \code{...}
##' @param style Style to join words with
##' @export
ids <- function(..., vals=list(...), style="snake") {
  nvals <- length(vals)
  combine <- make_combine(style)
  function(n=1) {
    combine(vapply(vals, sample, character(n), n))
  }
}

##' Ids based on a number of adjectives and an animal
##' @title Ids based on a number of adjectives and an animal
##' @param n_adjectives Number of adjectives to prefix the anmial with
##' @param style Style to join the words with
##' @export
##' @examples
##' a2a <- aa(2)
##' a2a()
adjective_animal <- function(n_adjectives=1, style="snake") {
  ids(vals=c(rep(list(adjectives), n_adjectives), list(animals)), style=style)
}

##' @rdname adjective_animal
##' @export
aa <- adjective_animal

##' Generate UUIDs using the uuid package
##' @title Generate UUIDs
##' @param n Number of ids to generate
##' @export
##' @importFrom uuid UUIDgenerate
uuid <- function(n) {
  vapply(seq_len(n), uuid::UUIDgenerate, character(1))
}

## random <- function(n, nchar=32, numbers=TRUE, lower=TRUE, upper=FALSE) {
##   pool <- c(if (numbers) 0:9,
##             if (lower) letters,
##             if (upper) LETTERS)
##   if (length(pool) == 0L) {
##     stop("Can't generate random ids with no pool")
##   }
##   i <- sample.int(length(pool), n * nchar, replace=TRUE)
##   apply(matrix(pool[i], n), 1, paste, collapse="")
## }
