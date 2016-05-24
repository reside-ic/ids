##' Ids based on a number of adjectives and an animal
##'
##' The list of adjectives and animals comes from
##' \url{https://github.com/a-type/adjective-adjective-animal}, and in
##' turn from \url{gfycat.com}
##'
##' @title Ids based on a number of adjectives and an animal
##' @param n_adjectives Number of adjectives to prefix the anmial with
##'
##' @param max_len The maximum length of a word part to include (this
##'   may be useful because some of the names are rather long.  This
##'   stops you generating a
##'   \code{hexakosioihexekontahexaphobic_queenalexandrasbirdwingbutterfly})
##' @inheritParams ids
##' @export
##' @examples
##' adjective_animal()
##' adjective_animal(n_adjectives=3)
##' adjective_animal(style="lower")
adjective_animal <- function(n=1, n_adjectives=1, style="snake", max_len=Inf) {
  if (is.finite(max_len)) {
    if (max_len < 3) {
      stop("max_len must be at least 3")
    }
    gfycat_adjectives <- gfycat_adjectives[nchar(gfycat_adjectives) <= max_len]
    gfycat_animals <- gfycat_animals[nchar(gfycat_animals) <= max_len]
  }
  vals <- c(rep(list(gfycat_adjectives), n_adjectives), list(gfycat_animals))
  ids(n, vals=vals, style=style)
}
