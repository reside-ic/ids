##' Ids based on a number of adjectives and an animal
##'
##' The list of adjectives and animals comes from
##' \url{https://github.com/a-type/adjective-adjective-animal}, and in
##' turn from \url{gfycat.com}
##'
##' @title Ids based on a number of adjectives and an animal
##' @param n_adjectives Number of adjectives to prefix the anmial with
##' @inheritParams ids
##' @export
##' @examples
##' adjective_animal()
##' adjective_animal(n_adjectives=3)
##' adjective_animal(style="lower")
adjective_animal <- function(n=1, n_adjectives=1, style="snake") {
  vals <- c(rep(list(gfycat_adjectives), n_adjectives), list(gfycat_animals))
  ids(n, vals=vals, style=style)
}
