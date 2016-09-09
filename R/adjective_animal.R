#' Ids based on a number of adjectives and an animal
#'
#' The list of adjectives and animals comes from
#' \url{https://github.com/a-type/adjective-adjective-animal}, and in
#' turn from \url{gfycat.com}
#'
#' @title Ids based on a number of adjectives and an animal
#' @param n_adjectives Number of adjectives to prefix the anmial with
#'
#' @param max_len The maximum length of a word part to include (this
#'   may be useful because some of the names are rather long.  This
#'   stops you generating a
#'   \code{hexakosioihexekontahexaphobic_queenalexandrasbirdwingbutterfly}).
#'   A vector of length 2 ca be passed in here in which case the
#'   first element will apply to the adjectives (all of them) and the
#'   second element will apply to the animals.
#'
#' @inheritParams ids
#' @export
#' @author Rich FitzJohn
#' @examples
#' # Generate a random identifier:
#' adjective_animal()
#'
#' # Generate a bunch all at once:
#' adjective_animal(5)
#'
#' # Control the style of punctuation with the style argument:
#' adjective_animal(style = "lower")
#' adjective_animal(style = "CONSTANT")
#' adjective_animal(style = "camel")
#' adjective_animal(style = "kebab")
#'
#' # Control the number of adjectives used
#' adjective_animal(n_adjectives = 3)
#'
#' # This can get out of hand quickly though:
#' adjective_animal(n_adjectives = 7)
#'
#' # Limit the length of adjectives and animals used:
#' adjective_animal(10, max_len = 6)
#'
#' # The lengths can be controlled for adjectives and animals
#' # separately, with Inf meaning no limit:
#' adjective_animal(10, max_len = c(6, Inf), n_adjectives = 2)
#'
#' # Pass n = NULL to bind arguments to a function
#' id <- adjective_animal(NULL, n_adjectives = 2, style = "dot", max_len = 6)
#' id()
#' id(10)
adjective_animal <- function(n = 1, n_adjectives = 1, style = "snake",
                             max_len = Inf) {
  if (any(is.finite(max_len))) {
    if (any(max_len < 3)) {
      stop("max_len must be at least 3")
    }
    if (length(max_len) == 1L) {
      max_len <- rep_len(max_len, 2)
    } else if (length(max_len) != 2L) {
      stop("max_len must be length 1 or 2")
    }
    gfycat_adjectives <-
      gfycat_adjectives[nchar(gfycat_adjectives) <= max_len[1]]
    gfycat_animals <- gfycat_animals[nchar(gfycat_animals) <= max_len[2]]
  }
  vals <- c(rep(list(gfycat_adjectives), n_adjectives), list(gfycat_animals))
  ids(n, vals = vals, style = style)
}
