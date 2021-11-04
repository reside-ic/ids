#' Ids based on a number of adjectives and an animal
#'
#' The list of adjectives and animals comes from
#' <https://github.com/a-type/adjective-adjective-animal>, and in turn
#' from <gfycat.com>
#'
#' @title Ids based on a number of adjectives and an animal
#' @param n_adjectives Number of adjectives to prefix the animal with
#'
#' @param max_len The maximum length of a word part to include (this
#'   may be useful because some of the names are rather long.  This
#'   stops you generating a
#'   `hexakosioihexekontahexaphobic_queenalexandrasbirdwingbutterfly`).
#'   A vector of length 2 can be passed in here in which case the
#'   first element will apply to the adjectives (all of them) and the
#'   second element will apply to the animals.
#'
#' @param alliterate Produce "alliterative" adjective animals (e.g.,
#'   `hessian_hamster`).  Note that this cannot provide an equal
#'   probability of any particular combination because it forces a
#'   weighted sampling.  Adjectives may also be repeated if
#'   `n_adjectives` is more than 1.
#'
#' @inheritParams ids
#' @export
#' @author Rich FitzJohn
#' @examples
#' # Generate a random identifier:
#' ids::adjective_animal()
#'
#' # Generate a bunch all at once:
#' ids::adjective_animal(5)
#'
#' # Control the style of punctuation with the style argument:
#' ids::adjective_animal(style = "lower")
#' ids::adjective_animal(style = "CONSTANT")
#' ids::adjective_animal(style = "camel")
#' ids::adjective_animal(style = "kebab")
#' ids::adjective_animal(style = "spongemock")
#'
#' # Control the number of adjectives used
#' ids::adjective_animal(n_adjectives = 3)
#'
#' # This can get out of hand quickly though:
#' ids::adjective_animal(n_adjectives = 7)
#'
#' # Limit the length of adjectives and animals used:
#' ids::adjective_animal(10, max_len = 6)
#'
#' # The lengths can be controlled for adjectives and animals
#' # separately, with Inf meaning no limit:
#' ids::adjective_animal(10, max_len = c(6, Inf), n_adjectives = 2)
#'
#' # Pass n = NULL to bind arguments to a function
#' id <- ids::adjective_animal(NULL, n_adjectives = 2,
#'                             style = "dot", max_len = 6)
#' id()
#' id(10)
#'
#' # Alliterated adjective animals always aid added awesomeness
#' ids::adjective_animal(10, n_adjectives = 3, alliterate = TRUE)
adjective_animal <- function(n = 1, n_adjectives = 1, style = "snake",
                             max_len = Inf, alliterate = FALSE) {
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
  if (alliterate) {
    aa_alliterate(n, vals, style)
  } else {
    vals <- c(rep(list(gfycat_adjectives), n_adjectives), list(gfycat_animals))
    ids(n, vals = vals, style = style)
  }
}


## We can generate alliterative ids by either rejection sampling
## (which will be hard with multiple adjectives) or by doing a
## weighted sample of letters and then working with each letter
## separately.  To do this properly we should compute the number of
## distinct combinations and avoid duplications but that seems
## excessive for this and is only an issue if the number of adjectives
## is greater than one.  Practically we found the number of duplicated
## names low enough when the max_len is not set too low and some were
## quite amusing, such as "hot_hot_hot_hen".
aa_alliterate <- function(n, vals, style) {
  m <- lapply(vals, function(x) split(x, factor(substr(x, 1, 1), letters)))
  m <- matrix(unlist(m, FALSE, FALSE), 26, length(vals),
              dimnames = list(letters, NULL))

  p <- rowSums(log(lengths(m)))
  p <- exp(p - max(p))
  p <- p / sum(p)

  gen1 <- function(start, n) {
    ids(n, vals = m[start, , drop = TRUE], style = style)
  }

  gen <- function(n = 1) {
    start <- letters[sample.int(26, n, prob = p, replace = TRUE)]
    ret <- character(n)
    for (i in unique(start)) {
      j <- start == i
      ret[j] <- gen1(i, sum(j))
    }
    ret
  }

  if (is.null(n)) {
    gen
  } else {
    gen(n)
  }
}
