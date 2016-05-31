## ---
## title: "ids"
## author: "Rich FitzJohn"
## date: "`r Sys.Date()`"
## output: rmarkdown::html_vignette
## vignette: >
##   %\VignetteIndexEntry{ids}
##   %\VignetteEngine{knitr::rmarkdown}
##   %\VignetteEncoding{UTF-8}
## ---

##+ echo=FALSE, results="hide"
knitr::opts_chunk$set(error=FALSE)
human_no <- function(x) {
  s <- log10(floor(x + 1))
  p <- c(0, thousand=3, million=6, billion=9, trillion=12)
  i <- s > p
  j <- max(which(i))
  str <- names(p)[j]
  if (nzchar(str)) {
    paste(signif(x / 10^p[[j]], 3), str)
  } else {
    as.character(x)
  }
}

## The `ids` package provides randomly generated ids in a number of
## different forms with different readability and sizes.

## ## Random bytes

## The `random_id` function generates random identifiers by generating
## `bytes` random bytes and converting to hex.  Rather than use R's
## random number stream we use `openssl` here.
ids::random_id()

## All `ids` functions take `n` as the first argument to be the number
## of identifiers generated:
ids::random_id(5)

## The default here is 16 bytes (so 2^128 = 3.4e38 combinations).  You
## can make these larger or smaller:
ids::random_id(5, 8)

## If `NULL` is provided as `n`, then a generating function is returned:
f <- ids::random_id(NULL, 8)
f

## This function sets all arguments except for `n`
f()
f(4)

## ## UUIDs

## The above look a lot like UUIDs but they are not actually UUIDs.
## The `uuid` package provides real UUIDs generated with libuuid, and
## the `ids::uuid` function provides an interface to that:
ids::uuid()

## As above, generate more than one UUID:
ids::uuid(4)

## Generate time-based UUIDs:
ids::uuid(4, use_time=TRUE)

## and optionally drop the hyphens:
ids::uuid(5, drop_hyphens=TRUE)

## ## Adjective animal

## Generate (somewhat) human readable identifiers by combining one or
## more adjectives with an animal name.
ids::adjective_animal()

## The list of adjectives and animals comes from
## [gfycat.com](http://gfycat.com), via
## https://github.com/a-type/adjective-adjective-animal

## Generate more than one identifier:
ids::adjective_animal(4)

## Use more than one adjective for very long idenfiers
ids::adjective_animal(4, 3)

## In addition to snake_case, the default, the punctuation between
## words can be changed to:
##
## kebab-case:
ids::adjective_animal(1, 2, style="kebab")

## dot.case:
ids::adjective_animal(1, 2, style="dot")

## camel-case:
ids::adjective_animal(1, 2, style="camel")

## PascalCase:
ids::adjective_animal(1, 2, style="pascal")

## CONSTANT_CASE
ids::adjective_animal(1, 2, style="constant")

## or with spaces, lower case:
ids::adjective_animal(1, 2, style="lower")

## UPPER CASE
ids::adjective_animal(1, 2, style="upper")

## Sentence case
ids::adjective_animal(1, 2, style="sentence")

## Title Case
ids::adjective_animal(1, 2, style="title")

##+ echo=FALSE, results="hide"
n1 <- length(ids:::gfycat_animals)
n2 <- length(ids:::gfycat_adjectives)

## There are `r n1` animal names and `r n2` adjectives so each one you
## add increases the idenfier space by a factor of `r n2`.  So for 1,
## 2, and 3 adjectives there are about `r human_no(n1 * n2)`,
## `r human_no(n1 * n2^2)` and `r human_no(n1 * n2^3)` possible combinations.

## This is a much smaller space than the identifiers above, but these
## are more readable and memorable.

## Note that here, the random nunbers are coming from R's random
## number stream so are affected by \code{set.seed}.

## ## Random sentences

## The `sentence` function creates a sentence style identifier.  This
## uses the approach described by Asana on [their
## blog](https://blog.asana.com/2011/09/6-sad-squid-snuggle-softly).
## This approach encodes 32 bits of information (so 2^32 ~= 4 billion
## possibilities) and in theory can be remapped to an integer if you
## really wanted to.
ids::sentence()

## As with `adjective_animal`, the case can be changed:
ids::sentence(2, "dot")

## If you would rather past tense for the verbs, then pass `past=TRUE`:
ids::sentence(4, past=TRUE)
