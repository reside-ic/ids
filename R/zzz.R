internals <- new.env(parent = emptyenv())

.onLoad <- function(...) {     # nolint
  initialise_random(internals) # nocov
}
