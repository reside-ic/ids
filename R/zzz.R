internals <- new.env(parent = emptyenv())

.onLoad <- function(...) {
  initialise_random(internals) # nocov
}
