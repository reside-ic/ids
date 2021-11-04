xoshiro128 <- function(state = NULL) {
  env <- new.env(parent = emptyenv())
  env$state <- random_state(state, 32, 4)
  list(
    state = function() {
      env$state
    },
    n_bytes_per_draw = 4L,
    yield = function() {
      result <- xoshiro128_scramble(env$state)
      env$state <- xoshiro128_advance(env$state)
      packBits(result, "raw")
    }
  )
}


xoshiro128_advance <- function(state) {
  t <- shift_l(state[, 2L], 9L)
  state[, 3L] <- state[, 3L] != state[, 1L]
  state[, 4L] <- state[, 4L] != state[, 2L]
  state[, 2L] <- state[, 2L] != state[, 3L]
  state[, 1L] <- state[, 1L] != state[, 4L]
  state[, 3L] <- state[, 3L] !=  t
  state[, 4L] <- rotl(state[, 4L], 11L)
  state
}


xoshiro128_scramble <- function(state) {
  bits_add(state[, 1L], state[, 4L])
}


rotl <- function(x, k) {
  n <- length(x)
  x[c(seq.int(to = n, length.out = k), seq_len(n - k))]
}


shift_l <- function(x, n) {
  c(logical(n), x[seq_len(length(x) - n)])
}


bits_add <- function(a, b) {
  while (any(b)) {
    carry <- a & b
    a <- a != b
    b <- shift_l(carry, 1L)
  }
  a
}


## These are just for testing, really, and convert from bits to
## integers without overflowing.
uint_to_bits <- function(x, width = 32L) {
  pow <- seq_len(width)
  as.logical((x %% 2^pow) %/% 2^(pow - 1)) # nolint
}


bits_to_uint <- function(x) {
  pow <- seq_along(x) - 1L
  sum(2^pow * x)
}


random_state <- function(state, n_bits_per_int, n_ints) {
  n_bytes_per_int <- n_bits_per_int / 8
  if (is.null(state)) {
    state <- random_seed(n_bits_per_int * n_ints)
  } else if (is.logical(state)) {
    stopifnot(length(state) == n_bits_per_int * n_ints)
  } else if (is.raw(state)) {
    stopifnot(length(state) == n_bytes_per_int * n_ints)
    state <- as.logical(rawToBits(state))
  } else {
    stop("Invalid input for state")
  }
  matrix(state, n_bits_per_int, n_ints)
}


random_seed <- function(n) {
  p <- Sys.getpid()
  t <- deparse(as.numeric(Sys.time()), control = "digits17")
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(c(p, t), tmp)
  hash <- unname(tools::md5sum(tmp))
  ## there seems to no great way of converting strings to raw really...
  m <- matrix(match(strsplit(hash, NULL)[[1]], c(0:9, letters[1:6])) - 1L, 2)
  data <- as.logical(rawToBits(as.raw(m[1, ] * 16 + m[2, ])))
  data[seq_len(n)]
}
