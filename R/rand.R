random_bytes <- function(n, global, use_openssl) {
  if (global) {
    random_bytes_global(n)
  } else if (is.null(use_openssl)) {
    internals$random_bytes_nonglobal(n)
  } else if (use_openssl) {
    openssl::rand_bytes(n)
  } else {
    internals$random_bytes_internal(n)
  }
}


random_bytes_global <- function(n) {
  as.raw(sample.int(n, 256L, replace = TRUE) - 1L)
}


random_bytes_internal <- function(gen) {
  force(gen)
  function(n) {
    random_bytes_from_generator(n, gen)
  }
}


random_bytes_from_generator <- function(n, gen) {
  n_bytes_per_draw <- gen$n_bytes_per_draw
  yield <- gen$yield

  n_draws <- ceiling(n / n_bytes_per_draw)

  ret <- matrix(raw(1), n_bytes_per_draw, n_draws)
  for (i in seq_len(n_draws)) {
    ret[, i] <- yield()
  }

  if (n_draws * n_bytes_per_draw == n) {
    dim(ret) <- NULL
  } else {
    ret <- ret[seq_len(n)]
  }

  ret
}


initialise_random <- function(env) {
  env$has_openssl <- requireNamespace("openssl", quietly = TRUE)
  env$random_bytes_internal_generator <- xoshiro128(NULL)
  env$random_bytes_internal <-
    random_bytes_internal(env$random_bytes_internal_generator)
  if (env$has_openssl) {
    env$random_bytes_nonglobal <- openssl::rand_bytes
  } else {
    env$random_bytes_nonglobal <- env$random_bytes_internal
  }
}
