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


random_bytes_r <- function(n) {
  as.raw(sample.int(n, 256L, replace = TRUE) - 1L)
}


initialise_random <- function(env) {
  env$has_openssl <- requireNamespace("openssl", quietly = TRUE)
  env$random_bytes_internal <- random_bytes_r
  if (env$has_openssl) {
    env$random_bytes_nonglobal <- openssl::rand_bytes
  } else {
    env$random_bytes_nonglobal <- env$random_bytes_internal
  }
}
