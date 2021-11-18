sample_str <- function(values, size, global, use_openssl) {
  values[random_integer(length(values), size, global, use_openssl)]
}


sample_str_weighted <- function(values, size, prob, global, use_openssl) {
  values[random_integer_weighted(prob, size, global, use_openssl)]
}


`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
