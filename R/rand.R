random_bytes <- function(n) {
  openssl::rand_bytes(n)
}
