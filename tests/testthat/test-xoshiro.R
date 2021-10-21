test_that("conversion work", {
  n <- 123456789
  b <- uint_to_bits(n)
  expect_equal(packBits(b), as.raw(c(0x15, 0xcd, 0x5b, 0x07)))
  expect_length(b, 32L)
  expect_equal(bits_to_uint(b), n)
})


test_that("rotl works", {
  n <- 123456789
  b <- uint_to_bits(n)
  expect_equal(rotl(b, 11), uint_to_bits(3731400762))
})


test_that("shift_l works", {
  n <- 123456789
  b <- uint_to_bits(n)
  expect_equal(shift_l(b, 9), uint_to_bits(3080333824))
})


test_that("xor works", {
  a <- uint_to_bits(123456789)
  b <- uint_to_bits(3080333824)
  expect_equal(a != b, uint_to_bits(2965497621))
})


test_that("bitwise addition is correct", {
  f <- function() {
    a <- runif(32) > 0.6
    b <- runif(32) > 0.4
    c <- uint_to_bits((bits_to_uint(a) + bits_to_uint(b)) %% 2^32)
    d <- bits_add(a, b)
    identical(c, d)
  }

  expect_true(all(replicate(100, f())))
})


test_that("Generate numbers", {
  n <- as.raw(as.integer(readLines("numbers.txt")))
  i <- seq_len(16)
  seed <- n[i]
  expected <- matrix(n[-i], 4)

  gen <- xoshiro128(seed)
  expect_equal(gen$state(),
               matrix(as.logical(rawToBits(seed)), 32))
  res <- array(random_bytes_from_generator(length(expected), gen),
               dim(expected))
  expect_identical(res, expected)
})


## Because we get bytes out in chunks of 4, we need to make sure that
## we do actually return the correct length is required.
test_that("compute fractional integers of random numbers", {
  gen1 <- xoshiro128(NULL)
  gen2 <- xoshiro128(gen1$state())

  b1 <- random_bytes_from_generator(32, gen1)
  b2 <- random_bytes_from_generator(30, gen2)
  expect_equal(b2, b1[1:30])

  expect_equal(gen1$yield(), gen2$yield())
})


test_that("sanitise xoshiro inputs", {
  expect_error(xoshiro128(sample),
               "Invalid input for state")
})
