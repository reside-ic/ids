re_random <- function(len) {
  sprintf("^[[:xdigit:]]{%s}", len)
}

test_that("uuid", {
  expect_equal(random_id(0), character(0))

  x1 <- random_id(1)
  expect_equal(length(x1), 1)
  expect_type(x1, "character")
  expect_match(x1, re_random(32))

  x10 <- random_id(10)
  expect_equal(length(x10), 10)
  expect_type(x10, "character")
  expect_true(all(grepl(re_random(32), x10)))
})

test_that("bind args", {
  f <- random_id(NULL, bytes = 5)
  expect_true(is.function(f))
  expect_equal(as.list(formals(f)), list(n = 1))
  expect_match(f(), re_random(10))
  expect_equal(nchar(f()), 10)
})

test_that("r byte stream", {
  set.seed(1)
  id1 <- random_id()
  set.seed(1)
  expect_false(id1 == random_id())
  set.seed(1)
  expect_false(id1 == random_id(NULL)())

  set.seed(1)
  id1 <- random_id(global = TRUE)
  set.seed(1)
  expect_true(id1 == random_id(global = TRUE))
  set.seed(1)
  expect_true(id1 == random_id(NULL, global = TRUE)())
})


test_that("Control used generator", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("mockery")
  skip_on_cran()

  on.exit(initialise_random(internals)) # we're going to mess this up

  ## Preliminaries - set up expected internal state
  internals$random_bytes_nonglobal <- mockery::mock()
  mock_openssl_rand_bytes <- mockery::mock()
  mockery::stub(random_bytes, "openssl::rand_bytes", mock_openssl_rand_bytes)
  set.seed(1)
  bytes_global <- random_bytes_global(100)
  state <- internals$random_bytes_internal_generator$state()

  ## Global random number state does not use openssl or our internal
  ## generator:
  set.seed(1)
  expect_identical(random_bytes(100, TRUE, TRUE), bytes_global)
  set.seed(1)
  expect_identical(random_bytes(100, TRUE, FALSE), bytes_global)
  mockery::expect_called(mock_openssl_rand_bytes, 0)
  mockery::expect_called(internals$random_bytes_nonglobal, 0)
  expect_identical(internals$random_bytes_internal_generator$state(), state)

  ## Non-global, depends on the ternary use_openssl
  ##
  ## If NULL we use whatever is registered into the internal object
  random_bytes(100, FALSE, NULL)
  mockery::expect_called(internals$random_bytes_nonglobal, 1)
  expect_equal(mockery::mock_args(internals$random_bytes_nonglobal)[[1]],
               list(100))

  ## If TRUE, we always use openssl:
  random_bytes(100, FALSE, TRUE)
  mockery::expect_called(mock_openssl_rand_bytes, 1)
  expect_equal(mockery::mock_args(mock_openssl_rand_bytes)[[1]],
               list(100))
  expect_identical(internals$random_bytes_internal_generator$state(), state)

  ## If TRUE, we always never openssl:
  random_bytes(100, FALSE, FALSE)
  mockery::expect_called(mock_openssl_rand_bytes, 1)
  mockery::expect_called(internals$random_bytes_nonglobal, 1)
  expect_false(
    identical(internals$random_bytes_internal_generator$state(), state))
})


test_that("Select appropriate nonglobal generator based on openssl avail", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("mockery")
  skip_on_cran()

  mockery::stub(initialise_random, "requireNamespace",
                mockery::mock(TRUE, FALSE))

  env1 <- new.env(parent = emptyenv())
  env2 <- new.env(parent = emptyenv())
  initialise_random(env1)
  expect_identical(env1$random_bytes_nonglobal, openssl::rand_bytes)
  initialise_random(env2)
  expect_identical(env2$random_bytes_nonglobal, env2$random_bytes_internal)

  cmp <- xoshiro128(env2$random_bytes_internal_generator$state())
  expect_identical(
    env2$random_bytes_internal(100),
    random_bytes_from_generator(100, cmp))
})


test_that("global random number generator returns correct number of bytes", {
  res <- random_bytes_global(42)
  expect_type(res, "raw")
  expect_length(res, 42)
  skip_on_cran()
  res <- random_bytes_global(1e6)
  expect_equal(
    sort(unique(as.integer(res))), 0:255)
})
