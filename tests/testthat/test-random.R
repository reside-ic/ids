context("random")

re_random <- function(len) {
  sprintf("^[[:xdigit:]]{%s}", len)
}

test_that("uuid", {
  expect_equal(random_id(0), character(0))

  x1 <- random_id(1)
  expect_equal(length(x1), 1)
  expect_is(x1, "character")
  expect_match(x1, re_random(32))

  x10 <- random_id(10)
  expect_equal(length(x10), 10)
  expect_is(x10, "character")
  expect_true(all(grepl(re_random(32), x10)))
})

test_that("bind args", {
  f <- random_id(NULL, bytes = 5)
  expect_is(f, "function")
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
  id1 <- random_id(use_openssl = FALSE)
  set.seed(1)
  expect_true(id1 == random_id(use_openssl = FALSE))
  set.seed(1)
  expect_true(id1 == random_id(NULL, use_openssl = FALSE)())
})
