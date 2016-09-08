context("uuid")

RE_UUID <- "^[[:xdigit:]]{8}-([[:xdigit:]]{4}-){3}[[:xdigit:]]{12}$"

test_that("uuid", {
  expect_equal(uuid(0), character(0))

  x1 <- uuid(1)
  expect_equal(length(x1), 1)
  expect_is(x1, "character")
  expect_match(x1, RE_UUID)

  x10 <- uuid(10)
  expect_equal(length(x10), 10)
  expect_is(x10, "character")
  expect_true(all(grepl(RE_UUID, x10)))

  expect_true(all(grepl("^[[:xdigit:]]{32}$", uuid(10, drop_hyphens = TRUE))))

  expect_match(uuid(use_time = TRUE), RE_UUID)
})

test_that("bind args", {
  f <- uuid(NULL)
  expect_is(f, "function")
  expect_equal(as.list(formals(f)), list(n = 1))
  expect_match(f(), RE_UUID)

  g <- uuid(NULL, TRUE)
  expect_match(g(), "^[[:xdigit:]]{32}$")
})
