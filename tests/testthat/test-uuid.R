re_uuid <- "^[[:xdigit:]]{8}-([[:xdigit:]]{4}-){3}[[:xdigit:]]{12}$" # nolint

test_that("uuid", {
  expect_equal(uuid(0), character(0))

  x1 <- uuid(1)
  expect_equal(length(x1), 1)
  expect_type(x1, "character")
  expect_match(x1, re_uuid)

  x10 <- uuid(10)
  expect_equal(length(x10), 10)
  expect_type(x10, "character")
  expect_true(all(grepl(re_uuid, x10)))

  expect_true(all(grepl("^[[:xdigit:]]{32}$", uuid(10, drop_hyphens = TRUE))))
})

test_that("bind args", {
  f <- uuid(NULL)
  expect_true(is.function(f))
  expect_equal(as.list(formals(f)), list(n = 1))
  expect_match(f(), re_uuid)
})
