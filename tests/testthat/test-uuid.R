context("uuid")

test_that("uuid", {
  expect_that(uuid(0), equals(character(0)))

  x1 <- uuid(1)
  expect_that(length(x1), equals(1))
  expect_that(x1, is_a("character"))

  x10 <- uuid(10)
  expect_that(length(x10), equals(10))
  expect_that(x10, is_a("character"))
})
