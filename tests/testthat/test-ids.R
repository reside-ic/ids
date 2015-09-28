context("ids")

test_that("animals", {
  re_snake2 <- "^([a-z]+)_([a-z]+)$"
  re_camel2 <- "^([a-z]+)([A-Z][a-z]+)$"
  re_snake3 <- "^([a-z]+)_([a-z]+)_([a-z]+)$"

  aa1 <- aa()
  res <- aa1()
  expect_that(res, matches(re_snake2))
  expect_that(aa(2)(), matches(re_snake3))

  expect_that(aa(style="camel")(), matches(re_camel2))
  res2 <- aa(style="camel")(2)
  expect_that(res2, matches(re_camel2))
  expect_that(all(sub(re_camel2, "\\1", res2) %in% adjectives),
              is_true())
  expect_that(all(tolower(sub(re_camel2, "\\2", res2)) %in% animals),
              is_true())

  ## Smoke test:
  for (s in names(cases())) {
    expect_that(aa(style=s)(), not(throws_error()))
  }
})

test_that("uuid", {
  expect_that(uuid(0), equals(character(0)))

  x1 <- uuid(1)
  expect_that(length(x1), equals(1))
  expect_that(x1, is_a("character"))

  x10 <- uuid(10)
  expect_that(length(x10), equals(10))
  expect_that(x10, is_a("character"))
})
