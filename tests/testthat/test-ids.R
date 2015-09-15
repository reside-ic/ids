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
