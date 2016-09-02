context("ids")

test_that("animals", {
  re_snake2 <- "^([a-z]+)_([a-z]+)$"
  re_camel2 <- "^([a-z]+)([A-Z][a-z]+)$"
  re_snake3 <- "^([a-z]+)_([a-z]+)_([a-z]+)$"

  res <- adjective_animal()
  expect_match(res, re_snake2)
  expect_match(adjective_animal(1, 2), re_snake3)

  expect_match(adjective_animal(style = "camel"), re_camel2)
  res2 <- adjective_animal(2, style = "camel")
  expect_match(res2, re_camel2)
  expect_true(all(sub(re_camel2, "\\1", res2) %in% gfycat_adjectives))
  expect_true(all(tolower(sub(re_camel2, "\\2", res2)) %in% gfycat_animals))

  ## Smoke test:
  for (s in names(cases())) {
    expect_is(adjective_animal(style = s), "character")
  }
})
