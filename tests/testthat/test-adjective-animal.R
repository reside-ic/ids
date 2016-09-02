context("adjective animal")

test_that("basic", {
  res <- adjective_animal()
  expect_is(res, "character")
  expect_equal(length(res), 1)
  expect_match(res, "^[a-z]+_[a-z]+$")
})

test_that("length", {
  expect_equal(length(adjective_animal(5)), 5)
  expect_equal(adjective_animal(0), character(0))
})

test_that("style", {
  expect_match(adjective_animal(style = "Pascal"), "^[A-Z][a-z]+[A-Z][a-z]+$")
  expect_match(adjective_animal(style = "camel"), "^[a-z]+[A-Z][a-z]+$")
  expect_match(adjective_animal(style = "snake"), "^[a-z]+_[a-z]+$")
  expect_match(adjective_animal(style = "kebab"), "^[a-z]+-[a-z]+$")
  expect_match(adjective_animal(style = "lower"), "^[a-z]+ [a-z]+$")
  expect_match(adjective_animal(style = "upper"), "^[A-Z]+ [A-Z]+$")
  expect_match(adjective_animal(style = "constant"), "^[A-Z]+_[A-Z]+$")
  expect_match(adjective_animal(style = "title"), "^[A-Z][a-z]+ [A-Z][a-z]+$")
  expect_match(adjective_animal(style = "sentence"), "^[A-Z][a-z]+ [a-z]+$")
})
