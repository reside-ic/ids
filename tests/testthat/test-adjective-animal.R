context("adjective animal")

test_that("data", {
  expect_false(any(grepl("\\s", gfycat_animals)))
  expect_false(any(grepl("\\s", gfycat_adjectives)))
})

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

test_that("restrict length", {
  tmp <- strsplit(adjective_animal(200, max_len = 5), "_", fixed = TRUE)
  expect_lte(max(sapply(tmp, nchar)), 5)

  tmp <- strsplit(adjective_animal(200, max_len = c(5, 10)), "_", fixed = TRUE)
  tmp <- apply(sapply(tmp, nchar), 1, max)
  expect_lte(tmp[[1]], 5)
  expect_lte(tmp[[2]], 10)
  expect_gt(tmp[[2]], tmp[[1]])
})

test_that("restrict length error cases", {
  expect_error(adjective_animal(max_len = 2),
               "max_len must be at least 3")
  expect_error(adjective_animal(max_len = -2),
               "max_len must be at least 3")

  ## This is ignored -- perhaps it should not be?
  expect_silent(adjective_animal(max_len = numeric(0)))

  expect_error(adjective_animal(max_len = c(5, 6, 7)),
               "max_len must be length 1 or 2")
  expect_error(adjective_animal(n_adjectives = 2, max_len = c(5, 6, 7)),
               "max_len must be length 1 or 2")
})

test_that("functional interface", {
  f <- adjective_animal(NULL, max_len = 10, style = "kebab", n_adjectives = 2)
  expect_is(f, "function")
  re <- "^[a-z]{2,10}-[a-z]{2,10}-[a-z]{2,10}$"
  expect_match(f(), re)
  x <- f(100)
  expect_true(all(grepl(re, x)))
})


test_that("alliterate", {
  res <- adjective_animal(10, alliterate = TRUE)
  res <- strsplit(res, "_", fixed = TRUE)
  m <- vapply(res, substr, character(2), 1, 1)
  expect_equal(m[1, ], m[2, ])
})


test_that("alliterate: functional", {
  res <- adjective_animal(NULL, n_adjective = 4, alliterate = TRUE)
  expect_is(res, "function")
  s <- strsplit(res(1), "_", fixed = TRUE)[[1]]
  expect_equal(length(unique(substr(s, 1, 1))), 1)
})
