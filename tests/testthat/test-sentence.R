context("sentence")

test_that("basic", {
  re <- "^[0-9]+(_[a-z]+){4}$"
  res <- sentence()
  expect_is(res, "character")
  expect_equal(length(res), 1)
  expect_match(res, re)
})

test_that("tense", {
  verb <- vapply(strsplit(sentence(100), "_", fixed = TRUE),
                 function(x) x[[4L]], character(1))
  expect_true(all(verb %in% asana_verbs_present))

  verb <- vapply(strsplit(sentence(100, past = TRUE), "_", fixed = TRUE),
                 function(x) x[[4L]], character(1))
  expect_true(all(verb %in% asana_verbs_past))
})

test_that("functional interface", {
  f <- sentence(NULL, style = "kebab", past = TRUE)
  expect_is(f, "function")
  re <- "^[0-9]+(-[a-z]+){4}$"
  expect_match(f(), re)
  res <- f(100)
  expect_true(all(grepl(re, res)))

  verb <- vapply(strsplit(res, "-", fixed = TRUE),
                 function(x) x[[4L]], character(1))
  expect_true(all(verb %in% asana_verbs_past))
})
