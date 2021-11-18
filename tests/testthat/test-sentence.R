test_that("basic", {
  re <- "^[0-9]+(_[a-z]+){4}$" # nolint
  res <- sentence()
  expect_type(res, "character")
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
  expect_true(is.function(f))
  re <- "^[0-9]+(-[a-z]+){4}$" # nolint
  expect_match(f(), re)
  res <- f(100)
  expect_true(all(grepl(re, res)))

  verb <- vapply(strsplit(res, "-", fixed = TRUE),
                 function(x) x[[4L]], character(1))
  expect_true(all(verb %in% asana_verbs_past))
})


test_that("can produce nonglobal sentence", {
  set.seed(1)
  s1 <- sentence(4)
  set.seed(1)
  expect_equal(sentence(4), s1)

  ## With 2^32 combinations collision over 4 attempts is probability ~ 2^-60
  set.seed(1)
  s2 <- sentence(4, global = FALSE)
  set.seed(1)
  s3 <- sentence(4, global = FALSE)
  expect_false(any(s2 == s1))
  expect_false(any(s3 == s1))
})
