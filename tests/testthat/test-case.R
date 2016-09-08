context("case")

test_that("style match", {
  expect_equal(check_style("p")$name, "pascal")
  expect_equal(check_style("pascal")$name, "pascal")
  expect_equal(check_style("pascalcase")$name, "pascal")
  expect_equal(check_style("PascalCase")$name, "pascal")
  expect_equal(check_style("Pascal-Case")$name, "pascal")
  expect_equal(check_style("Pascal_Case")$name, "pascal")
})

test_that("camel", {
  expect_equal(toupper_camel(character(0)), character(0))
  expect_equal(toupper_camel("aaa"), "aaa")
  expect_equal(toupper_camel(c("aaa", "bbb")), c("aaa", "Bbb"))
  expect_equal(toupper_camel(rbind(c("aaa", "bbb"),
                                   c("ccc", "ddd"))),
               rbind(c("aaa", "Bbb"),
                     c("ccc", "Ddd")))
})

test_that("sentence", {
  tr <- make_combine("sentence")
  expect_equal(tr(c("foo", "bar")), "Foo bar")
  expect_equal(tr(rbind(c("foo", "bar"),
                        c("another", "sentence"))),
               c("Foo bar",
                 "Another sentence"))
})

test_that("error cases", {
  expect_error(check_style(NULL), "must be a character vector")
  expect_error(check_style(1), "must be a character vector")
  expect_error(check_style(character(0)), "must be a scalar")
  expect_error(check_style(c("camel", "upper")), "must be a scalar")
  expect_error(check_style("unknown"), "Invalid style 'unknown'")
})
