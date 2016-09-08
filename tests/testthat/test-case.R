context("case")

test_that("style match", {
  expect_that(check_style("p")$name, equals("pascal"))
  expect_that(check_style("pascal")$name, equals("pascal"))
  expect_that(check_style("pascalcase")$name, equals("pascal"))
  expect_that(check_style("PascalCase")$name, equals("pascal"))
  expect_that(check_style("Pascal-Case")$name, equals("pascal"))
  expect_that(check_style("Pascal_Case")$name, equals("pascal"))
})

test_that("camel", {
  expect_that(toupper_camel(character(0)), equals(character(0)))
  expect_that(toupper_camel("aaa"), equals("aaa"))
  expect_that(toupper_camel(c("aaa", "bbb")), equals(c("aaa", "Bbb")))
  expect_that(toupper_camel(rbind(c("aaa", "bbb"),
                                  c("ccc", "ddd"))),
              equals(rbind(c("aaa", "Bbb"),
                           c("ccc", "Ddd"))))
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
