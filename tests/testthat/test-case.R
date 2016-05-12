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
})
