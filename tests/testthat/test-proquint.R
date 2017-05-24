context("proquint")

test_that("word conversions", {
  i <- 10^(0:4)
  ## From Python:
  cmp <- c("babad", "babap", "badoh", "bazom", "fisib")
  ## Ours
  w <- int_to_proquint_word(i)
  expect_identical(w, cmp)

  j <- proquint_word_to_int(w, TRUE)
  expect_equal(j, i)
  expect_is(j, "integer")

  k <- proquint_word_to_int(w, FALSE)
  expect_identical(j, k)
  expect_is(k, "integer")
})

## This is exhaustive, because it's not that slow to do everything:
test_that("word conversions - exhaustive", {
  i <- seq_len(PROQUINT_WORD) - 1L
  w <- int_to_proquint_word(i)
  expect_equal(w, cache$proquint_words)
  expect_equal(int_to_proquint_word(i, FALSE), cache$proquint_words)
  ## word -> index
  expect_equal(proquint_word_to_int(w), i)
  expect_equal(proquint_word_to_int(w, FALSE), i)
})

test_that("proquint conversions", {
  ## Then start on the multi-word cases
  known <- c("babad" = 1,
             "babap" = 10,
             "badoh" = 100,
             "bazom" = 1000,
             "fisib" = 10000,
             "babad-mipob" = 100000,
             "babaz-hanab" = 1000000,
             "bafim-nipab" = 10000000,
             "biluj-vahab" = 100000000,
             "govip-somab" = 1000000000)

  expect_equal(proquint_to_int(names(known), use_cache = TRUE),
               unname(known))
  expect_equal(proquint_to_int(names(known), use_cache = FALSE),
               unname(known))

  ## Then the reverse:
  expect_equal(int_to_proquint(unname(known), TRUE), names(known))
  expect_equal(int_to_proquint(unname(known), FALSE), names(known))
})

## Around the transition:
test_that("1-2 word transition corner cases", {
  i <- (PROQUINT_WORD - 2):(PROQUINT_WORD + 2)
  expected <- c("zuzuv", "zuzuz", "babad-babab", "babad-babad", "babad-babaf")
  expect_equal(int_to_proquint(i), expected)
  expect_equal(int_to_proquint(i, FALSE), expected)
  expect_equal(proquint_to_int(expected, use_cache = TRUE), i)
  expect_equal(proquint_to_int(expected, use_cache = FALSE), i)
})

test_that("sampled words do not depend on cache", {
  set.seed(1)
  w1 <- proquint_sample_words(10, TRUE)
  set.seed(1)
  w2 <- proquint_sample_words(10, FALSE)
  expect_identical(w1, w2)
})

test_that("generate openssl random numbers", {
  i <- rand_i16(1, TRUE)
  expect_is(i, "integer")
  expect_gte(i, 0L)
  expect_lt(i, 2^16)

  j <- rand_i16(10, TRUE)
  expect_is(j, "integer")
  expect_equal(length(j), 10)
  expect_true(all(j >= 0L))
  expect_true(all(j < 2^16))

  set.seed(1)
  i <- rand_i16(100, TRUE)
  set.seed(1)
  j <- rand_i16(100, TRUE)
  expect_false(identical(i, j))

  i <- rand_i16(2^16 * 16, TRUE)
  n <- tabulate(i + 1, 2^16)
  expect_true(all(i >= 0L))
  expect_true(all(i < 2^16))
})

test_that("openssl random identifiers", {
  set.seed(1)
  w1 <- proquint(100, use_openssl = TRUE)
  set.seed(1)
  w2 <- proquint(100, use_openssl = TRUE)
  expect_false(identical(w1, w2))
})

test_that("bad type on conversion", {
  expect_error(int_to_proquint("one"), "Invalid type for 'x'")
  expect_error(int_to_proquint(TRUE), "Invalid type for 'x'")
  expect_error(int_to_proquint(1+4i), "Invalid type for 'x'")
})

test_that("word -> int translation: missing values", {
  i <- 12345L
  w <- int_to_proquint_word(i)
  expect_identical(proquint_word_to_int(NA), NA_integer_)
  expect_identical(proquint_word_to_int(NA_character_), NA_integer_)
  expect_identical(proquint_word_to_int(c(NA_character_, w)),
                   c(NA_integer_, i))
  expect_identical(proquint_word_to_int(c(NA, w, w, NA)),
                   c(NA_integer_, i, i, NA_integer_))
})

test_that("int -> word translation: missing values", {
  i <- 12345L
  w <- int_to_proquint_word(i)
  expect_identical(int_to_proquint_word(NA), NA_character_)
  expect_identical(int_to_proquint_word(NA_integer_), NA_character_)
  expect_identical(int_to_proquint_word(c(NA_integer_, i)),
                   c(NA_character_, w))
  expect_identical(int_to_proquint_word(c(NA, i, i, NA)),
                   c(NA_character_, w, w, NA_character_))
})

test_that("identifier -> int translation: missing values", {
  i <- 12345L
  w <- int_to_proquint_word(i)
  ib <- openssl::bignum(i)

  expect_identical(proquint_to_int(NA, "integer"), NA_integer_)
  expect_identical(proquint_to_int(NA, "numeric"), NA_real_)
  expect_identical(proquint_to_int(NA, "bignum"), list(NULL))

  expect_identical(proquint_to_int(NA_character_, "integer"), NA_integer_)
  expect_identical(proquint_to_int(NA_character_, "numeric"), NA_real_)
  expect_identical(proquint_to_int(NA_character_, "bignum"), list(NULL))

  expect_identical(proquint_to_int(c(NA, w), "integer"), c(NA_integer_, i))
  expect_identical(proquint_to_int(c(NA, w), "numeric"), c(NA_real_, i))
  expect_identical(proquint_to_int(c(NA, w), "bignum"),
                   list(NULL, ib))

  expect_identical(proquint_to_int(c(NA, w, w, NA), "integer"),
                   c(NA_integer_, i, i, NA_integer_))
  expect_identical(proquint_to_int(c(NA, w, w, NA), "numeric"),
                   c(NA_real_, i, i, NA_real_))
  expect_identical(proquint_to_int(c(NA, w, w, NA), "bignum"),
                   list(NULL, ib, ib, NULL))
})

test_that("int -> identifier translation: missing values", {
  i <- 12345L
  w <- int_to_proquint_word(i)
  ib <- openssl::bignum(i)

  expect_identical(int_to_proquint(NA), NA_character_)
  expect_identical(int_to_proquint(NA_integer_), NA_character_)
  expect_identical(int_to_proquint(NA_real_), NA_character_)
  expect_identical(int_to_proquint(list(NULL)), NA_character_)

  expect_identical(int_to_proquint(c(NA_integer_, i)), c(NA, w))
  expect_identical(int_to_proquint(c(NA_real_, i)), c(NA, w))
  expect_identical(int_to_proquint(list(NULL, ib)), c(NA, w))

  expect_identical(int_to_proquint(c(NA_integer_, i, i, NA_integer_)),
                   c(NA_character_, w, w, NA_character_))
  expect_identical(int_to_proquint(c(NA_real_, i, i, NA_real_)),
                   c(NA_character_, w, w, NA_character_))
  expect_identical(int_to_proquint(list(NULL, ib, ib, NULL)),
                   c(NA_character_, w, w, NA_character_))
})

test_that("identifier: 0", {
  expect_identical(proquint(0), character(0))
})

test_that("identifier: 1", {
  x <- proquint(1)
  expect_is(x, "character")
  expect_equal(length(x), 1)
  re <- sprintf("^%s-%s$", PROQUINT_RE_WORD, PROQUINT_RE_WORD)
  expect_match(x, re)
})

test_that("identifier: n", {
  n <- 10
  x <- proquint(n)
  expect_is(x, "character")
  expect_equal(length(x), n)
  re <- sprintf("^%s-%s$", PROQUINT_RE_WORD, PROQUINT_RE_WORD)
  expect_match(x, re, all = TRUE)
})

test_that("vary word length", {
  re3 <- sprintf("^%s-%s-%s$",
                 PROQUINT_RE_WORD, PROQUINT_RE_WORD, PROQUINT_RE_WORD)
  expect_match(proquint(1, 3), re3)
  expect_match(proquint(1, 1), PROQUINT_RE1)
})

test_that("functional interface", {
  re3 <- sprintf("^%s-%s-%s$",
                 PROQUINT_RE_WORD, PROQUINT_RE_WORD, PROQUINT_RE_WORD)
  f <- proquint(NULL, 3)
  expect_is(f, "function")
  expect_match(f(), re3)
  expect_equal(length(f()), 1)
  expect_equal(length(f(10)), 10)
})

## -- below here is all boring error handling stuff

## Validate words
test_that("invalid word -> int", {
  expect_error(proquint_word_to_int("hello!"),
               "Invalid proquint word: 'hello!'")
  expect_error(proquint_word_to_int(c("hello!", "babad"),),
               "Invalid proquint word: 'hello!'")
  expect_error(proquint_word_to_int(c("hello!", "babad", "yo!"),),
               "Invalid proquint word: 'hello!', 'yo!'")
  expect_error(proquint_word_to_int(1),
               "Invalid proquint word: '1'")
})

## Validate word indexes
test_that("invalid word -> int", {
  expect_error(int_to_proquint_word(-1),
               "Invalid proquint word index (out of range): -1",
               fixed = TRUE)
  expect_error(int_to_proquint_word(c(-1, 1)),
               "Invalid proquint word index (out of range): -1",
               fixed = TRUE)
  expect_error(int_to_proquint_word(c(-1, 1, 65536)),
               "Invalid proquint word index (out of range): -1, 65536",
               fixed = TRUE)
  expect_error(int_to_proquint_word("babad"),
               "Invalid proquint word index (not numeric)",
               fixed = TRUE)
})

test_that("zero length input", {
  expect_equal(proquint_word_to_int(NULL), integer(0))
  expect_equal(proquint_word_to_int(character(0)), integer(0))
  ## This is not terrific but it is what it is:
  expect_equal(proquint_word_to_int(integer(0)), integer(0))

  expect_equal(int_to_proquint_word(NULL), character(0))
  expect_equal(int_to_proquint_word(integer(0)), character(0))
})

test_that("zero length input", {
  expect_identical(proquint_to_int(NULL, "integer"), integer(0))
  expect_identical(proquint_to_int(NULL, "numeric"), numeric(0))
  expect_identical(proquint_to_int(NULL, "bignum"), list())

  expect_identical(proquint_to_int(character(0), "integer"), integer(0))
  expect_identical(proquint_to_int(character(0), "numeric"), numeric(0))
  expect_identical(proquint_to_int(character(0), "bignum"), list())
})

test_that("zero length input", {
  expect_identical(int_to_proquint(NULL), character(0))
  expect_identical(int_to_proquint(logical(0)), character(0))
  expect_identical(int_to_proquint(integer(0)), character(0))
  expect_identical(int_to_proquint(numeric(0)), character(0))
  expect_identical(int_to_proquint(list()), character(0))
})

test_that("invalid identifier", {
  expect_error(proquint_to_int(TRUE), "Expected a character vector for 'p'")
  expect_error(proquint_to_int("aaaaa"), "Invalid identifier: 'aaaaa'")
})

test_that("proquint to int, varying formats", {
  i <- c(1, 10, 100, 1000, 10000)
  w <- int_to_proquint_word(i)
  expect_identical(proquint_to_int(w, "integer"), as.integer(i))
  expect_identical(proquint_to_int(w, "numeric"), as.numeric(i))
  expect_identical(proquint_to_int(w, "bignum"),
                   lapply(i, openssl::bignum))
})

test_that("int to proquint, varying formats", {
  i <- c(1, 10, 100, 1000, 10000)
  w <- int_to_proquint_word(i)
  expect_identical(int_to_proquint(as.integer(i)), w)
  expect_identical(int_to_proquint(as.numeric(i)), w)
  expect_identical(int_to_proquint(lapply(i, openssl::bignum)), w)
})

test_that("integer overflow", {
  big <- .Machine$integer.max * 2
  pq <- int_to_proquint(big)
  expect_is(pq, "character")
  expect_error(proquint_to_int(pq, "integer"),
               "Integer overflow: cannot represent proquint as integer")
  expect_identical(proquint_to_int(pq, "numeric"), big)
  expect_identical(proquint_to_int(pq, "bignum"),
                   list(openssl::bignum(big)))
})

test_that("numeric overflow", {
  pow <- log2(2/.Machine$double.eps) + 1
  big <- openssl::bignum(2)^pow + 1

  pq <- int_to_proquint(big)
  expect_is(pq, "character")
  expect_error(proquint_to_int(pq, "integer"),
               "Integer overflow: cannot represent proquint as integer")
  expect_error(proquint_to_int(pq, "numeric"),
               "Numeric overflow: cannot represent proquint as numeric")
  expect_identical(proquint_to_int(pq, "bignum"),
                   list(openssl::bignum(big)))
})

## This supports my temporary as_integer_bignum function:
test_that("as_integer_bignum", {
  x <- c(0L, 255L, 256L, 1000L, 200000L, .Machine$integer.max)
  for (i in x) {
    expect_identical(as_integer_bignum(openssl::bignum(i)), i)
  }
})
