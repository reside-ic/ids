#!/usr/bin/env Rscript
root <- here::here()
cpp11::cpp_source("generate.cc")
seed <- as.raw(c(0xf8, 0x43, 0xa6, 0x80,
                 0xa1, 0xfc, 0xd6, 0x2a,
                 0x0d, 0xd1, 0xba, 0x32,
                 0xe0, 0x54, 0x14, 0x69))
x <- xoshiro_run(seed, 10)
n <- as.character(as.integer(c(seed, x)))
writeLines(n, "numbers.txt")
writeLines(n, file.path(root, "tests/testthat/numbers.txt"))
