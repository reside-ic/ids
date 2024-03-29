#!/usr/bin/env Rscript
read <- function(url) {
  strip_whitespace <- function(x) {
    gsub("(^\\s|\\s*$|\xc2\xa0)", "", x, perl = TRUE)
  }
  f <- tempfile()
  download.file(url, f, method = "curl")
  dat <- readChar(f, file.size(f))
  dat <- gsub("(^.+?=\\s*|;\\s*$)", "", dat)
  res <- sort(unique(strip_whitespace(tolower(jsonlite::fromJSON(dat)))))
  res <- setdiff(res, "mouse/mice")
  ok <- grepl("^[a-z]+$", res)
  if (!all(ok)) {
    stop("Fix the names")
  }
  res
}

fmt <- file.path(
  "https://raw.githubusercontent.com/a-type/adjective-adjective-animal",
  "master/lib/lists/%s.js")

gfycat_animals <- read(sprintf(fmt, "animals"))
gfycat_adjectives <- read(sprintf(fmt, "adjectives"))

save(gfycat_animals, gfycat_adjectives, file = "R/sysdata.rda")
