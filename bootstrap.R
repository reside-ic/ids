#!/usr/bin/env Rscript
read <- function(url) {
  f <- tempfile()
  download.file(url, f, method="curl")
  jsonlite::fromJSON(suppressWarnings(readLines(f)))
}

fmt <- "https://raw.githubusercontent.com/a-type/adjective-adjective-animal/master/lists/%s.json"

animals <- read(sprintf(fmt, "animals"))
adjectives <- read(sprintf(fmt, "adjectives"))

adjectives <- sort(unique(tolower(adjectives)))

save(animals, adjectives, file="R/sysdata.rda")
