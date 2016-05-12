#!/usr/bin/env Rscript
read <- function(url) {
  f <- tempfile()
  download.file(url, f, method="curl")
  dat <- readChar(f, file.size(f))
  dat <- sub(".+?=\\s*", "", dat)
  dat <- sub(";\\s*", "", dat)
  sort(unique(tolower(jsonlite::fromJSON(dat))))
}

fmt <- "https://raw.githubusercontent.com/a-type/adjective-adjective-animal/master/lib/lists/%s.js"

gfycat_animals <- read(sprintf(fmt, "animals"))
gfycat_adjectives <- read(sprintf(fmt, "adjectives"))

save(gfycat_animals, gfycat_adjectives, file="R/sysdata.rda")
