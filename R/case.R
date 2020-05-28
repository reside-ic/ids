# PascalCase
# camelCase
# snake_case
# kebab-case
# Title Case
# lower case
# upper case
# CONSTANT_CASE
# Sentence case
check_style <- function(style) {
  if (!is.character(style)) {
    stop("style must be a character vector")
  }
  if (length(style) != 1L) {
    stop("style must be a scalar")
  }
  pos <- cases()
  i <- pmatch(sub("[_-]?case$", "", tolower(style)), names(pos))
  if (is.na(i)) {
    stop(sprintf("Invalid style '%s'", style))
  }
  ret <- pos[[i]]
  ret$name <- names(pos)[[i]]
  ret
}

make_combine <- function(style) {
  dat <- check_style(style)
  join <- dat$join
  tr <- dat$tr
  function(x) {
    if (is.matrix(x)) {
      apply(tr(x), 1, paste, collapse = join)
    } else {
      paste(tr(x), collapse = join)
    }
  }
}

toupper_initial <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

toupper_camel <- function(x) {
  if (is.matrix(x)) {
    x[, -1] <- toupper_initial(x[, -1])
  } else {
    x[-1] <- toupper_initial(x[-1])
  }
  x
}

toupper_sentence <- function(x) {
  if (is.matrix(x)) {
    x[, 1] <- toupper_initial(x[, 1])
  } else {
    x[1] <- toupper_initial(x[1])
  }
  x
}

toupper_pascal <- function(x) {
  x[] <- toupper_initial(x)
  x
}

tospongemock <- function(x) {
  x[] <- vapply(x, tospongemock1, "")
  x
}

##' @importFrom stats runif
tospongemock1 <- function(x) {
  xx <- strsplit(x, NULL)[[1L]]
  is_letter <- xx %in% letters
  if (!any(is_letter)) {
    return(x)
  }
  i <- c(runif(1) < .5, runif(sum(is_letter) - 1) < 0.85)
  to_upper <- cumsum(i) %% 2 == 0L
  xx[is_letter][to_upper] <- toupper(xx[is_letter][to_upper])
  paste(xx, collapse = "")
}

cases <- function() {
  list(snake      = list(join = "_", tr = identity),
       kebab      = list(join = "-", tr = identity),
       dot        = list(join = ".", tr = identity),
       camel      = list(join = "",  tr = toupper_camel),
       pascal     = list(join = "",  tr = toupper_pascal),
       lower      = list(join = " ", tr = identity), # lower case already
       upper      = list(join = " ", tr = toupper),
       title      = list(join = " ", tr = toupper_pascal),
       sentence   = list(join = " ", tr = toupper_sentence),
       constant   = list(join = "_", tr = toupper),
       spongemock = list(join = "-", tr = tospongemock))
}
