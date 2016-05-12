##' Generate UUIDs using the uuid package
##' @title Generate UUIDs
##'
##' @inheritParams ids
##'
##' @param drop_hyphens Drop the hyphens from the UUID?
##'
##' @export
##'
##' @importFrom uuid UUIDgenerate
uuid <- function(n=1, drop_hyphens=FALSE) {
  if (is.null(n)) {
    force(drop_hyphens)
    function(n=1) {
      uuid(n, drop_hyphens)
    }
  } else {
    res <- vapply(seq_len(n), uuid::UUIDgenerate, character(1))
    if (drop_hyphens) gsub("-", "", res, fixed=TRUE) else res
  }
}
