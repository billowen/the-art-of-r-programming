findols <- function(x) {
  findol <- function(xrow) {
    mdn <- median(xrow)
    devs <- abs(xrow - mdn)
    which.max(devs)
  }
  apply(x, 1, findol)
}