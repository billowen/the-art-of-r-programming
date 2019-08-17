imin <- function(x) {
  lx <- length(x)
  i <- x[lx]
  j <- which.min(x[(i + 1) : (lx - 1)])
  k <- i + j
  c(k, x[k])
}

mind <- function(d) {
  n <- nrow(d)
  dd <- cbind(d, 1:n)
  wmins <- apply(dd[-n,], 1, imin)
  i <- which.min(wmins[2, ])
  j <- wmins[1, i]
  c(d[i, j], i, j)
}