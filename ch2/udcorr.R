findud <- function(v) {
  vud <- v[-1] - v[-length(v)]
  ifelse(vud > 0, 1, -1)
}

udcorr <- function(x, y) {
  ud <- lapply(list(x, y), findud)
  mean(ud[[1]] == ud[[2]])
}