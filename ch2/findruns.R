findruns <- function(x, k) {
  n <- length(x)
  runs <- NULL
  for (i in 1: (n - k + 1)) {
    if (all(x[i: (i + k - 1)] == 1)) runs <- c(runs, i)
  }
  runs
}

findruns1 <- function(x, k) {
  n <- length(x)
  runs <- vector(length = n)
  count <- 0
  
  for (i in 1: (n - k + 1)) {
    if (all(x[i: (i + k - 1)] == 1)) {
      count <- count + 1
      runs[count] <- i
    }
  }
  
  if (count > 0) {
    runs <- runs[1: count]
  } else {
    runs <- NULL
  }
  
  runs
}