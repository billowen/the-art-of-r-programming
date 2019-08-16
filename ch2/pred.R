preda <- function(x, k) {
  n <- length(x)
  k2 <- k / 2
  pred <- vector(length = n - k)
  for (i in 1 : (n - k)) {
    if (sum(x[i : (i + k - 1)]) >= k2) 
      pred[i] <- 1
    else
      pred[i] <- 0
  }
  
  mean(abs(pred - x[(k+1):n]))
}

predb <- function(x, k) {
  n <- length(x)
  k2 <- k / 2
  pred <- vector(length = n- k)
  sm <- sum(1 : k)
  if (sm >= k2) pred[1] <- 1 else pred[1] <- 0
  if (n - k >= 2) {
    for (i in 2: (n-k)) {
      sm <- sm + x[i+k-1] - x[i-1]
      if (sm >= k2) pred[i] <- 1 else pred[i] <- 0
    } 
  }
  
  mean(abs(pred - x[(k+1):n]))
}