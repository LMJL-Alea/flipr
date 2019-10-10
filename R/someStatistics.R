stat_hotelling <- function(d, indices) {
  if (is.null(dim(d))) d <- matrix(d, ncol = 1)
  indices2 <- seq_len(nrow(d))[-indices]
  X <- d[indices , , drop = FALSE]
  Y <- d[indices2, , drop = FALSE]
  nx <- nrow(X)
  ny <- nrow(Y)
  Xbar <- apply(X, 2, mean)
  Ybar <- apply(Y, 2, mean)
  Sx <- cov(X)
  Sy <- cov(Y)
  Spooled <- ((nx - 1) * Sx + (ny - 1) * Sy) / (nx + ny - 2)
  Sinv <- solve(Spooled)
  D <- Xbar - Ybar
  as.numeric(t(D) %*% Sinv %*% D)
}
