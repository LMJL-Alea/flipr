#' Test Statistics for the Two-Sample Problem
#'
#' This is a collection of functions that provide test statistics to be used
#' into the permutation scheme for performing tests. These test statistics can
#' be divided into two categories: traditional statistics that use empirical
#' moments and inter-point statistics that only rely on pairwise dissimilarities
#' between data points.
#'
#' @section Traditional Test Statistics:
#'
#' - \code{\link{stat_hotelling}} implements Hotelling's \eqn{T^2} statistic for
#' multivariate data with \eqn{p < n}.
#' - \code{\link{stat_student}} or \code{\link{stat_t}} implements Student's
#' statistic (originally assuming equal variances and thus using the pooled
#' empirical variance estimator). See \code{\link[stats]{t.test}} for details.
#' - \code{\link{stat_welch}} implements Student-Welch statistic which is
#' essentially a modification of Student's statistic accounting for unequal
#' variances. See \code{\link[stats]{t.test}} for details.
#' - \code{\link{stat_fisher}} or \code{\link{stat_f}} implements Fisher's
#' variance ratio statistic. See \code{\link[stats]{var.test}} for details.
#' - \code{\link{stat_mean}} implements a statistic that computes the difference
#' between the means.
#' - \code{\link{stat_bs}} implements the statistic proposed by Bai & Saranadasa
#' (1996) for high-dimensional multivariate data.
#'
#' @section Inter-Point Test Statistics:
#'
#' - \code{\link{stat_student_ip}} or \code{\link{stat_t_ip}} implements a
#' Student-like test statistic based on inter-point distances only as described
#' in Lovato et al. (2020).
#' - \code{\link{stat_fisher_ip}} or \code{\link{stat_f_ip}} implements a
#' Fisher-like test statistic based on inter-point distances only as described
#' in Lovato et al. (2020).
#' - \code{\link{stat_bg_ip}} implements the statistic proposed by Biswas &
#' Ghosh (2014).
#' - \code{\link{stat_energy_ip}} implements the class of energy-based
#' statistics as described in Székely & Rizzo (2013);
#' - \code{\link{stat_cq_ip}} implements the statistic proposed by Chen & Qin
#' (2010).
#' - \code{\link{stat_mod_ip}} implements a statistic that computes the mean of
#' inter-point distances.
#' - \code{\link{stat_dom_ip}} implements a statistic that computes the distance
#' between the medoids of the two samples, possibly standardized by the pooled
#' corresponding variances.
#'
#' @references
#' Bai, Z., & Saranadasa, H. (1996). Effect of high dimension: by an example of
#' a two sample problem. Statistica Sinica, 311-329.
#'
#' Lovato, I., Pini, A., Stamm, A., & Vantini, S. (2020). Model-free two-sample
#' test for network-valued data. Computational Statistics & Data Analysis, 144,
#' 106896.
#'
#' Biswas, M., & Ghosh, A. K. (2014). A nonparametric two-sample test applicable
#' to high dimensional data. Journal of Multivariate Analysis, 123, 160-171.
#'
#' Székely, G. J., & Rizzo, M. L. (2013). Energy statistics: A class of
#' statistics based on distances. Journal of statistical planning and inference,
#' 143(8), 1249-1272.
#'
#' Chen, S. X., & Qin, Y. L. (2010). A two-sample test for high-dimensional data
#' with applications to gene-set testing. The Annals of Statistics, 38(2),
#' 808-835.
#'
#' @param data Either a list of the `n1 + n2` concatenated observations with the
#'   original `n1` observations from the first sample on top and the original
#'   `n2` observations from the second sample below. Or a dissimilarity matrix
#'   stored as a \code{\link[stats]{dist}} object for all inter-point statistics
#'   whose function name should end with `_ip()`.
#' @param indices An integer vector specifying the indices in `data` that are
#'   considered to belong to the first sample.
#' @param validate Boolean indicating whether the format of the input data
#'   should be checked before the statistic is actually computed. Default is
#'   `FALSE` as these functions are meant to be used deeply into the permutation
#'   mechanism and thus called a large number of times.
#' @param alpha A scalar value specifying the power to which the dissimilarities
#'   should be elevated in the computation of the inter-point energy statistic.
#'   Default is `1L`.
#' @param standardize A boolean specifying whether the distance between medoids
#'   in the \code{\link{stat_dom_ip}} function should be normalized by the
#'   pooled corresponding variances. Default is `TRUE`.
#'
#' @return A real scalar giving the value of test statistic for the permutation
#'   specified by the integer vector `indices`.
#' @name test-statistic
#'
#' @examples
#' n <- 10L
#' mx <- 0
#' sigma <- 1
#'
#' # Two different models for the two populations
#' x <- rnorm(n = n, mean = mx, sd = sigma)
#' x <- as.list(x)
#' delta <- 10
#' my <- mx + delta
#' y <- rnorm(n = n, mean = my, sd = sigma)
#' y <- as.list(y)
#' stat_hotelling(c(x, y), 1:n)
#' stat_t(c(x, y), 1:n)
#' stat_mean(c(x, y), 1:n)
NULL

#' @rdname test-statistic
#' @export
stat_hotelling <- function(data, indices, validate = FALSE) {
  if (validate && !is.list(data))
    abort("The input data for Hotelling's statistic should be a list.")
  n <- length(data)
  nx <- length(indices)
  ny <- n - nx
  indices2 <- seq_len(n)[-indices]
  X <- purrr::reduce(data[indices], rbind)
  Y <- purrr::reduce(data[indices2], rbind)
  Xbar <- colMeans(X)
  Ybar <- colMeans(Y)
  Sx <- stats::cov(X)
  Sy <- stats::cov(Y)
  Spooled <- ((nx - 1) * Sx + (ny - 1) * Sy) / (nx + ny - 2)
  Sinv <- solve(Spooled)
  D <- Xbar - Ybar
  as.numeric(t(D) %*% Sinv %*% D)
}

#' @rdname test-statistic
#' @export
stat_welch <- function(data, indices, validate = FALSE) {
  if (validate && !is.list(data))
    abort("The input data for Welch's statistic should be a list.")
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  stats::t.test(x1, x2, var.equal = FALSE)$statistic
}

#' @rdname test-statistic
#' @export
stat_student <- function(data, indices, validate = FALSE) {
  if (validate && !is.list(data))
    abort("The input data for Student's statistic should be a list.")
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  stats::t.test(x1, x2, var.equal = TRUE)$statistic
}

#' @rdname test-statistic
#' @export
stat_t <- stat_student

#' @rdname test-statistic
#' @export
stat_fisher <- function(data, indices, validate = FALSE) {
  if (validate && !is.list(data))
    abort("The input data for Fisher's variance ratio statistic should be a list.")
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  stats::var.test(x1, x2)$statistic
}

#' @rdname test-statistic
#' @export
stat_f <- stat_fisher

#' @rdname test-statistic
#' @export
stat_mean <- function(data, indices, validate = FALSE) {
  if (validate && !is.list(data))
    abort("The input data for the mean difference statistic should be a list.")
  n <- length(data)
  n1 <- length(indices)
  n2 <- n - n1
  indices2 <- seq_len(n)[-indices]
  x1 <- unlist(data[indices])
  x2 <- unlist(data[indices2])
  mean(x1) - mean(x2)
}

#' @rdname test-statistic
#' @export
stat_bs <- function(data, indices, validate = FALSE) {
  if (validate && !is.list(data))
    abort("The input data for the Bai-Saranadasa statistic should be a list.")
  n <- length(data)
  nx <- length(indices)
  ny <- n - nx
  indices2 <- seq_len(n)[-indices]
  X <- purrr::reduce(data[indices], rbind)
  Y <- purrr::reduce(data[indices2], rbind)
  Xbar <- colMeans(X)
  Ybar <- colMeans(Y)
  Sx <- stats::cov(X)
  Sy <- stats::cov(Y)
  Sn <- ((nx - 1) * Sx + (ny - 1) * Sy) / (nx + ny)
  D <- Xbar - Ybar
  as.numeric(t(D) %*% D) - (nx + ny) / (nx * ny) * sum(diag(Sn))
}

#' @rdname test-statistic
#' @export
stat_student_ip <- function(data, indices, validate = FALSE) {
  if (validate && !inherits(data, "dist"))
    abort("The input data for the inter-point Student's statistic should be a dist object.")
  n <- attr(data, "Size")
  indices2 <- seq_len(n)[-indices]
  stat_student_impl(data, indices, indices2)
}

#' @rdname test-statistic
#' @export
stat_t_ip <- stat_student_ip

#' @rdname test-statistic
#' @export
stat_fisher_ip <- function(data, indices, validate = FALSE) {
  if (validate && !inherits(data, "dist"))
    abort("The input data for the inter-point Fisher's variance ratio statistic should be a dist object.")
  n <- attr(data, "Size")
  indices2 <- seq_len(n)[-indices]
  stat_fisher_impl(data, indices, indices2)
}

#' @rdname test-statistic
#' @export
stat_f_ip <- stat_fisher_ip

#' @rdname test-statistic
#' @export
stat_bg_ip <- function(data, indices, validate = FALSE) {
  if (validate && !inherits(data, "dist"))
    abort("The input data for the inter-point Biswas-Ghosh statistic should be a dist object.")
  n <- attr(data, "Size")
  indices2 <- seq_len(n)[-indices]
  stat_bg_impl(data, indices, indices2)
}

#' @rdname test-statistic
#' @export
stat_energy_ip <- function(data, indices, validate = FALSE, alpha = 1) {
  if (validate && !inherits(data, "dist"))
    abort("The input data for the inter-point energy statistic should be a dist object.")
  n <- attr(data, "Size")
  indices2 <- seq_len(n)[-indices]
  stat_energy_impl(data, indices, indices2, alpha)
}

#' @rdname test-statistic
#' @export
stat_cq_ip <- function(data, indices, validate = FALSE) {
  if (validate && !inherits(data, "dist"))
    abort("The input data for the inter-point Chen-Qin statistic should be a dist object.")
  n <- attr(data, "Size")
  indices2 <- seq_len(n)[-indices]
  stat_cq_impl(data, indices, indices2)
}

#' @rdname test-statistic
#' @export
stat_mod_ip <- function(data, indices, validate = FALSE) {
  if (validate && !inherits(data, "dist"))
    abort("The input data for the inter-point Mean-Of-Distances statistic should be a dist object.")
  xy <- data[indices, -indices]
  mean(xy)
}

#' @rdname test-statistic
#' @export
stat_dom_ip <- function(data, indices, validate = FALSE, standardize = TRUE) {
  if (validate && !inherits(data, "dist"))
    abort("The input data for the inter-point Distance-Of-Medoids statistic should be a dist object.")

  n <- attr(data, "Size")
  n1 <- length(indices)
  n2 <- n - n1

  ssd1_vec <- numeric(n1)
  for (i in seq_along(indices))
    ssd1_vec[i] <- sum(data[indices[i], indices]^2)
  km1 <- indices[which.min(ssd1_vec)]

  ssd2_vec <- numeric(n2)
  indices <- seq_len(n)[-indices]
  for (i in seq_along(indices))
    ssd2_vec[i] <- sum(data[indices[i], indices]^2)
  km2 <- indices[which.min(ssd2_vec)]

  stat <- data[km1, km2]

  if (!standardize)
    return(stat)

  ssd1 <- min(ssd1_vec)
  ssd2 <- min(ssd2_vec)
  pooled_variance <- (ssd1 + ssd2) / (n - 2)
  stat / sqrt(pooled_variance)
}
