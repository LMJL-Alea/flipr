#' Test Statistics for the (M)ANOVA Problem
#'
#' This is a collection of functions that provide test statistics to be used
#' into the permutation scheme for performing (M)ANOVA. These test statistics can
#' be divided into two categories: traditional statistics that use empirical
#' moments and inter-point statistics that only rely on pairwise dissimilarities
#' between data points.
#'
#' @section Traditional Test Statistics:
#'
#' - [`stat_anova_f()`] implements the F statistic used in traditional (M)ANOVA.
#'
#' @section Inter-Point Test Statistics:
#'
#' - [`stat_anova_f_ip()`] implements a pseudo F statistic based on inter-point
#' distances only as described in Shinohara et al. (2020).
#'
#' @references
#' Chambers, J. M., Freeny, A and Heiberger, R. M. (1992) Analysis of variance;
#' designed experiments. Chapter 5 of Statistical Models in S eds J. M. Chambers
#' and T. J. Hastie, Wadsworth & Brooks/Cole.
#'
#' Krzanowski, W. J. (1988) Principles of Multivariate Analysis. A User's
#' Perspective. Oxford.
#'
#' Hand, D. J. and Taylor, C. C. (1987) Multivariate Analysis of Variance and
#' Repeated Measures. Chapman and Hall.
#'
#' Shinohara, Russell T., et al. "Distance‐based analysis of variance for brain
#' connectivity." Biometrics 76.1 (2020): 257-269.
#'
#' @param data Either a list of the `n` pooled data points or a dissimilarity
#'   matrix stored as a \code{\link[stats]{dist}} object for all inter-point
#'   statistics whose function name should end with `_ip()`.
#' @param memberships An integer vector specifying the membership of each data
#'   point.
#' @param ... Extra parameters specific to some statistics.
#'
#' @return A numeric value storing the value of test statistic given the
#'   (possibly permuted) memberships specified by `memberships`.
#' @name anova-stats
#'
#' @examples
#' npk2 <- npk
#' npk2$foo <- rnorm(24)
#' n <- nrow(npk2)
#' data1 <- purrr::array_tree(npk2$yield, margin = 1)
#' stat_anova_f(data1, npk2$block)
#' data2 <- purrr::array_tree(cbind(npk2$yield, npk2$foo), margin = 1)
#' stat_anova_f(data2, npk2$block)
#' D <- dist(cbind(npk2$yield, npk2$foo))
#' stat_anova_f_ip(D, npk2$block)
NULL

#' @rdname anova-stats
#' @export
stat_anova_f <- function(data, memberships, ...) {
  data <- do.call(rbind, data)
  memberships <- as.factor(memberships)
  if (ncol(data) == 1) {
    fit <- stats::aov(data ~ memberships)
    return(summary(fit)[[1]][1, 4])
  }
  fit <- stats::manova(data ~ memberships)
  summary(fit)$stats[1, 3]
}

#' @rdname anova-stats
#' @export
stat_anova_f_ip <- function(data, memberships, ...) {
  stat_anova_f_ip_impl(data, memberships)
}
