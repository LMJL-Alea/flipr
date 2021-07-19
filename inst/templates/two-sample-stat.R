#' Test Statistic for the Two-Sample Problem
#'
#' This function computes the test statistic...
#'
#' @param data A list storing the concatenation of the two samples from which
#'   the user wants to make inference. Alternatively, a distance matrix stored
#'   in an object of class \code{\link[stats]{dist}} of pairwise distances
#'   between data points.
#' @param indices1 An integer vector that contains the indices of the data
#'   points belong to the first sample in the current permuted version of the
#'   data.
#'
#' @return A numeric value evaluating the desired test statistic.
#' @export
#'
#' @examples
#' # TO BE DONE BY THE DEVELOPER OF THE PACKAGE
stat_{{{name}}} <- function(data, indices1) {
  n <- if (inherits(data, "dist"))
    attr(data, "Size")
  else if (inherits(data, "list"))
    length(data)
  else
    stop("The `data` input should be of class either list or dist.")

  indices2 <- seq_len(n)[-indices1]

  x <- data[indices1]
  y <- data[indices2]

  # Here comes the code that computes the desired test
  # statistic from input samples stored in lists x and y

}
