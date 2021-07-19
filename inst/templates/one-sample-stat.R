#' Test Statistic for the One-Sample Problem
#'
#' This function computes the test statistic...
#'
#' @param data A list storing the sample from which the user wants to make
#'   inference.
#' @param flips A numeric vectors of `-1`s and `1`s to be used to randomly flip
#'   some data points around the center of symmetric of the distribution of the
#'   sample.
#'
#' @return A numeric value evaluating the desired test statistic.
#' @export
#'
#' @examples
#' # TO BE DONE BY THE DEVELOPER OF THE PACKAGE
stat_{{{name}}} <- function(data, flips) {
  stopifnot(inherits(data, "list"))
  n <- length(data)

  # Here comes the code that computes the desired test
  # statistic from input sample stored in list x

}
