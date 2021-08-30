#' Test Statistic Template
#'
#' This function is a helper to automatically generate an `.R` file populated
#' with a skeleton of a typical test function compatible with `flipr`.
#'
#' @param nsamples An integer specifying the number of samples to be used.
#'   Defaults to `1L`. Currently only works for one- or two-sample problems.
#' @param stat_name A string specifying the name of the test statistic that is
#'   being implemented. Defaults to `mystat`.
#'
#' @return Creates a dedicated `.R` file with a template of code for the
#'   function that implements the test statistic and saves it to the `R/` folder
#'   of your package.
#' @export
#'
#' @examples
#' \dontrun{
#' use_stat()
#' }
use_stat <- function(nsamples = 1, stat_name = "mystat") {
  # setup file name replacing spaces or _ with -
  file_name <- gsub("[_ ]", "-", stat_name)
  # setup stat name replacing spaces or - with _
  stat_name <- gsub("[- ]", "_", stat_name)
  if (nsamples == 1) {
    usethis::use_template(
      template = "one-sample-stat.R",
      save_as = paste0("R/", file_name, ".R"),
      data = list(name = stat_name),
      open = TRUE,
      package = "flipr"
    )
  } else {
    usethis::use_template(
      template = "two-sample-stat.R",
      save_as = paste0("R/", file_name, ".R"),
      data = list(name = stat_name),
      open = TRUE,
      package = "flipr"
    )
  }
}
