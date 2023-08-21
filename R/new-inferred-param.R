new_inferred_param <- function(label,
                               type = c("double", "integer"),
                               point_estimate = dials::unknown(),
                               range = c(dials::unknown(), dials::unknown()),
                               inclusive = c(TRUE, TRUE),
                               trans = NULL,
                               values = NULL,
                               finalize = NULL) {
  res <- dials::new_quant_param(
    type = type,
    range = range,
    inclusive = inclusive,
    trans = trans,
    values = values,
    label = label,
    finalize = finalize
  )
  point_estimate_validate(point_estimate, range, inclusive)
  res$point_estimate <- point_estimate
  class(res) <- c("inferred_param", class(res))
  res
}

point_estimate_validate <- function(x,
                                    range = c(dials::unknown(), dials::unknown()),
                                    inclusive = c(TRUE, TRUE)) {
  if (dials::is_unknown(x)) return(TRUE)

  check_lower <- if (dials::is_unknown(range[1]))
    TRUE
  else {
    if (inclusive[1]) x >= range[1] else x > range[1]
  }

  check_upper <- if (dials::is_unknown(range[2]))
    TRUE
  else {
    if (inclusive[2]) x <= range[2] else x < range[2]
  }

  check_lower && check_upper
}

#' @export
print.inferred_param <- function(x, digits = 3, ...)
{
  class(x) <- class(x)[-1]
  print(x, digits = digits)
  cat_inferred_param_point_estimate(x, digits = digits)
  invisible(x)
}

cat_inferred_param_point_estimate <- function(param, digits = 3) {
  label <- if (!is.null(param$trans))
    "Point estimate (transformed scale): "
  else
    "Point estimate: "
  value <- if (!dials::is_unknown(param$point_estimate))
    format(param$point_estimate, digits = digits)
  else
    "?"
  cat(paste0(label, value, "\n", collapse = ""))
}
