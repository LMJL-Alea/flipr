#' Two-Sample Permutation P-Value Function Graph
#'
#' This function plots the p-value function for a single parameter of interest,
#' estimated via permutations.
#'
#' @inheritParams two_sample_ci
#' @param confidence_interval A length-2 numerical vector providing a confidence
#'   interval for the parameter under investigation to be used for setting the
#'   boundaries of the grid onto which the p-value will be displayed. Default is
#'   `NULL`, in which case the confidence interval is computed via the
#'   \code{\link{two_sample_ci}} function.
#' @param n_grid_in An integer specifying the size of the grid onto which the
#'   p-value function will actually be evaluated. Default is `20L`.
#' @param n_grid_out An integer specifying the size of the grid onto which the
#'   p-value function will be extrapolated for display. Default is `100L`.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
#'
#' @examples
#' x1 <- rnorm(10)
#' x2 <- rnorm(10, mean = 3)
#' null_spec <- function(y, parameters) {y - parameters[1]}
#' \dontrun{
#'  two_sample_viz(
#'     null_specification = null_spec,
#'     x = x1,
#'     y = x2,
#'     statistic = stat_t,
#'     B = 10000
#'   )
#' }
two_sample_viz <- function(null_specification,
                           x, y,
                           alpha = 0.05,
                           statistic = stat_hotelling,
                           B = 1000L,
                           alternative = "two_tail",
                           type = "exact",
                           point_estimate = NULL,
                           confidence_interval = NULL,
                           lower_bound = 0,
                           upper_bound = 1,
                           n_grid_in = 20L,
                           n_grid_out = 100L,
                           seed = NULL) {
  stopifnot(length(statistic) == 1)

  if (is.null(seed)) {
    seed <- 1234
    cli::cli_alert_info(
      "Setting the seed for sampling permutations
      is mandatory for obtaining a continuous p-value
      function. Using `seed = 1234`."
    )
  }

  if (is.null(point_estimate)) {
    point_estimate <- two_sample_pe(
      null_specification = null_specification,
      x = x,
      y = y,
      statistic = statistic,
      B = B,
      alternative = alternative,
      type = type,
      lower_bound = lower_bound,
      upper_bound = upper_bound,
      seed = seed
    )
  }

  if (is.null(confidence_interval)) {
    confidence_interval <- two_sample_ci(
      null_specification = null_specification,
      x = x,
      y = y,
      alpha = alpha,
      statistic = statistic,
      B = B,
      alternative = alternative,
      type = type,
      point_estimate = point_estimate,
      seed = seed
    )
  }

  coef <- 0.5
  min_value <- confidence_interval[1] -
    coef * (point_estimate - confidence_interval[1])
  max_value <- confidence_interval[2] +
    coef * (confidence_interval[2] - point_estimate)

  grid_in <- generate_grid(
    center_value = point_estimate,
    min_value = min_value,
    max_value = max_value,
    n = n_grid_in
  )

  df <- tibble(
    parameter = generate_grid(
      center_value = point_estimate,
      min_value = min_value,
      max_value = max_value,
      n = n_grid_out
    ),
    pvalue = stats::approx(
      x = grid_in,
      y = two_sample_pf(
        parameters = grid_in,
        null_specification = null_specification,
        x = x,
        y = y,
        statistic = stat_t,
        B = B,
        alternative = alternative,
        type = type,
        seed = seed
      ),
      xout = .data$parameter
    )$y
  )

  color_palette <- viridisLite::viridis(3)

  df %>%
    ggplot(aes(.data$parameter, .data$pvalue)) +
    geom_line() +
    labs(
      title = format_title(paste(
        alternative,
        type,
        "p-value function"
      )),
      subtitle = format_title(paste(
        "Using",
        B,
        "randomly sampled permutations from seed",
        seed
      )),
      y = "p-value"
    ) +
    theme_bw() +
    scale_y_continuous(limits = c(0, 1)) +
    geom_vline(
      xintercept = point_estimate,
      color = color_palette[3]
    ) +
    geom_vline(
      xintercept = confidence_interval,
      color = color_palette[2],
      linetype = "dashed"
    ) +
    geom_hline(
      yintercept = alpha,
      color = color_palette[1],
      linetype = "dashed"
    ) +
    geom_area(
      data = df %>%
        subset(
          .data$parameter >= confidence_interval[1] &
            .data$parameter <= confidence_interval[2]
        ),
      fill = color_palette[2],
      alpha = 0.2
    ) +
    scale_x_continuous(
      breaks = round(
        x = c(confidence_interval[1], point_estimate, confidence_interval[2]),
        digits = 3
      )
    )
}

generate_grid <- function(center_value, min_value, max_value, n) {
  stopifnot(center_value > min_value && center_value < max_value)
  c(
    seq(min_value, center_value, len = n / 2 + 1)[1:(n / 2)],
    center_value,
    seq(center_value, max_value, len = n / 2 + 1)[-1]
  )
}

format_title <- function(x) {
  x <- gsub("_", "-", x)
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}
