#' Visualization of Plausibility Functions
#'
#' This function plots the plausibility function for up to two parameters of
#' interest.
#'
#' @param pf A \code{\link{PlausibilityFunction}} object.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
#'
#' @examples
#' x <- rnorm(10)
#' y <- rnorm(10, mean = 2)
#' null_spec <- function(y, parameters) {purrr::map(y, ~ .x - parameters[1])}
#' stat_functions <- list(stat_t)
#' stat_assignments <- list(mean_param = 1)
#' pf <- PlausibilityFunction$new(
#'   null_spec = null_spec,
#'   stat_functions = stat_functions,
#'   stat_assignments = stat_assignments,
#'   x, y
#' )
#' plot_pf(pf)
plot_pf <- function(pf, ngrid = 10, ncores = 1, subtitle = "") {
  if (pf$nparams > 2)
    abort("Only one- or two-dimensional plausibility functions can currently be plotted.")

  if (is.null(pf$grid))
    abort("The plausbility function has not yet been evaluated on a grid of parameters. Consider running the `$set_grid()` method first.")

  color_palette <- viridisLite::viridis(3)

  if (pf$nparams == 1) {
    pf$grid %>%
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
  } else {
    nm <- names(pf$param_list)
    pf$grid |>
      ggplot(aes_string(nm[1], nm[2], z = "pvalue")) +
      geom_contour_filled(binwidth = 0.05) +
      labs(
        title = "Contour plot of the plausibility function",
        subtitle = subtitle,
        x = pf$param_list[[1]]$label,
        y = pf$param_list[[2]]$label,
        fill = "p-value"
      ) +
      theme_minimal()
  }
}

format_title <- function(x) {
  x <- gsub("_", "-", x)
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}
