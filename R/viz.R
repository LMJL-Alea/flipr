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
plot_pf <- function(pf, ngrid = 100, ncores = 1) {
  if (pf$nparams > 2)
    abort("Only one- or two-dimensional plausibility functions can currently be plotted.")

  for (i in 1:pf$nparams) {
    rngs <- dials::range_get(pf$param_list[[i]], original = FALSE)
    if (dials::is_unknown(rngs[1]) || dials::is_unknown(rngs[2]))
      abort("Ranges for parameters are not set. Consider running the `$set_parameter_bounds()` method.")
  }

  df <- pf$param_list |>
    # dials::grid_latin_hypercube(size = ngrid) |>
    dials::grid_regular(levels = 10) |>
    tibble::add_row(
      pf$point_estimates |>
        set_names(names(pf$param_list)) |>
        tibble::as_tibble_row()
    )
  df$pvalue <- df |>
    purrr::array_tree(margin = 1) |>
    pbapply::pbsapply(pf$get_value)

  return(df)

  color_palette <- viridisLite::viridis(3)

  if (pf$nparams == 1) {
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
  } else {
    df |>
      ggplot(aes_string(names(pf$param_list)[1], names(pf$param_list)[2], z = "pvalue")) +
      geom_contour_filled(binwidth = 0.05) +
      labs(
        title = "Contour plot of the p-value surface",
        subtitle = "Using Tippett's non-parametric combination",
        x = expression(delta),
        y = expression(rho),
        fill = "p-value"
      ) +
      theme_minimal()
  }
}

format_title <- function(x) {
  x <- gsub("_", "-", x)
  paste0(toupper(substring(x, 1, 1)), tolower(substring(x, 2)))
}
