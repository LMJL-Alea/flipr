#' Two-Sample Permutation P-Value Function
#'
#' This function calculates the permutation p-value function for two-sample
#' problems. This is done through the specification of a set of null hypotheses
#' of the form `F_X = F_{g(Y, parameters)}` where `g` is a user-supplied
#' function.
#'
#' @param parameters A list of vectors specifying a set of candidate parameter
#'   values under the null hypothesis.
#' @param null_specification A function with two arguments `y` and `parameters`
#'   such that `F_X = F_{null_specification(Y, parameters)}` under the null
#'   hypothesis.
#' @inheritParams two_sample_test
#'
#' @return A vector of p-values of the permutation test for each parameter
#'   values specified through the first argument.
#' @export
#'
#' @examples
#' x1 <- rnorm(10)
#' x2 <- rnorm(10, 3)
#' null_spec <- function(y, parameters) {y - parameters[1]}
#' two_sample_pf(
#'   parameters = 3,
#'   null_specification = null_spec,
#'   x = x1,
#'   y = x2,
#'   statistic = stat_t,
#'   seed = 1234,
#'   B = 1000,
#'   alternative = "two_tail"
#' )
two_sample_pf <- function(parameters,
                          null_specification,
                          x, y,
                          statistic = stat_hotelling,
                          B = 1000L,
                          alternative = "right_tail",
                          combine_with = "tippett",
                          type = "exact",
                          seed = NULL) {
  if (!is.null(seed)) withr::local_seed(seed)
  null_spec <- rlang::as_function(null_specification)
  parameters %>%
    purrr::map_dbl(~ {
      y <- null_spec(y, .x)
      two_sample_test(
        x = x,
        y = y,
        statistic = statistic,
        B = B,
        type = type,
        alternative = alternative,
        combine_with = combine_with
      )$pvalue
    })
}

two_sample_pf_new <- function(null_specification,
                              x, y,
                              max_confidence_level = NULL,
                              statistic = stat_hotelling,
                              B = 1000L,
                              alternative = "right_tail",
                              combine_with = "tippett",
                              type = "exact",
                              seed = NULL) {
  if (!rlang::is_list(null_specification))
    abort("The null_specification argument should be a list.")

  if (!rlang::is_dictionaryish(null_specification))
    abort("The null_specification list should be named with a distinct name for each entry.")

  if (length(null_specification) == 1) {
    param_name <- names(null_specification)
    null_specification <- c(null_specification, null_specification)
    names(null_specification) <- c("npc", param_name)
  }

  n_params <- length(statistic)
  if (length(null_specification) != n_params + 1)
    abort("The number of null specification functions does not match the number of test statistics.")

  null_specs <- purrr::map(null_specification, rlang::as_function)

  if (is.null(seed)) {
    seed <- 1234
    cli::cli_alert_info(
      "Setting the seed for sampling permutations is mandatory for obtaining a continuous p-value function. Using `seed = 1234`."
    )
  }

  pvf_exact <- setup_pvf(
    seed = seed,
    null_spec = null_specs[[1]],
    x = x,
    y = y,
    statistic = statistic,
    B = B,
    type = type,
    alternative = alternative,
    combine_with = combine_with
  )

  if (is.null(max_confidence_level)) return(pvf_exact)

  param_names <- names(null_specs[-1])
  grid_list <- purrr::pmap(
    .l = list(null_specs[-1], param_names, statistic),
    .f = ~ {
      null_spec <- ..1
      param_name <- ..2
      stat <- ..3
      pvf_temp <- setup_pvf(
        seed = seed,
        null_spec = null_spec,
        x = x,
        y = y,
        statistic = stat,
        B = B,
        type = type,
        alternative = "two_tail"
      )

      writeLines(paste(
        "- Computing a point estimate for parameter",
        param_name,
        "..."
      ))
      pe <- two_sample_pe_new(pvf_temp)
      print(pe)

      writeLines(paste(
        "- Computing a confidence interval with confidence level",
        max_confidence_level,
        "for parameter",
        param_name,
        "..."
      ))
      ci <- two_sample_ci_new(
        pvf_temp,
        alpha = 1 - max_confidence_level,
        point_estimate = pe
      )
      print(ci)

      c(pe, ci)
    }
  )

  grid_size <- 20
  grid_out <- (grid_size + 1)^n_params

  if (grid_out > 1000)
    abort(
      "The output grid size is too large and will take too long to compute. Please consider either a coarser grid or inferring less parameters or reducing the number of permutations (it is though recommended that B > 1000)."
    )

  writeLines(paste(
    "- Computing the exact p-value function on a grid of size",
    grid_out,
    "..."
  ))
  x_in <- generate_grid(grid_list, n = grid_size)
  y_in <- x_in %>%
    as.list() %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    pvf_exact()
  # or avoid going through list if x_in was a matrix -> apply(x_in, 1, pvf_exact)
  # return(list(x_in, y_in))

  writeLines("- Computing Kriging model for interpolation using the DiceKriging::km() function...")
  km <- DiceKriging::km(
    design = x_in,
    response = qnorm(y_in),
    nugget = sqrt(.Machine$double.eps),
    control = list(trace = FALSE),
    covtype = "exp"
  )

  pvf_approx <- function(parameters) {
    df <- parameters %>%
      purrr::transpose() %>%
      purrr::simplify_all() %>%
      purrr::set_names(param_names) %>%
      tibble::as_tibble()
    interp <- predict(km, newdata = df, type = "UK", se.compute = FALSE)
    pnorm(interp$mean)
  }

  x_in$pvalue <- y_in

  attr(pvf_approx, "x") <- x
  attr(pvf_approx, "y") <- y
  attr(pvf_approx, "statistic") <- statistic
  attr(pvf_approx, "B") <- B
  attr(pvf_approx, "type") <- type
  attr(pvf_approx, "alternative") <- alternative
  attr(pvf_approx, "combine_with") <- combine_with
  attr(pvf_approx, "seed") <- seed
  attr(pvf_approx, "max_confidence_level") <- max_confidence_level
  attr(pvf_approx, "grid") <- x_in

  class(pvf_approx) <- "pvalue_function"
  pvf_approx
}

setup_pvf <- function(null_spec, x, y, statistic = stat_t, B = 1000, type = "exact", alternative = "two_tail", combine_with = "tippett", max_confidence_level = NULL, point_estimate = NULL, confidence_interval = NULL, grid_size = NULL, seed = NULL) {
  pvf <- function(parameters) {
    withr::local_seed(seed)
    parameters %>%
      purrr::map_dbl(~ {
        y <- null_spec(y, .x)
        two_sample_test(
          x = x,
          y = y,
          statistic = statistic,
          B = B,
          type = type,
          alternative = alternative,
          combine_with = combine_with
        )$pvalue
      })
  }

  attr(pvf, "x") <- x
  attr(pvf, "y") <- y
  attr(pvf, "statistic") <- statistic
  attr(pvf, "B") <- B
  attr(pvf, "type") <- type
  attr(pvf, "alternative") <- alternative
  attr(pvf, "combine_with") <- combine_with
  attr(pvf, "seed") <- seed

  if (!is.null(max_confidence_level))
    attr(pvf, "max_confidence_level") <- max_confidence_level
  if (!is.null(point_estimate))
    attr(pvf, "point_estimate") <- point_estimate
  if (!is.null(confidence_interval))
    attr(pvf, "confidence_interval") <- confidence_interval
  if (!is.null(grid_size))
  attr(pvf, "grid_size") <- grid_size

  class(pvf) <- "pvalue_function"
  pvf
}
