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
  n_params <- length(statistic)
  null_specification <- rlang::as_function(null_specification)

  if (is.null(seed)) {
    seed <- 1234
    cli::cli_alert_info(
      "Setting the seed for sampling permutations is mandatory for obtaining a continuous p-value function. Using `seed = 1234`."
    )
  }

  pvf_exact <- setup_pvf(
    null_spec = null_specification,
    x = x,
    y = y,
    statistic = statistic,
    B = B,
    type = type,
    alternative = alternative,
    combine_with = combine_with,
    seed = seed
  )

  if (is.null(max_confidence_level)) return(pvf_exact)

  writeLines(paste("- Computing point estimates for all", n_params, "parameters..."))
  genoud_results <- rgenoud::genoud(
    fn = pvf_exact,
    nvars = n_params,
    max = TRUE,
    pop.size = 50,
    max.generations = 20,
    wait.generations = 5,
    BFGSburnin = 5
  )

  Bmin <- 10 * (n_params - 1 + max_confidence_level) / (1 - max_confidence_level)
  if (B < Bmin) {
    cli::cli_alert_info(paste0(
      "The required number of permutations (B = ",
      B,
      ") is insufficient for reaching the input maximum confidence level (",
      round(max_confidence_level * 100, digits = 2),
      "%). Setting it to B = ",
      Bmin,
      "."
    ))
    B <- as.integer(Bmin)
  }

  writeLines("- Setting up individual null specifications...")
  null_specs <- 1:n_params %>%
    purrr::map(~ function(y, parameters) {
      all_parameters <- genoud_results$par
      all_parameters[.x] <- parameters[1]
      null_specification(y, all_parameters)
    })

  writeLines("- Computing evaluation grid...")
  grid_list <- 1:n_params %>%
    purrr::map(~ {
      null_spec <- null_specs[[.x]]
      stat <- statistic[[.x]]
      pvf_temp <- setup_pvf(
        null_spec = null_spec,
        x = x,
        y = y,
        statistic = stat,
        B = B,
        type = type,
        alternative = "two_tail",
        seed = seed
      )

      writeLines(paste(
        "- Computing a point estimate for parameter",
        .x,
        "..."
      ))
      pe <- two_sample_pe_new(pvf_temp)

      writeLines(paste(
        "- Computing a confidence interval with confidence level",
        max_confidence_level,
        "for parameter",
        .x,
        "..."
      ))
      ci <- two_sample_ci_new(
        pvf_temp,
        alpha = (1 - max_confidence_level) / n_params,
        point_estimate = pe
      )

      c(pe, ci)
    })

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
    purrr::array_tree(margin = 1) %>%
    purrr::map_dbl(pvf_exact)

  writeLines("- Computing Kriging model for interpolation using the DiceKriging::km() function...")
  km <- DiceKriging::km(
    design = x_in,
    response = qnorm(y_in),
    nugget = sqrt(.Machine$double.eps),
    control = list(trace = FALSE),
    covtype = "exp"
  )

  pvf_approx <- function(parameters) {
    interp <- predict(
      km,
      newdata = rbind(parameters),
      type = "UK",
      se.compute = FALSE,
      checkNames = FALSE
    )
    pnorm(interp$mean)
  }

  setup_pvf(
    null_spec = null_specification,
    x = x,
    y = y,
    statistic = statistic,
    B = B,
    type = type,
    alternative = alternative,
    combine_with = combine_with,
    seed = seed,
    pvf = pvf_approx,
    max_confidence_level = max_confidence_level,
    design = x_in,
    response = y_in
  )
}

setup_pvf <- function(null_spec, x, y,
                      statistic = stat_t,
                      B = 1000,
                      type = "exact",
                      alternative = "two_tail",
                      combine_with = "tippett",
                      seed = NULL,
                      pvf = NULL,
                      max_confidence_level = NULL,
                      design = NULL,
                      response = NULL) {
  if (is.null(pvf)) {
    pvf <- function(parameters) {
      withr::local_seed(seed)
      y <- null_spec(y, parameters)
      two_sample_test(
        x = x,
        y = y,
        statistic = statistic,
        B = B,
        type = type,
        alternative = alternative,
        combine_with = combine_with
      )$pvalue
    }
  }

  attr(pvf, "null_spec") <- null_spec
  attr(pvf, "x") <- x
  attr(pvf, "y") <- y
  attr(pvf, "statistic") <- statistic
  attr(pvf, "B") <- B
  attr(pvf, "type") <- type
  attr(pvf, "alternative") <- alternative
  attr(pvf, "combine_with") <- combine_with
  attr(pvf, "seed") <- seed
  attr(pvf, "max_confidence_level") <- max_confidence_level
  attr(pvf, "design") <- design
  attr(pvf, "response") <- response

  class(pvf) <- "pvalue_function"
  pvf
}
