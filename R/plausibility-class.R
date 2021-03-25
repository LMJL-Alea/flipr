#' R6 Class representing a plausibility function
#'
#' @description A plausibility function is...
#'
#' @export
PlausibilityFunction <- R6::R6Class(
  classname = "PlausibilityFunction",
  public = list(

    #' @field null_spec A function or an R object coercible into a function (via
    #'   `rlang::as_function()`) specifying how the `y` sample should be
    #'   transformed to make it exchangeable with the `x` sample under a null
    #'   hypothesis.
    null_spec = NULL,

    #' @description Change the value of the `null_spec` field.
    #'
    #' @param val New `null_spec` function.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec0 <- function(y, parameters) {y / parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$null_spec
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf$set_null_spec(null_spec)
    #' pf$null_spec
    set_null_spec = function(val) {
      self$null_spec <- val
    },

    #' @field x A vector, matrix or list specifying the observed first sample.
    x = NULL,

    #' @description Change the value of the `x` field.
    #'
    #' @param val New value for the first sample `x`.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$x
    #' x <- rnorm(10)
    #' pf$set_x(x)
    #' pf$x
    set_x = function(val) {
      self$x <- val
    },

    #' @field y A vector, matrix or list specifying the observed second sample.
    y = NULL,

    #' @description Change the value of the `y` field.
    #'
    #' @param val New value for the second sample `y`.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$y
    #' y <- rnorm(10, mean = 4)
    #' pf$set_y(y)
    #' pf$y
    set_y = function(val) {
      self$y <- val
    },

    #' @description Create a new plausibility function object.
    #'
    #' @param null_spec A function or an R object coercible into a function (via
    #'   `rlang::as_function()`) specifying how the `y` sample should be
    #'   transformed to make it exchangeable with the `x` sample under a null
    #'   hypothesis.
    #' @param x A vector, matrix or list specifying the observed first sample.
    #' @param y A vector, matrix or list specifying the observed second sample.
    #' @param seed A numeric value specifying the seed to be used. Default is
    #'   `NULL` in which case `seed = 1234` is used and the user is informed of
    #'   this setting.
    #'
    #' @return A new `PlausibilityFunction` object.
    initialize = function(null_spec, x, y, seed = NULL) {
      self$null_spec <- null_spec
      self$x <- x
      self$y <- y
      if (is.null(seed)) {
        cli::cli_alert_warning(
          "Setting the seed for sampling permutations is mandatory for obtaining a continuous p-value function. Using `seed = 1234`."
        )
        seed <- 1234
      }
      self$seed <- seed
    },

    #' @field statistic A list of functions (or of R objects coercible into
    #'   functions via `rlang::as_function`) specifying the test statistics to
    #'   be used for each inferred parameter. Default is `stat_t`.
    statistic = "stat_t",

    #' @description Change the value of the `statistic` field.
    #'
    #' @param val New value for the list of test statistics to be used for each
    #'   inferred parameter.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$statistic
    #' pf$set_statistic(stat_f)
    #' pf$statistic
    set_statistic = function(val) {
      self$statistic <- val
      private$nparams <- length(val)
    },

    #' @field nperms A numeric value specifying the number of permutations to be
    #'   sampled. Default is `1000L`.
    nperms = 1000L,

    #' @description Change the value of the `nperms` field.
    #'
    #' @param val New value for the number of permutations to be sampled.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$nperms
    #' pf$set_nperms(10000)
    #' pf$nperms
    set_nperms = function(val) {
      self$nperms <- val
    },

    #' @field alternative A string specifying the type of hypothesis testing
    #'   that should be performed. Choices are `"two_tail"`, `"left_tail"` and
    #'   `"right_tail`. Default is `"two_tail"`.
    alternative = "two_tail",

    #' @description Change the value of the `alternative` field.
    #'
    #' @param val New value for the type of alternative hypothesis.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$alternative
    #' pf$set_alternative("right_tail")
    #' pf$alternative
    set_alternative = function(val) {
      self$alternative = val
    },

    #' @field aggregator A string specifying which function should be use to
    #'   aggregate test statistic values when the non-parametric combination
    #'   approach is used (i.e. when several parameters are inferred). Choices
    #'   are `"tippett"` and `"fisher` for now. Default is `"tippett"`.
    aggregator = "tippett",

    #' @description Change the value of the `aggregator` field.
    #'
    #' @param val New value for the function to be used for aggregating multiple
    #'   test statistic values during the non-parametric combination process.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$aggregator
    #' pf$set_aggregator("right_tail")
    #' pf$aggregator
    set_aggregator = function(val) {
      self$aggregator = val
    },

    #' @field seed A numeric value specifying the seed from which permutations are
    #'   randomly sampled. Default is `1234`.
    seed = NULL,

    #' @description Change the value of the `seed` field.
    #'
    #' @param val New value for the seed from which permutations are
    #'   randomly sampled.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$seed
    #' pf$set_seed("0000")
    #' pf$seed
    set_seed = function(val) {
      self$seed <- val
    },

    #' @description Computes an indicator of the plausibility of specific values
    #'   for the parameters of interest in the form of a p-value of an
    #'   hypothesis test against these values.
    #'
    #' @param parameters A vector whose length should match the length of the
    #'   `statistic` field providing specific values of the parameters of
    #'   interest for assessment of their plausibility in the form of a p-value
    #'   of the corresponding hypothesis test.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$get_value(2)
    get_value = function(parameters) {
      withr::local_seed(self$seed)
      y <- self$null_spec(self$y, parameters)
      two_sample_test(
        x = self$x,
        y = y,
        statistic = self$statistic,
        B = self$nperms,
        alternative = self$alternative,
        combine_with = self$aggregator
      )$pvalue
    },

    #' @field ncores An integer value specifying the number of cores to run
    #'   optimization and grid computation in parallel. Defaults to `1L`.
    ncores = 1L,

    #' @description Change the value of the `ncores` field.
    #'
    #' @param val New value for the number of cores to use for optimization and
    #'   grid computation in parallel.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$ncores
    #' pf$set_ncores(2)
    #' pf$ncores
    set_ncores = function(val) {
      self$ncores <- val
    },

    #' @field point_estimates A numeric vector providing point estimates for the
    #'   parameters of interest.
    point_estimates = NULL,

    #' @description Change the value of the `point_estimates` field.
    #'
    #' @param val New value for the point estimates of the parameters of
    #'   interest.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$point_estimates
    #' pf$set_point_estimates(2)
    #' pf$point_estimates
    set_point_estimates = function(val) {
      self$point_estimates <- val
      private$set_univariate_nulls()
    },

    #' @description Updates the `point_estimates` field by maximizing the
    #'   plausibility function, searching for the values of the parameters of
    #'   interest for which plausibility is close to one.
    #'
    #' @param overwrite A boolean specifying whether the `point_estimates` field
    #'   should be updated when it has already been set. Defaults to `FALSE`.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {y - parameters[1]}
    #' pf <- PlausibilityFunction$new(null_spec, x, y)
    #' pf$point_estimates
    #' pf$update_point_estimates()
    #' pf$point_estimates
    #' pf$update_point_estimates()
    update_point_estimates = function(overwrite = FALSE) {
      if (!is.null(self$point_estimates) && !overwrite) {
        cli::cli_alert_warning(
          "Point estimates have already been computed. Run `$update_point_estimates(overwrite = TRUE)` if you want them to be computed again."
        )
        return()
      }
      cli::cli_alert_info("Computing point estimates for all parameters...")
      cl <- FALSE
      if (self$ncores > 1 && requireNamespace("parallel", quietly = TRUE))
        cl <- parallel::makeCluster(self$ncores)
      self$point_estimates <- rgenoud::genoud(
        fn = self$get_value,
        nvars = private$nparams,
        max = TRUE,
        pop.size = 50,
        max.generations = 20,
        wait.generations = 5,
        BFGSburnin = 5,
        print.level = 0,
        cluster = cl
      )$par
      if (self$ncores > 1 && requireNamespace("parallel", quietly = TRUE))
        parallel::stopCluster(cl)
      private$set_univariate_nulls()
    },

    max_confidence = 0.999,

    set_max_confidence = function(val) {
      self$max_confidence <- val
    },

    design = NULL,

    set_design = function(val) {
      self$design <- val
    },

    update_design = function(overwrite = FALSE) {
      if (!is.null(self$design) && !overwrite) {
        cli::cli_alert_warning(
          "Design points have already been computed. Run `$update_design(overwrite = TRUE)` if you want them to be computed again."
        )
        return()
      }

      if (is.null(self$point_estimates))
          self$update_point_estimates()

      grid_list <- 1:private$nparams %>%
        purrr::map(~ {
          null_spec <- private$univariate_nulls[[.x]]
          pvf_temp <- PlausibilityFunction$new(null_spec, self$x, self$y, self$seed)
          pvf_temp$set_statistic(self$statistic[[.x]])
          pvf_temp$set_nperms(self$nperms)
          pvf_temp$set_alternative("two_tail")

          cli::cli_alert_info(paste(
            "Computing a point estimate for parameter",
            .x,
            "..."
          ))
          pe <- two_sample_pe_new(pvf_temp)
          print(pe)

          cli::cli_alert_info(paste(
            "Computing a confidence interval with confidence level",
            self$max_confidence,
            "for parameter",
            .x,
            "..."
          ))
          ci <- two_sample_ci_new(
            pvf_temp,
            alpha = (1 - self$max_confidence) / private$nparams,
            point_estimate = pe
          )

          c(pe, ci)
        })

      grid_size <- 20
      grid_out <- (grid_size + 1)^private$nparams

      if (grid_out > 1000)
        abort(
          "The output grid size is too large and will take too long to compute. Please consider either a coarser grid or inferring less parameters or reducing the number of permutations (it is though recommended that B > 1000)."
        )
      self$design <- generate_grid(grid_list, n = grid_size)
    },

    response = NULL,

    set_response = function() {
      if (is.null(self$design))
        self$update_design()
      cli::cli_alert_info(paste(
        "Computing the exact plausibility function on a grid of size",
        nrow(self$design),
        "..."
      ))
      if (requireNamespace("progressr", quietly = TRUE)) {
        p <- progressr::progressor(along = 1:nrow(self$design))
      }
      if (self$ncores > 1 && requireNamespace("furrr", quietly = TRUE)) {
        self$response <- self$design %>%
          purrr::array_tree(margin = 1) %>%
          furrr::future_map_dbl(~ {
            if (requireNamespace("progressr", quietly = TRUE)) {
              p()
            }
            self$get_value(.x)
          })
      } else {
        self$response <- self$design %>%
          purrr::array_tree(margin = 1) %>%
          purrr::map_dbl(~ {
            if (requireNamespace("progressr", quietly = TRUE)) {
              p()
            }
            self$get_value(.x)
          })
      }
    },

    get_prediction = function(parameters) {
      if (is.null(private$kriging_model))
        private$fit_kriging_model()
      interp <- predict(
        private$kriging_model,
        newdata = rbind(parameters),
        type = "UK",
        se.compute = FALSE,
        checkNames = FALSE
      )
      pnorm(interp$mean)
    }
  ),
  private = list(
    nparams = 1,

    univariate_nulls = NULL,
    set_univariate_nulls = function() {
      private$univariate_nulls <- 1:private$nparams %>%
        purrr::map(~ function(y, parameters) {
          all_parameters <- self$point_estimates
          all_parameters[.x] <- parameters[1]
          self$null_spec(y, all_parameters)
        })
    },

    kriging_model = NULL,
    fit_kriging_model = function() {
      if (is.null(self$design))
        self$update_design()
      if (is.null(self$response))
        self$set_response()

      cli::cli_alert_info("Computing Kriging model for interpolation using the DiceKriging::km() function...")
      private$kriging_model <- DiceKriging::km(
        design = self$design,
        response = qnorm(self$response),
        nugget = sqrt(.Machine$double.eps),
        control = list(trace = FALSE),
        covtype = "exp"
      )
    }
  )
)
