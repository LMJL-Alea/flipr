#' R6 Class representing a plausibility function
#'
#' @description A plausibility function is...
#'
#' @export
PlausibilityFunction <- R6::R6Class(
  classname = "PlausibilityFunction",
  public = list(
    #' @field param_list A list of functions of class `param` produced via
    #'   \code{\link[dials]{new_quant_param}} that stores the parameters to be
    #'   inferred along with important properties such as their name, range,
    #'   etc. Defaults to `NULL`.
    param_list = NULL,

    #' @description Create a new plausibility function object.
    #'
    #' @param null_spec A function or an R object coercible into a function (via
    #'   `rlang::as_function()`). For one-sample problems, it should transform
    #'   the `x` sample (provided as first argument) using the parameters (as
    #'   second argument) to make its distribution centered symmetric. For
    #'   two-sample problems, it should transform the `y` sample (provided as
    #'   first argument) using the parameters (as second argument) to make it
    #'   exchangeable with the `x` sample under a null hypothesis.
    #' @param stat_functions A vector or list of functions (or R objects
    #'   coercible into functions via `rlang::as_function()`) specifying the
    #'   whole set of test statistics that should be used.
    #' @param stat_assignments A named list of integer vectors specifying which
    #'   test statistic should be associated with each parameter. The length of
    #'   this list should match the number of parameters under investigation and
    #'   is thus used to set it. Each element of the list should be named after
    #'   the parameter it identifies.
    #' @param ... Vectors, matrices or lists providing the observed samples.
    #' @param seed A numeric value specifying the seed to be used. Defaults to
    #'   `NULL` in which case `seed = 1234` is used and the user is informed of
    #'   this setting.
    #'
    #' @return A new `PlausibilityFunction` object.
    initialize = function(null_spec,
                          stat_functions,
                          stat_assignments,
                          ...,
                          seed = NULL) {
      if (!is_function(null_spec))
        abort("The `null_spec` argument should be of class `function`.")
      private$set_null_spec(null_spec)

      private$set_stat_functions(stat_functions)

      if (!is_list(stat_assignments))
        abort("The `stat_assignements` argument should be of class `list`.")
      if (!is_named(stat_assignments))
        abort("The `stat_assignments` list should be a named list`.")
      private$set_stat_assignments(stat_assignments)
      private$set_nparams(length(stat_assignments))

      private$set_data(...)

      param_names <- names(stat_assignments)
      self$param_list <- list2()
      for (param in param_names) {
        param_label <- strsplit(param, "_")[[1]]
        param_label <- param_label[-length(param_label)]
        param_label <- paste(param_label, collapse = " ")
        param_label <- toupper(param_label)

        self$param_list <- c(
          self$param_list,
          list2(!!param := eval_tidy(expr(dials::new_quant_param(
              type = "double",
              range = c(dials::unknown(), dials::unknown()),
              inclusive = c(TRUE, TRUE),
              label = unlist(list2(!!param := !!param_label)),
              finalize = get_ci
            )
          )))
        )
      }

      if (is.null(seed)) {
        cli::cli_alert_warning(
          "Setting the seed for sampling permutations is mandatory for obtaining a continuous p-value function. Using `seed = 1234`."
        )
        seed <- 1234
      }
      private$seed <- seed
    },

    #' @field nparams An integer specifying the number of parameters to be
    #'   inferred. Default is `1L`.
    nparams = 1,

    #' @field nperms An integer specifying the number of permutations to be
    #'   sampled. Default is `1000L`.
    nperms = 1000L,

    #' @description Change the value of the `nperms` field.
    #'
    #' @param val New value for the number of permutations to be sampled.
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
    #' pf$nperms
    #' pf$set_nperms(10000)
    #' pf$nperms
    set_nperms = function(val) {
      self$nperms <- val
    },

    #' @field nperms_max An integer specifying the total number of distinct
    #'   permutations that can be made given the sample sizes.
    nperms_max = NULL,

    #' @description Change the value of the `nperms_max` field.
    #'
    #' @param val New value for the total number of of possible distinct
    #'   permutations.
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
    #' pf$nperms_max
    #' pf$set_nperms(10000)
    #' pf$nperms_max
    set_nperms_max = function(val) {
      self$nperms_max <- val
    },

    #' @field alternative A string specifying the type of alternative
    #'   hypothesis. Choices are `"two_tail"`, `"left_tail"` and `"right_tail`.
    #'   Defaults to `"two_tail"`.
    alternative = "two_tail",

    #' @description Change the value of the `alternative` field.
    #'
    #' @param val New value for the type of alternative hypothesis.
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
    #' pf$alternative
    #' pf$set_alternative("right_tail")
    #' pf$alternative
    set_alternative = function(val) {
      if (!(val %in% private$alternative_choices))
        abort(paste0(
          "The `alternative` argument should be one of ",
          private$alternative_choices,
          "."
        ))
      self$alternative <- val
    },

    #' @field aggregator A string specifying which function should be used to
    #'   aggregate test statistic values when non-parametric combination is used
    #'   (i.e. when multiple test statistics are used). Choices are `"tippett"`
    #'   and `"fisher` for now. Defaults to `"tippett"`.
    aggregator = "tippett",

    #' @description Change the value of the `aggregator` field.
    #'
    #' @param val New value for the string specifying which function should be
    #'   used to aggregate test statistic values when non-parametric combination
    #'   is used (i.e. when multiple test statistics are used).
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
    #' pf$aggregator
    #' pf$set_aggregator("fisher")
    #' pf$aggregator
    set_aggregator = function(val) {
      if (!(val %in% private$aggregator_choices))
        abort(paste0(
          "The `aggregator` argument should be one of ",
          private$aggregator_choices,
          "."
        ))
      self$aggregator <- val
    },

    #' @description Computes an indicator of the plausibility of specific values
    #'   for the parameters of interest in the form of a p-value of an
    #'   hypothesis test against these values.
    #'
    #' @param parameters A vector whose length should match the `nparams` field
    #'   providing specific values of the parameters of interest for assessment
    #'   of their plausibility in the form of a p-value of the corresponding
    #'   hypothesis test.
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
    #' pf$get_value(2)
    get_value = function(parameters) {
      if (length(parameters) != self$nparams)
        abort(paste0(
          "The plausibility function has been defined to infer ",
          self$nparams,
          " parameters and you are trying to evaluate it for a vector of parameters of length ",
          length(parameters),
          "."
        ))
      withr::local_seed(private$seed)
      if (private$nsamples == 1) {
        x <- private$null_spec(private$data[[1]], parameters)
        one_sample_test(
          x = x,
          stats = private$stat_functions,
          B = self$nperms,
          M = self$nperms_max,
          alternative = self$alternative,
          combine_with = self$aggregator
        )$pvalue
      } else {
        y <- private$null_spec(private$data[[2]], parameters)
        two_sample_test(
          x = private$data[[1]],
          y = y,
          stats = private$stat_functions,
          B = self$nperms,
          M = self$nperms_max,
          alternative = self$alternative,
          combine_with = self$aggregator
        )$pvalue
      }
    },

    #' @field ncores An integer value specifying the number of cores to use for:
    #' - maximizing the plausibility function to compute point estimates of the
    #' parameters of interest; and,
    #' - computing the exact plausibility function on a sensible grid of
    #' parameter values.
    #'
    #' Defaults to `1L`.
    ncores = 1L,

    #' @field cluster A cluster object created via
    #'   \code{\link[parallel]{makeCluster}}. Defaults to `FALSE` in which case
    #'   no cluster is created and computations are run sequentially.
    cluster = FALSE,

    #' @description Change the value of the `ncores` field.
    #'
    #' @param val New value for the number of cores to use for parallel
    #'   computations.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean_param = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$ncores
    #' pf$cluster
    #' pf$set_ncores(2)
    #' pf$ncores
    #' pf$cluster
    set_ncores = function(val) {
      self$ncores <- val
      self$cluster <- parallel::makeCluster(self$ncores)
    },

    #' @field point_estimates A numeric vector providing point estimates for the
    #'   parameters of interest.
    point_estimates = NULL,

    #' @description Change the value of the `point_estimates` field.
    #'
    #' @param val A numeric vector providing rough point estimates for the
    #'   parameters under investigation.
    #' @param lower_bound A scalar or numeric vector specifying the lower bounds
    #'   for each parameter under investigation. If it is a scalar, the value is
    #'   used as lower bound for all parameters. Defaults to `-10`.
    #' @param upper_bound A scalar or numeric vector specifying the lower bounds
    #'   for each parameter under investigation. If it is a scalar, the value is
    #'   used as lower bound for all parameters. Defaults to `10`.
    #' @param ncores An integer specifying the number of cores to use for
    #'   maximizing the plausibility function to get a point estimate of the
    #'   parameters. Defaults to `1L`.
    #' @param estimate A boolean specifying whether the rough point estimate
    #'   provided by `val` should serve as initial point for maximizing the
    #'   plausibility function (`estimate = TRUE`) or as final point estimate
    #'   for the parameters (`estimate = FALSE`). Defaults to `FALSE`.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean_param = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$point_estimates
    #' pf$set_point_estimates(mean(y) - mean(x))
    #' pf$point_estimates
    set_point_estimates = function(val = NULL,
                                   lower_bound = -10,
                                   upper_bound =  10,
                                   ncores = 1L,
                                   estimate = FALSE) {
      if (!is.null(val) && !estimate) {
        self$point_estimates <- val
      } else {
        if (length(lower_bound) == 1)
          lower_bound <- rep(lower_bound, self$nparams)
        if (length(upper_bound) == 1)
          upper_bound <- rep(upper_bound, self$nparams)
        opt <- compute_point_estimate(
          pf = self,
          guess = val,
          lower_bound = lower_bound,
          upper_bound = upper_bound,
          ncores = ncores
        )
        self$point_estimates <- opt$par
      }
      private$set_univariate_nulls()
    },

    #' @field max_conf_level A numeric value specifying the maximum confidence
    #'   level that we aim to achieve for the confidence regions. This is used
    #'   to compute bounds on each parameter of interest in order to fit a
    #'   Kriging model that approximates the expensive plausibility function on
    #'   a hypercube. Defaults to `0.99`.
    max_conf_level = 0.99,

    #' @description Change the value of the `max_conf_level` field.
    #'
    #' @param val New value for the maximum confidence level that we aim to
    #'   achieve for the confidence regions.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean_param = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$max_conf_level
    #' pf$set_max_conf_level(0.999)
    #' pf$max_conf_level
    set_max_conf_level = function(val) {
      self$max_conf_level <- val
    },

    #' @description Change the value of the `param_list` field.
    #'
    #' Updates the range of the parameters under investigation.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean_param = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$set_point_estimates(mean(y) - mean(x))
    #' pf$param_list
    #' pf$set_parameter_bounds()
    #' pf$param_list
    set_parameter_bounds = function() {
      if (is.null(self$point_estimates)) {
        abort("No point estimates are available. Please run the `$set_point_estimates()` method first.")
      }

      for (param_index in 1:self$nparams) {
        pvf_temp <- PlausibilityFunction$new(
          null_spec = private$univariate_nulls[[param_index]],
          stat_functions = private$stat_functions,
          stat_assignments = private$stat_assignments[param_index],
          !!!private$data,
          seed = private$seed
        )
        pvf_temp$set_nperms(self$nperms)
        pvf_temp$set_alternative(self$alternative)

        pe <- self$point_estimates[param_index]
        conf_level <- 1 - (1 - self$max_conf_level) / self$nparams

        cli::cli_alert_info(paste0(
          "Computing a confidence interval with confidence level ",
          conf_level,
          " for parameter ",
          names(self$param_list)[param_index],
          "..."
        ))

        self$param_list[[param_index]] <- dials::finalize(
          object = self$param_list[[param_index]],
          pf = pvf_temp,
          conf_level = conf_level,
          point_estimate = pe
        )
      }
    },

    #' @field grid A tibble storing evaluations of the plausibility function on
    #'   a regular centered grid of the parameter space. Defaults to `NULL`.
    grid = NULL,

    #' @description Computes a tibble storing a regular centered grid of the
    #'   parameter space.
    #'
    #' @param npoints An even integer specifying the number of points to
    #'   discretize each dimension. Defaults to `20L`.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean_param = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$set_point_estimates(mean(y) - mean(x))
    #' pf$set_parameter_bounds()
    #' pf$set_grid(npoints = 2L)
    set_grid = function(npoints = 20L) {
      for (i in 1:self$nparams) {
        rngs <- dials::range_get(self$param_list[[i]], original = FALSE)
        if (dials::is_unknown(rngs$lower) || dials::is_unknown(rngs$upper))
          abort("Ranges for parameters are not set. Consider running the `$set_parameter_bounds()` method first.")
      }
      self$grid <- grid_centered(self$param_list, self$point_estimates, levels = npoints)
    },

    #' @description Updates the `grid` field with a `pvalue` column storing
    #'   evaluations of the plausibility function on the regular centered grid
    #'   of the parameter space.
    #'
    #' @param ncores An integer specifying the number of cores to run
    #'   evaluations in parallel. Defaults to `1L`.
    #'
    #' @examples
    #' x <- rnorm(10)
    #' y <- rnorm(10, mean = 2)
    #' null_spec <- function(y, parameters) {
    #'   purrr::map(y, ~ .x - parameters[1])
    #' }
    #' stat_functions <- list(stat_t)
    #' stat_assignments <- list(mean_param = 1)
    #' pf <- PlausibilityFunction$new(
    #'   null_spec = null_spec,
    #'   stat_functions = stat_functions,
    #'   stat_assignments = stat_assignments,
    #'   x, y
    #' )
    #' pf$set_point_estimates(mean(y) - mean(x))
    #' pf$set_parameter_bounds()
    #' pf$set_grid(npoints = 2L)
    #' pf$evaluate_grid()
    evaluate_grid = function(ncores = 1L) {
      if (is.null(self$grid))
        abort("There is no grid set up for evaluation yet. Consider running the `$set_grid()` method first.")
      cl <- NULL
      if (ncores > 1)
        cl <- parallel::makeCluster(ncores)
      self$grid$pvalue <- self$grid |>
        purrr::array_tree(margin = 1) |>
        pbapply::pbsapply(self$get_value, cl = cl)
      if (ncores > 1L)
        parallel::stopCluster(cl)
    },

    #' @description Fit a Kriging model to approximate the plausibility
    #'   function.
    set_kriging_model = function() {
      if (is.null(self$grid))
        abort("There is no `grid` field to estimate the Kriging model from. Please consider running the `$set_grid()` method first.")
      cli::cli_alert_info("Computing Kriging model for interpolation using the `DiceKriging::km()` function...")
      resp <- qnorm(self$grid$pvalue)
      valid_points <- is.finite(resp)
      resp <- resp[valid_points]
      design <- subset(self$grid, select = -pvalue)[valid_points, ]
      private$kriging_model <- DiceKriging::km(
        design = design,
        response = resp,
        nugget = sqrt(.Machine$double.eps),
        control = list(trace = FALSE),
        covtype = "exp"
      )
    },

    #' @description Computes an indicator of the plausibility of specific values
    #'   for the parameters of interest in the form of a p-value of an
    #'   hypothesis test against these values. The computation is an
    #'   approximation obtained by using a Kriging model to approximate the
    #'   exact plausibility function on a hypercube.
    #'
    #' @param parameters A vector whose length should match the `nparams` field
    #'   providing specific values of the parameters of interest for assessment
    #'   of their plausibility in the form of a p-value of the corresponding
    #'   hypothesis test.
    #'
    #' @examples
    #' # TO DO
    get_prediction = function(parameters) {
      if (is.null(private$kriging_model)) {
        abort("No Kriging model is available for the computation of predictions. Consider fitting one using the method `$set_kriging_model()` first.")
      }
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
    null_spec = NULL,
    set_null_spec = function(val) {
      private$null_spec <- rlang::as_function(val)
    },

    data = NULL,
    nsamples = 2,
    set_data = function(...) {
      private$data <- convert_to_list(...)
      private$nsamples <- length(private$data)
      if (private$nsamples > 2)
        abort("The PlausibilityFunction class currently only support one- and two-sample problems.")
    },

    set_nparams = function(val) {
      self$nparams <- val
    },

    stat_functions = NULL,
    set_stat_functions = function(val) {
      private$stat_functions <- purrr::map(val, rlang::as_function)
    },

    seed = 1234,

    alternative_choices = c("two_tail", "left_tail", "right_tail"),
    aggregator_choices = c("tippett", "fisher"),

    univariate_nulls = NULL,
    set_univariate_nulls = function() {
      private$univariate_nulls <- 1:self$nparams %>%
        purrr::map(~ function(y, parameters) {
          all_parameters <- self$point_estimates
          all_parameters[.x] <- parameters[1]
          private$null_spec(y, all_parameters)
        })
    },

    stat_assignments = NULL,
    set_stat_assignments = function(val) {
      private$stat_assignments <- val
    },

    kriging_model = NULL
  )
)
