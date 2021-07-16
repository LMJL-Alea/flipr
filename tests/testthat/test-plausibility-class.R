###############  REGULAR TESTS ############
test_that("Regular test - Documentation example", {
  # Arrange
  set.seed(123)
  n <- 15
  x1 <- rnorm(n = n, mean = 0, sd = 1)
  x2 <- rnorm(n = n, mean = 1, sd = 1)
  d <- mean(x2) - mean(x1)
  null_spec <- function(y, parameters) {
    purrr::map(y, ~ .x - parameters)
  }
  my_t_stat <- function(data, indices) {
    n <- length(data)
    n1 <- length(indices)
    n2 <- n - n1
    indices2 <- seq_len(n)[-indices]
    x1 <- unlist(data[indices])
    x2 <- unlist(data[indices2])
    stats::t.test(x = x1, y = x2, var.equal = TRUE)$statistic
  }
  stat_functions <- list(my_t_stat)
  stat_assignments <- list(delta = 1)

  pf <- PlausibilityFunction$new(
    seed = 1234,
    null_spec = null_spec,
    stat_functions = stat_functions,
    stat_assignments = stat_assignments,
    x1, x2
  )

  # Act
  actual <- pf$get_value(d)

  # Assert
  expected <- 0.99
  expect_gt(actual, expected)
  #tolerance
  # my_stat
})



test_that("Snapshot test - Two normal distributions with different means and variances", {
  # Arrange
  set.seed(123)
  n <- 15
  x1 <- rnorm(n = n, mean = 0, sd = 1)
  x2 <- rnorm(n = n, mean = 1, sd = 4)
  d <- mean(x2) - sd(x2)*mean(x1)/sd(x1)
  r <- sd(x2)/sd(x1)
  null_spec <- function(y, parameters) {
    purrr::map(y, ~ (.x - parameters[1])/parameters[2])
  }
  stat_functions <- list(stat_t, stat_f)
  stat_assignments <- list(delta = c(1,2), rho = 2)

  pf <- PlausibilityFunction$new(
    seed = 1234,
    null_spec = null_spec,
    stat_functions = stat_functions,
    stat_assignments = stat_assignments,
    x1, x2
  )

  # Act
  actual <- pf$get_value(c(d, r))

  expected <- 1
  expect_equal(actual, expected)
})


test_that("Regular test - Almost constant series x1 and x2", {
  # Arrange
  n <- 8
  x1 <- rep(c(1,3), n)
  x2 <- rep(c(3,5), n)
  null_spec <- function(y, parameters) {
    purrr::map(y, ~ .x - parameters)
  }
  stat_functions <- list(stat_t)
  stat_assignments <- list(delta = 2)
  pf <- PlausibilityFunction$new(
    seed = 1234,
    null_spec = null_spec,
    stat_functions = stat_functions,
    stat_assignments = stat_assignments,
    x1, x2
  )

  # Act
  actual <- pf$get_value(2)

  # Assert
  expected <- 1
  expect_equal(actual, expected)
})

############### "SNAPSHOT TYPE" TESTS ############



test_that("Snapshot test - Documentation example", {
  # Arrange
  set.seed(123)
  n <- 15
  x1 <- rnorm(n = n, mean = 0, sd = 1)
  x2 <- rnorm(n = n, mean = 1, sd = 1)
  null_spec <- function(y, parameters) {
    purrr::map(y, ~ .x - parameters)
  }
  my_t_stat <- function(data, indices) {
    n <- length(data)
    n1 <- length(indices)
    n2 <- n - n1
    indices2 <- seq_len(n)[-indices]
    x1 <- unlist(data[indices])
    x2 <- unlist(data[indices2])
    stats::t.test(x = x1, y = x2, var.equal = TRUE)$statistic
  }
  stat_functions <- list(my_t_stat)
  stat_assignments <- list(delta = 1)

  pf <- PlausibilityFunction$new(
    seed = 1234,
    null_spec = null_spec,
    stat_functions = stat_functions,
    stat_assignments = stat_assignments,
    x1, x2
  )

  # Act
  actual <- pf$get_value(0)

  # Assert
  expect_snapshot(actual)
})



test_that("Snapshot test - Two normal distributions with different means and variances", {
  # Arrange
  set.seed(123)
  n <- 15
  x1 <- rnorm(n = n, mean = 0, sd = 1)
  x2 <- rnorm(n = n, mean = 1, sd = 4)
  null_spec <- function(y, parameters) {
    purrr::map(y, ~ (.x - parameters[1])/parameters[2])
  }
  stat_functions <- list(stat_t, stat_f)
  stat_assignments <- list(delta = c(1,2), rho = 2)

  pf <- PlausibilityFunction$new(
    seed = 1234,
    null_spec = null_spec,
    stat_functions = stat_functions,
    stat_assignments = stat_assignments,
    x1, x2
  )

  # Act
  actual <- pf$get_value(c(1, 4))

  # Assert
  expect_snapshot(actual)
})

############### ANOMALY TESTS ############

# test_that("Anomaly test - message if null_spec is not a function", {
#   # Arrange
#   n <- 8
#   x1 <- rep(c(1,3), n)
#   x2 <- rep(c(3,5), n)
#   null_spec <- "A string"
#   stat_functions <- list(stat_t)
#   stat_assignments <- list(delta = 2)
#
#   # Act & assert
#   expect_message(pf <- PlausibilityFunction$new(
#     seed = 1234,
#     null_spec = null_spec,
#     stat_functions = stat_functions,
#     stat_assignments = stat_assignments,
#     x1, x2), "The `null_spec` argument should be of class `function`.")
# })
