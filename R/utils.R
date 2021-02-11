convert_to_list <- function(...) {
  l <- rlang::list2(...)
  n <- length(l)

  if (n == 0) return(NULL)

  if (is.numeric(l[[1]])) {
    coherent_inputs <- TRUE
    for (i in 2:n) {
      if (!is.numeric(l[[i]])) {
        coherent_inputs <- FALSE
        break
      }
    }
    stopifnot(coherent_inputs)
    return(purrr::map(l, convert_vector_to_list))
  }

  if (is.matrix(l[[1]])) {
    coherent_inputs <- TRUE
    for (i in 2:n) {
      if (!is.numeric(l[[i]]) || (ncol(l[[i]]) != ncol(l[[1]]))) {
        coherent_inputs <- FALSE
        break
      }
    }
    stopifnot(coherent_inputs)
    return(purrr::map(l, convert_matrix_to_list))
  }

  coherent_inputs <- TRUE
  for (i in 1:n) {
    if (!is.list(l[[i]])) {
      coherent_inputs <- FALSE
      break
    }
  }
  stopifnot(coherent_inputs)

  l
}

convert_vector_to_list <- function(x) {
  as.list(x)
}

convert_matrix_to_list <- function(x) {
  purrr::map(1:nrow(x), ~ x[.x, ])
}
