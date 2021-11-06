# Functions to validate inputs to tunevt()

#' Check if Y is a valid input to tunevt
#'
#' @inheritParams tunevt
#'
#' @return TRUE if \code{Y} is a valid input. Errors otherwise.
#'
validate_Y <- function(data, Y) {
  if ( !(Y %in% colnames(data)) ) {
    stop("'Y' does not correspond to a column of 'data'.")
  }
  return(TRUE)
}

#' Check if Trt is a valid input to tunevt
#'
#' @inheritParams tunevt
#'
#' @return TRUE if \code{Trt} is a valid input. Errors otherwise.
#'
validate_Trt <- function(data, Trt) {
  if ( !(Trt %in% colnames(data)) ) {
    stop("'Trt' does not correspond to a column of 'data'.")
  } else if ( !is.logical(data[[Trt]]) & !setequal(unique(data[[Trt]]), 0:1) ) {
    stop("Column 'Trt' must be logical or 0/1")
  }
  return(TRUE)
}

#' Check if alpha0 is a valid input to tunevt
#'
#' @inheritParams tunevt
#'
#' @return TRUE if \code{alpha0} is a valid input. Errors otherwise.
#'
validate_alpha0 <- function(data, alpha0) {
  if (  alpha0 <= 0 | 1 <= alpha0 ) {
    stop("'alpha0' must greater than 0 and less than 1.")
  }
  return(TRUE)
}

#' Check if p_reps is a valid input to tunevt
#'
#' @inheritParams tunevt
#'
#' @return TRUE if \code{p_reps} is a valid input. Errors otherwise.
#'
validate_p_reps <- function(data, p_reps) {
  if (  !(p_reps %% 1 == 0) | p_reps < 1 ) {
    stop("'p_reps' must be a positive integer.")
  }
  return(TRUE)
}
