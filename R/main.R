
#' Fit a tuned Virtual Twins model
#'
#' \code{tunevt} fits a Virtual Twins model to estimate factors and subgroups
#' associated with differential treatment effects while controlling the Type I
#' error rate of falsely detecting at least one heterogeneous effect when the
#' treatment effect is uniform across the study population.
#'
#' Virtual Twins is a two-step approach to detecting differential treatment
#' effects. Subjects' conditional average treatment effects (CATEs) are first
#' estimated in Step 1 using a flexible model. Then, a simple and interpretable
#' model is fit in Step 2 to model these estimated CATEs as a function of the
#' covariates.
#'
#' The Step 2 model is dependent on some tuning parameter. This parameter is
#' selected to control the Type I error rate by permuting the data under the
#' null hypothesis of a constant treatment effect and identifying the minimal
#' null penalty parameter (MNPP), which is the smallest penalty parameter that
#' yields a Step 2 model with no covariate effects. The \code{1-alpha0} quantile
#' of the distribution of is then used to fit the Step 2 model on the original
#' data.
#'
#' @param data a data frame containing a response, binary treatment indicators,
#'   and covariates.
#' @param Trt a string specifying the name of the column of \code{data}
#'   contains the treatment indicators.
#' @param Y a string specifying the name of the column of \code{data}
#'   contains the response.
#' @param step1 character strings specifying the Step 1 model. Supports
#'   either "\code{lasso}", "\code{mars}", "\code{randomforest}", or
#'   "\code{superlearner}".
#' @param step2 a character string specifying the Step 2 model. Supports
#'   "\code{lasso}", "\code{rtree}", or "\code{ctree}".
#' @param alpha0 the nominal Type I error rate.
#' @param p_reps the number of permutations to run.
#' @param keepz logical. Should the estimated CATE from Step 1 be returned?
#' @param ... additional arguments to the Step 1 model call.
#'
#' @return an object of class \code{"tunevt"}.
#'
#'   An object of class \code{"tunevt"} is a list containing at least the
#'   following components:
#'     \item{call}{the matched call}
#'     \item{vtmod}{the model estimated by the given \code{step2} procedure fit
#'       with the permuted tuning parameter for the estimated CATEs from the
#'       \code{step1} model. See \code{\link{vt2_lasso}},
#'       \code{\link{vt2_rtree}}, or \code{\link{vt2_ctree}} for specifics.}
#'     \item{mnpp}{the MNPP for the estimated CATEs from Step 1.}
#'     \item{theta_null}{a vector of the MNPPs from each permutation under
#'       the null hypothesis.}
#'     \item{z}{if \code{keepz = TRUE}, the estimated CATEs from the
#'       \code{step1} model.}
#'
#' @references{
#'
#'   \insertRef{foster_subgroup_2011}{tunevt}
#'
#' }
#'
#' @examples
#' data(tunevt_example)
#' tunevt(
#'   tunevt_example, step1 = "lasso", step2 = "rtree",
#'   alpha0 = 0.05, p_reps = 10, keepz = TRUE
#' )
#'
#' @export
tunevt <- function(
  data, Y = "Y", Trt = "Trt", step1 = "randomforest", step2 = "rtree",
  alpha0, p_reps, keepz = FALSE, ...)
  {

  cl <- match.call()

  # Check inputs
  validate_Y(data = data, Y = Y)
  validate_Trt(data = data, Trt = Trt)
  validate_alpha0(data = data, alpha0 = alpha0)
  validate_p_reps(data = data, p_reps = p_reps)

  # Subset data by treatment indicator for Step 1
  d0 <- subset_trt(data, value = 0, Trt = Trt)
  d1 <- subset_trt(data, value = 1, Trt = Trt)

  # Estimate marginal average treatment effect
  zbar <- mean(d1[, 1]) - mean(d0[, 1])

  # Permutation to get the null distribution of the MNPP
  theta <- tune_theta(data = data, Trt = Trt, Y = Y, zbar = zbar,
                      step1 = step1, step2 = step2,
                      alpha0 = alpha0, p_reps = p_reps,
                      ...)

  # Fit Virtual Twins
  # Step 1
  vt1 <- get_vt1(step1)
  z <- vt1(data, Trt = Trt, Y = Y, ...)

  # Step 2
  vt2 <- get_vt2(step2)
  mod <- vt2(z, data, Trt = Trt, Y = Y, theta = theta$theta)

  # MNPP for the original data
  mnpp <- get_mnpp(z = z, data = data, step2 = step2, Trt = Trt, Y = Y)

  re <- list(
    vtmod = mod,
    theta_null = theta$theta_grid,
    mnpp = mnpp
  )

  re$call <- cl

  if ( keepz ) {
    re$z <- z
  }

  class(re) <- "tunevt"

  return(re)

}
