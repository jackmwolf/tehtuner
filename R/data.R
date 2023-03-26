#' Simulated example data
#'
#' Simulated data from a clinical trial with heterogeneous treatment effects
#' where the CATE was a function of V1 and V9.
#'
#' @format A data frame with 1000 rows and 12 columns:
#' \describe{
#'   \item{Trt}{Binary treatment indicator}
#'   \item{Y}{Continuous response}
#'   \item{V1,V2,V3,V4,V5,V6,V7,V8}{Continuous covariates}
#'   \item{V9,V10}{Binary covariates}
#' }
"tehtuner_example"
