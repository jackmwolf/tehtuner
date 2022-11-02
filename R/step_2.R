#' Estimate the CATE using the Lasso for Step 2
#'
#' @inheritParams tunevt
#' @inheritParams get_mnpp
#' @param theta lasso penalty parameter (\code{lambda})
#'
#' @importFrom glmnet glmnet
#' @importFrom stats predict coef
#'
#' @family VT Step 2 functions
#'
#' @return a list of length 3 containing the following elements:
#'   \item{mod}{an object of class \code{glmnet}. See
#'     \code{\link[glmnet]{glmnet}}.}
#'   \item{coefficients}{coefficients associated with the penalty parameter
#'     \code{theta}.}
#'   \item{fitted.values}{predicted values associated with the penalty parameter
#'     \code{theta}.}
#'
vt2_lasso <- function(z, data, Trt, Y, theta) {

  keep_x_fit <- !(names(data) %in% c(Y, Trt))
  X <- data.matrix(subset(data, select = keep_x_fit))

  mod <- glmnet(
    x = X,
    y = data.matrix(z)
  )
  beta <- coef(mod, s = theta)
  preds <- drop(predict(mod, s = theta, newx = X))

  return(list(
    mod = mod, coefficients = beta, fitted.values = preds, theta = theta
  ))
}


#' Estimate the CATE using a regression tree for Step 2
#'
#' @inheritParams tunevt
#' @inheritParams get_mnpp
#' @param theta tree complexity parameter (\code{cp})
#'
#' @importFrom rpart rpart
#' @importFrom stats predict coef
#'
#' @family VT Step 2 functions
#'
#' @return an object of class \code{rpart}. See
#'   \code{\link[rpart]{rpart.object}}.
#'
vt2_rtree <- function(z, data, Trt, Y, theta) {

  keep_x_fit <- !(names(data) %in% c(Y, Trt))
  mod <- rpart::rpart(
    z ~ ., data = subset(data, select = keep_x_fit),
    method = "anova",
    cp = theta
  )

  return(mod)

}


#' Estimate the CATE using a conditional inference tree for Step 2
#'
#' @inheritParams tunevt
#' @inheritParams get_mnpp
#' @param theta the value of the test statistic that must be exceeded in order
#'   to implement a split (\code{mincriterion})
#'
#' @importFrom party ctree ctree_control
#' @importFrom stats predict coef
#'
#' @family VT Step 2 functions
#'
#' @return An object of class \code{BinaryTree-class}. See
#'   \code{\link[party]{BinaryTree-class}}.
#'
vt2_ctree <- function(z, data, Trt, Y, theta) {

  data$z <- z
  keep_x_fit <- !(names(data) %in% c(Y, Trt))
  mod <- party::ctree(
    z ~ ., data = subset(data, select = keep_x_fit),
    controls = party::ctree_control(
      mincriterion = theta,
      testtype = "Teststatistic")
  )

  return(mod)

}


#' Get the appropriate Step 2 estimation function associated with a method
#'
#' @inheritParams tunevt
#'
#' @return a function that fits a model for the CATE through Step 2 of Virtual
#'   Twins
#'
get_vt2 <- function(step2) {
  if (step2 == "lasso") {
    f <- vt2_lasso
  } else if (step2 == "rtree") {
    f <- vt2_rtree
  } else if (step2 == "ctree") {
    f <- vt2_ctree
  } else {
    stop("Invalid argument to 'step2'. Accepts 'lasso', 'rtree', and 'ctree'.")
  }
  return(f)
}
