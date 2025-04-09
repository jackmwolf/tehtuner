
#' Estimate the CATE Using the Lasso for Step 1 of Virtual Twins
#'
#' @inheritParams tunevt
#' @param ... additional arguments to \code{cv.glmnet}
#'
#' @return Estimated CATEs for each subject in \code{data}.
#'
#' @importFrom glmnet cv.glmnet
#' @importFrom stats predict model.matrix
#'
#' @family VT Step 1 functions
#'
vt1_lasso <- function(data, Trt, Y, ...) {

  d0 <- subset_trt(data, value = 0, Trt = Trt)
  d1 <- subset_trt(data, value = 1, Trt = Trt)

  keep_x_fit <- !(names(d0) %in% c(Y, Trt))

  # Fit glmnet
  m0 <- glmnet::cv.glmnet(
    x = data.matrix(
      model.matrix(~ 0 + ., data = subset(d0, select = keep_x_fit))),
    y = d0[[Y]],
    weights = NULL,
    ...
    )

  m1 <- glmnet::cv.glmnet(
    x = data.matrix(
      model.matrix(~ 0 + ., data = subset(d1, select = keep_x_fit))),
    y = d1[[Y]],
    weights = NULL,
    ...
  )

  keep_x_pred <- !(names(data) %in% c(Y, Trt))
  data_pred <- subset(data, select = keep_x_pred)

  # Estimated expectation under control and treatment
  e0 <- predict(
    m0,
    newx = data.matrix(model.matrix(~ 0 + ., data = data_pred)),
    s = "lambda.1se"
    )

  e1 <- predict(
    m1,
    newx = data.matrix(model.matrix(~ 0 + ., data = data_pred)),
    s = "lambda.1se"
  )

  # Estimated CATE
  z <- drop(e1 - e0)

  return(z = z)
}

#' Estimate the CATE Using a Random Forest for Step 1 of Virtual Twins
#'
#' @inheritParams tunevt
#' @param ... additional arguments to \code{rfsrc}
#'
#' @return Estimated CATEs for each subject in \code{data}.
#'
#' @importFrom randomForestSRC tune rfsrc
#' @importFrom stats predict as.formula
#'
#' @family VT Step 1 functions
#'
vt1_rf <- function(data, Trt, Y, ...) {

  d0 <- subset_trt(data, value = 0, Trt = Trt)
  d1 <- subset_trt(data, value = 1, Trt = Trt)

  formula <- as.formula(paste0(Y, "~ ."))

  # Tune random forests
  t0 <- randomForestSRC::tune(
    formula, data = d0, doBest = TRUE
    )
  t1 <- randomForestSRC::tune(
    formula, data = d1, doBest = TRUE
    )

  # Fit random forests with tuned parameters
  m0 <- randomForestSRC::rfsrc(
    formula, d0, nodesize = t0$optimal[1], mtry = t0$optimal[2],
    ...
    )

  m1 <- randomForestSRC::rfsrc(
    formula, d1, nodesize = t1$optimal[1], mtry = t1$optimal[2],
    ...
  )

  e0 <- predict(m0, data)$predicted
  e1 <- predict(m1, data)$predicted

  # Estimated CATE and convert to vector
  z <- e1 - e0
  z <- c(z)

  return(z = z)
}

#' Estimate the CATE Using MARS for Step 1 of Virtual Twins
#'
#' @inheritParams tunevt
#' @param ... additional arguments to \code{earth}
#'
#' @return Estimated CATEs for each subject in \code{data}.
#'
#' @importFrom earth earth
#' @importFrom stats predict as.formula
#'
#' @family VT Step 1 functions
#'
vt1_mars <- function(data, Trt, Y, ...) {

  d0 <- subset_trt(data, value = 0, Trt = Trt)
  d1 <- subset_trt(data, value = 1, Trt = Trt)

  formula <- as.formula(paste0(Y, "~ ."))

  # Fit MARS models
  m0 <- earth::earth(
    formula, data = d0, ...
    )
  m1 <- earth::earth(
    formula, data = d1, ...
    )

  e0 <- predict(m0, newdata = data, type = "response")
  e1 <- predict(m1, newdata = data, type = "response")

  # Estimated CATE
  z <- e1 - e0

  return(z = z)
}

#' Estimate the CATE Using Super Learner for Step 1 of Virtual Twins
#'
#' @inheritParams tunevt
#' @param SL.library Either a character vector of prediction algorithms or a
#'   list containing character vector. See \code{SuperLearner} for more details.
#' @param ... additional arguments to \code{SuperLearner}
#'
#' @return Estimated CATEs for each subject in \code{data}.
#'
#' @import SuperLearner
#' @importFrom stats predict
#'
#' @family VT Step 1 functions
#'
vt1_super <- function(data, Trt, Y, SL.library, ...) {

  d0 <- subset_trt(data, value = 0, Trt = Trt)
  d1 <- subset_trt(data, value = 1, Trt = Trt)

  keep_x_fit <- !(names(d0) %in% c(Y, Trt))

  # Fit Super Learner models
  m0 <- SuperLearner::SuperLearner(
    Y = d0[[Y]],
    X = subset(d0, select = keep_x_fit),
    SL.library = SL.library,
    ...
  )

  m1 <- SuperLearner::SuperLearner(
    Y = d1[[Y]],
    X = subset(d1, select = keep_x_fit),
    SL.library = SL.library,
    ...
  )

  keep_x_pred <- !(names(data) %in% c(Y, Trt))
  e0 <- SuperLearner::predict.SuperLearner(
    m0, subset(data, select = keep_x_pred),
    onlySL = TRUE, type = "response"
    )
  e1 <- SuperLearner::predict.SuperLearner(
    m1, subset(data, select = keep_x_pred),
    onlySL = TRUE, type = "response"
  )

  # Estimated CATE
  z <- drop(e1$pred - e0$pred)

  return(z = z)
}

#' Get the appropriate Step 1 estimation function associated with a method
#'
#' @inheritParams tunevt
#'
#' @return a function that estimates the CATE through Step 1 of Virtual Twins
#'
get_vt1 <- function(step1) {
  if (step1 == "lasso") {
    f <- vt1_lasso
  } else if (step1 == "mars") {
    f <- vt1_mars
  } else if (step1 == "randomforest") {
    f <- vt1_rf
  } else if (step1 == "superlearner") {
    f <- vt1_super
  } else {
    stop("Invalid argument to 'step1'. Accepts 'lasso', 'mars', 'randomforest', and 'superlearner'.")
  }
  return(f)
}

subset_trt <- function(data, value, Trt) {

  # A <- data[[Trt]] == value
  # data_trt <- subset(data, subset = A, select = c(-Trt))
  data_trt <- data[data[[Trt]] == value, !names(data) %in% c(Trt)]

  return(data_trt)
}
