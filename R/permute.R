#' Estimate the penalty parameter for Step 2 of Virtual Twins
#'
#' Permutes data under the null hypothesis of a constant treatment effect and
#' calculates the MNPP on each permuted data set. The \code{1 - alpha} quantile
#' of the distribution is taken.
#'
#' @inheritParams tunevt
#' @param zbar the estimated marginal treatment effect
#'
#' @importFrom stats quantile
#' @importFrom foreach foreach `%dopar%`
#'
#' @return the estimated penalty parameter
#'
tune_theta <- function(data, Trt, Y, zbar, step1, step2, alpha0, p_reps,
                       parallel, ...) {
  # To pass ... to replicate()
  arg_list <- list(...)
  get_theta_null0 <- function(...) {
    get_theta_null(
      data = data, Trt = Trt, Y = Y, zbar = zbar,
      step1 = step1, step2 = step2, ...
    )
  }

  # Null distribution of MNPP
  if (parallel) {
    thetas <- foreach(i = seq(p_reps), .combine = c) %dopar% {
      do.call(get_theta_null0, args = arg_list)
    }
  } else {
    thetas <- replicate(
      p_reps,
      expr = {
        do.call(get_theta_null0, args = arg_list)
      },
      simplify = TRUE
    )
  }

  # Take the 1 - alpha0 quantile of the null distribution
  theta_alpha <- quantile(thetas, probs = 1 - alpha0, na.rm = TRUE, type = 2)

  return(list(theta = theta_alpha, theta_grid = thetas))
}

#' Generate a dataset with permuted treatment indicators
#'
#' Sets the marginal treatment effect to zero and then permute all treatment
#' indicators.
#'
#' @inheritParams tunevt
#' @param zbar the estimated marginal treatment effect
#'
#' @return a permuted dataset of the same size as \code{data}
#'
permute <- function(data, Trt, Y, zbar) {

  A <- data[[Trt]] == 1
  data_p <- data

  # Subtract marginal effect from those with Trt == 1 before permuting
  data_p[[Y]][A] <- data[[Y]][A] - zbar

  # Permute treatment indicators
  data_p[[Trt]] <- sample(data_p[[Trt]], size = nrow(data))

  return(data_p)
}

#' Permute a dataset under the null hypothesis and get the MNPP
#'
#' @inheritParams permute
#' @inheritParams tunevt
#'
#' @return the MNPP for the permuted data set
get_theta_null <- function(data, Trt, Y, zbar, step1, step2, ...) {

  data_p <- permute(data, Trt = Trt, Y = Y, zbar)

  # Estimate CATE on permuted data through Step 1 model
  vt1 <- get_vt1(step1)
  z <- vt1(data_p, Trt = Trt, Y = Y, ...)


  theta <- get_mnpp(z, data_p, step2, Trt = Trt, Y = Y)

  return(theta)
}


#' Get the MNPP for the Step 2 model
#'
#' Find the lowest penalty parameter so that the Step 2 model fit for the
#' estimated CATE from Step 1 is constant for all subjects.
#'
#' @param z a numeric vector of estimated CATEs from Step 1
#' @inheritParams tunevt
#'
get_mnpp <- function(z, data, step2, Trt, Y) {
  if (step2 == "lasso") {

    f <- get_mnpp.lasso

  } else if (step2 == "rtree") {

    f <- get_mnpp.rtree

  } else if (step2 == "ctree") {

    f <- get_mnpp.ctree

  } else {

    stop("Invalid input to step2. Accepts 'lasso', 'rtree' and 'ctree'.")

  }

  theta <- f(z, data, Trt = Trt, Y = Y)
  return(theta)
}

#' Get the MNPP for a Model fit via Lasso
#'
#' Finds the lowest penalty parameter for a null lasso model.
#'
#' @inheritParams get_mnpp
#'
#' @importFrom glmnet glmnet
#'
get_mnpp.lasso <- function(z, data, Trt, Y) {
  mod <- glmnet::glmnet(
    x = data.matrix(subset(data, select = !names(data) %in% c(Y, Trt))),
    y = data.matrix(z)
    )

  theta <- max(mod$lambda)

  return(theta)
}

#' Get the MNPP for a Regression Tree
#'
#' Finds the lowest complexity parameter for a null regression tree fit
#'
#' @inheritParams get_mnpp
#'
#' @return the MNPP
#'
#' @importFrom rpart rpart
#'
get_mnpp.rtree <- function(z, data, Trt, Y) {

  mod <- rpart::rpart(
    z ~ ., data = subset(data, select = !names(data) %in% c(Y, Trt)),
    method = "anova",
    cp = 0
  )

  theta <- mod$frame[1, "complexity"]

  return(theta)
}

#' Get the MNPP for a Conditional Inference Tree
#'
#' Finds the lowest test statistic for a null conditional inference tree
#'
#' @inheritParams get_mnpp
#'
#' @importFrom stats optimize
#' @return the MNPP
#'
get_mnpp.ctree <- function(z, data, Trt, Y) {

  # Use a coarse exponential grid to find an upper bound for theta
  theta <- 1
  k <- 1.5

  if (test_null_theta_ctree(theta, z, data, Trt, Y)) {
    lb <- 0
    ub <- 1
  } else{
    null_theta <- FALSE
    while( !null_theta ) {
      theta <- theta * k
      null_theta <- test_null_theta_ctree(theta, z, data, Trt, Y)
    }

    lb <- theta * 1/k
    ub <- theta
  }

  f <- function(theta) test_null_theta_ctree(theta, z, data, Trt, Y) * 1/theta

  theta <- optimize(f, interval = c(lb, ub), maximum = TRUE)$maximum

  return(theta)
}

#' Test if a Value Gives a Null Conditional Inference Tree
#'
#' Fits a conditional inference tree with minimal test statistic \code{theta}
#' and tests if the tree has more than one terminal node.
#'
#' @inheritParams get_mnpp.ctree
#' @param theta a positive double
#'
#' @importFrom party ctree ctree_control
#' @importFrom stringr str_trim str_match
#' @importFrom utils capture.output
#'
#' @return a boolean. \code{True} if \code{theta} is large enough to give a null
#'   conditional inference tree. \code{False} otherwise.
#'
test_null_theta_ctree <- function(theta, z, data, Trt, Y) {

  data$z <- z
  keep_x_fit <- !(names(data) %in% c(Y, Trt))
  mod.ctree <- party::ctree(
    z ~ ., data = subset(data, select = keep_x_fit),
    controls = party::ctree_control(
      mincriterion = theta,
      testtype = "Teststatistic")
    )
  raw <- capture.output(mod.ctree@tree)
  vars <- table(stringr::str_trim(stringr::str_match(raw, "\\)(.+?)>")[,2]))
  nvars <- sum(vars)

  return(nvars == 0)
}
