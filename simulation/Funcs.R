#Functions for Data Simulations

# source('C:/Users/Chuyu/Desktop/Research/Genentech/Packs.R')


####################################### h, g Data Generation ###################
## Data generation with p = 10, 20, 50 #########################################

# Generate n x n AR(1) correlation matrix
ar1_cor <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) -
                    (1:n - 1))
  rho^exponent
}

# p:  number of covariates
# h:  function giving covariate main effectas
# g:  function giving CATE
# sd: residual standard error for normal error model
# hs: logical, if true gives heteroskedastic errors with treatment group
#     having sd 1.5 times larger than the control's residual sd
dg0 <- function(p = 20, h, g, sd=4, hs = FALSE) {
  N <- 1000 # Fixed sample size
  n <- N# 2*N #N is 1000, 200 or 80,

  if (p == 10) {
    pc <- 8 # Continuous covariates

  } else if (p == 20) {
    pc <- 16 # Continuous covariates

  } else if (p == 50) {
    pc <- 40 # Continuous covariates

  } else {
    stop("Argument 'p' can only be 10, 20, or 50.")
  }

  # Generate covariates
  m <- rnorm(pc, 0, 3) #mean for each continuous covariate
  r <- ar1_cor(n = pc, rho = 0.6) # correlation structure
  V <- mvrnorm(n = n, mu = m, Sigma = r, empirical=TRUE)
  pb <- p - pc # Binary covarites
  C <- matrix(rbinom(pb*n, size=1, prob=0.7), ncol = pb, byrow = TRUE)
  X <- cbind(V, C)

  temp0 <- h(X, p, m)
  temp1 <- temp0 + g(X, p, m)

  Y0 <- rnorm(n, mean = temp0, sd=sd) #adjust sd here to make R^2 reasonable
  Y1 <- rnorm(n, mean = temp1, sd=(sd * (1 + 0.5 * hs)))
  # main simulations have sd = 4 in both groups
  # sd increased for simulations with lower R-squared
  # if hs TRUE then there are heteroskedastic errors and the outcome
  # under the treatment has residual error 1.5 times higher than the control.

  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  reg <- as.data.frame(cbind(Y,V,C))
  names(reg) <- c("Trt", "Y", paste("V", 1:ncol(X), sep=""))

  #Benefit (ITE > 0)
  benefit <- (temp1-temp0)>0 ##
  return(list(reg = reg, bene=benefit, temp0=temp0, temp1=temp1))
}

# Test r2 of outcome under control explained by covariate direct effects
testr2 <- function(h, nsim = 100, p = 20) {
  r2 <- replicate(nsim, expr = {
    N <- 1000 # Fixed sample size
    n <- N# 2*N #N is 1000, 200 or 80,

    if (p == 10) {
      pc <- 8 # Continuous covariates

    } else if (p == 20) {
      pc <- 16 # Continuous covariates

    } else if (p == 50) {
      pc <- 40 # Continuous covariates

    } else {
      stop("Argument 'p' can only be 10, 20, or 50.")
    }

    # Generate covariates
    m <- rnorm(pc, 0, 3) #mean for each continuous covariate
    r <- ar1_cor(n = pc, rho = 0.6) # correlation structure
    V <- MASS::mvrnorm(n = n, mu = m, Sigma = r, empirical=TRUE)
    pb <- p - pc # Binary covarites
    C <- matrix(rbinom(pb*n, size=1, prob=0.7), ncol = pb, byrow = TRUE)
    X <- cbind(V, C)

    y0 <- h(X, p, m)
    y <- rnorm(n, mean = y0, sd = 4)

    r2 <- 1 - var(y0 - y) / var(y)

  })
  mean(r2)
}

### Covariate main effects ----
# Goal: Have R^2 in (65%, 75%) predicting Y|T=0 from g(X) for all sizes of p
h1 <- function(X, p, m) {
  if (p == 10) {
    # 10 main effects, 8 continuous, 2 binary
    y0 <- X %*% c(rep(1.25,10))

  } else if (p == 20) {
    # 12 main effects, 10 continuous, 2 binary
    y0 <- X %*% c(rep(1,10), rep(0,6), rep(1,2), rep(0,2) )

  } else if (p == 50) {
    # 15 main effects, 12 continuous, 3 binary
    y0 <- X %*% c(rep(1,12), rep(0,28), rep(1,3), rep(0,7) )

  } else {
    stop("Argument 'p' can only be 10, 20, or 50.")
  }
  drop(y0)
}

h2 <- function(X, p, m) {
  if (p == 10) {
    # 10 main effects, 8 continuous, 2 binary
    y0 <- X[, 1] + X[, 2] +
      2 * (
        (X[, 3] - m[3])^2 + (X[, 4] - m[4])^2 -
          (X[, 5] - m[5])^2 +
          (X[, 6] > m[6]) + (X[, 7] > m[7]) +
          (X[, 8] > m[8])
      ) +
      X[, 9] + X[, 10]
  } else if (p == 20) {
    # 12 main effects
      y0 <- X[, 1] + X[, 2] +
        1.5 * (
          (X[, 3] - m[3])^2 + (X[, 4] - m[4])^2 +
            (X[, 5] - m[5])^2 + (X[, 6] - m[6])^2 +
            (X[, 7] > m[7]) + (X[, 8] > m[8]) +
            (X[, 9] > m[9]) + (X[, 10] > m[10])
        ) +
        X[, 17] + X[, 18]

  } else if (p == 50) {
    # 15 main effects, 12 continuous, 3 binary
    y0 <- X[, 1] + X[, 2] +
      1.25 * (
        (X[, 3] - m[3])^2 + (X[, 4] - m[4])^2 +
          (X[, 5] - m[5])^2 + (X[, 6] - m[6])^2 +
          (X[, 7] - m[7])^2 +
          (X[, 8] > m[8]) + (X[, 9] > m[9]) +
          (X[, 10] > m[10]) + (X[, 11] > m[11]) +
          (X[, 12] > m[12])
      ) +
      X[, 41] + X[, 42] + X[, 43]
  } else {
    stop("Argument 'p' can only be 10, 20, or 50.")
  }
  drop(y0)
}

# Nonlinear main effects with interactions
# Added 2022-01-21 per Reviewer #1
h3 <- function(X, p, m) {
  if (p == 10) {
    # 10 main effects, 8 continuous, 2 binary
    y0 <- X[, 1] + X[, 2] +
      0.5 * (X[, 1] - m[1]) * (X[, 2] - m[2]) + # Continuous - continuous interaction
      -1 * X[, 9] * (X[, 1] - m[1]) + #Continuous - binary interaction
      2 * (
        (X[, 3] - m[3])^2 + (X[, 4] - m[4])^2 +
          (X[, 5] - m[5])^2 +
          (X[, 6] > m[6]) + (X[, 7] > m[7]) +
          (X[, 8] > m[8])
      ) +
      X[, 9] + X[, 10]
  } else if (p == 20) {
    # 12 main effects
    y0 <- X[, 1] + X[, 2] +
      0.5 * (X[, 1] - m[1]) * (X[, 2] - m[2]) +
      -1 * X[, 17] * (X[, 1] - m[1]) + #Continuous - binary interaction
      1.25 * (
        (X[, 3] - m[3])^2 + (X[, 4] - m[4])^2 +
          (X[, 5] - m[5])^2 + (X[, 6] - m[6])^2 +
          (X[, 7] > m[7]) + (X[, 8] > m[8]) +
          (X[, 9] > m[9]) + (X[, 10] > m[10])
      ) +
      X[, 17] + X[, 18]

  } else if (p == 50) {
    # 15 main effects, 12 continuous, 3 binary
    y0 <- X[, 1] + X[, 2] +
      0.5 * (X[, 1] - m[1]) * (X[, 2] - m[2]) +
      -1 * X[, 42] * (X[, 1] - m[1]) + #Continuous - binary interaction
      1.25 * (
        (X[, 3] - m[3])^2 + (X[, 4] - m[4])^2 +
          (X[, 5] - m[5])^2 + (X[, 6] - m[6])^2 +
          (X[, 7] - m[7])^2 +
          (X[, 8] > m[8]) + (X[, 9] > m[9]) +
          (X[, 10] > m[10]) + (X[, 11] > m[11]) +
          (X[, 12] > m[12])
      ) +
      X[, 41] + X[, 42] + X[, 43]
  } else {
    stop("Argument 'p' can only be 10, 20, or 50.")
  }
  drop(y0)
}

### Treatment effects ----
# Goal have equal variance in d for both h1 and h2
g0 <- function(X, p, m) {
  d <- 2
  d
}

g1 <- function(X, p, m) {
  if (p == 10) {
    # 2 predictive covariates
    d <- X[, 1] + X[, 9]
    d <- d / 2

  } else if (p == 20) {
    # 4 predictive covariates
    d <- X[, 1] + X[, 2] + X[, 10] + X[, 17]
    d <- d / 2

  } else if (p == 50) {
    # 4 predictive covariates, same as p == 20 case
    d <- X[, 1] + X[, 2] + X[, 10] + X[, 41]
    d <- d / 2

  } else {
    stop("Argument 'p' can only be 10, 20, or 50.")
  }
  drop(drop(d - mean(d) + 2))
}

g2 <- function(X, p, m) {
  if (p == 10) {
    # 2 predictive covariates
    a <- 2
    # d <- a * (X[, 1] - m[1])^2 + X[, 9]
    d <- a * (X[, 1] > m[1]) + X[, 9]

  } else if (p == 20) {
    # 4 predictive covariates
    a <- 2
    # d <- a * (X[, 1] - m[1])^2 + a * (X[, 2] > m[2]) +
    #   a * (X[, 10] - m[10])^2 + X[, 17]
    d <- a * (X[, 1] > m[1]) + a/2 * (X[, 2] > m[2]) +
      a/4 * (X[, 10] > m[10]) + X[, 17]

  } else if (p == 50) {
    # 4 predictive covariates, same as p == 20 case
    a <- 2
    # d <- a * (X[, 1] - m[1])^2 + a * (X[, 2] > m[2]) +
    #   a * (X[, 10] - m[10])^2 + X[, 41]
    d <- a * (X[, 1] > m[1]) + a/2 * (X[, 2] > m[2]) +
      a/4 * (X[, 10] > m[10]) + X[, 41]

  } else {
    stop("Argument 'p' can only be 10, 20, or 50.")
  }
  drop(drop(d - mean(d) + 2))
}


# Nonlinear CATE with interactions
# Added 2022-01-21 per Reviewer #1
g3 <- function(X, p, m) {
  if (p == 10) {
    # 2 predictive covariates
    a <- 1
    d <- a * (X[, 1] > m[1]) + X[, 9] +
      a/4 * (X[, 1] - m[1]) * X[, 9]

  } else if (p == 20) {
    # 4 predictive covariates
    a <- 1
    d <- a * (X[, 1] > m[1]) + a/2 * (X[, 2] > m[2]) +
      a/4 * (X[, 10] > m[10]) + X[, 17] +
      a/4 * (X[, 1] - m[1]) * X[, 17]

  } else if (p == 50) {
    # 4 predictive covariates, same as p == 20 case
    a <- 1
    d <- a * (X[, 1] > m[1]) + a/2 * (X[, 2] > m[2]) +
      a/4 * (X[, 10] > m[10]) + X[, 41] +
      a/4 * (X[, 1] - m[1]) * X[, 41] +
      a/8 * (X[, 2] - m[2]) * (X[, 10] - m[10])

  } else {
    stop("Argument 'p' can only be 10, 20, or 50.")
  }
  drop(drop(d - mean(d) + 2))
}


# CATE with ATE == 0
g00 <- function(X, p, m) {
  g0(X, p, m) - 2
}

g01 <- function(X, p, m) {
  g1(X, p, m) - 2
}

g02 <- function(X, p, m) {
  g2(X, p, m) - 2
}

g03 <- function(X, p, m) {
  g3(X, p, m) - 2
}



# SIX DATA GENERATION SCENARIOS -----------------------------------------------
## original manuscript scenarios ----
lin_null_dg <- function(p) {
  dg0(p, h = h1, g = g0)
}

lin_lin_dg <- function(p) {
  dg0(p, h = h1, g = g1)
}

lin_nonlin_dg <- function(p) {
  dg0(p, h = h1, g = g2)
}

nonlin_null_dg <- function(p) {
  dg0(p, h = h2, g = g0)
}

nonlin_lin_dg <- function(p) {
  dg0(p, h = h2, g = g1)
}

nonlin_nonlin_dg <- function(p) {
  dg0(p, h = h2, g = g2)
}


## New data generation with more interactions per Reviewer #1 feedback ----
lin_nonlin2_dg <- function(p) {
  dg0(p, h = h1, g = g3)
}

nonlin2_null_dg <- function(p) {
  dg0(p, h = h3, g = g0)
}

nonlin2_lin_dg <- function(p) {
  dg0(p, h = h3, g = g1)
}

nonlin2_nonlin2_dg <- function(p) {
  dg0(p, h = h3, g = g3)
}

## low r2 generation ----
lin_null_dg_r2 <- function(p) {
  dg0(p, h = h1, g = g0, sd = 12)
}

lin_lin_dg_r2 <- function(p) {
  dg0(p, h = h1, g = g1, sd = 12)
}

lin_nonlin_dg_r2 <- function(p) {
  dg0(p, h = h1, g = g3, sd = 12)
}

nonlin_null_dg_r2 <- function(p) {
  dg0(p, h = h3, g = g0, sd = 12)
}

nonlin_lin_dg_r2 <- function(p) {
  dg0(p, h = h3, g = g1, sd = 12)
}

nonlin_nonlin_dg_r2 <- function(p) {
  dg0(p, h = h3, g = g3, sd = 12)
}

## heteroskedastic errors by treatment ----
lin_null_dg_hs <- function(p) {
  dg0(p, h = h1, g = g0, hs = TRUE)
}

lin_lin_dg_hs <- function(p) {
  dg0(p, h = h1, g = g1, hs = TRUE)
}

lin_nonlin_dg_hs <- function(p) {
  dg0(p, h = h1, g = g3, hs = TRUE)
}

nonlin_null_dg_hs <- function(p) {
  dg0(p, h = h3, g = g0, hs = TRUE)
}

nonlin_lin_dg_hs <- function(p) {
  dg0(p, h = h3, g = g1, hs = TRUE)
}

nonlin_nonlin_dg_hs <- function(p) {
  dg0(p, h = h3, g = g3, hs = TRUE)
}

####################################### VT Step 1 #######################################
#Get individual treatment effects

#LASSO models for trt/ctrl arms, errors "nope" when lasso model is too sparse
i.las <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)
  m0 <- cv.glmnet(x = data.matrix(subset(dat0, select = -c(Y, Trt))),
                  y = dat0$Y,
                  family = "gaussian", alpha = 1, standardize = TRUE)
  m1 <- cv.glmnet(x = data.matrix(subset(dat1, select = -c(Y, Trt))),
                  y = dat1$Y,
                  family = "gaussian", alpha = 1, standardize = TRUE)
  p0 <- coef(m0, s="lambda.1se")
  p1 <- coef(m1, s="lambda.1se")
  # ifelse(length(p0@i)<2 | length(p1@i)<2, stop("nope"), x <- 1)
  pred0 <- predict(m0, newx = data.matrix(subset(dat, select = -c(Y, Trt))), s = "lambda.1se")
  pred1 <- predict(m1, newx = data.matrix(subset(dat, select = -c(Y, Trt))), s = "lambda.1se")
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}
# i.las <- function(train, test, vt1_est = list()){
#   dat0 <- subset(train, train$Trt==0)
#   dat1 <- subset(train, train$Trt==1)
#   m0 <- cv.glmnet(x = data.matrix(subset(dat0, select = -c(Y, Trt))),
#                   y = dat0$Y,
#                   family = "gaussian", alpha = 1, standardize = TRUE)
#   m1 <- cv.glmnet(x = data.matrix(subset(dat1, select = -c(Y, Trt))),
#                   y = dat1$Y,
#                   family = "gaussian", alpha = 1, standardize = TRUE)
#   p0 <- coef(m0, s="lambda.1se")
#   p1 <- coef(m1, s="lambda.1se")
#   # ifelse(length(p0@i)<2 | length(p1@i)<2, stop("nope"), x <- 1)
#   pred0 <- predict(m0, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
#   pred1 <- predict(m1, newx = data.matrix(subset(test, select = -c(Y, Trt))), s = "lambda.1se")
#   Z <- pred1-pred0
#   return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
# }

#RF with RandomforestSRC
i.rf <- function(train, test, vt1_est = list()){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  if (!(is.null(vt1_est$m0))) {
    # Faster models with optimal paramaters from original stage 1 model
    m0 <- rfsrc(Y~. , data = subset(dat0, select = -Trt),
                nodesize = vt1_est$m0$optimal[1], mtry = vt1_est$m0$optimal[2])
    m1 <- rfsrc(Y~. , data = subset(dat1, select = -Trt),
                nodesize = vt1_est$m1$optimal[1], mtry = vt1_est$m1$optimal[2])

    pred0 <- predict(m0, subset(test, select = -c(Y, Trt)))$predicted
    pred1 <- predict(m1, subset(test, select = -c(Y, Trt)))$predicted

  } else {
    m0 <- tune(Y~. , data = subset(dat0, select = -Trt), doBest = TRUE) #automatically tunes forest
    m1 <- tune(Y~. , data = subset(dat1, select = -Trt), doBest = TRUE)

    m0_rf <- rfsrc(Y ~ ., data = subset(dat0, select = -Trt),
                   nodesize = m0$optimal[1], mtry = m0$optimal[2])

    m1_rf <- rfsrc(Y ~ ., data = subset(dat1, select = -Trt),
                   nodesize = m1$optimal[1], mtry = m1$optimal[2])

    pred0 <- predict(m0_rf, subset(test, select = -c(Y, Trt)))$predicted
    pred1 <- predict(m1_rf, subset(test, select = -c(Y, Trt)))$predicted
  }

  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

i.rf.fast <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)

  m0 <- rfsrc(Y ~ ., data = subset(dat0, select = -Trt))

  m1 <- rfsrc(Y ~ ., data = subset(dat1, select = -Trt))

  pred0 <- predict(m0, subset(dat, select = -c(Y, Trt)))$predicted
  pred1 <- predict(m1, subset(dat, select = -c(Y, Trt)))$predicted

  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}
# i.rf.fast <- function(train, test, vt1_est = list()){
#   dat0 <- subset(train, train$Trt==0)
#   dat1 <- subset(train, train$Trt==1)
#
#   m0 <- rfsrc(Y ~ ., data = subset(dat0, select = -Trt))
#
#   m1 <- rfsrc(Y ~ ., data = subset(dat1, select = -Trt))
#
#   pred0 <- predict(m0, subset(test, select = -c(Y, Trt)))$predicted
#   pred1 <- predict(m1, subset(test, select = -c(Y, Trt)))$predicted
#
#   Z <- pred1-pred0
#   return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
# }

#Piecewise model with MARS
i.mars <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)
  m0 <- earth(Y~., data = subset(dat0, select = -Trt))
  m1 <- earth(Y~., data = subset(dat1, select = -Trt))
  pred0 <- predict(m0, newdata = subset(dat, select = -c(Y, Trt)), type = "response")
  pred1 <- predict(m1, newdata = subset(dat, select = -c(Y, Trt)), type = "response")
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}
# i.mars <- function(train, test, vt1_est = list()){
#   dat0 <- subset(train, train$Trt==0)
#   dat1 <- subset(train, train$Trt==1)
#   m0 <- earth(Y~., data = subset(dat0, select = -Trt))
#   m1 <- earth(Y~., data = subset(dat1, select = -Trt))
#   pred0 <- predict(m0, newdata = subset(test, select = -c(Y, Trt)), type = "response")
#   pred1 <- predict(m1, newdata = subset(test, select = -c(Y, Trt)), type = "response")
#   Z <- pred1-pred0
#   return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
# }

#SVM using kernlab inside of caret
i.svm <- function(train, test, vt1_est = list()){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  m0 <- caret::train(Y~., data = subset(dat0, select = -Trt),
                     method = "svmRadial",
                     tuneLength = 3,
                     trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3))
  m1 <- caret::train(Y~., data = subset(dat1, select = -Trt),
                     method = "svmRadial",
                     tuneLength = 3,
                     trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3))
  pred0 <- predict(m0$finalModel, newdata=subset(test, select = -c(Y, Trt)))
  pred1 <- predict(m1$finalModel, newdata=subset(test, select = -c(Y, Trt)))
  Z <- pred1-pred0
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

#Piecewise model with Superlearner
i.super <- function(train, test, vt1_est = list()){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)
  SL.earth.def = function(...) { #changing to earth package defaults
    SL.earth(..., degree = 1, penalty = 2)
  }
  slmethods <- c("SL.glmnet", "SL.randomForest","SL.earth.def")
  m0 <- SuperLearner(Y = dat0$Y,
                     X = as.data.frame(subset(dat0, select = -c(Y, Trt))),
                     family = gaussian(),
                     SL.library = slmethods)
  m1 <- SuperLearner(Y = dat1$Y,
                     X = as.data.frame(subset(dat1, select = -c(Y, Trt))),
                     family = gaussian(),
                     SL.library = slmethods)
  pred0 <- predict.SuperLearner(m0, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  pred1 <- predict.SuperLearner(m1, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  Z <- pred1$pred-pred0$pred
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

# Modified superlearner with faster ensemble methods and fewer CV folds
i.super.fast <- function(dat, vt1_est = list()){
  dat0 <- subset(dat, dat$Trt==0)
  dat1 <- subset(dat, dat$Trt==1)

  slmethods <- c("SL.glmnet.fast", "SL.randomForest.fast","SL.earth.def")
  m0 <- SuperLearner.fast(
    Y = dat0$Y,
    X = as.data.frame(subset(dat0, select = -c(Y, Trt))),
    family = gaussian(),
    SL.library = slmethods)
  m1 <- SuperLearner.fast(
    Y = dat1$Y,
    X = as.data.frame(subset(dat1, select = -c(Y, Trt))),
    family = gaussian(),
    SL.library = slmethods)

  pred0 <- predict.SuperLearner(m0, as.data.frame(subset(dat, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  pred1 <- predict.SuperLearner(m1, as.data.frame(subset(dat, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  Z <- pred1$pred-pred0$pred
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}
# i.super.fast <- function(train, test, vt1_est = list()){
#   dat0 <- subset(train, train$Trt==0)
#   dat1 <- subset(train, train$Trt==1)
#
#   slmethods <- c("SL.glmnet.fast", "SL.randomForest.fast","SL.earth.def")
#   m0 <- SuperLearner.fast(
#     Y = dat0$Y,
#     X = as.data.frame(subset(dat0, select = -c(Y, Trt))),
#     family = gaussian(),
#     SL.library = slmethods)
#   m1 <- SuperLearner.fast(
#     Y = dat1$Y,
#     X = as.data.frame(subset(dat1, select = -c(Y, Trt))),
#     family = gaussian(),
#     SL.library = slmethods)
#
#   pred0 <- predict.SuperLearner(m0, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
#   pred1 <- predict.SuperLearner(m1, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
#   Z <- pred1$pred-pred0$pred
#   return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
# }

SL.glmnet.fast <- function(...) {
  SL.glmnet(...)
}

SL.randomForest.fast <- function(...) { # Default ntree is 1000, cut this down
  SL.randomForest(..., ntree = 250)
}

SL.earth.def = function(...) { #changing to earth package defaults
  SL.earth(..., degree = 1, penalty = 2)
}

SuperLearner.fast <- function(...) { # Change 10 folds to 3 folds
  SuperLearner(..., cvControl = SuperLearner::SuperLearner.CV.control(V = 3L))
}

SL.gam.fast <- function (Y, X, newX, family, obsWeights, deg.gam = 2, cts.num = 4,
                         ...)  {
  if (!require("gam")) {
    stop("SL.gam requires the gam package, but it isn't available")
  }
  if ("mgcv" %in% loadedNamespaces())
    warning("mgcv and gam packages are both in use. You might see an error because both packages use the same function names.")
  cts.x <- apply(X, 2, function(x) (length(unique(x)) > cts.num))
  if (sum(!cts.x) > 0) {
    gam.model <- as.formula(paste("Y~", paste(paste("s(",
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam,
                                                    ")", sep = ""), collapse = "+"),
                                  "+", paste(colnames(X[, !cts.x, drop = FALSE]),
                                             collapse = "+")))
  }
  else {
    gam.model <- as.formula(paste("Y~", paste(paste("s(",
                                                    colnames(X[, cts.x, drop = FALSE]), ",", deg.gam,
                                                    ")", sep = ""), collapse = "+")))
  }
  if (sum(!cts.x) == length(cts.x)) {
    gam.model <- as.formula(paste("Y~", paste(colnames(X),
                                              collapse = "+"), sep = ""))
  }
  fit.gam <- gam::gam(gam.model, data = X, family = family,
                      control = gam::gam.control(maxit = 25, bf.maxit = 25),
                      weights = obsWeights)
  # if (packageVersion("gam") >= 1.15) {
  pred <- gam::predict.Gam(fit.gam, newdata = newX, type = "response")
  # }
  # else {
  #   stop("This SL.gam wrapper requires gam version >= 1.15, please update the gam package with 'update.packages('gam')'")
  # }
  fit <- list(object = fit.gam)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- c("SL.gam")
  return(out)
}

# Modified superlearner with faster ensemble methods and fewer CV folds
# AND a GAM in the ensemble
i.super.fast2 <- function(train, test, vt1_est = list()){
  dat0 <- subset(train, train$Trt==0)
  dat1 <- subset(train, train$Trt==1)

  slmethods <- c("SL.glmnet.fast", "SL.randomForest.fast","SL.earth.def", "SL.gam.fast")
  m0 <- SuperLearner.fast(
    Y = dat0$Y,
    X = as.data.frame(subset(dat0, select = -c(Y, Trt))),
    family = gaussian(),
    SL.library = slmethods)
  m1 <- SuperLearner.fast(
    Y = dat1$Y,
    X = as.data.frame(subset(dat1, select = -c(Y, Trt))),
    family = gaussian(),
    SL.library = slmethods)

  pred0 <- predict.SuperLearner(m0, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  pred1 <- predict.SuperLearner(m1, as.data.frame(subset(test, select = -c(Y, Trt))), onlySL = TRUE, type = "response")
  Z <- pred1$pred-pred0$pred
  return(list(pred0=pred0, pred1=pred1, Z=Z, m0=m0, m1=m1))
}

####################################### VT Step 2 #######################################
#make trees using test set, run the estimated test set treatment effects through to get classification

#not doing a step 2, just using est from step 1 to get trt assignment
c.none <- function(dat, est){
  preds <- est$Z
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  # mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  return(list(nwg=sum(wg), mse=mse))
}

#using a tree to get number of misclassified, and grabbing the predictors
c.tree <- function(dat, est){
  test <- dat$reg
  test$Z <- as.double(est$Z)
  tfit <- caret::train(Z~., data = subset(test, select = -c(Y, Trt)),
                       method = "rpart2",
                       tuneLength = 3,
                       trControl=trainControl(method = "repeatedcv", number = 10, repeats = 3))
  preds <- predict(tfit$finalModel, newdata = subset(test, select = -c(Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  # mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  #getting predictors from tree:
  vars <- unique(tfit$finalModel$frame$var)
  tvars <- vars[!vars=="<leaf>"]
  return(list(nwg=sum(wg), mse=mse, vars=tvars, nvars = length(tvars)))
}


# CHANGED 2021-07-26 !!! Now fit Lasso as-is
#linear model for step 2
c.lin <- function(dat, est){
  test <- dat$reg
  test$Z <- est$Z
  m <- cv.glmnet(x = data.matrix(subset(test, select = -c(Y, Trt, Z))),
                    y = data.matrix(est$Z),
                    family = "gaussian", alpha = 1, standardize = TRUE)

  lasso.coefs <- coef(m, s = "lambda.min")
  lvars <- rownames(lasso.coefs)[which(lasso.coefs != 0)]
  lvars <- setdiff(lvars, "(Intercept)")

  preds <- predict(m, newx = data.matrix(subset(test, select = -c(Y, Trt, Z))), s = "lambda.min")
  # #getting top predictors:
  # p <- coef(m, s="lambda.min") %>% as.matrix()
  # pv <- p[-1]
  # vars <- rownames(p)[abs(p)>=sort(abs(pv), decreasing = T)[top] & p!=0]
  # lvars <- vars[!vars=="(Intercept)"]
  # #fitting the linear model with top predictors
  # fit <- lm(paste("Z~", paste(lvars, collapse = "+")), data = test)
  # preds <- predict(fit, newdata = subset(test, select = -c(Z, Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  # mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  return(list(nwg=sum(wg), mse=mse, vars=lvars))
}

#using a conditional inference tree
c.ctree <- function(dat, est){
  test <- dat$reg
  test$Z <- as.double(est$Z)
  tfit <- caret::train(Z~., data = subset(test, select = -c(Y, Trt)),
                      method = 'ctree2',
                      trControl = trainControl(method = "repeatedcv", number=10, repeats = 3),
                      tuneGrid = expand.grid(maxdepth = c(1:3), mincriterion=0.95),
                      metric='RMSE')
  preds <- predict(tfit$finalModel, newdata = subset(test, select = -c(Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  # mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  #getting predictors from tree:
  raw <- capture.output(tfit$finalModel@tree)
  vars <- unique(str_trim(str_match(raw, "\\)(.+?)>")[,2]))
  vars <- vars[!is.na(vars)]
  return(list(nwg=sum(wg), mse=mse, vars=vars))
}
