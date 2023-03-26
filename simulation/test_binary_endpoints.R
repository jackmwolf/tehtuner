# Confirm method maintains type-I error for binary endpoints
#
# This should not depend on the Step 2 method but only on how well Step 1
# method estimates the ITEs under the null hypothesis.
#
# We will consider a small simulation study with n = 1000 and p = 20.
# The outcome will be generated through a probit model with a similar linear
# component to the linear + null combination from Wolf et al. (2022).

# Data generation ==============================================================
library(MASS)

ar1_cor <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) -
                    (1:n - 1))
  rho^exponent
}

# p:  number of covariates
# h:  function giving covariate main effectas
# g:  function giving CATE
dg0 <- function(p = 20, h = h1, g = g0) {
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

  # Expected potential outcome under null
  temp0 <- h(X, p, m)
  temp0_scaled <- scale(temp0)

  # Treatment effect of 1 on probit scale
  temp1_scaled <- temp0_scaled + 1

  Y0 <- rnorm(n, mean = temp0_scaled, sd=1)
  Y1 <- rnorm(n, mean = temp1_scaled, sd=1)

  # Convert to binary endpopints
  Y0 <- Y0 > 0
  Y1 <- Y1 > 0

  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  reg <- as.data.frame(cbind(Y,V,C))
  names(reg) <- c("Trt", "Y", paste("V", 1:ncol(X), sep=""))

  #Benefit (ITE > 0)
  benefit <- (temp1_scaled-temp0_scaled)>0 ##
  return(list(reg = reg, bene=benefit, temp0=temp0_scaled, temp1=temp1_scaled))
}

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

dg <- function() {
  dg0(p = 20, h = h1, g = g0)
}

test <- dg()

run_sim <- function(seed, step1 = "lasso", ...) {
  set.seed(seed)
  data <- dg()
  mod <- tunevt(
    data = data$reg, Y = "Y", Trt = "Trt",
    step1 = step1, step2 = "rtree", p_reps = 100,
    alpha = 0.2, binary_Y = TRUE
    )
  reject <- unname(mod$mnpp > quantile(mod$theta_null, 0.80))
  reject
}

# Run simulations --------------------------------------------------------------
library(foreach)
M <- 500

cl <- parallel::makeCluster(10)
doParallel::registerDoParallel(cl)

re <- foreach(i = seq(M), .combine = c) %do% {
  re <- run_sim(seed = i)
}

mean(re)

parallel::stopCluster(cl)

table(re)
# re
# FALSE  TRUE
# 204   296

prop.test(sum(re), n = M, p = 0.2)
# 1-sample proportions test with continuity correction
#
# data:  sum(re) out of M, null probability 0.2
# X-squared = 477.75, df = 1, p-value < 2.2e-16
# alternative hypothesis: true p is not equal to 0.2
# 95 percent confidence interval:
#   0.5473710 0.6351937
# sample estimates:
#   p
# 0.592
