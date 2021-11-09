library(MASS)

# Main data generating function ---
dg0 <- function(p = 20, h, g) {
  N <- 200 # Fixed sample size
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

  Y0 <- rnorm(n, mean = temp0, sd=4) #adjust sd here to make R^2 reasonable
  Y1 <- rnorm(n, mean = temp1, sd=4)

  Y <- rbind(cbind(Trt=0,Y0)[1:(n/2),] , cbind(Trt=1,Y1)[((n/2)+1):n,])
  reg <- as.data.frame(cbind(Y,V,C))
  names(reg) <- c("Trt", "Y", paste("V", 1:ncol(X), sep=""))

  return(reg)
}

# Generate n x n AR(1) correlation matrix
ar1_cor <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) -
                    (1:n - 1))
  rho^exponent
}


### Covariate main effects ----
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

### Treatment effects ----

g2 <- function(X, p, m) {
  if (p == 10) {
    # 2 predictive covariates
    a <- 4
    d <- a * (X[, 1] > m[1]) + X[, 9]

  } else if (p == 20) {
    # 4 predictive covariates
    a <- 4
    d <- a * (X[, 1] > m[1]) + a/2 * (X[, 2] > m[2]) +
      a/4 * (X[, 10] > m[10]) + X[, 17]

  } else if (p == 50) {
    # 4 predictive covariates, same as p == 20 case
    a <- 4
    d <- a * (X[, 1] > m[1]) + a/2 * (X[, 2] > m[2]) +
      a/4 * (X[, 10] > m[10]) + X[, 41]

  } else {
    stop("Argument 'p' can only be 10, 20, or 50.")
  }
  drop(drop(d - mean(d) + 2))
}

# Simulate data ---
set.seed(1000)
tunevt_example <- dg0(p = 10, h = h2, g = g2)
save(tunevt_example, file = "data/tunevt_example.rda")
