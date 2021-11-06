# Functions to permute data under H0 of no TEH and identify tuning paramaters
# that give Stage 2 models correctly indicating no TEH

permute <- function(dat) {
  
  # Estimated treatment effect
  d <- mean(dat$reg$Y[dat$reg$Trt == 1]) - mean(dat$reg$Y[dat$reg$Trt == 0])
  
  # Remove main treatment effect from all observations with T_i = 1
  dat$reg$Y[dat$reg$Trt == 1] <- dat$reg$Y[dat$reg$Trt == 1] - d
  # mean(dat$reg$Y[dat$reg$Trt == 1]) - mean(dat$reg$Y[dat$reg$Trt == 0])
  
  # Permute treatment indicators
  dat$reg[, "Trt"] <- sample(dat$reg[, "Trt"], size = nrow(dat$reg))
  
  # Add main effect to subjects with treatment after permutation ??
  # Mean effect after permutation
  # dp <- mean(dat$reg$Y[dat$reg$Trt == 1]) - mean(dat$reg$Y[dat$reg$Trt == 0])
  
  return(dat)
}

# Get appropriate tuning paramater using all three step 2 methods
get_theta_null <- function(dat, vt1_est = list()) {

  dat_p <- permute(dat)
  est <- vt1(dat_p$reg, vt1_est = vt1_est) # set permuting = TRUE for faster (and less) tuned RF
  
  theta <- c(
    "tree"  = get_theta_null.tree(dat_p, est),
    "lasso" = get_theta_null.lasso(dat_p, est),
    "ctree" = get_theta_null.ctree(dat_p, est)
    )
  
  return(theta)
}

# Genenerate a grid of correct thetas under H0 and take the 1 - alpha percentile
tune_theta <- function(dat, alpha, p_reps, vt1_est = list()) {
  
  thetas <- replicate(p_reps, get_theta_null(dat = dat, vt1_est = vt1_est), simplify = TRUE)
  theta_a0 <- apply(thetas, 1, quantile, probs = 1 - alpha, na.rm = TRUE, type = 2)
  
  theta_a <- cbind(alpha = matrix(alpha, ncol = 1), matrix(theta_a0, nrow = length(alpha)))
  colnames(theta_a) <- c("alpha", rownames(thetas))
  
  return(list(theta = theta_a, theta_grid = thetas))
}


get_theta_null.tree <- function(dat, est) {
  test <- dat$reg
  test$Z <- as.double(est$Z)
  
  mod.tree <- rpart(
    Z ~ ., data = subset(test, select = -c(Y, Trt)),
    method = "anova",
    cp = 0
  )
  
  theta <- mod.tree$frame[1, "complexity"]
  
  return(theta)
}

get_theta_null.lasso <- function(dat, est) {
  test <- dat$reg
  test$Z <- as.double(est$Z)
  
  mod.lasso <- glmnet(x = data.matrix(subset(test, select = -c(Y, Trt, Z))), 
                      y = data.matrix(est$Z))
  
  theta <- max(mod.lasso$lambda)
  
  return(theta)
}

get_theta_null.ctree <- function(dat, est) {
  test <- dat$reg
  test$Z <- as.double(est$Z)

  # Test if a theta gives a null tree
  test_null_theta <- function(theta) {
    mod.ctree <- ctree(Z ~ ., data = subset(test, select = -c(Y, Trt)),
                       controls = ctree_control(mincriterion = theta,
                                                testtype = "Teststatistic"))
    raw <- capture.output(mod.ctree@tree)
    vars <- table(str_trim(str_match(raw, "\\)(.+?)>")[,2]))
    nvars <- sum(vars)
    return(nvars == 0)
  }
  
  
  # Use a coarse exponential grid to find an upper bound for theta  
  theta <- 1
  k <- 1.5
  
  if (test_null_theta(theta)) {
    lb <- 0
    ub <- 1
  } else{
    null_theta <- FALSE
    while( !null_theta ) {
      theta <- theta * k
      null_theta <- test_null_theta(theta)
    }
    
    lb <- theta * 1/k
    ub <- theta
  }
  
  f <- function(theta) test_null_theta(theta) * 1/theta
  
  theta <- optimize(f, interval = c(lb, ub), maximum = TRUE)$maximum
    
  # theta <- 1
  # null_theta <- FALSE
  # while(!null_theta) {
  #   theta <- theta * 10^(1/20)
  #   null_theta <- test_null_theta(theta)
  # }


  return(theta)
}



# using a regression tree with tuned complexity parameter
c.tuned_tree <- function(dat, est, theta){
  test <- dat$reg
  test$Z <- as.double(est$Z)
  
  tfit <- rpart(Z ~ ., data = subset(test, select = -c(Y, Trt)), 
                method = "anova", cp = 0)
  # Information if H0 were true, what actually was the lambda we "should" have picked?
  # theta_max_null[["tree"]] <- mod.tree$frame[1, "complexity"]
  tfit <- prune(tfit, cp = theta)
  
  preds <- predict(tfit, newdata = subset(test, select = -c(Y, Trt)))
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  # mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  
  #getting predictors from tree: 
  vars <- as.character(unique(tfit$frame$var))
  tvars <- vars[!vars=="<leaf>"]
  return(list(nwg=sum(wg), mse=mse, vars=tvars, nvars = length(tvars)))
}


# using lasso with tuned lambda parameter
c.tuned_lasso <- function(dat, est, theta){
  test <- dat$reg
  test$Z <- est$Z

  l.fit <- glmnet(x = data.matrix(subset(test, select = -c(Y, Trt, Z))), 
                  y = data.matrix(est$Z))
  lasso.coefs <- coef(l.fit, s = theta)
  lvars <- rownames(lasso.coefs)[which(lasso.coefs != 0)]
  lvars <- setdiff(lvars, "(Intercept)")

  preds <- predict(l.fit, newx = data.matrix(subset(test, select = -c(Y, Trt, Z))), s = theta)
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  # mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  return(list(nwg=sum(wg), mse=mse, vars=lvars))
}

#using a conditional inference tree
c.tuned_ctree <- function(dat, est, theta){
  test <- dat$reg
  test$Z <- est$Z
  
  tfit <- ctree(Z ~ ., data = subset(test, select = -c(Y, Trt)),
                     controls = ctree_control(mincriterion = theta,
                                              testtype = "Teststatistic"))
  preds <- predict(tfit, newdata = subset(test, select = -c(Y, Trt)))
  
  #ones in wrong group (trt does benefit if preds>0)
  wg <- (preds>0)!=dat$bene
  #mean[(outcome if follow estimated trt)-(optimal outcome)]^2
  # mse <- mean((dat$temp1[wg]-dat$temp0[wg])^2)
  mse <- mean((preds - (dat$temp1 - dat$temp0))^2)
  #getting predictors from tree: 
  raw <- capture.output(tfit@tree)
  vars <- unique(str_trim(str_match(raw, "\\)(.+?)>")[,2]))
  vars <- vars[!is.na(vars)]
  return(list(nwg=sum(wg), mse=mse, vars=vars))
}
