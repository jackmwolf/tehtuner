
test_that(
  "Identifies Trt and Y", {
    dat <- tehtuner_example[c(1:10, 501:510), ]
    names(dat)[names(dat) == "Trt"] <- "A"
    names(dat)[names(dat) == "Y"] <- "Outcome"

    set.seed(1)

    expect_no_error(
      tunevt(
        data = dat, Y = "Outcome", Trt = "A",
        step1 = "mars", step2 = "rtree", alpha0 = 0.95, p_reps = 1
        )
    )
  }
)

test_that(
  "Step 1 wrappers", {
    data <- tehtuner_example[c(1:50, 501:550), ]

    d0 <- tehtuner:::subset_trt(data, value = 0, Trt = "Trt")
    d1 <- tehtuner:::subset_trt(data, value = 1, Trt = "Trt")

    keep_x_fit <- !(names(d0) %in% c("Y", "Trt"))
    dx0 <- subset(d0, select = keep_x_fit)
    dx1 <- subset(d1, select = keep_x_fit)
    dx <- subset(data, select = keep_x_fit)

    # Lasso --------------------------------------------------------------------
    # Using wrapper
    set.seed(1)
    z1 <- tehtuner:::vt1_lasso(data, "Trt", "Y")

    # Manually
    set.seed(1)
    m0 <- glmnet::cv.glmnet(x = data.matrix(dx0), y = d0[["Y"]])
    m1 <- glmnet::cv.glmnet(x = data.matrix(dx1), y = d1[["Y"]])

    e0 <- predict(m0, newx = data.matrix(dx), s = "lambda.1se")
    e1 <- predict(m1, newx = data.matrix(dx), s = "lambda.1se")

    z2 <- drop(e1 - e0)

    expect_equal(z1, z2)

    # Mars ---------------------------------------------------------------------
    # Using wrapper
    z1 <- tehtuner:::vt1_mars(data, "Trt", "Y")

    # Manually
    m0 <- earth::earth(Y ~ ., data = d0)
    m1 <- earth::earth(Y ~ ., data = d1)

    e0 <- predict(m0, newdata = data, type = "response")
    e1 <- predict(m1, newdata = data, type = "response")

    z2 <- e1 - e0

    expect_equal(z1, z2)

    # Random forest ------------------------------------------------------------
    # Using wrapper
    set.seed(1)
    z1 <- tehtuner:::vt1_rf(data, "Trt", "Y")

    # Manually
    set.seed(1)
    t0 <- randomForestSRC::tune(Y ~ ., data = d0, doBest = TRUE)
    t1 <- randomForestSRC::tune(Y ~ ., data = d1, doBest = TRUE)

    m0 <- randomForestSRC::rfsrc(
      Y ~ ., d0, nodesize = t0$optimal[1], mtry = t0$optimal[2],
    )
    m1 <- randomForestSRC::rfsrc(
      Y ~ ., d1, nodesize = t1$optimal[1], mtry = t1$optimal[2],
    )

    e0 <- predict(m0, data)$predicted
    e1 <- predict(m1, data)$predicted

    z2 <- e1 - e0
    z2 <- c(z2)

    expect_equal(z1, z2)

    # Super learner ------------------------------------------------------------
    # Simple and fast library for testing

    SL.library <- c("SL.lm", "SL.rpart")
    # Using wrapper
    set.seed(1)
    z1 <- tehtuner:::vt1_super(data, "Trt", "Y", SL.library = SL.library)

    # Manually
    set.seed(1)
    m0 <- SuperLearner::SuperLearner(
      Y = d0[["Y"]], X = dx0, SL.library = SL.library
    )

    m1 <- SuperLearner::SuperLearner(
      Y = d1[["Y"]], X = dx1, SL.library = SL.library
    )

    e0 <- SuperLearner::predict.SuperLearner(
      m0, data, onlySL = TRUE, type = "response"
    )
    e1 <- SuperLearner::predict.SuperLearner(
      m1, data, onlySL = TRUE, type = "response"
    )

    z2 <- drop(e1$pred - e0$pred)

    expect_equal(z1, z2)

  }
)



test_that(
  "get_mnpp returns a null penalty parameter", {
    # Get estimates of Zi for testing get_mnpp and Step 2 wrapperes
    set.seed(1)
    data <- tehtuner_example[c(1:50, 501:550), ]

    z <- tehtuner:::vt1_lasso(data, "Trt", "Y")

    # lasso --------------------------------------------------------------------
    # get mnpp
    mnpp_lasso <- tehtuner:::get_mnpp(z, data, "lasso", "Trt", "Y")
    # fit model with mnpp
    mod_lasso <- tehtuner:::vt2_lasso(z, data, "Trt", "Y", mnpp_lasso)
    # all predictions should be equal
    expect_equal(length(unique(mod_lasso$fitted.values)), 1)

    # regression tree ----------------------------------------------------------
    # get mnpp
    mnpp_rtree <- tehtuner:::get_mnpp(z, data, "rtree", "Trt", "Y")
    # fit model with mnpp
    mod_rtree <- tehtuner:::vt2_rtree(z, data, "Trt", "Y", mnpp_rtree)
    # all predictions should be equal
    expect_equal(length(unique(predict(mod_rtree))), 1)

    # classification tree ------------------------------------------------------
    # get mnpp
    mnpp_classtree <- tehtuner:::get_mnpp(
      z, data, "classtree", "Trt", "Y", threshold = 0)
    # fit model with mnpp
    mod_classtree <- tehtuner:::vt2_classtree(
      z, data, "Trt", "Y", mnpp_classtree, threshold = 0)
    # all predictions should be equal
    expect_equal(length(unique(predict(mod_classtree)[, 1])), 1)

    # conditional inference tree -----------------------------------------------
    # get mnpp
    mnpp_ctree <- tehtuner:::get_mnpp(z, data, "ctree", "Trt", "Y")
    # fit model with mnpp
    mod_ctree <- tehtuner:::vt2_ctree(z, data, "Trt", "Y", mnpp_ctree)
    # all predictions should be equal
    expect_equal(length(unique(predict(mod_ctree))), 1)

  }
)
