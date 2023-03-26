
test_that(
  "Identifies Trt and Y", {
    dat <- tehtuner_example[c(1:10, 501:510), ]
    names(dat)[names(dat) == "Trt"] <- "A"
    names(dat)[names(dat) == "Y"] <- "Outcome"

    expect_no_error(
      tunevt(
        data = dat, Y = "Outcome", Trt = "A",
        step1 = "mars", step2 = "rtree", alpha0 = 0.95, p_reps = 1
        )
    )
  }
)
