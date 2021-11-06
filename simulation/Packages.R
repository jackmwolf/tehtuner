pcks <-
  c(
    "crayon",
    "dplyr",
    "foreach",
    "glmnet",
    "MASS",
    "rpart",
    "rpart.plot",
    "randomForest",
    "randomForestSRC",
    "withr",
    "ggplot2",
    "caret",
    "ranger",
    "mvtnorm",
    "modeltools",
    "zoo",
    "sandwich",
    "strucchange",
    "party",
    "Formula",
    "plotrix",
    "TeachingDemos",
    "plotmo",
    "earth",
    "nnls",
    "kernlab",
    "SuperLearner",
    "purrr",
    "stringr",
    "glmnet",
    "gam"
)

suppressMessages(lapply(pcks, require, character.only = TRUE,
                        lib.loc = "/panfs/roc/groups/11/koopmein/wolfx681/RPackages")
                 )
