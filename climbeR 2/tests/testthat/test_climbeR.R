library("testthat")
library("climbeR")
require("survival")

rg.veteran <- readRDS("rg_veteran.rds")

# test_that("expect getAndPlotFirstAndSecondOrderMetric produces the same 
#           evaluated data",{
#               expect_equal(getAndPlotFirstAndSecondOrderMetric(rg.veteran)$eval_data,
#                            veteran_result$eval_data)
#               expect_equal(getAndPlotFirstAndSecondOrderMetric(rg.veteran)$so_vs_fo,
#                            veteran_result$so_vs_fo)
#           })

test_that("plotting function produces correct result, given eval data",{
    expect_equal(plotFirstAndSecondOrderMetric(rg.veteran),
                 veteran_result$eval_data$so_vs_fo)
})

test_that("forest averaged minimal depth of a maximal subtree calculation",{
    expect_equal(forestAvgMaxSubtree(rg.veteran), veteral_result$eval_data)
})

test_that("test that number of variables in the forest is less than or
          equal to the number of independent vars", {
              forest <- rg.veteran$forest
              num_vars <- length(forest$independent.variable.names)
              binned_forest <- binForestByDepth(forest)
              expect_lte(length(unique(unlist(a$depth_bins))), num_vars)
          })

# test_that("test that a RF generated from Iris has the correct properties",{
#     require(ranger)
#     
#     ## Classification forest with default settings
#     ranger(Species ~ ., data = iris)
#     
#     ## Prediction
#     train.idx <- sample(nrow(iris), 2/3 * nrow(iris))
#     iris.train <- iris[train.idx, ]
#     iris.test <- iris[-train.idx, ]
#     rg.iris <- ranger(Species ~ ., data = iris.train, write.forest = TRUE,
#                       importance = "impurity")
#     
#     # call to climber function
#     result <- getAndPlotFirstAndSecondOrderMetric(rg.iris)
#     # ^ evaluated data ^
#     eval_data <- result$subtree_metrics
#     # second order vs first order plot
#     so_vs_fo <- result$plot
# })