library("testthat")
library("climbeR")
require("survival")

veteran_result <- readRDS("veteran_result.rds")
rg.veteran <- readRDS("rg_veteran.rds")

test_that("expect getAndPlotFirstAndSecondOrderMetric produces the same 
          evaluated data",{
    expect_equal(getAndPlotFirstAndSecondOrderMetric(rg.veteran)$eval_data,
                 veteran_result$eval_data)
    expect_equal(getAndPlotFirstAndSecondOrderMetric(rg.veteran)$so_vs_fo,
                 veteran_result$so_vs_fo)
})

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

