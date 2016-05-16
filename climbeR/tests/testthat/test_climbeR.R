library("testthat")
library("climbeR")
require("survival")

rg.veteran <- readRDS("rg_veteran.rds")
forest <- rg.veteran$forest
num_vars <- length(forest$independent.variable.names)

test_that("all independent variables were accounted for",
          {
              eval_data <- calculateAMDMS(rg.veteran)
              expect_equal(rg.veteran$num.independent.variables,
                           nrow(eval_data))
          })

test_that("test the properties of the object returned by binForestByDepth", 
          {
              binned_forest <- binForestByDepth(forest)
              
              # number of vars in forest is less than or equal to num indep.
              # variables
              expect_lte(length(binned_forest$variable_ids_used), num_vars)
              expect_lte(length(binned_forest$depth_bins), num_vars)
              # minimum depth is 1 (at least one feature must occupy the 
              # root node)
              expect_equal(min(unlist(binned_forest$depth_bins)), 1)
              # number of trees binned is correct
              expect_equal(rg.veteran$num.trees, 
                           length(binned_forest$depth_bins[[1]]))
              expect_equal(rg.veteran$num.trees, 
                           length(binned_forest$forest_depths))
          })

test_that("test properties of the object returned by countSplitsPerVar",
          {

              
              splits_per_var <- countSplitsPerVar(rg.veteran$forest)
              # all variables accounted for
              expect_equal(nrow(splits_per_var), num_vars)
              # normalized counts less than 1, greater than or equal to 0
              expect_true(all(splits_per_var$normalized_counts < 1))
              expect_true(all(splits_per_var$normalized_counts >= 0))
          })

test_that("test depth binning recursive function",
          {
              binned_tree = 
                  startRecursiveDepthBinning(forest$split.varIDs[[1]])
              # first bin should only have one element (the root)
              expect_equal(length(binned_tree[[1]]), 1)
              
              # all populated depths should contain elements
              expect_true(all(lapply(binned_tree,length) > 0))
          })