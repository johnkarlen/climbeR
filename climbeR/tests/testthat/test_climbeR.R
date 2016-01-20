library("testthat")
library("climbeR")
require("survival")

veteran_result <- readRDS("veteran_result.rds")
rg.veteran <- readRDS("rg_veteran.rds")

test_that("expect getAndPlotSOvsFO produces the same evaluated data",{
  expect_equal(getAndPlotSOvsFO(rg.veteran)$eval_data, veteran_result$eval_data)
})
