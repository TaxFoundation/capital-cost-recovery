library(testthat)

source(file.path("..", "capital_cost_recovery.R"))

test_results <- test_dir("./", reporter="summary")