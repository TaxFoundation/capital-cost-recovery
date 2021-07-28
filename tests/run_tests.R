library(testthat)
library(here)

# get directory for this project (in this case, it's where .git is)
proj_dir <- here::here()

# call capital_cost_recovery.R script
source(file.path(proj_dir, "capital_cost_recovery.R"))

# run all tests in test_dir
test_results <- test_dir(file.path(proj_dir, 'tests'), reporter="summary")
