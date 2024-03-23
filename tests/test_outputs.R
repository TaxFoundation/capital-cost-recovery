library(here)

# get directory for this project (in this case, it's where .git is)
proj_dir <- here::here()

# Test files in the final-ouptuts directory
file_list = list("asset_averages", "cit_rates_timeseries", "eu_cctb",
                 "npv_europe", "npv_ranks_2022",
                 "npv_weighted_timeseries")
for (file in file_list) {
  file_test <- file.path(proj_dir, "final-outputs", paste(file, ".csv", sep=""))
  test <- read.delim(file_test, sep=",", fill = TRUE)
  file_expected <- file.path(proj_dir, "final-outputs-expected", paste(file, ".csv", sep=""))
  expected <- read.delim(file_expected, sep=",", fill = TRUE)
  test_that(paste("Test", file, "results"),{
  expect_equal(test, expected)
  })
}

# Test file in the final-data directory
test_that(paste("Test final-data results"),{
    test <- read.delim(subfile.path(proj_dir, "final-data", paste("npv_all_years", ".csv", sep="")), sep=",", fill = TRUE)
    expected <- read.delim(file.path(proj_dir, "final-data-expected", paste("npv_all_years", ".csv", sep="")), sep=",", fill = TRUE)
    expect_equal(test, expected)
  })
