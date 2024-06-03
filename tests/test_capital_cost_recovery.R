
test_that("Test SL",{
  expect_equal(SL(0.20, 0.12), 0.807469869)
})

test_that("Test DB",{
  expect_equal(DB(0.20, 0.12), 0.7)
})
