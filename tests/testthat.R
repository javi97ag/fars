library(testthat)
library(fars)

test_check("fars")
test_that("Filename",{
  expect_equal(fars_Simple::make_filename(2013), "accident_2013.csv.bz2")
})
