context("Generic test")

test_that("file name is correct", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})
