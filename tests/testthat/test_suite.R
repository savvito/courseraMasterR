#  test suite

library(testthat)

test_that("correct file name is returned",
          expect_that(make_filename('2012'), equals("accident_2012.csv.bz2"))
)
