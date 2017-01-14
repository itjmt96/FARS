context("Test the make_filename function")

library(readr)
library(dplyr)

test_that("make_filename(year) generates file name correctly based on year parameter", {
        expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))
        expect_equal(make_filename(2013), "accident_2013.csv.bz2")

})

test_that("fars_read(filename) throws error", {
        expect_error(fars_read('abcd'), "file 'abcd' does not exist", fixed=TRUE)
        expect_is(fars_read('accident_2013.csv.bz2'), "tbl_df")
})

