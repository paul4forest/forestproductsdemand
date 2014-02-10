# Test the functions script func.r
# Author Paul Rougieux, European Forest Institute

library(testthat)
setwd("..")


context("In ./code/func.r")
source("code/func.r")

test_that("The itemname() function returns the characters Roundwood", {
    expect_that(FAO$itemname(1861) , equals("Roundwood")) 
})


