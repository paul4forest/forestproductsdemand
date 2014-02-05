# Test the functions script func.r
# Author Paul Rougieux, European Forest Institute

library(testthat)
setwd("..")
source("code/func.r")

context("In .code/func.r")

test_that("The itemname() function returns the characters Roundwood", {
    expect_that(itemname(1861) , equals("Roundwood")) 
})


