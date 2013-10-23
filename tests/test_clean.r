library(testthat)

# Test the clean script
# See article on testthat
# http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

setwd("Y:/Macro/forestproductsdemand//code")
# source("clean0.2.r")


context("Number of European Countries")

test_that("There are 27 countries in the EU table", {
    expect_that(nrow(EU), equals(27)) 
})


