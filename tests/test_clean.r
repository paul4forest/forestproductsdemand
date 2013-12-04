# Test the clean script
# Author Paul Rougieux, European Forest Institute
# See article on testthat
# http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

require(testthat)
setwd("..")
source("code/clean.r")


context("In clean.r")

test_that("Long trade table kept all information from the original table", {
    # Paper products
    expect_that(nrow(pptrade), equals(2*nrow(pp))) 
    expect_that(pptrade$Quantity[pptrade$Trade=="Import"], equals(pp$Import_Quantity))
    expect_that(pptrade$Price_Trade[pptrade$Trade=="Import"], equals(pp$Import_Price))
    # Sawnwood
    expect_that(nrow(swdtrade), equals(2*nrow(swd))) 
    expect_that(swdtrade$Quantity[swdtrade$Trade=="Import"], equals(swd$Import_Quantity))
    expect_that(swdtrade$Price_Trade[swdtrade$Trade=="Import"], equals(swd$Import_Price))
})

