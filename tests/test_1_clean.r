# Test the clean script
# Author Paul Rougieux, European Forest Institute
# See article on testthat
# http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf

require(testthat)
setwd("..")


context("In ./code/clean.r")
source("code/clean.r")

test_that("Long trade table kept all information from the original table", {
    # Working table pp should be put in the environment by the clean script
    # We compare this table to others in the list paperproducts
    expect_that(nrow(paperproducts$trade), equals(2*nrow(pp))) 
    expect_that(paperproducts$trade$Quantity[p$trade$Trade=="Import"], equals(pp$Import_Quantity))
    expect_that(paperproducts$trade$Price_Trade[p$trade$Trade=="Import"], equals(pp$Import_Price))
    # Sawnwood
    expect_that(nrow(sawnwood$trade), equals(2*nrow(swd))) 
    expect_that(sawnwood$trade$Quantity[sawnwood$trade$Trade=="Import"], equals(swd$Import_Quantity))
    expect_that(sawnwood$trade$Price_Trade[sawnwood$trade$Trade=="Import"], equals(swd$Import_Price))
})

test_that("Aggregated tables kept information",{
    agg <- paperproducts$eu_aggregates
    expect_that(sum(agg$Value[agg$Element=="Consumption"]),
                equals(sum(pp$Consumption)))
    agg <- sawnwood$eu_aggregates
    expect_that(sum(agg$Value[agg$Element=="Consumption"]),
                equals(sum(swd$Consumption)))
})
