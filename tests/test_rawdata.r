
library(testthat)
# Test the raw data
# Author Paul Rougieux - European Forest Institute

setwd("Y:/Macro/Demand Econometric Models/rawdata/")
print("Load raw data")
print(load(file = "Paper and paperboard.rdata"))
print(load(file = "GDP_Deflator_Exchange_Rate_Population.rdata"))


EU = read.csv("EUCountries.csv", as.is=TRUE)
stopifnot(nrow(EU)==27) # Check if there are 27 countries

#################
# Test EU table #
#################
context("Number of European Countries")

test_that("There are 27 countries in the EU table", {
    expect_that(nrow(EU), equals(27)) 
})

#################
# Test WB table #
#################
context("Euro coutries")

# Countries that don't have an exchange rate in 2005 # Should contain EURO area countries
unique(wb$Country[is.na(wb$ExchR) & wb$Year==2005])


# This is changing 

# Test GDP deflator calculation with available 2005 data



