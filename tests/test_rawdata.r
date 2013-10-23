
library(testthat)
# Test the raw data
# Author Paul Rougieux - European Forest Institute

setwd("Y:/Macro/forestproductsdemand/rawdata/")
print("Loading raw data")
print(load(file = "Paper and paperboard.rdata"))
print(load(file = "GDP_Deflator_Exchange_Rate_Population.rdata"))
EU = read.csv("EUCountries.csv", as.is=TRUE)

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
context("Euro coutries in the World Bank table")

# # Countries that don't have an exchange rate in 2005 # Should contain EURO area countries
# unique(wb$Country[is.na(wb$ExchR) & wb$Year==2005])
# 
# 
# # Euro exchange rate to dollard from World Bank
# exchr.euro = subset(GDPDeflExchRPop, Country=="Euro area"&Year>=1999, select=c(Year, ExchR)) 
# names(exchr.euro) = c("Year", "ExchReur")
# 
# # # Check euro start year: compare last year of wb exchange rate with the EU table start year
# # If all start years correspond then we can replace NA values in Exchrate
# mutate(merge(ddply(subset(wb, Year>1993 & is.na(ExchR), select=c(Country, Year)),
#                    .(Country), summarize, NA_ExchR_Year = min(Year)),
#              subset(EU, select=c("Country", "Euro_Start_Year"))),
#        diff = NA_ExchR_Year-Euro_Start_Year)



# Test GDP deflator calculation with available 2005 data




context("Future work")

test_that("add a plot of net trade by country for all EU countries", {
    expect_that(1, equals(2)) 
})