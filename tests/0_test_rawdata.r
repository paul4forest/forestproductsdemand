# Test the raw data
# Author Paul Rougieux - European Forest Institute

require(testthat)
require(plyr)

################
# Load rawdata #
################
# Because I usually run all tests with the command test_dir("tests")  
# the working directory is set to /tests. This is inconvenient in practice.
# Therefore I change the working directory to the root project directory ".."
setwd("..")
load(file = "rawdata/Paper and paperboard.rdata")
pp = paperAndPaperboardProducts
load(file = "rawdata/sawnwood.RData")
load(file = "rawdata/GDP_Deflator_Exchange_Rate_Population.rdata")
EU = read.csv("rawdata/EUCountries.csv", as.is=TRUE)


#####################
# Test raw FAO data #
#####################
context("In rawdata folder, test FAO data")

test_that("There are 27 countries in the EU table", {
    expect_that(nrow(EU), equals(27)) 
})

test_that("EU production of 3 paper products match total", {
    # In raw data folder, before the clean openration total Item is called "Paper and Paperboard"
    sum3 = sum(subset(pp$aggregates, FAOST_CODE==5706&Item!="Paper and Paperboard", 
                      select=c(Production)))
    tot =  sum(subset(pp$aggregates, FAOST_CODE==5706&Item=="Paper and Paperboard",
                      select=c(Production)))
    expect_that(sum3, equals(tot)) 
})
# For information, detailed Production volumes per Item in Europe 
# ddply(subset(pp$aggregates,FAOST_CODE==5706&Item!="Paper and Paperboard"),.(Item),summarize, sum = sum(Production))

test_that("EU production of 2 sawnwood products match total", {
    # In raw data folder, before the clean openration total Item is called "Paper and Paperboard"
    sum3 = sum(subset(sawnwood$aggregates, FAOST_CODE==5706&Item!="Sawnwood", 
                      select=c(Production)))
    tot =  sum(subset(sawnwood$aggregates, FAOST_CODE==5706&Item=="Sawnwood", 
                      select=c(Production)))
    expect_that(sum3, equals(tot)) 
})


############################
# Test raw World Bank data #
############################
context("In rawdata folder, test World Bank data")

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

# 
# context("Future work")
# 
# test_that("add a plot of net trade by country for all EU countries", {
#     expect_that(1, equals(2)) 
# })