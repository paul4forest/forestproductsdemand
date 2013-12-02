# This file loads world bank data 
#
# Input: World bank website accessed with the R FAOSTAT package
# Output: RDATA file containing GDP, exchange rate and deflator data
#         saved in the /rawdata folder 
#
# Author: Paul Rougieux, European Forest Institute

require(FAOSTAT)

###############################
# See tests in test_rawdata.r #
###############################
# Test at the bottom of the script should be moved to test_rawdata.r

####################################
# Download World Bank data for GDP #
####################################
# See below for World bank metadata
# A function to download aggregates from WDI is also available,
# I haven't tried it yet as of 26 May 2013

# Load GDP current LCU
GDPcurrentLCU =  getWDI(indicator = "NY.GDP.MKTP.CN")
names(GDPcurrentLCU) = c("Country", "ISO2_WB_CODE", "Year", "GDPcurrentLCU")

# Load GDP current US$
GDPcurrentUSD = getWDI(indicator = "NY.GDP.MKTP.CD")
names(GDPcurrentUSD) = c("Country", "ISO2_WB_CODE", "Year", "GDPcurrentUSD")

# Load GDP constant US$
GDPconstantUSD = getWDI(indicator = "NY.GDP.MKTP.KD")
names(GDPconstantUSD ) = c("Country", "ISO2_WB_CODE", "Year", "GDPconstantUSD")

# Load GDP deflator annual
deflatorGDP.annual = getWDI(indicator = "NY.GDP.DEFL.KD.ZG")
names(deflatorGDP.annual) = c("Country", "ISO2_WB_CODE", "Year", "Deflator")

# Load Exchange rate 
exchrate = getWDI(indicator = "PA.NUS.FCRF")
names(exchrate) = c("Country", "ISO2_WB_CODE", "Year", "ExchR")

# Load population 
population = getWDI(indicator = "SP.POP.TOTL")
names(population) = c("Country", "ISO2_WB_CODE", "Year", "Population")

# Merge GDP, Deflator and exchange rate tables
GDPDefl = merge(GDPcurrentLCU, deflatorGDP.annual)
GDPDeflExchR = merge(GDPDefl, exchrate)
GDPDeflExchRPop = merge(GDPDeflExchR , population)

# Check if tables have the same number of rows
stopifnot(nrow(GDPcurrentLCU) == nrow(GDPDeflExchRPop))
stopifnot(nrow(deflatorGDP.annual) == nrow(GDPDeflExchRPop))
stopifnot(nrow(exchrate) == nrow(GDPDeflExchRPop))
stopifnot(nrow(population) == nrow(GDPDeflExchRPop))


################################
# Save objects in R data files #
################################
#Save the Table containing GDP, Deflator and exchange rate
save(GDPDeflExchRPop, 
     file = "rawdata/GDP_Deflator_Exchange_Rate_Population.RData")

#######################
# World bank metadata #
#######################
# See also http://data.worldbank.org/indicator

# GDP (current LCU)
getWDImetaData("NY.GDP.MKTP.CN",printMetaData = TRUE, saveMetaData = FALSE)

# GDP (current US$)
getWDImetaData("NY.GDP.MKTP.CD",printMetaData = TRUE, saveMetaData = FALSE)

# Inflation, GDP deflator (annual %)
getWDImetaData("NY.GDP.DEFL.KD.ZG",printMetaData = TRUE, saveMetaData = FALSE)

# Exchange rate
getWDImetaData("PA.NUS.FCRF",printMetaData = TRUE, saveMetaData = FALSE)

# Population
getWDImetaData("SP.POP.TOTL",printMetaData = TRUE, saveMetaData = FALSE)


#########
# Tests #
#########
# Add GDP in current and constant USD to the main data frame
GDP_Table = merge(GDPDeflExchR, GDPcurrentUSD)
GDP_Table = merge(GDP_Table, GDPconstantUSD)

# Subset data for France and the Euro area between 2000 and 2010 
# French exchange rate for 2000-2010 is the same as the Euro area
FR = subset(GDP_Table, Year>1999 & Year<2011 & 
             Country%in%c("Euro area","France"))
FR$ExchREUR = FR[FR$Country=="Euro area" ,c("ExchR")]
FR = subset(FR,Country=="France",-c(ExchR))

########################################################################
# Test if GDP in current US dollards is the same as the calculated one #
########################################################################
# The difference is not 0 exactly because GDP values are rounded.
# But it should be smaller than 1% of the GDP value in USD
stopifnot(FR$GDPcurrentLCU / FR$ExchREUR - FR$GDPcurrentUSD < FR$GDPcurrentUSD/100)

###################################################################################
# Test if GDP in constant US dollards base 2000 is the same as the calculated one #
###################################################################################

# Calculate the deflator base 2000
# Reduce() is explained here: http://stackoverflow.com/questions/16024046/apply-in-r-recursive-function-that-operates-on-its-own-previous-result
# Right is a logical indicating whether to proceed from left to right (default) or from right to left.
deflator = FR$Deflator[FR$Year>2000]
FR$Defl2000 = Reduce(function(u,v) u*(1+v/100),deflator,init=1,accum=TRUE)

# GDP in current LCU/(defl fr 2000 * exchrate 2000)
stopifnot(FR$GDPcurrentLCU / (FR$Defl2000 * FR$ExchREUR[FR$Year==2000]) -
    FR$GDPconstantUSD < FR$GDPconstantUSD/100)

