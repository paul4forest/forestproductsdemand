# Author Paul Rougieux, European Forest Institute
library(FAOSTAT)
####################################
# Load FAOSTAT and World Bank data #
####################################
setwd("Y:/Macro/forestproductsdemand/rawdata/")
print(load(file = "Paper and paperboard.rdata"))
print(load(file = "GDP_Deflator_Exchange_Rate_Population.rdata"))
EU = read.csv("EUCountries.csv", as.is=TRUE)

################
# Explore Raw data # 
################
# visualise missing values and explore specific data issues such as entry in the Euro area


# Countries that don't have an exchange rate in 2005 # Should contain EURO area countries
unique(wb$Country[is.na(wb$ExchR) & wb$Year==2005])

# Euro Area has the Exchange rate from 1999 or from the country's entry into the euro zone
subset(GDPDeflExchRPop, Country=="Euro area") # or
subset(GDPDeflExchRPop, ISO2_WB_CODE=="XC")

# European Union population
plot(subset(GDPDeflExchRPop, Country=="European Union", select=c(Year,Population)))

# Show FAO country and region metatables for France
subset(FAOcountryProfile, ISO2_WB_CODE=="FR")
subset(FAOregionProfile, FAOST_CODE==68)

# Euro area countries
subset(EU,Euro_Start_Year>0, select=c("Country", "Euro_Start_Year","ExchRLCUtoEuro"))
# as of 2014 add LVL 0.702804 (Latvian lats)

# order wb by Country and Year
wb = arrange(wb, Country, Year)
