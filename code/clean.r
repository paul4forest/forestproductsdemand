# See the file "clean EU15PaperDemand.r" for cleaning specific to
#    the Chas Amil and Buongiorno demand function estimates
#
# Input: .RDATA files containing
#            Paper products production and trade volume and value from FAOSTAT
#            GDP, deflator, exchange rate and population from world bank
#
# Output: .RDATA files containing data frames of Paper products (and maybe other products)
#             price time series in constant USD of baseyear, based on trade values
#             Consumption volume for EU 27 countries
#
# Author: Paul Rougieux - European Forest Institute
 
library(plyr)
library(FAOSTAT) # May remove it if I don't use it

baseyear = 2010 # Define the baseyear for constant GDP calculations and price deflator

# See tests in the /tests directory

####################################
# Load FAOSTAT and World Bank data #
####################################
cat("Load raw data: ")
cat(load(file = "rawdata/Paper and paperboard.rdata")," ")
cat(load(file = "rawdata/sawnwood.RData"), " ")
# cat(load(file = "rawdata/roundwood.RData"), " ")
cat(load(file = "rawdata/GDP_Deflator_Exchange_Rate_Population.rdata"), "\n")
EU = read.csv("rawdata/EUCountries.csv", as.is=TRUE)

# Select products for EU27 Countries
pp = subset(paperAndPaperboardProducts$entity, FAOST_CODE %in% EU$FAOST_CODE)
swd = subset(sawnwood$entity, FAOST_CODE %in% EU$FAOST_CODE)
# rwd = subset(roundwood$entity, FAOST_CODE %in% EU$FAOST_CODE)
###########################
###########################
## Clean World Bank data ##
###########################
###########################
# Prepare GDP data, calculate deflator and GDP in current USD
#
# Select EU27 countries and rename World Bank data frame to shorter name wb
wb = subset(GDPDeflExchRPop, ISO2_WB_CODE %in% EU$ISO2_WB_CODE) 

# Rename Slovakia
wb$Country[wb$Country=="Slovak Republic"] = "Slovakia"

# Add local currency exchange rate to Euro and EU15 membership
wb = merge(wb, subset(EU, select=c(ISO2_WB_CODE, ExchRLCUtoEuro, EU15) ))


#####################################################################
# Convert Exchrate in Euro area countries to the Euro exchange rate #
#####################################################################
# Euro exchange rate to dollard from World Bank
exchr.euro = subset(GDPDeflExchRPop, Country=="Euro area"&Year>=1999, select=c(Year, ExchR)) 
names(exchr.euro) = c("Year", "ExchReur")

# # Check euro start year: compare last year of wb exchange rate with the EU table start year
# # If all start years correspond then we can replace NA values in Exchrate
# mutate(merge(ddply(subset(wb, Year>1993 & is.na(ExchR), select=c(Country, Year)),
#                    .(Country), summarize, NA_ExchR_Year = min(Year)),
#              subset(EU, select=c("Country", "Euro_Start_Year"))),
#        diff = NA_ExchR_Year-Euro_Start_Year)

# Split non EURO countries 
wb.neuro = subset(wb, Country%in%EU$Country[EU$Euro_Start_Year==0] )
# SPlit eurozone countries before and after their entry into the zone
# This based on the fact that there are NA values for the Exchange rate from local currency to dollar
wb.euro.after = subset(wb, Country%in%EU$Country[EU$Euro_Start_Year>0] &
                           Year>1993 & is.na(ExchR))
wb.euro.before = subset(wb, Country%in%EU$Country[EU$Euro_Start_Year>0] &
                            !(Year>1993 & is.na(ExchR)))

# Add exchange rate
wb.neuro$ExchReur = wb.neuro$ExchR
wb.euro.after = merge(wb.euro.after, exchr.euro, all.x=TRUE)
wb.euro.before = mutate(wb.euro.before, ExchReur = ExchR/ExchRLCUtoEuro)

# Combine euro and neuro together
stopifnot(nrow(wb)==nrow(wb.euro.after) + nrow(wb.euro.before) + nrow(wb.neuro))
wb = rbind(wb.euro.before, wb.euro.after, wb.neuro)
rm(wb.euro.before, wb.euro.after, wb.neuro )


###########################
# Calculate deflator base #
###########################
deflator = function(dtf){
    # Deflator after base year
    d = dtf$Deflator[dtf$Year>baseyear]
    dtf$DeflBase[dtf$Year>=baseyear] =
        Reduce(function(u,v) u*(1+v/100), d,init=1,accum=TRUE)
    
    # Deflator before base year (calculated from right to left)
    d = dtf$Deflator[dtf$Year<=baseyear]
    dtf$DeflBase[dtf$Year<=baseyear] = 
        Reduce(function(u,v) v/(1+u/100), d[-1], init=1, accum=TRUE, right=TRUE)
    return(dtf)
}
wb = ddply(wb, .(Country), deflator)


# Calculate the US deflator for baseyear, rename column to DeflUS
US = subset(GDPDeflExchRPop, Country =="United States", select=c(Country,Year,Deflator))
US = deflator(US)
names(US) = c("Country", "Year", "Deflator", "DeflUS")

# Calculate the EUro area deflator for baseyear, rename column to DeflEUR
EUR = subset(GDPDeflExchRPop, Country == "Euro area", select=c(Country, Year, Deflator))
EUR = deflator(EUR)
names(EUR) = c("Country", "Year", "Deflator", "DeflEUR")


##############################################
# Calculate GDP in constant USD of base year #
##############################################
wb = ddply(wb, .(Country), mutate,
           GDPconstantUSD = GDPcurrentLCU / (DeflBase * ExchReur[Year==baseyear]))


###########################
###########################
## Clean  FAOSTAT   data ##
###########################
###########################


################################################
# Calculate apparent consumption and net trade #
################################################
calculateConsumptionNetTrade = function(dtf){
    # Change NA values to 0 - Not recommended 
    # But makes sence at least that import into Finland and Sweden are 0
    dtf[is.na(dtf)] = 0
    
    # Calculate apparent consumption and net trade
    dtf = mutate(dtf, Consumption = Production + Import_Quantity - Export_Quantity, 
                 Net_Trade =  Export_Quantity - Import_Quantity)
    return(dtf)
}


###########################
# Add GDP and US Deflator #
###########################
addGDPandDeflator = function(dtf){
    # Add GDPconstantUSD
    dtf = merge(dtf, wb[c("Year","Country","GDPconstantUSD")])
    
    # Add GDP deflator for the USA
    dtf = merge(dtf, subset(US, select=c(Year,DeflUS)))
    return(dtf)
}

#################################################
# Calculate prices in constant USD of base year #
#################################################
calculateConstantPrices = function(dtf){
    # Ponderation of import and export prices as used in Chas-Amil and Buongiorno 2000
    dtf = mutate(dtf, Price = (Import_Value + Export_Value)/
                     (Import_Quantity + Export_Quantity) / DeflUS *1000)
    
    # Import and export prices
    dtf = mutate(dtf, Import_Price = Import_Value / Import_Quantity / DeflUS*1000)
    dtf = mutate(dtf, Export_Price = Export_Value / Export_Quantity / DeflUS*1000)
    return(dtf)
}

#######################################################
# Create a table in long format containing trade data # 
#######################################################
reshapeLongTradeTable = function(dtf){
    # Might want to use the reshape2 package
    dtftrade = subset(dtf, select=-c(Production, DeflUS, Price))
    dtftrade = reshape(dtftrade, 
                       idvar=c("Country", "Year", "Item"), 
                       varying=list(c("Import_Quantity", "Export_Quantity"),
                                    c("Import_Value", "Export_Value"),
                                    c("Import_Price", "Export_Price")), 
                       v.names=c("Quantity", "Value", "Price_Trade"),
                       timevar="Trade", times=c("Import", "Export"), 
                       direction="long" )
    
    row.names(dtftrade) = NULL
    return(dtftrade)
}


##################################################################
# Create an aggregated table of consumption and price for Europe #
##################################################################
aggregateConsPriceTable = function(dtf){
    dtfagg = subset(dtf, select=c("Item", "Year", "Consumption", "Production", "Net_Trade",
                                  "Import_Quantity", "Export_Quantity", 
                                  "Import_Value", "Export_Value"))
    
    # Remove NA values not good, but do it here to calculate the aggregate
    dtfagg[is.na(dtfagg)] = 0 
    
    
    #  Sum volumes and values over the European Union
    dtfagg = aggregate(dtfagg[c("Consumption", "Production", "Net_Trade",
                                "Import_Quantity", "Export_Quantity", 
                                "Import_Value", "Export_Value")], 
                       dtfagg[c("Item", "Year")],sum)
    
    # Add GDP deflator for the USA
    dtfagg = merge(dtfagg, subset(US, select=c(Year,DeflUS)))
    
    # Add GDP deflator for the EU
    dtfagg = merge(dtfagg, 
                   subset(GDPDeflExchRPop, Country=="European Union", 
                          select=c(Year, Deflator)))
    
    
    # Ponderation of import and export prices as used in Chas-Amil and Buongiorno 2000
    dtfagg = mutate(dtfagg, Price = (Import_Value + Export_Value)/
                        (Import_Quantity + Export_Quantity) / DeflUS *1000)
    #                PriceConstantEURO = 
    
    
    elements = c("Consumption", "Production", "Net_Trade",
                 "Import_Quantity", "Export_Quantity", 
                 "Import_Value", "Export_Value","Price")
    
    dtfagg = reshape(dtfagg, 
                     idvar=c("Year", "Item"), 
                     varying=list(elements), 
                     times=factor(elements, ordered=TRUE, levels=elements), 
                     timevar="Element", v.names=c("Value"),
                     direction="long" )
    row.names(dtfagg) = NULL
    dtfagg$DeflUS = NULL
    return(dtfagg)
}

#########################################################################
# Keep Consumption, price and revenue, remove Production and trade data #
#########################################################################
removeProdTradeKeepConsPrice = function(dtf){
    subset(dtf,select=c(Year, Country, Item, 
                        Price, Consumption, Net_Trade,
                        GDPconstantUSD,
                        Import_Price, Export_Price))
}
 

######################################
# Changes specific to paper products #
######################################
# Rename item vectors
pp$Item[pp$Item=="Paper and Paperboard"] = "Total Paper and Paperboard"
pp$Item[pp$Item=="Other Paper+Paperboard"] = "Other Paper and Paperboard"
pp$Item[pp$Item=="Printing+Writing Paper"] = "Printing and Writing Paper"

# Change item to an ordered factor, same as in Table 3 of ChasAmil2000
pp$Item = factor(pp$Item, ordered=TRUE,
                 levels=c("Total Paper and Paperboard", "Newsprint",
                          "Printing and Writing Paper", 
                          "Other Paper and Paperboard"))

# Changes specific to Sawnwood products 
# Rename item vector
swd$Item[swd$Item=="Sawnwood"] = "Total Sawnwood"
swd$Item[swd$Item=="Sawnwood (C)"] = "Sawnwood Coniferous"
swd$Item[swd$Item=="Sawnwood (NC)"] = "Sawnwood Non Coniferous"

# Change item to an ordered factor
swd$Item = factor(swd$Item, ordered=TRUE,
                  levels=c("Total Sawnwood","Sawnwood Coniferous", "Sawnwood Non Coniferous"))


###########################################
# Call clean functions for paper products #
###########################################
# See also specific changes above
pp = calculateConsumptionNetTrade(pp)
pp = addGDPandDeflator(pp)
pp = calculateConstantPrices(pp)
# pp is now the most complete table

# Tables with reshaped data or aggregated information
pptrade = reshapeLongTradeTable(pp)
ppagg = aggregateConsPriceTable(pp)
paperProducts = removeProdTradeKeepConsPrice(pp)
paperProducts = arrange(paperProducts, Item, Country, Year)

##############################################
# Call clean functions for Sawnwood products #
##############################################
# See also specific changes above
swd = calculateConsumptionNetTrade(swd)
swd = addGDPandDeflator(swd)
swd = calculateConstantPrices(swd)
# swd is now the most complete table
# Rename swd Coniferous and non coniferous
unique(swd$Item)

# Tables with reshaped data or aggregated information
swdtrade = reshapeLongTradeTable(swd)
swdagg = aggregateConsPriceTable(swd)
sawnwood = removeProdTradeKeepConsPrice(swd)
sawnwood = arrange(sawnwood, Item, Country, Year)


##############################################
# Call clean functions for roundnwood products #
##############################################
# rwd = calculateConsumptionNetTrade(rwd)
# rwd = addGDPandDeflator(rwd)
# rwd = calculateConstantPrices(rwd)
# # rwd is now the most complete table
# 
# # Tables with reshaped data or aggregated information
# rwdtrade = reshapeLongTradeTable(rwd)
# rwdagg = aggregateConsPriceTable(rwd)
# roundwood = removeProdTradeKeepConsPrice(rwd)
# roundwood = arrange(sawnwood, Item, Country, Year)



####################
# Save to end data #
####################
save(paperProducts, ppagg, pptrade, wb, file="enddata/EU27 paper products demand.rdata")

save(sawnwood, swdagg, swdtrade, wb, file="enddata/EU27 sawnwood demand.rdata")

# save(roundwood, rwdagg, rwdtrade, wb, file="endata/EU27 roundwood demand.Rdata")
