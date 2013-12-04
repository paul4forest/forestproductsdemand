# This script prepares data to reproduce the paper by Chas Amil and Buongiorno from
# 2000 “The demand for paper and paperboard - Econometric model for the European Union”
#
# A newer version is located in clean.r
# Cleaning has been made general so that it should be possible to modify the script 
# to extend the time series or the number of countries included
# 
# Author: Paul Rougieux - European Forest Institute
#
# Input: FAOSTAT and world bank data in the form of .RDATA files 
#        NA values in the input production and trade data 
#           are changed to 0 before consumption is calculated !
# Output: Data frames of paperproduct consumption and price, and GDP and population data
#         in a Rdata file "/enddata/EU15 paper products.rdata"
#

library(ggplot2)
library(plyr)
library(xlsx) # To export data to Excel

####################################
# Load FAOSTAT and World Bank data #
####################################
setwd("Y:/Macro/Demand Econometric Models/rawdata/")
print(load(file = "GDP_Deflator_Exchange_Rate_Population.rdata"))
print(load(file = "Paper and paperboard.rdata"))
EU = read.csv("EUCountries.csv", as.is=TRUE)


#############################################################################
# Select GDP and other WB data for 15 countries belonging to the EU in 1995 #
#############################################################################
EU15 = subset(EU ,EU15==1,select=c("Country","FAOST_CODE"))

# GDP in EU15 without Belgium-Luxembourg
GDPEU15 = subset(GDPDeflExchRPop,Country %in% EU15$Country) 
unique(GDPEU15$Country)

# Sum GDP of Belgium and Luxembourg, keep deflator of Belgium (biggest country)
GDPBELU = subset(GDPDeflExchRPop,Country %in% c("Belgium","Luxembourg"))
BELU = aggregate(GDPcurrentLCU ~ Year, data=GDPBELU, sum, na.action=na.pass)
stopifnot(nrow(BELU)==nrow(GDPBELU)/2)
GDPBE = subset(GDPBELU, Country == "Belgium")
GDPBE$Country = "Belgium-Luxembourg"
GDPBE$ISO2_WB_CODE = "BELU"
GDPBE$GDPcurrentLCU = BELU$GDPcurrentLCU

# Add Belgium-Luxembourg data back in main GDP table
GDPEU15 = rbind(GDPEU15, GDPBE)

# Write to csv 
#write.csv(GDPEU15, file="GDP and deflator.csv")


#####################################################################
# Convert Exchrate in Euro area countries to the Euro exchange rate #
#####################################################################
# World bank data doesn't have exchange rate for euro countries after 1999
# See http://www.ecb.int/press/pr/date/1998/html/pr981231_2.en.html
# Currency Units of national currencyfor ? 1
# Denmark, Sweden and the United kingdom never had the EURO
# Greece didn't have the EURO in 1999 but got it in 2001

# Euro Exchange Rate from 1999 included
EUR = subset(GDPDeflExchRPop, Country=="Euro area"&Year>=1999, select=c(Year,ExchR))
names(EUR) = c("Year", "ExchREUR")

# Extract Euro area countries from 1999
EuroArea99 = EU$Country[EU$Euro_Start_Year==1999]
GDPEuroArea99 = subset(GDPEU15, Country %in% EuroArea99 )
# Add ExchREUR
GDPEuroArea = merge(GDPEuroArea99, EUR, by="Year",all=TRUE) 

# Dollar exchange rate before 1999 is LCUExchRate / LCUExchRate to euro
for (country in EuroArea99){
    GDPEuroArea$ExchREUR[GDPEuroArea$Country==country&GDPEuroArea$Year<1999] = 
        GDPEuroArea$ExchR[GDPEuroArea$Country==country&GDPEuroArea$Year<1999] / 
            EU$Euro_Exchange_Rate[EU$Country==country]
    }
# Check ExchREUR column for France (I've done the calculation in excel)
#subset(GDPEuroArea, Country=="France", select=c(Year,ExchREUR))

# Greece joined in 2001
EUR = subset(EUR,Year>=2001)
GDPGreece = subset(GDPEU15, Country=="Greece")
GDPGreece = merge(GDPGreece, EUR, by="Year",all=TRUE) 
country = "Greece"
GDPGreece$ExchREUR[GDPGreece$Country==country&GDPGreece$Year<2001] = 
    GDPGreece$ExchR[GDPGreece$Country==country&GDPGreece$Year<2001] / 
        EU$Euro_Exchange_Rate[EU$Country==country]

# Combine all tables back into one
GDP_Rest = subset(GDPEU15, !Country %in% c(EuroArea99, "Greece"))
GDP_Rest$ExchREUR = GDP_Rest$ExchR 
GDP = rbind(GDPEuroArea, GDPGreece, GDP_Rest)

# Check if all rows originaly in GDPEU15 are still there
stopifnot(nrow(GDPEU15) == nrow(GDPGreece) + nrow(GDPEuroArea) + nrow(GDP_Rest))
nrow(GDP)


####################################
# Calculate the deflator base 1987 #
####################################
baseyear = 1987
#calculate deflator 
deflator = function(df){
    # Deflator after base year
    d = df$Deflator[df$Year>baseyear]
    df$DeflBase[df$Year>=baseyear] =
        Reduce(function(u,v) u*(1+v/100), d,init=1,accum=TRUE)

    # Deflator before base year (calculated from right to left)
    d = df$Deflator[df$Year<=baseyear]
    df$DeflBase[df$Year<=baseyear] = 
        Reduce(function(u,v) v/(1+u/100), d[-1], init=1, accum=TRUE, right=TRUE)
    return(df)
}
# Add deflator to the GDP table using the split/apply/combine pattern
x = split(GDP,GDP$Country)
x = lapply(x,deflator)
GDP = unsplit(x,GDP$Country)

# Same using plyr
test= ddply(GDP, .(Country), deflator)

# Note the use of the '.' function to allow
# group and sex to be used without quoting
#ddply(dfx, .(group, sex), summarize,
# mean = round(mean(age), 2),
# sd = round(sd(age), 2))


# Calculate the US deflator for baseyear, rename column to DeflUS
US = subset(GDPDeflExchRPop, Country =="United States", select=c(Country,Year,Deflator))
US = deflator(US)
names(US) = c("Country", "Year", "Deflator", "DeflUS")


##############################################
# Calculate GDP in constant US $ of baseyear #
##############################################
# Using the split/apply/combine pattern
constantGDP = function(df){
    df$GDPconstantUSD = df$GDPcurrentLCU / (df$DeflBase * df$ExchREUR[df$Year==baseyear])
    return(df)
}
x = split(GDP,GDP$Country)
x = lapply(x,constantGDP)
GDP = unsplit(x,GDP$Country)


# Old method using a loop instead of the split/apply/combine pattern
#y = data.frame()
#for (country in unique(GDP$Country)){
#        y = rbind(y,constantGDP(subset(GDP, Country==country)))}


####################################################
# Calculate apparent consumption in EU15 countries #
####################################################
# Extract prodution and trade in volume and value for EU15 countries 
# pp means Paper Products
pp = subset(paperAndPaperboardProducts$entity,FAOST_CODE %in% EU15$FAOST_CODE )

# Change NA values to 0 - Not recommended 
# But makes sence at least that import into Finland and Sweden are 0
pp[is.na(pp)] = 0

# Calculate apparent consumption
pp$Consumption = pp$Production + pp$Import_Quantity - pp$Export_Quantity

# Add GDPconstantUSD
pp = merge(pp, GDP[c("Year","Country","GDPconstantUSD")])

# Rename "Total paper and paperboard" and "Printing and Writing Paper"
pp$Item[pp$Item=="Paper and Paperboard"] = "Total Paper and Paperboard"
pp$Item[pp$Item=="Other Paper+Paperboard"] = "Other Paper and Paperboard"
pp$Item[pp$Item=="Printing+Writing Paper"] = "Printing and Writing Paper"

# Change item to an ordered factor, same as in Table 3 of ChasAmil2000
pp$Item = factor(pp$Item, ordered=TRUE,
                 levels=c("Total Paper and Paperboard", "Newsprint",
                          "Printing and Writing Paper", 
                          "Other Paper and Paperboard"))

##############################################################################
# Calculate price in constant US $ based on unit values of import and export #
##############################################################################
# Add GDP deflator for the USA
pp = merge(pp, subset(US, select=c(Year,DeflUS)))

# Calculate real $ prices per metric ton
# Price is the weighted average of the unit values of import and export
# (imp vol * imp value / imp vol + exp vol * exp value / exp vol) / (imp vol + exp vol)
#  =  (imp value + exp value) / (imp vol + exp vol)
# The GDP deflator for the united states was applied to nominal unitvalues
pp$Price = (pp$Import_Value + pp$Export_Value)/
                 (pp$Import_Quantity + pp$Export_Quantity) / 
                     pp$DeflUS *1000


########################
# save file to enddata #
########################
# Remove Production and trade data keep Consumption, price and revenue 
paperProductsDemand = subset(pp,select=c(Year, Country, Item, 
                                         Price, Consumption, GDPconstantUSD))

# Sort 
paperProductsDemand = arrange(paperProductsDemand, Item, Country, Year)

# Save to RDATA file
save(paperProductsDemand, GDP, file="../enddata/EU15 paper products demand.rdata")

# Export to Excel
wb = createWorkbook()
style = CellStyle(wb) + Font(wb, isBold=TRUE) + Border()  # header style
addDataFrame(paperProductsDemand, row.names=FALSE, colnamesStyle=style,
             sheet = createSheet(wb, sheetName = "PaperDemand"))
addDataFrame(GDP, row.names=FALSE, colnamesStyle=style, 
             sheet = createSheet(wb, sheetName = "GDPandPopulation"))
saveWorkbook(wb, "../enddata/Data for Demand Model EU15.xlsx")
names(getSheets(wb) )

##################
# Test functions #
##################
# Check if FAOST codes from the .csv file are the same as in the FAOSTAT module
df = merge(EU15, countrycodes, by="Country")
stopifnot(df$FAOST_CODE.x == df$FAOST_CODE.y)

# Check if EURO calculation kept all rows from the GDPEU15 table
stopifnot(nrow(GDPEU15) == nrow(GDPGreece) + nrow(GDPEuroArea) + nrow(GDP_Rest))

# Calculate  growth rate for France
GDPconsFR = GDP$GDPconstantUSD[GDP$Country=="France"]
rate = c(NA, 100 * ( GDPconsFR[-1] / GDPconsFR[-length(GDPconsFR)] - 1 ))
# print(data.frame(Year=GDP$Year[GDP$Country=="France"],Growth_Rate=rate))


# Then please check if growth rates in constant US$ correspond to WB data for the whole period
# I would need to download growth data from the WB
