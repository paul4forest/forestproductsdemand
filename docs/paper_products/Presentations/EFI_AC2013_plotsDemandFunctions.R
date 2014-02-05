# Presentation at the EFICENT-OEF annual meeting during the EFI 2013 conference
# Demand functions for paper products


####################################################
# Load and prepare consumption, GDP and price data #
####################################################
library(ggplot2)
library(plyr)
library(xtable)

setwd("Y:/Macro/forestproductsdemand/enddata/")
load("EU15 paper products demand.rdata")

# Create a column consumption at year minus one (t-1)
pp = paperProductsDemand

###############################################################
# Plot consumption of paper and paperboard products in the EU #
###############################################################
# Select Consumption data from 1969 to 1995 
ppc = subset(pp, Year>=1969, select=c(Item, Country, Year, Consumption))
ppc = aggregate(ppc["Consumption"], ppc[c("Item","Year")], sum)

# Change Tons to Million Tons
ppc$Consumption = ppc$Consumption/1e6

# Plot total consumption at the european level
ggplot(data=ppc, aes(x=Year, y=Consumption, colour=Item)) + 
    geom_line(size=1) + xlab("Years") + ylab("Million metric Ton") +
    theme_bw() + theme(legend.position= "bottom") 

# Plot consumption for all countries
ggplot(data=subset(pp, Item=="Total Paper and Paperboard"), aes(x=Year, y=Consumption, colour=Item)) + 
    geom_line(size=1) + xlab("Years") + ylab("Thousad metric Ton") +
    theme_bw() + theme(legend.position= "bottom") + facet_wrap(~Country, scales = "free_y")

##################################
# Plot GDP at the european level #
##################################
GDP2 = subset(GDP, select=c(Year, Country, GDPconstantUSD))
GDP2 = aggregate(GDP["GDPconstantUSD"], GDP2[c("Year")], sum, na.rm=TRUE)

# # PlotGDP at the european 15 level
ggplot(data=subset(GDP2, Year>=1969), aes(x=Year, y=GDPconstantUSD)) + 
    geom_line(size=1) + xlab("Years") + ylab("GDP in constant USD") +
    theme_bw() + theme(legend.position= "bottom")

# 1 plot per country
ggplot(data=GDP, aes(x=Year, y=GDPconstantUSD, colour=Country)) + 
    geom_line(size=1) + xlab("Years") + ylab("GDP in constant USD") +
    theme_bw() + theme(legend.position= "none") + facet_wrap(~Country, scales = "free_y")

# 1 plot per country, same scale
ggplot(data=GDP, aes(x=Year, y=GDPconstantUSD, colour=Country)) + 
    geom_line(size=1) + xlab("Years") + ylab("GDP in constant USD") +
    theme_bw() + theme(legend.position = "none") + facet_wrap(~Country)

