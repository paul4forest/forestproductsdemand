#
# Prepare graphs for a presentation at a conference in Lillehammer, Norway
# Author Paul Rougieux - European Forest Institute

library(ggplot2)
setwd("Y:/Macro/forestproductsdemand/")
print(load("./enddata/EU27 paper products demand.rdata"))

# Load also raw data for some analysis
setwd("Y:/Macro/forestproductsdemand/rawdata/")
print(load(file = "Paper and paperboard.rdata"))

# Set a convenient place to store images of plots
setwd("docs/ExploreData/Presentations/")
pp = paperProducts # Give a shorter name to the data frame


# GDP EU 27
GDPEU = aggregate(wb["GDPconstantUSD"], wb[c("Year")], sum, na.rm=TRUE)
ggplot(data=subset(GDPEU, Year>=1969), aes(x=Year, y=GDPconstantUSD/1e9)) + 
    geom_line(size=1) + xlab("Years") + ylab("GDP in constant (2010) billion USD") +
    theme_bw()

# GDP EU 27 by country
ggplot(data=subset(wb, Year>=1969), aes(x=Year, y=GDPconstantUSD/1e9)) + 
    geom_line(size=1) + xlab("Years") + ylab("GDP in constant (2010) billion USD") +
    facet_wrap(~Country, scales = "free_y") +
    scale_x_continuous(breaks=seq(1970,2010,20)) +    
    theme_bw()


# Create a new table with graphics papers following Lauri's comments
ppgraph = subset(pp, Item %in% c("Newsprint","Printing and Writing Paper"))
ppgraph = aggregate(ppgraph[c("Consumption", "Net_Trade")],
                    ppgraph[c("Year", "Country")], sum)
ppgraph$Item = "Graphics Paper"


# Add other paper and paperboard
ppgraph = rbind(ppgraph, 
                subset(pp, Item == "Other Paper and Paperboard", 
                       c(Year, Country, Item, Consumption, Net_Trade)))


# Plot graphics paper and OPAP in EU 27
GPEU = aggregate(ppgraph["Consumption"], ppgraph[c("Year", "Item")], sum, na.rm=TRUE)
ggplot(data=GPEU)+ geom_line(aes(x=Year, y=Consumption/1e+06, colour=Item), size=1) + 
    xlab("Year") + ylab("Million metric Ton") + 
    theme_bw()


# Plot Graphics paper in all EU27 countries
ggplot(data=ppgraph)+
    geom_line(aes(x=Year, y=Consumption/1e+06, colour=Item), size=1) + 
    facet_wrap(~Country, scales = "free_y") +
    xlab("Year") + ylab("Million metric Ton") + 
    scale_x_continuous(breaks=seq(1970,2010,20)) +    
    theme_bw() 

# Command to add different color of curves
#     scale_colour_manual(values = c("Blue", "Green"))


# Select the 5 largest consumers and the 2 largest exporters
ppgraph7 = subset(ppgraph, 
                 Country %in% c("Finland", "France", "Germany", "Italy",
                                "Spain", "Sweden", "United Kingdom"))


# plot graphics paper in the 7 major countries
ggplot(data=ppgraph7)+
    geom_line(aes(x=Year, y=Consumption/1e+06, colour=Item), size=1) + 
    facet_wrap(~Country, scales = "free_y", nrow=2) +
    xlab("Year") + ylab("Million metric Ton") + 
    scale_x_continuous(breaks=seq(1970,2010,20)) +
    theme_bw() 


# Plot graphics paper consumption and net trade in the 7 countries
# Create a vector of the 7 Countries for plotting purposes

ppgraph7$Country = factor(ppgraph7$Country, ordered=TRUE, 
                          levels = c("France", "Germany", "Italy", "Spain",
                                     "United Kingdom", "Finland", "Sweden"))

ggplot(data=subset(ppgraph7))+
    geom_line(aes(x=Year, y=Consumption/1e+06, 
                  colour=Item, linetype = "Consumption"), size=1) + 
    geom_line(aes(x=Year, y=Net_Trade/1e+06, 
                  colour=Item, linetype = "Net trade"),size=1) + 
    facet_wrap(~Country, scales = "free_y", nrow=2) +
    xlab("Year") + ylab("Million metric Ton") + 
    scale_x_continuous(breaks=seq(1970,2010,20)) +
    theme_bw()


# Prices of graphics paper in the EUropean Union
ppaggw = reshape(ppagg, timevar="Element", idvar=c("Year", "Item"), direction="wide")
names(ppaggw) = c("Year", "Item", sub("Value.","",names(ppaggw[-1:-2])))
ppgraphagg = subset(ppaggw, Item %in% c("Newsprint","Printing and Writing Paper"))


# Add a new aggregate for graphics paper
ppgraphagg =  aggregate(ppgraphagg[c("Consumption", "Net_Trade", "Import_Quantity", 
                                     "Export_Quantity", "Import_Value", "Export_Value")],
                                 ppgraphagg[c("Year")], sum)
ppgraphagg$Item = "Graphics Paper"
ppgraphagg = rbind(ppgraphagg, 
                   subset(ppaggw, Item =="Other Paper and Paperboard",
                          select=c("Year", "Item","Consumption", "Net_Trade",
                                   "Import_Quantity", "Export_Quantity",
                                   "Import_Value", "Export_Value")))

ggplot(data=subset(ppgraphagg)) +
    geom_line(aes(x=Year, y=Import_Quantity/Consumption, color=Item)) +
    ylab("Import quantity / consumption")


# Select only the 7 important country
# Select the 5 largest consumers and the 2 largest exporters
pptrade5 = subset(pptrade, 
                  Country %in% c("France", "Germany", "Italy",
                                 "Spain", "United Kingdom"))

# Plot graphics paper consumption and net trade in the 7 countries
# Create a vector of the 7 Countries for plotting purposes
pptrade5$Country = factor(pptrade5$Country, ordered=TRUE, 
                          levels = c("France", "Germany", "Italy", "Spain",
                                     "United Kingdom", "Finland", "Sweden"))


ggplot(data=subset(pptrade5, Trade=="Import"&Item!="Total Paper and Paperboard")) +
    geom_line(aes(x=Year, y=Quantity/Consumption, color=Item)) +
    facet_wrap(~Country, nrow=1) +
    ylab("Import quantity / consumption") +
    scale_x_continuous(breaks=seq(1970,2010,20)) 
