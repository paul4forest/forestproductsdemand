# Prepare graphs for a presentation at a thesis committee meeting in Nancy
# Author Paul Rougieux - European Forest Institute

library(ggplot2)

print(load("enddata/EU27 paper products demand.rdata"))
pp = paperProducts # Give a shorter name to the data frame

# Load also raw data for some analysis
# print(load(file = "rawdata/Paper and paperboard.rdata"))

# Convenience function to set a place to store images of plots
figpath = function(filename){
    paste("docs/ExplorePaperProducts/Presentations/",filename, sep="")  
}


##############################################
# Overview Production, Consumption and Trade #
##############################################
# Create an aggregated table with graphics papers following Lauri's comments
ppgraphagg = subset(ppagg, Item %in% c("Newsprint","Printing and Writing Paper"), 
                    select=-c(Deflator))
ppgraphagg = aggregate(ppgraphagg[c("Value")], ppgraphagg[c("Year", "Element")], sum)
ppgraphagg$Item = "Graphics Paper"
ppgraphagg = rbind(ppgraphagg, subset(ppagg, Item=="Other Paper and Paperboard",
                         select=c(Year, Item, Element, Value)))

# 5 plots in frames cons, prod, imp, exp, net-trade
p = ggplot(data=subset(ppgraphagg, Element%in%c("Consumption", "Production", "Net_Trade",
                                          "Import_Quantity","Export_Quantity"))) +
    geom_line(aes(x=Year, y=Value/1e+06, colour=Item), size=1) +
    xlab("Year") + ylab("Million metric Ton") + theme_bw() + facet_wrap(~Element)
pdf(figpath("GPEUProdandTrade.pdf"), 10,7)
plot(p)
dev.off()

##################################################################
# Create a table with graphics papers following Lauri's comments #
##################################################################
ppgraph = subset(pp, Item %in% c("Newsprint","Printing and Writing Paper"))
ppgraph = aggregate(ppgraph[c("Consumption", "Net_Trade")],
                    ppgraph[c("Year", "Country")], sum)
ppgraph$Item = "Graphics Paper"
ppgraph = rbind(ppgraph, subset(pp, Item == "Other Paper and Paperboard", 
                                c(Year, Country, Item, Consumption, Net_Trade)))

# Plot graphics paper and OPAP in EU 27
GPEU = aggregate(ppgraph["Consumption"], ppgraph[c("Year", "Item")], sum, na.rm=TRUE)
p = ggplot(data=GPEU)+ geom_line(aes(x=Year, y=Consumption/1e+06, colour=Item), size=1) 
p = p + xlab("Year") + ylab("Million metric Ton") + theme_bw()
pdf(figpath("GPEUCons.pdf"), 10,5)
plot(p)
dev.off()

# Select the 5 largest consumers and the 2 largest exporters
ppgraph7 = subset(ppgraph, 
                  Country %in% c("Finland", "France", "Germany", "Italy",
                                 "Spain", "Sweden", "United Kingdom"))

# Plot graphics paper consumption and net trade in the 7 countries
# Create a vector of the 7 Countries for plotting purposes

ppgraph7$Country = factor(ppgraph7$Country, ordered=TRUE, 
                          levels = c("France", "Germany", "Italy", "Spain",
                                     "United Kingdom", "Finland", "Sweden"))

p = ggplot(data=ppgraph7) +
    geom_line(aes(x=Year, y=Consumption/1e+06, colour=Item), size=1) +
    xlab("Year") + ylab("Million metric Ton") + 
    scale_x_continuous(breaks=seq(1970,2010,20)) +       
    theme_bw() + facet_wrap(~Country, nrow=2)

pdf(figpath("GP5CountriesCons.pdf"), 10,5)
plot(p)
dev.off()

# Plot with net trade
ggplot(data=ppgraph7)+
    geom_line(aes(x=Year, y=Consumption/1e+06, 
                  colour=Item, linetype = "Consumption"), size=1) + 
    geom_line(aes(x=Year, y=Net_Trade/1e+06, 
                  colour=Item, linetype = "Net trade"),size=1) + 
    facet_wrap(~Country, scales = "free_y", nrow=2) +
    xlab("Year") + ylab("Million metric Ton") + 
    scale_x_continuous(breaks=seq(1970,2010,20)) +
    theme_bw()

pdf(figpath("GP5CountriesConsNetTrade.pdf"), 10,5)
plot(p)
dev.off()



#############
# GDP EU 27 #
#############
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

# Plot Graphics paper in all EU27 countries
ggplot(data=ppgraph)+
    geom_line(aes(x=Year, y=Consumption/1e+06, colour=Item), size=1) + 
    facet_wrap(~Country, scales = "free_y") +
    xlab("Year") + ylab("Million metric Ton") + 
    scale_x_continuous(breaks=seq(1970,2010,20)) +    
    theme_bw() 

# Command to add different color of curves
#     scale_colour_manual(values = c("Blue", "Green"))




# plot graphics paper in the 7 major countries
ggplot(data=ppgraph7)+
    geom_line(aes(x=Year, y=Consumption/1e+06, colour=Item), size=1) + 
    facet_wrap(~Country, scales = "free_y", nrow=2) +
    xlab("Year") + ylab("Million metric Ton") + 
    scale_x_continuous(breaks=seq(1970,2010,20)) +
    theme_bw() 






# Old stuff copied from Lillehammer 




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
