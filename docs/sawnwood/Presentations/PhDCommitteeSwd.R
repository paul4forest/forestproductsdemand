library(ggplot2)
library(plyr)
print(load("enddata/EU27 sawnwood demand.rdata"))
EU = read.csv("rawdata/EUCountries.csv")
swd = sawnwood # Give a shorter name to the data frame

# Convenience function to set a place to store images of plots
figpathswd = function(filename){
    paste("docs/ExploreSawnwood/Presentations/",filename, sep="")  
}

#################################
# Plot swd consumption in EU 27 #
#################################
swdEU = aggregate(swd["Consumption"], swd[c("Year", "Item")], sum, na.rm=TRUE)

p = ggplot(data=subset(swdEU, Item!="Total Sawnwood")) +
    geom_line(aes(x=Year, y=Consumption/1e+06, colour=Item), size=1) +
    xlab("Year") + ylab("Million m3") + theme_bw()

pdf(figpathswd("swdEUCons.pdf"), 10,5)
plot(p)
dev.off()

###############################################################################
# Plot sawnwood consumption and net trade in the 10 major consuming countries #
###############################################################################
# Find out which are the major consumers in 2012
swdCons = arrange(subset(swd,Year==2012&Item=="Total Sawnwood"), -Consumption)[1:10,]
# Just for information, the major net traders in 2012
arrange(subset(swd,Year==2012&Item=="Total Sawnwood"), -Net_Trade)[1:10,1:7]
 # Select the 10 largest consumers 
swd10 = subset(swd, Country %in% swdCons$Country)
# Create an ordered vector of the 10 Countries for plotting purposes
swd10$Country = factor(swd7$Country, ordered=TRUE, swdCons$Country)

p = ggplot(data=subset(swd10, Item!="Total Sawnwood")) +
    geom_line(aes(x=Year, y=Consumption/1e+06, colour=Item), size=1) +
    xlab("Year") + ylab("Million m3") + 
    scale_x_continuous(breaks=seq(1970,2010,20)) +       
    theme_bw() + facet_wrap(~Country, nrow=2) 
#     scale_color_manual(values=c("green", "blue"))

pdf(figpathswd("swd10CountriesCons.pdf"), 12,5)
plot(p)
dev.off()


