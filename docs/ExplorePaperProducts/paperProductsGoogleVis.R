
# Trying the Google Vis API 
# Gadget examples here:
# http://code.google.com/p/google-motion-charts-with-r/wiki/GadgetExamples

library(googleVis)
library(plyr)
print(load("enddata/EU27 paper products demand.rdata"))
pp = paperProducts

##############
# Line Chart #
##############
# Recalculate aggregate so that it stays in wide format
ppagg2 = subset(pp, select=c("Item", "Year", "Consumption", "Net_Trade"))

# Remove NA values not good, but do it here to calculate the aggregate
ppagg2[is.na(ppagg2)] = 0 
ppagg2 = aggregate(ppagg2[c("Consumption", "Net_Trade")], ppagg2[c("Item", "Year")],sum)
newsCons = subset(ppagg2, Item=="Newsprint")
m = gvisLineChart(newsCons,xvar="Year", yvar=c("Consumption", "Net_Trade"),
                  options=list(title="EU27 Newsprint Consumption and Net Trade"))
plot(m)


####################################
# MOtion Chart Consumption and GDP #
####################################
# Plot comparing consumption with GDP
# news = subset(pp, Item=="Newsprint", select=c(Year, Country, GDPconstantUSD, Consumption))
# m = gvisMotionChart(news, idvar="Country", timevar="Year", colorvar="Consumption")

ppwide = reshape(subset(pp, select=c(Year, Country, Item, Consumption, GDPconstantUSD)),
                 v.names = "Consumption", idvar = c("Year","Country","GDPconstantUSD"),
                 timevar = "Item", direction = "wide")

# Rename total so that it appears first (somehow yvar parameter doesn't work in motion chart)
ppwide$"Consumption Total Paper and Paperboard" = ppwide$"Consumption.Total Paper and Paperboard"
ppwide$"Consumption.Total Paper and Paperboard" = NULL
ppwide = subset(ppwide, Year>=1970)
m = gvisMotionChart(ppwide, idvar="Country", timevar="Year", 
                     xvar="GDPconstantUSD", yvar="Consumption Total Paper and Paperboard",
                    colorvar="Consumption Total Paper and Paperboard")
# plot(m)

# Export to a web page
# How-To found here:
# http://stackoverflow.com/questions/4646779/embedding-googlevis-charts-into-a-web-site
# cat(m$html$chart, file="docs/ExplorePaperProducts/tmp.html")
g <- createGoogleGadget(m) # For a google site
cat(g, file="docs/ExplorePaperProducts/PaperConsumptionAndGDP.xml")


##########################################################
# MOtion Chart Consumption per capita and GDP per capita #
##########################################################
# Plot comparing consumption per capita with GDP per capita
# Keep Consumption, Population and GDP in the table for the size of the bubbles
# Add Population data
pp = merge(pp, subset(wb,select=c(Country,Year,Population)), all.x=TRUE)
# calculate Consumption per capita and GDP per capita #
pp = mutate(pp, ConsPerCapita = Consumption/Population*1000,
            GDPpercapita = GDPconstantUSD/Population)


ppwide = reshape(subset(pp, select=c(Year, Country, Item, ConsPerCapita, GDPpercapita)),
                 v.names = "ConsPerCapita", idvar = c("Year","Country","GDPpercapita"),
                 timevar = "Item", direction = "wide")

# Add Total Consumption to the table for the size of the bubbles
pptot = subset(pp, Item=="Total Paper and Paperboard", select=c(Year,Country,Consumption))
ppwide = merge(ppwide,pptot,all.x=TRUE)

m = gvisMotionChart(ppwide, idvar="Country", timevar="Year", 
                    yvar="ConsPerCapita.Total Paper and Paperboard", xvar="GDPpercapita", 
                    colorvar="ConsPerCapita.Total Paper and Paperboard", size="Consumption")
# plot(m)
g <- createGoogleGadget(m) # For a google site
cat(g, file="docs/ExplorePaperProducts/PaperConsumptionAndGDPperCapita.xml")


# Post to my blog
# The googleVis package comes with the function createGoogleGadget, which
# takes a gvis-object and wraps it into an XML gadget file. Here is an example with
# a motion chart:
#     R> M <- gvisMotionChart(Fruits, "Fruit", "Year")
# R> G <- createGoogleGadget(M)
# R> cat(G, file="myGadget.xml")
# In order to use the gadget, the file myGadget.xml has to be hosted online, e.g. using
# Google Docs. Suppose the URL to the gadget is http://example.com/myGadget.xml,
# than you can embed the gadget


