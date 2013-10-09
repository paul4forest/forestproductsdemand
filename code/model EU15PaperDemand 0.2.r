###################################################
# Main model is in a .rnw file in the Docs folder #
###################################################
# Reproduce estimates by Chas Amil and Buongiorno 2000
# 
# Input: A table of production and trade data for paper and paperboard products
#            Including GDP and Price information
# Output: Estimates of demand elasticities
#
# Author: Paul Rougieux, European Forest Institute
#
# For plot to visualise demand data, see in the /docs folder
#
library(plyr)
library(ggplot2)

####################################################
# Load and prepare consumption, GDP and price data #
####################################################
setwd("Y:/Macro/Demand Econometric Models/enddata/")
print(load("EU15 paper products demand.rdata"))

# Create a column consumption at year minus one (t-1)
pp = ddply(paperProductsDemand, .(Country,Item), mutate, 
           Consum_t_1 = c(NA,Consumption[-length(Consumption)]))

#############################################
# Estimate model for Total Paper by Country #
#############################################
tp = subset(pp, Item=="Total Paper and Paperboard")

# Static model (alpha = 1 => delta3=0)
stm = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price), data=tp, na.action=na.exclude)
summary(stm)

# Dynamic model 
dym = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), data=tp)
#str(summary(dym))
summary(dym)$r.squared
cochrane.orcutt(dym)

# A function that will apply the dynamic model to all countries 
dynmodel = function(df){
  dym = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), data=df)
  coefs = data.frame(t(summary(dym)$coefficients[,1:2]))
  if (nrow(coefs)==2){
    coefs$R_Squared  = c(summary(dym)$r.squared,NA)
  }
  return(coefs)
}

# Split by countries and apply the dynamic model to each country
x = split(tp,tp$Country)
x = lapply(x, dynmodel)
x$Ireland = NULL
x = do.call(rbind, x)
round(x[,-1],2)

dymF = lm(data=x$France, log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1))
coefs = data.frame(t(summary(dym)$coefficients[,1:2]))
coefs$R_Squared  = c(summary(dym)$r.squared,NA)

plot(dymF)

# Apply cochrane Orcutt correction
# Using the orcutt package, see example on page 3
# http://cran.r-project.org/web/packages/orcutt/orcutt.pdf
# A man on this page says that its a historicaly method:
# https://stat.ethz.ch/pipermail/r-help/2002-January/017774.html
cochrane.orcutt(dym)


#####################################################
# Estimate model for all products using OLS pooling #
#####################################################
ppool = subset(pp, Year>=1969&Year<=1992)

# Remove Ireland because we don't have Deflator data 
# Remove the Year 1969 for Germany because it doesn't have GDP for that year
ppool = subset(ppool, Country!="Ireland")
ppool = subset(ppool, !(Country=="Germany"&Year==1969))

# A function that creates a dynamic model 
dynModel = function(dtf){
    lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1),
       data=dtf)
}

# Create a list of dynamic models for all products (Items)
dm = dlply(ppool, .(Item), dynModel)

# Extract estimated coefficients 
ldply(names(dm),
      function(m) c(Item = m, round(dm[[m]]$coefficients[-1],2)))

# Density plot of the studentized residuals 
#     for Newsprint
plot(density.default(x = rstudent(dm$Newsprint)), main="Newsprint")

#     For the 3 products and total
par(mfrow=c(2,2))
l_ply(names(dm), 
      function(m) plot(density.default(x = rstudent(dm[[m]])), main=m))
par(mfrow=c(1,1))

# Hat values
hatvalues(dm$Newsprint)

# Quantile plot to check normal distribution
qqplot(dm$Newsprint, distribution="norm")

# Data frame of model values
# dm$Newsprint$model
plot(dm$Newsprint$fitted.values)
plot(dm$Newsprint$residuals)
dm$Newsprint$call



#########################
# Plot price histograms #
#########################
hist(pp$Price)

# Plot 4 histograms, one for each product and total
ggplot(pp, aes(Price)) + geom_histogram(binwidth = 100)+
    facet_wrap(~ Item )

# Loop for all years, 
#pdf(file = "Price histograms of Paper and paperboard in EU15.pdf")
#for (y in rev(unique(pp4hist$Year))){
#    p = ggplot(pp4hist[pp4hist$Year==y,], aes(Price)) + 
#        geom_histogram(binwidth = 100)+
#        scale_x_continuous(limits=c(0,2000))+
#        scale_y_continuous(limits=c(0,15))+
#        facet_wrap(~ Item) +
#        ggtitle(paste("Price of paper and paperboard products in EU15 countries in",as.character(y))) +
#        ylab("Number of countries") + xlab("Price in constant US$ per Ton")
#    print(p)
#}
#dev.off()


