# Reproduce estimates by Chas Amil and Buongiorno 2000
# 
# Input: A table of production and trade data for paper and paperboard products
#            Including GDP and Price information
# Output: Estimates of demand elasticities
#
# Author: Paul Rougieux, European Forest Institute
#
library(plyr)

# Load consumption, GDP and price data

setwd("Y:/Macro/Demand Econometric Models/enddata/")
print(load("EU15 paper products demand.rdata"))

########################################################
# Prepare estimation data for Chas-Amil and buongiorno #
########################################################
pp = paperProductsDemand
# Create a column consumption at t-1 
createConsum_t_1 = function(df){
  df$Consum_t_1 = c(NA,df$Consumption[-length(df$Consumption)])
  return(df)
}
x = split(pp, list(pp$Country, pp$Item))
x = lapply(x, createConsum_t_1)
pp = unsplit(x, list(pp$Country, pp$Item))

# Creade a column Consum_t_1_
pp = ddply(pp, .(Country,Item), mutate, 
            Consum_t_1_bis = c(NA,Consumption[-length(Consumption)]))

pp2 = mutate(pp2, Consum_t_1_Comp = Consum_t_1 - Consum_t_1_bis)

summary(pp2)
# Select only interesting Years and columns for Chas Amil and Buongiorno
ppcab = subset(pp, Year>=1969&Year<=1992, 
               select=c(Year, Country, Item, Price, Consumption, Consum_t_1, GDPconstantUSD))

#############################################
# Estimate model for Total Paper by Country #
#############################################
tp = subset(ppcab, Item=="Paper and Paperboard")

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
