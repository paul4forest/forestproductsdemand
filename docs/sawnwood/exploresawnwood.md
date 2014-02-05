Demand for Sawnwood in the European Union
=========================================





### Load data
FAOSTAT is the source of forest products data and
World Bank is the source of GDP, deflator and exchange rate data.  

```r
setwd("../..")
print(load("enddata/EU27 sawnwood demand.rdata"))
```

```
## [1] "sawnwood" "swdagg"   "swdtrade" "wb"
```

```r
EU = read.csv("rawdata/EUCountries.csv")
swd = sawnwood  # Give a shorter name to the data frame
```


Consumption, Production and Trade Volumes
-----------------
In the following file, we explore a data table containing paper and paperboard consumption data for __27__ countries from 1961 to 2012.  
__In 2012, the overall EU consumption, production and trade in million Tons per item was:__

```
##                        Item Consumption Production Net_Trade Import Export
## 154          Total Sawnwood          85         98        13     34     47
## 155     Sawnwood Coniferous          75         89        14     30     43
## 156 Sawnwood Non Coniferous           9          9         0      4      4
```





```r
# Consumption, production, trade and net trade at the European Level
ggplot(data = subset(swdagg, !Element %in% c("Import_Value", "Export_Value", "Price"))) + 
    geom_line(aes(x = Year, y = Value/1e+06, colour = Item), size = 1) + facet_wrap(~Element) + 
    ylab("Million metric Ton") + theme_bw()
```

![plot of chunk consumptionTotalEU](figure/consumptionTotalEU.png) 



Relationship between log(Consumption) and log(GDP) 
--------------------------------------------------
### Total Sawnwood
Data for 2000 and beyond in red
Data for EU15 countries in Blue

```r
plot(log(Consumption) ~ log(GDPconstantUSD), data = subset(swd, Item == "Total Sawnwood"))
```

```
## Warning: NaNs produced
```

```r
points(log(Consumption) ~ log(GDPconstantUSD), data = subset(swd, Item == "Total Sawnwood" & 
    Year > 1999), col = "red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
# swd = merge(swd, head(subset(EU, select=c(Country,EU15))))
```



### Explore the influence of Year and Net_Trade by country

```r

p = ggplot(data = subset(swd, Item != "Total Sawnwood"), aes(x = log(GDPconstantUSD), 
    y = log(Consumption))) + facet_wrap(~Item)
p + scale_shape_identity() + geom_point(aes(color = Country, shape = 1 + 2 * (Net_Trade > 
    0)))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 



### Plot for Total Sawnwood 

```r

p = ggplot(data = subset(swd, Item == "Total Sawnwood"), aes(x = log(GDPconstantUSD), 
    y = log(Consumption))) + facet_wrap(~Item)
p + scale_shape_identity() + geom_point(aes(color = Country, shape = 1 + 2 * (Net_Trade > 
    0)))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


# Sort countries by net trade - Maybe this can be put in the clean script?
