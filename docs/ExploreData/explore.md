Demand for paper and paperboard products in the European Union
========================================================
* [Consumption, Production and Trade](#CPT)
* [Consumption](#Consumption)
 * [Total consumption by Country](#TCbyCountry)
 * [Consumption of Graphics paper and other paper by Country](#CGPOpapbyCountry)
 * [Consumption and net trade by Country](#CNTbyCountry)
* [GDP by Country](#GDPbyCountry)
* [Prices](#Prices)
* [Distributions](#Distribution)
* [Plot log of the estimation data](#EstimationData)




### Load data
FAOSTAT is the source of forest products data and
World Bank is the source of GDP, deflator and exchange rate data.  

```r
print(load("Y:/Macro/forestproductsdemand/enddata/EU27 paper products demand.rdata"))
```

```
## [1] "paperProducts" "ppagg"         "pptrade"       "wb"
```

```r
pp = paperProducts  # Give a shorter name to the data frame
```


<a name="CPT"></a>

Consumption, Production and Trade Volumes
-----------------
In the following file, we explore a data table containing paper and paperboard consumption data for __27__ countries from 1961 to 2012.  
__In 2012, the overall EU consumption, production and trade in million Tons per item was:__

```
##                           Item Consumption Production Net_Trade Import Export
## 205 Total Paper and Paperboard          80         92        12     49     61
## 206                  Newsprint           7          8         0      5      5
## 207 Printing and Writing Paper          24         32         8     20     27
## 208 Other Paper and Paperboard          49         53         4     24     28
```




```r
# Consumption, production, trade and net trade at the European Level
ggplot(data = subset(ppagg, !Element %in% c("Import_Value", "Export_Value", "Price"))) + 
    geom_line(aes(x = Year, y = Value/1e+06, colour = Item), size = 1) + facet_wrap(~Element) + 
    ylab("Million metric Ton") + theme_bw()
```

![plot of chunk consumptionTotalEU](figure/consumptionTotalEU.png) 

Net trade is slightly in favor of exports for most products except for newsprint. From bilateral trade statistics (not shown here), we know that most of the trade is happening inside the European Union.


<a name="Consumption"></a>
Consumption
-----------
<a name="TCbyCountry"></a>
### Consumption by country 
Apparent consumption of Total paper and paperboard by country 

```r
ggplot(data = subset(pp, Item == "Total Paper and Paperboard")) + geom_line(aes(x = Year, 
    y = Consumption/1e+06, colour = Item), size = 1) + facet_wrap(~Country, scales = "free_y") + 
    xlab("Year") + ylab("Million metric Ton") + theme_bw() + theme(legend.position = "bottom")
```

![plot of chunk consumptionbyCountry](figure/consumptionbyCountry.png) 

The largest consumers of paper and paperboard products are France, Germany, Italy, Spain, United Kingdom.


<a name="CGPOpapbyCountry"></a>

### Apparent consumption of graphics paper and other paper and paperboard by country 


```r
# Create a new table with graphics papers following Lauri's comments
ppgraph = subset(pp, Item %in% c("Newsprint", "Printing and Writing Paper"))
nrow(ppgraph)
```

```
## [1] 2275
```

```r
ppgraph = aggregate(ppgraph[c("Consumption", "Net_Trade")], ppgraph[c("Year", "Country")], 
    sum)
nrow(ppgraph)
```

```
## [1] 1138
```

```r
ppgraph$Item = "Graphics Paper"
ppgraph = rbind(ppgraph, subset(pp, Item == "Other Paper and Paperboard", c(Year, 
    Country, Item, Consumption, Net_Trade)))

ggplot(data = ppgraph) + geom_line(aes(x = Year, y = Consumption/1e+06, colour = Item), 
    size = 1) + facet_wrap(~Country, scales = "free_y") + xlab("Year") + ylab("Million metric Ton") + 
    theme_bw() + theme(legend.position = "bottom")
```

![plot of chunk graphAndOtherconsumptionbyCountry](figure/graphAndOtherconsumptionbyCountry.png) 

```r
scale_colour_manual(values = c("Blue", "Green"))
```

```
## discrete_scale(aesthetics = aesthetic, scale_name = "manual", 
##     palette = pal)
```


<a name="CNTbyCountry"></a>

### Consumption and Net Trade by Country
Apparent consumption and Net Trade of Total paper and paperboard by country 

```r
ggplot(data = subset(pp, Item == "Total Paper and Paperboard")) + geom_line(aes(x = Year, 
    y = Consumption/1e+06, colour = Item, linetype = "Consumption"), size = 1) + 
    geom_line(aes(x = Year, y = Net_Trade/1e+06, colour = Item, linetype = "Net trade"), 
        size = 1) + facet_wrap(~Country, scales = "free_y") + xlab("Year") + ylab("Million metric Ton") + 
    theme_bw() + theme(legend.position = "bottom")
```

![plot of chunk consumptionNetTradebyCountry](figure/consumptionNetTradebyCountry.png) 

Large producing countries such as Finland and Sweden produce over 10 million tons of paper products per year and export most of their production. Germany is the largest consumer with over 20 million tons of paper products consumed annually, Germany's net trade is slightly positive. France is the second largest consumer and is slightly importing. The United Kingdom also consumes around 10 million tons annually and is importing more than half of its production.

<a name="GDPbyCountry"></a>

### GDP by country

```r
# 1 plot per country
ggplot(data = wb, aes(x = Year, y = GDPconstantUSD/1e+09)) + geom_line(size = 1) + 
    xlab("Years") + ylab("GDP (constant) in 2010 Billion USD ") + theme_bw() + theme(legend.position = "none") + 
    facet_wrap(~Country, scales = "free_y")
```

![plot of chunk GDPbyCountry](figure/GDPbyCountry.png) 



<a name="Prices"></a>

Prices of paper and paperboard
----------------
### EU prices
Average trade price of paper products in the European Union

```r
ggplot(data = subset(ppagg, Element == "Price")) + geom_line(aes(x = Year, y = Value, 
    color = Item)) + ylab("Price in (2010) constant USD /m3")
```

![plot of chunk priceEU](figure/priceEU.png) 

Price has lowered since 2010, probably due to overcapacity in the market.

### Prices per country
Prices are a ponderation of import and export prices as used in Chas-Amil and Buongiorno 2000. Prices are expressed in constant US dollars of 2010.
```
Price = (Import_Value + Export_Value)/
            (Import_Quantity + Export_Quantity) / DeflUS *1000)
```
Countries with a particularly high price were removed from the set. 

```r
library(scales)
ggplot(data = subset(pp, !Country %in% c("Slovakia", "Malta"))) + geom_line(aes(x = Year, 
    y = Price, color = Item)) + facet_wrap(~Country) + ylab("Price in (2010) constant USD /m3") + 
    scale_y_continuous(labels = dollar)
```

![plot of chunk pricesByCountry](figure/pricesByCountry.png) 


### Trade prices by Country

```r
ggplot(data = subset(pptrade, !Country %in% c("Slovakia", "Malta"))) + geom_line(aes(x = Year, 
    y = Price_Trade, color = Item, linetype = Trade)) + facet_wrap(~Country) + ylab("Price in (2010) constant USD /m3")
```

![plot of chunk tradePricesByCountry](figure/tradePricesByCountry.png) 


### Trade prices for Sweden, Germany, France and Finland

```r
# Sweden Germany France Finland
ggplot(data = subset(pptrade, Country %in% c("Sweden", "Germany", "France", "Finland"))) + 
    geom_line(aes(x = Year, y = Price_Trade, color = Item, linetype = Trade)) + facet_wrap(~Country) + 
    ylab("Price in (2010) constant USD /m3")
```

![plot of chunk tradePricesSGFF](figure/tradePricesSGFF.png) 


<a name="Distribution"></a>

Distribution of the estimation data for 4 important years
---------------
## Cross sectional distribution of demand 
Number of countries for a given demand volume in million m3

```r
# 1980, 1990, 2000, 2010
pp4hist = subset(pp, Year %in% c(1980, 1990, 2000, 2010) & Item == "Total Paper and Paperboard")
ggplot(data = pp4hist) + geom_histogram(aes(x = Consumption/1e+06), binwidth = 1) + 
    facet_wrap(~Year)
```

![plot of chunk histdemand](figure/histdemand.png) 

Number of countries for a given demand volume in log of the volume in m3

```r
# Consumption in log
ggplot(data = pp4hist) + geom_histogram(aes(x = log(Consumption)), binwidth = 1) + 
    facet_wrap(~Year)
```

![plot of chunk histdemandlog](figure/histdemandlog.png) 


## Distribution of prices

```r
ggplot(data = pp4hist) + geom_histogram(aes(x = log(Price)), binwidth = 0.1) + facet_wrap(~Year)
```

![plot of chunk histGDPlog1](figure/histGDPlog1.png) 


## Distribution of GDP

```r
ggplot(data = pp4hist) + geom_histogram(aes(x = log(GDPconstantUSD)), binwidth = 1) + 
    facet_wrap(~Year)
```

![plot of chunk histGDPlog2](figure/histGDPlog2.png) 



<a name="EstimationData"></a>
Plot log of the estimation data 
-------------------------------
We will estimate the model
$$ log(Consumption) = \beta_0 + \beta_1 log(GDP) + $$

Lets look at the relationship between log(Consumption) and log(GDP) first.
### Total Paper and Paperboard
2012 data in red

```r
plot(log(Consumption) ~ log(GDPconstantUSD), data = subset(pp, Item == "Total Paper and Paperboard"))
```

```
## Warning: NaNs produced
```

```r
points(log(Consumption) ~ log(GDPconstantUSD), data = subset(pp, Item == "Total Paper and Paperboard" & 
    Year == 2012), col = "red")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


### Explore the influence of Year and Net_Trade by country
Sort countries by Net_Trade, then display a color for each country

```r
d = data.frame(s = c(45, 43))
ppc = subset(pp, Item == "Total Paper and Paperboard" & Year == 2012)
ppc = ppc[order(ppc$Net_Trade), ]
pp$Country = factor(pp$Country, levels = ppc$Country, ordered = TRUE)
p = ggplot(pp, aes(x = log(GDPconstantUSD), y = log(Consumption))) + facet_wrap(~Item)
p + geom_point(aes(alpha = Year, color = Country))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


Check the [ggplot shape parameter](http://sape.inf.usi.ch/quick-reference/ggplot2/shape) and the  `scale_shape_identity` to improve this graph with a "+" sign for positive net trade and a "o" sign for negative net trade. Plot only Printing and Writing Paper for this graph.

```r

p = ggplot(data = subset(pp, Item == "Printing and Writing Paper"), aes(x = log(GDPconstantUSD), 
    y = log(Consumption))) + facet_wrap(~Item)
p + scale_shape_identity() + geom_point(aes(color = Country, shape = 1 + 2 * (Net_Trade > 
    0)))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 



### Total Paper and Paperboard

```r
plot(log(Consumption) ~ log(Price), data = subset(pp, Item == "Total Paper and Paperboard"))
```

```
## Warning: NaNs produced
```

```r
points(log(Consumption) ~ log(Price), data = subset(pp, Item == "Total Paper and Paperboard" & 
    Year == 2012), col = "red")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


### All products

```r
p = ggplot(pp, aes(x = log(Price), y = log(Consumption), alpha = Year)) + facet_wrap(~Item)
p + geom_point(aes(color = Country, alpha = Year))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

Countries that trade less seem to have higher prices.

Change point shapes with a "+" sign for positive net trade and a "o" sign for negative net trade. Show only Printing and writing paper for this graph.

```r
p = ggplot(data = subset(pp, Item == "Printing and Writing Paper"), aes(x = log(Price), 
    y = log(Consumption))) + facet_wrap(~Item)
p + scale_shape_identity() + geom_point(aes(color = Country, alpha = Year, shape = 1 + 
    2 * (Net_Trade > 0)))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

