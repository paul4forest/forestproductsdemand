Paper Product Consumption models
========================================================





Data
----
### Load 
FAOSTAT is the source of forest products data and
World Bank is the source of GDP, deflator and exchange rate data.  

```r
source("code/func.r")
print(load("enddata/EU27 paper products demand.rdata"))
```

```
## [1] "paperProducts" "ppagg"         "pptrade"       "wb"
```

```r
pp <- paperProducts # Give a shorter name to the data frame
EU15 <- unique(wb$Country[wb$EU15==1])
```


### Prepare data

```r
# Create a column consumption at year t-1
pp <- ddply(pp, .(Country,Item), mutate, 
           Consum_t_1 = c(NA,Consumption[-length(Consumption)]))

# Select same data as in the chasamil paper
chasamil <- subset(pp, Year>=1969&Year<=1992&Country%in%EU15 & 
                      Item=="Printing and Writing Paper", select=-Item)
```

Le panel est il cylindré?

OLS by country
--------------
The Worldbank databank doesn't contain a deflator for Ireland over the time period of interest. Therefore we couldn't calculate a GDP in constant USD for Ireland.

### Serial temporal autocorrelation on one country

```r
# Change to a time series
consfr <- subset(chasamil,Country=="France")
consfr.ts <- ts(consfr$Consumption, start=min(consfr$Year))
plot(consfr.ts)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-11.png) 

```r
acf(consfr.ts,lag.max=100)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-12.png) 

```r
# numerical values of this autocorrelation function
a <- acf(consfr.ts,lag.max=100,plot=FALSE)
class(a)
```

```
## [1] "acf"
```

```r


# Autocorrelation coefficients to be applied on all countries
autocorr <- function(dtf){
    cons.ts <- ts(dtf$Consumption, start=min(dtf$Year))
    return(acf(cons.ts,lag.max=100,plot=FALSE)$acf)
}
a = ddply(chasamil, .(Country), autocorr)
autocorrTable <- melt(a, id.vars = c("Country"))
ggplot(autocorrTable) + aes(x=variable, y=value) + geom_bar() + facet_wrap(~Country)
```

```
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
## Mapping a variable to y and also using stat="bin".
##   With stat="bin", it will attempt to set the y value to the count of cases in each group.
##   This can result in unexpected behavior and will not be allowed in a future version of ggplot2.
##   If you want y to represent counts of cases, use stat="bin" and don't map a variable to y.
##   If you want y to represent values in the data, use stat="identity".
##   See ?geom_bar for examples. (Deprecated; last used in version 0.9.2)
```

```
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
## Warning: Stacking not well defined when ymin != 0
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-13.png) 



### For all countries in the dataset

```r
#Remove Ireland because it doesn't have GDPConstantUSD Data
chasamil <- subset(chasamil, Country!="Ireland")
# Remove 1969 for Germany because Germany doesn't have GDP for that year
chasamil <- subset(chasamil, !(Country=="Germany"&Year==1969))

# A function that applies the dynamic model to a country 
dynModelCountry = function(dtf){
  dym = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1),
           data=dtf)
  coefs = data.frame(t(summary(dym)$coefficients[,1:2]))
  coefs$Country = unique(dtf$Country)
  if (nrow(coefs)==2){
    coefs$R_Squared  = c(summary(dym)$r.squared,NA)
  }
  return(coefs)
}

# Test the model for one country
dynModelCountry(subset(chasamil,Country=="France"))
```

```
##            X.Intercept. log.GDPconstantUSD. log.Price. log.Consum_t_1. Country
## Estimate        -17.533              1.1427    -0.1749         0.09427  France
## Std. Error        5.124              0.2954     0.1066         0.22250  France
##            R_Squared
## Estimate      0.9063
## Std. Error        NA
```

```r

# # Apply the dynamic model to each and all countries
x = ddply(chasamil, .(Country), dynModelCountry)

# Reorder and rename columns for esthetical reasons
x = x[c( "Country", "log.GDPconstantUSD.", "log.Price.", 
         "log.Consum_t_1.", "R_Squared")]
names(x) = c("Country", "Y", "P", "Dt_1", "R2")
x$Country[is.na(x$R2)] = ""
x[,-1] = round(x[,-1],2)
x$Y[is.na(x$R2)] = paste("(", x$Y[is.na(x$R2)], ")", sep="")
x$P[is.na(x$R2)] = paste("(", x$P[is.na(x$R2)], ")", sep="")
x$Dt_1[is.na(x$R2)] = paste("(", x$Dt_1[is.na(x$R2)], ")", sep="")

# Print table to html
print(xtable(x, caption="Demand equations for total paper and paperboard by country", 
             label="DemandByCountry"),  type = "html")
```

```
## <!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
## <!-- Fri Feb 07 18:13:01 2014 -->
## <TABLE border=1>
## <CAPTION ALIGN="bottom"> Demand equations for total paper and paperboard by country </CAPTION>
## <TR> <TH>  </TH> <TH> Country </TH> <TH> Y </TH> <TH> P </TH> <TH> Dt_1 </TH> <TH> R2 </TH>  </TR>
##   <TR> <TD align="right"> 1 </TD> <TD> Austria </TD> <TD> 0.33 </TD> <TD> -0.12 </TD> <TD> 0.7 </TD> <TD align="right"> 0.59 </TD> </TR>
##   <TR> <TD align="right"> 2 </TD> <TD>  </TD> <TD> (0.45) </TD> <TD> (0.43) </TD> <TD> (0.16) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 3 </TD> <TD> Denmark </TD> <TD> 1.29 </TD> <TD> 0.04 </TD> <TD> 0.25 </TD> <TD align="right"> 0.90 </TD> </TR>
##   <TR> <TD align="right"> 4 </TD> <TD>  </TD> <TD> (0.36) </TD> <TD> (0.11) </TD> <TD> (0.19) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 5 </TD> <TD> Finland </TD> <TD> 1.47 </TD> <TD> -0.64 </TD> <TD> 0.3 </TD> <TD align="right"> 0.78 </TD> </TR>
##   <TR> <TD align="right"> 6 </TD> <TD>  </TD> <TD> (0.38) </TD> <TD> (0.27) </TD> <TD> (0.19) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 7 </TD> <TD> France </TD> <TD> 1.14 </TD> <TD> -0.17 </TD> <TD> 0.09 </TD> <TD align="right"> 0.91 </TD> </TR>
##   <TR> <TD align="right"> 8 </TD> <TD>  </TD> <TD> (0.3) </TD> <TD> (0.11) </TD> <TD> (0.22) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 9 </TD> <TD> Germany </TD> <TD> 1.95 </TD> <TD> -0.16 </TD> <TD> -0.05 </TD> <TD align="right"> 0.98 </TD> </TR>
##   <TR> <TD align="right"> 10 </TD> <TD>  </TD> <TD> (0.33) </TD> <TD> (0.06) </TD> <TD> (0.18) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 11 </TD> <TD> Greece </TD> <TD> 1.3 </TD> <TD> -0.15 </TD> <TD> 0.4 </TD> <TD align="right"> 0.90 </TD> </TR>
##   <TR> <TD align="right"> 12 </TD> <TD>  </TD> <TD> (0.58) </TD> <TD> (0.17) </TD> <TD> (0.23) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 13 </TD> <TD> Italy </TD> <TD> 1.29 </TD> <TD> -0.09 </TD> <TD> 0.09 </TD> <TD align="right"> 0.93 </TD> </TR>
##   <TR> <TD align="right"> 14 </TD> <TD>  </TD> <TD> (0.31) </TD> <TD> (0.08) </TD> <TD> (0.21) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 15 </TD> <TD> Netherlands </TD> <TD> 0.97 </TD> <TD> -0.07 </TD> <TD> 0.33 </TD> <TD align="right"> 0.86 </TD> </TR>
##   <TR> <TD align="right"> 16 </TD> <TD>  </TD> <TD> (0.33) </TD> <TD> (0.12) </TD> <TD> (0.21) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 17 </TD> <TD> Portugal </TD> <TD> 0.7 </TD> <TD> 0.36 </TD> <TD> 0.72 </TD> <TD align="right"> 0.92 </TD> </TR>
##   <TR> <TD align="right"> 18 </TD> <TD>  </TD> <TD> (0.37) </TD> <TD> (0.2) </TD> <TD> (0.17) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 19 </TD> <TD> Spain </TD> <TD> 2.14 </TD> <TD> 0.12 </TD> <TD> 0.02 </TD> <TD align="right"> 0.97 </TD> </TR>
##   <TR> <TD align="right"> 20 </TD> <TD>  </TD> <TD> (0.5) </TD> <TD> (0.11) </TD> <TD> (0.22) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 21 </TD> <TD> Sweden </TD> <TD> 1.13 </TD> <TD> -0.66 </TD> <TD> 0.59 </TD> <TD align="right"> 0.72 </TD> </TR>
##   <TR> <TD align="right"> 22 </TD> <TD>  </TD> <TD> (0.71) </TD> <TD> (0.35) </TD> <TD> (0.18) </TD> <TD align="right">  </TD> </TR>
##   <TR> <TD align="right"> 23 </TD> <TD> United Kingdom </TD> <TD> 1.04 </TD> <TD> -0.15 </TD> <TD> 0.5 </TD> <TD align="right"> 0.96 </TD> </TR>
##   <TR> <TD align="right"> 24 </TD> <TD>  </TD> <TD> (0.29) </TD> <TD> (0.08) </TD> <TD> (0.15) </TD> <TD align="right">  </TD> </TR>
##    <A NAME=DemandByCountry></A>
## </TABLE>
```


On the subject of Cochrane Orcutt method, [John Fox wrote](https://stat.ethz.ch/pipermail/r-help/2002-January/017774.html): "I'm not sure why you'd want to use these methods, other than for 
historical reasons." And he gave a function cochrane.orcutt.lm which takes a linear-model object as an input.


Panel data models
-----------------
Over the 1960 - 1992 estimation period.

```r

# Set data as panel data
pchasamil <- plm.data(chasamil, index=c("Country","Year"))
```


### Pooled OLS

```r
pooling <- plm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), 
               data=pchasamil, model= "pooling")
printSummaryTable(pooling)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Fri Feb 07 18:13:01 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> X.Intercept. </TH> <TH> log.GDPconstantUSD. </TH> <TH> log.Price. </TH> <TH> log.Consum_t_1. </TH>  </TR>
  <TR> <TD align="right"> Estimate </TD> <TD align="right"> -1.08 </TD> <TD align="right"> 0.11 </TD> <TD align="right"> -0.08 </TD> <TD align="right"> 0.91 </TD> </TR>
  <TR> <TD align="right"> Std. Error </TD> <TD align="right"> 0.50 </TD> <TD align="right"> 0.03 </TD> <TD align="right"> 0.05 </TD> <TD align="right"> 0.02 </TD> </TR>
   </TABLE>


```r
summary(pooling)
```

```
## Oneway (individual) effect Pooling Model
## 
## Call:
## plm(formula = log(Consumption) ~ log(GDPconstantUSD) + log(Price) + 
##     log(Consum_t_1), data = pchasamil, model = "pooling")
## 
## Unbalanced Panel: n=12, T=23-24, N=287
## 
## Residuals :
##     Min.  1st Qu.   Median  3rd Qu.     Max. 
## -1.04000 -0.06780  0.00362  0.07020  0.67300 
## 
## Coefficients :
##                     Estimate Std. Error t-value Pr(>|t|)    
## (Intercept)          -1.0845     0.5026   -2.16  0.03179 *  
## log(GDPconstantUSD)   0.1085     0.0306    3.55  0.00046 ***
## log(Price)           -0.0803     0.0505   -1.59  0.11335    
## log(Consum_t_1)       0.9097     0.0235   38.65  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    474
## Residual Sum of Squares: 9.49
## R-Squared      :  0.98 
##       Adj. R-Squared :  0.966 
## F-statistic: 4617.43 on 3 and 283 DF, p-value: <2e-16
```


### Fixed effects or within estimator

```r
fixed <- plm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), 
               data=pchasamil, model= "within")
printSummaryTable(fixed)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Fri Feb 07 18:13:01 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> log.GDPconstantUSD. </TH> <TH> log.Price. </TH> <TH> log.Consum_t_1. </TH>  </TR>
  <TR> <TD align="right"> Estimate </TD> <TD align="right"> 0.68 </TD> <TD align="right"> -0.11 </TD> <TD align="right"> 0.64 </TD> </TR>
  <TR> <TD align="right"> Std. Error </TD> <TD align="right"> 0.10 </TD> <TD align="right"> 0.06 </TD> <TD align="right"> 0.05 </TD> </TR>
   </TABLE>


### Random effects estimator

```r
random <- plm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), 
               data=pchasamil, model= "random")
```

```
## Error: the estimated variance of the individual effect is negative
```

```r
printSummaryTable(random)
```

```
## Error: object 'random' not found
```

The estimation of the random effect model tells me that there is an Error in swar(object, data, effect) : "the estimated variance of the individual effect is negative".

