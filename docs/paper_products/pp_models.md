Paper Product Consumption models
========================================================





Data
----
### Load 
FAOSTAT is the source of forest products data and
World Bank is the source of GDP, deflator and exchange rate data.  

```r
source("code/func.r")
print(load("enddata/EU28 paper products.rdata"))
```

```
## [1] "paperproducts"
```

```r
print(load("enddata//world Bank GDP defl pop"))
```

```
## [1] "wb"  "US"  "EUR"
```

```r
pp <- paperproducts$entity # Give a shorter name to the data frame
EU15 <- unique(wb$Country[wb$EU15==1])
```


### Prepare data

```r
# Create a column consumption at year t-1
pp <- ddply(pp, .(Country,Item), mutate, 
           Consum_t_1 = c(NA,Consumption[-length(Consumption)]))

# Select same data as in the chasamil paper
chasamil <- subset(pp, Year>=1969&Year<=1992&Country%in%EU15 & 
                      Item=="Total Paper and Paperboard")

# GDP per capita
```

Le panel est il cylindré?

OLS by country
--------------
The Worldbank databank doesn't contain a deflator for Ireland over the time period of interest. Therefore we couldn't calculate a GDP in constant USD for Ireland.


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
## Estimate         -9.898              1.1001    -0.4892        -0.08684  France
## Std. Error        6.735              0.3001     0.2914         0.20794  France
##            R_Squared
## Estimate      0.5691
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

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Tue Mar 18 17:46:06 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Demand equations for total paper and paperboard by country </CAPTION>
<TR> <TH>  </TH> <TH> Country </TH> <TH> Y </TH> <TH> P </TH> <TH> Dt_1 </TH> <TH> R2 </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Austria </TD> <TD> 1.52 </TD> <TD> -0.05 </TD> <TD> -0.22 </TD> <TD align="right"> 0.90 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD>  </TD> <TD> (0.3) </TD> <TD> (0.09) </TD> <TD> (0.22) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Denmark </TD> <TD> 0.52 </TD> <TD> -0.12 </TD> <TD> 0.63 </TD> <TD align="right"> 0.83 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD>  </TD> <TD> (0.24) </TD> <TD> (0.11) </TD> <TD> (0.16) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Finland </TD> <TD> 0.99 </TD> <TD> -0.15 </TD> <TD> 0.14 </TD> <TD align="right"> 0.93 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD>  </TD> <TD> (0.25) </TD> <TD> (0.08) </TD> <TD> (0.2) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> France </TD> <TD> 1.1 </TD> <TD> -0.49 </TD> <TD> -0.09 </TD> <TD align="right"> 0.57 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD>  </TD> <TD> (0.3) </TD> <TD> (0.29) </TD> <TD> (0.21) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> Germany </TD> <TD> 0.93 </TD> <TD> -0.11 </TD> <TD> 0.33 </TD> <TD align="right"> 0.96 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD>  </TD> <TD> (0.24) </TD> <TD> (0.06) </TD> <TD> (0.18) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> Greece </TD> <TD> 0.64 </TD> <TD> -0.02 </TD> <TD> 0.66 </TD> <TD align="right"> 0.91 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD>  </TD> <TD> (0.31) </TD> <TD> (0.13) </TD> <TD> (0.15) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 13 </TD> <TD> Italy </TD> <TD> 0.96 </TD> <TD> 0 </TD> <TD> 0.1 </TD> <TD align="right"> 0.91 </TD> </TR>
  <TR> <TD align="right"> 14 </TD> <TD>  </TD> <TD> (0.24) </TD> <TD> (0.07) </TD> <TD> (0.22) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 15 </TD> <TD> Netherlands </TD> <TD> 0.9 </TD> <TD> -0.16 </TD> <TD> 0.26 </TD> <TD align="right"> 0.92 </TD> </TR>
  <TR> <TD align="right"> 16 </TD> <TD>  </TD> <TD> (0.26) </TD> <TD> (0.08) </TD> <TD> (0.21) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 17 </TD> <TD> Portugal </TD> <TD> 0.69 </TD> <TD> -0.12 </TD> <TD> 0.56 </TD> <TD align="right"> 0.91 </TD> </TR>
  <TR> <TD align="right"> 18 </TD> <TD>  </TD> <TD> (0.32) </TD> <TD> (0.17) </TD> <TD> (0.21) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD> Spain </TD> <TD> 1.66 </TD> <TD> -0.09 </TD> <TD> 0.07 </TD> <TD align="right"> 0.98 </TD> </TR>
  <TR> <TD align="right"> 20 </TD> <TD>  </TD> <TD> (0.41) </TD> <TD> (0.05) </TD> <TD> (0.22) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 21 </TD> <TD> Sweden </TD> <TD> 1.17 </TD> <TD> -0.08 </TD> <TD> -0.17 </TD> <TD align="right"> 0.77 </TD> </TR>
  <TR> <TD align="right"> 22 </TD> <TD>  </TD> <TD> (0.26) </TD> <TD> (0.1) </TD> <TD> (0.22) </TD> <TD align="right">  </TD> </TR>
  <TR> <TD align="right"> 23 </TD> <TD> United Kingdom </TD> <TD> 0.51 </TD> <TD> -0.16 </TD> <TD> 0.45 </TD> <TD align="right"> 0.80 </TD> </TR>
  <TR> <TD align="right"> 24 </TD> <TD>  </TD> <TD> (0.14) </TD> <TD> (0.07) </TD> <TD> (0.18) </TD> <TD align="right">  </TD> </TR>
   <A NAME=DemandByCountry></A>
</TABLE>


On the subject of Cochrane Orcutt method, [John Fox wrote](https://stat.ethz.ch/pipermail/r-help/2002-January/017774.html): "I'm not sure why you'd want to use these methods, other than for 
historical reasons." And he gave a function cochrane.orcutt.lm which takes a linear-model object as an input.


Autocorrelation
---------------

```r
fr <- subset(chasamil, Country=="France", select=c("Consumption","GDPconstantUSD",
                                                   "Price", "Consum_t_1"))

m_fr = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1),
          data=fr)
summary(m_fr)
```

```
## 
## Call:
## lm(formula = log(Consumption) ~ log(GDPconstantUSD) + log(Price) + 
##     log(Consum_t_1), data = fr)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.7010 -0.0618  0.0344  0.1047  0.2800 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)   
## (Intercept)          -9.8975     6.7352   -1.47   0.1572   
## log(GDPconstantUSD)   1.1001     0.3001    3.67   0.0015 **
## log(Price)           -0.4892     0.2914   -1.68   0.1088   
## log(Consum_t_1)      -0.0868     0.2079   -0.42   0.6807   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.198 on 20 degrees of freedom
## Multiple R-squared:  0.569,	Adjusted R-squared:  0.504 
## F-statistic:  8.8 on 3 and 20 DF,  p-value: 0.000637
```

```r

resid <- data.frame(r = m_fr$residuals[-1],
                    r_t_1 = m_fr$residuals[-length(m_fr$residuals)])

mr_fr <- lm(r ~ r_t_1, data=resid)
summary(mr_fr)
```

```
## 
## Call:
## lm(formula = r ~ r_t_1, data = resid)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.6973 -0.0626  0.0331  0.1067  0.2850 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)
## (Intercept) -0.00593    0.03985   -0.15     0.88
## r_t_1        0.00810    0.21787    0.04     0.97
## 
## Residual standard error: 0.191 on 21 degrees of freedom
## Multiple R-squared:  6.58e-05,	Adjusted R-squared:  -0.0476 
## F-statistic: 0.00138 on 1 and 21 DF,  p-value: 0.971
```

```r


# Function from https://stat.ethz.ch/pipermail/r-help/2002-January/017774.html
cochrane.orcutt.lm <- function(mod){
     X <- model.matrix(mod)
     y <- model.response(model.frame(mod))
     e <- residuals(mod)
     n <- length(e)
     names <- colnames(X)
     rho <- sum(e[1:(n-1)]*e[2:n])/sum(e^2)
     y <- y[2:n] - rho * y[1:(n-1)]
     X <- X[2:n,] - rho * X[1:(n-1),]
     mod <- lm(y ~ X - 1)
     result <- list()
     result$coefficients <- coef(mod)
     names(result$coefficients) <- names
     summary <- summary(mod, corr = F)
     result$cov <- (summary$sigma^2) * summary$cov.unscaled
     dimnames(result$cov) <- list(names, names)
     result$sigma <- summary$sigma
     result$rho <- rho
     class(result) <- 'cochrane.orcutt'
     result
     }

summary(m_fr)
```

```
## 
## Call:
## lm(formula = log(Consumption) ~ log(GDPconstantUSD) + log(Price) + 
##     log(Consum_t_1), data = fr)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.7010 -0.0618  0.0344  0.1047  0.2800 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)   
## (Intercept)          -9.8975     6.7352   -1.47   0.1572   
## log(GDPconstantUSD)   1.1001     0.3001    3.67   0.0015 **
## log(Price)           -0.4892     0.2914   -1.68   0.1088   
## log(Consum_t_1)      -0.0868     0.2079   -0.42   0.6807   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.198 on 20 degrees of freedom
## Multiple R-squared:  0.569,	Adjusted R-squared:  0.504 
## F-statistic:  8.8 on 3 and 20 DF,  p-value: 0.000637
```

```r
cochrane.orcutt.lm(m_fr)
```

```
## $coefficients
##         (Intercept) log(GDPconstantUSD)          log(Price)     log(Consum_t_1) 
##            -12.3964              1.1974             -0.4810             -0.1015 
## 
## $cov
##                     (Intercept) log(GDPconstantUSD) log(Price) log(Consum_t_1)
## (Intercept)             56.4013           -2.204901  -0.701397        0.558383
## log(GDPconstantUSD)     -2.2049            0.105622   0.003584       -0.045768
## log(Price)              -0.7014            0.003584   0.087839        0.001172
## log(Consum_t_1)          0.5584           -0.045768   0.001172        0.044097
## 
## $sigma
## [1] 0.2002
## 
## $rho
## [1] 0.008895
## 
## attr(,"class")
## [1] "cochrane.orcutt"
```

```r



m_fi = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1),
           data=subset(chasamil, Country=="Finland", 
                       select=c("Consumption","GDPconstantUSD","Price", "Consum_t_1")))
summary(m_fi)
```

```
## 
## Call:
## lm(formula = log(Consumption) ~ log(GDPconstantUSD) + log(Price) + 
##     log(Consum_t_1), data = subset(chasamil, Country == "Finland", 
##     select = c("Consumption", "GDPconstantUSD", "Price", "Consum_t_1")))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.11496 -0.04948 -0.00607  0.04725  0.15296 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -11.9563     3.5159   -3.40  0.00284 ** 
## log(GDPconstantUSD)   0.9918     0.2454    4.04  0.00064 ***
## log(Price)           -0.1462     0.0848   -1.72  0.10011    
## log(Consum_t_1)       0.1428     0.1987    0.72  0.48075    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0707 on 20 degrees of freedom
## Multiple R-squared:  0.927,	Adjusted R-squared:  0.917 
## F-statistic: 85.2 on 3 and 20 DF,  p-value: 1.45e-11
```

```r
cochrane.orcutt.lm(m_fi)
```

```
## $coefficients
##         (Intercept) log(GDPconstantUSD)          log(Price)     log(Consum_t_1) 
##           -12.01301             1.03482            -0.17130             0.08124 
## 
## $cov
##                     (Intercept) log(GDPconstantUSD) log(Price) log(Consum_t_1)
## (Intercept)            11.50435           -0.788861   0.064835        0.563712
## log(GDPconstantUSD)    -0.78886            0.057513  -0.005004       -0.044564
## log(Price)              0.06484           -0.005004   0.006905        0.001147
## log(Consum_t_1)         0.56371           -0.044564   0.001147        0.039223
## 
## $sigma
## [1] 0.06869
## 
## $rho
## [1] -0.01619
## 
## attr(,"class")
## [1] "cochrane.orcutt"
```

```r


m_au = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1),
           data=subset(chasamil, Country=="Austria", 
                       select=c("Consumption","GDPconstantUSD","Price", "Consum_t_1")))
summary(m_au)
```

```
## 
## Call:
## lm(formula = log(Consumption) ~ log(GDPconstantUSD) + log(Price) + 
##     log(Consum_t_1), data = subset(chasamil, Country == "Austria", 
##     select = c("Consumption", "GDPconstantUSD", "Price", "Consum_t_1")))
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.15264 -0.04643 -0.00522  0.02962  0.19486 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         -21.5788     4.8155   -4.48  0.00023 ***
## log(GDPconstantUSD)   1.5187     0.2977    5.10  5.4e-05 ***
## log(Price)           -0.0500     0.0898   -0.56  0.58379    
## log(Consum_t_1)      -0.2157     0.2178   -0.99  0.33375    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.0834 on 20 degrees of freedom
## Multiple R-squared:  0.901,	Adjusted R-squared:  0.886 
## F-statistic: 60.7 on 3 and 20 DF,  p-value: 3.17e-10
```

```r
cochrane.orcutt.lm(m_au)
```

```
## $coefficients
##         (Intercept) log(GDPconstantUSD)          log(Price)     log(Consum_t_1) 
##           -20.10410             1.45022            -0.07356            -0.18488 
## 
## $cov
##                     (Intercept) log(GDPconstantUSD) log(Price) log(Consum_t_1)
## (Intercept)             23.3117           -1.408846  -0.028396        0.926055
## log(GDPconstantUSD)     -1.4088            0.091237  -0.002716       -0.065131
## log(Price)              -0.0284           -0.002716   0.007829        0.003329
## log(Consum_t_1)          0.9261           -0.065131   0.003329        0.051643
## 
## $sigma
## [1] 0.08413
## 
## $rho
## [1] -0.0966
## 
## attr(,"class")
## [1] "cochrane.orcutt"
```



Correlation between model variables

```r
cor(fr)
```

```
##                Consumption GDPconstantUSD     Price Consum_t_1
## Consumption         1.0000        0.84841 -0.203207   0.710083
## GDPconstantUSD      0.8484        1.00000 -0.061652   0.836072
## Price              -0.2032       -0.06165  1.000000  -0.008624
## Consum_t_1          0.7101        0.83607 -0.008624   1.000000
```




Statistical tests
-----------------

### Serial temporal autocorrelation on one country

```r
# Change to a time series
consfr <- subset(chasamil,Country=="France")
consfr.ts <- ts(consfr$Consumption, start=min(consfr$Year))
plot(consfr.ts)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-51.png) 

```r
acf(consfr.ts,lag.max=100)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-52.png) 

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
```

```
## Error: Results do not have equal lengths
```

```r
autocorrTable <- melt(a, id.vars = c("Country"))
```

```
## Error: cannot coerce class ""acf"" to a data.frame
```

```r
ggplot(autocorrTable) + aes(x=variable, y=value) +
    geom_bar(stat="identity") + facet_wrap(~Country)
```

```
## Error: object 'autocorrTable' not found
```




Panel data models
-----------------
Over the 1960 - 1992 estimation period.

```r

# Set data as panel data
pchasamil <- plm.data(chasamil, index=c("Country","Year"))
```

```
## serie Item  is constant and has been removed
```


### Pooled OLS

```r
pooling <- plm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), 
               data=pchasamil, model= "pooling")
```

series Item is constant and has been removed

```r
printSummaryTable(pooling)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Tue Mar 18 17:46:06 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> X.Intercept. </TH> <TH> log.GDPconstantUSD. </TH> <TH> log.Price. </TH> <TH> log.Consum_t_1. </TH>  </TR>
  <TR> <TD align="right"> Estimate </TD> <TD align="right"> -1.01 </TD> <TD align="right"> 0.16 </TD> <TD align="right"> -0.11 </TD> <TD align="right"> 0.84 </TD> </TR>
  <TR> <TD align="right"> Std. Error </TD> <TD align="right"> 0.37 </TD> <TD align="right"> 0.03 </TD> <TD align="right"> 0.04 </TD> <TD align="right"> 0.03 </TD> </TR>
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
## -1.02000 -0.04860  0.00739  0.05430  0.73100 
## 
## Coefficients :
##                     Estimate Std. Error t-value Pr(>|t|)    
## (Intercept)          -1.0069     0.3672   -2.74   0.0065 ** 
## log(GDPconstantUSD)   0.1576     0.0285    5.53  7.4e-08 ***
## log(Price)           -0.1064     0.0350   -3.04   0.0026 ** 
## log(Consum_t_1)       0.8374     0.0281   29.76  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Total Sum of Squares:    335
## Residual Sum of Squares: 4.01
## R-Squared      :  0.988 
##       Adj. R-Squared :  0.974 
## F-statistic: 7789.34 on 3 and 283 DF, p-value: <2e-16
```


### Fixed effects or within estimator

```r
fixed <- plm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), 
               data=pchasamil, model= "within")
```

series Item is constant and has been removed

```r
printSummaryTable(fixed)
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Tue Mar 18 17:46:07 2014 -->
<TABLE border=1>
<TR> <TH>  </TH> <TH> log.GDPconstantUSD. </TH> <TH> log.Price. </TH> <TH> log.Consum_t_1. </TH>  </TR>
  <TR> <TD align="right"> Estimate </TD> <TD align="right"> 0.73 </TD> <TD align="right"> -0.10 </TD> <TD align="right"> 0.44 </TD> </TR>
  <TR> <TD align="right"> Std. Error </TD> <TD align="right"> 0.07 </TD> <TD align="right"> 0.03 </TD> <TD align="right"> 0.05 </TD> </TR>
   </TABLE>


### Random effects estimator

```r
random <- plm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), 
               data=pchasamil, model= "random")
```

series Item is constant and has been removed

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

