Paper Product Consumption models
========================================================
* change prices to fictive euro prices to see if exchange rates 
have an effect on the significance of the estimated price elasticities.





Data
----
### Load 
FAOSTAT is the source of forest products data and
World Bank is the source of GDP, deflator and exchange rate data.  

```r
source("code/func.r")
load("enddata/EU28 paper products base year 1987.rdata")
load("enddata/world Bank GDP defl pop 1987.rdata")
pp <- paperproducts$entity # Give a shorter name to the data frame
# Add GDPconstantEUR
pp <- merge(pp, wb[c("Country", "Year", "GDPconstantEUR")], all.x=TRUE)
EU15 <- unique(wb$Country[wb$EU15==1])
```


### Prepare data

```r
# Create a column consumption at year t-1
pp <- ddply(pp, .(Country,Item), mutate, 
           Consum_t_1 = c(NA,Consumption[-length(Consumption)]))

# Select same data as in the chasamil paper
chasamil <- subset(pp, Year>=1969&Year<=1992&Country%in%EU15)
# Remove 1969 for Germany because Germany doesn't have GDP for that year
chasamil <- subset(chasamil, !(Country=="Germany"&Year==1969))

# Subset only total value for the first estimations on country time series
chasamil_tot <- subset(chasamil, Item=="Total Paper and Paperboard")
```


Le panel est il cylindré?

OLS by country
--------------
### For all countries in the dataset

```r

# A function that returns coefficients and sd of the dynamic model to a country 
dynModelCountry = function(dtf){
  dym = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1),
           data=dtf)
  dym$Country = unique(dtf$Country)
  return(dym)
}

# Model for one country 
dynm_fr <- dynModelCountry(subset(chasamil_tot,Country=="France"))
country_series_table(dynm_fr)
```

```
##            Country      Y     P   Dt_1   R2
## Estimate    France   0.55 -0.42  -0.03 0.54
## Std. Error         (0.16) (0.3) (0.21)
```

```r
# Othere way to display it 
print(xtable(dynm_fr), type= "html")
```

```
## <!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
## <!-- Thu Mar 20 16:46:14 2014 -->
## <TABLE border=1>
## <TR> <TH>  </TH> <TH> Estimate </TH> <TH> Std. Error </TH> <TH> t value </TH> <TH> Pr(&gt |t|) </TH>  </TR>
##   <TR> <TD align="right"> (Intercept) </TD> <TD align="right"> 4.0019 </TD> <TD align="right"> 4.0207 </TD> <TD align="right"> 1.00 </TD> <TD align="right"> 0.3315 </TD> </TR>
##   <TR> <TD align="right"> log(GDPconstantUSD) </TD> <TD align="right"> 0.5465 </TD> <TD align="right"> 0.1646 </TD> <TD align="right"> 3.32 </TD> <TD align="right"> 0.0034 </TD> </TR>
##   <TR> <TD align="right"> log(Price) </TD> <TD align="right"> -0.4175 </TD> <TD align="right"> 0.3038 </TD> <TD align="right"> -1.37 </TD> <TD align="right"> 0.1846 </TD> </TR>
##   <TR> <TD align="right"> log(Consum_t_1) </TD> <TD align="right"> -0.0294 </TD> <TD align="right"> 0.2105 </TD> <TD align="right"> -0.14 </TD> <TD align="right"> 0.8904 </TD> </TR>
##    </TABLE>
```

```r

# Create a list of models
tot_m <- dlply(chasamil_tot, .(Country), dynModelCountry)
tot_m.coefs <- ldply(tot_m, country_series_table)
```



```r
# Print table to html
print(xtable(tot_m.coefs,
             caption="Demand equations for total paper and paperboard by country", 
             label="DemandByCountry"),  type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Thu Mar 20 16:46:14 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Demand equations for total paper and paperboard by country </CAPTION>
<TR> <TH>  </TH> <TH> Country </TH> <TH> Y </TH> <TH> P </TH> <TH> Dt_1 </TH> <TH> R2 </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Austria </TD> <TD> 1.56 </TD> <TD> -0.25 </TD> <TD> 0.02 </TD> <TD> 0.87 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD>  </TD> <TD> (0.42) </TD> <TD> (0.12) </TD> <TD> (0.23) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Denmark </TD> <TD> 0.23 </TD> <TD> -0.14 </TD> <TD> 0.7 </TD> <TD> 0.83 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD>  </TD> <TD> (0.12) </TD> <TD> (0.12) </TD> <TD> (0.15) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Finland </TD> <TD> 0.37 </TD> <TD> -0.19 </TD> <TD> 0.37 </TD> <TD> 0.91 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD>  </TD> <TD> (0.13) </TD> <TD> (0.1) </TD> <TD> (0.2) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> France </TD> <TD> 0.55 </TD> <TD> -0.42 </TD> <TD> -0.03 </TD> <TD> 0.54 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD>  </TD> <TD> (0.16) </TD> <TD> (0.3) </TD> <TD> (0.21) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> Germany </TD> <TD> 0.25 </TD> <TD> -0.05 </TD> <TD> 0.97 </TD> <TD> 0.93 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD>  </TD> <TD> (0.31) </TD> <TD> (0.09) </TD> <TD> (0.09) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> Greece </TD> <TD> 0.19 </TD> <TD> 0.16 </TD> <TD> 0.44 </TD> <TD> 0.91 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD>  </TD> <TD> (0.09) </TD> <TD> (0.16) </TD> <TD> (0.23) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 13 </TD> <TD> Ireland </TD> <TD> 0.08 </TD> <TD> -0.05 </TD> <TD> 0.61 </TD> <TD> 0.67 </TD> </TR>
  <TR> <TD align="right"> 14 </TD> <TD>  </TD> <TD> (0.05) </TD> <TD> (0.12) </TD> <TD> (0.2) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 15 </TD> <TD> Italy </TD> <TD> 0.23 </TD> <TD> 0.07 </TD> <TD> 0.29 </TD> <TD> 0.89 </TD> </TR>
  <TR> <TD align="right"> 16 </TD> <TD>  </TD> <TD> (0.08) </TD> <TD> (0.09) </TD> <TD> (0.23) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 17 </TD> <TD> Netherlands </TD> <TD> 0.16 </TD> <TD> -0.08 </TD> <TD> 0.92 </TD> <TD> 0.87 </TD> </TR>
  <TR> <TD align="right"> 18 </TD> <TD>  </TD> <TD> (0.24) </TD> <TD> (0.13) </TD> <TD> (0.1) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD> Portugal </TD> <TD> 0.36 </TD> <TD> -0.01 </TD> <TD> 0.06 </TD> <TD> 0.94 </TD> </TR>
  <TR> <TD align="right"> 20 </TD> <TD>  </TD> <TD> (0.08) </TD> <TD> (0.13) </TD> <TD> (0.22) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 21 </TD> <TD> Spain </TD> <TD> 0.18 </TD> <TD> 0 </TD> <TD> 0.65 </TD> <TD> 0.97 </TD> </TR>
  <TR> <TD align="right"> 22 </TD> <TD>  </TD> <TD> (0.11) </TD> <TD> (0.07) </TD> <TD> (0.19) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 23 </TD> <TD> Sweden </TD> <TD> 0.44 </TD> <TD> -0.03 </TD> <TD> -0.1 </TD> <TD> 0.75 </TD> </TR>
  <TR> <TD align="right"> 24 </TD> <TD>  </TD> <TD> (0.11) </TD> <TD> (0.1) </TD> <TD> (0.22) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 25 </TD> <TD> United Kingdom </TD> <TD> 0.11 </TD> <TD> -0.16 </TD> <TD> 0.67 </TD> <TD> 0.74 </TD> </TR>
  <TR> <TD align="right"> 26 </TD> <TD>  </TD> <TD> (0.05) </TD> <TD> (0.09) </TD> <TD> (0.17) </TD> <TD>  </TD> </TR>
   <A NAME=DemandByCountry></A>
</TABLE>



### Same model from 1978 only

```r
# Model with GDP and Price in constant USD from 1978
tot_m <- dlply(subset(chasamil_tot, Year>=1978), .(Country), dynModelCountry)
tot_m.coefs <- ldply(tot_m, country_series_table)
print(xtable(tot_m.coefs,
             caption="Tot paper Demand Model with GDP and Price in constant USD from 1978", 
             label="DemandByCountry"),  type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Thu Mar 20 16:46:14 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Tot paper Demand Model with GDP and Price in constant USD from 1978 </CAPTION>
<TR> <TH>  </TH> <TH> Country </TH> <TH> Y </TH> <TH> P </TH> <TH> Dt_1 </TH> <TH> R2 </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Austria </TD> <TD> 1.72 </TD> <TD> -0.22 </TD> <TD> -0.14 </TD> <TD> 0.64 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD>  </TD> <TD> (0.6) </TD> <TD> (0.22) </TD> <TD> (0.35) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Denmark </TD> <TD> 0.98 </TD> <TD> 0.25 </TD> <TD> 0.31 </TD> <TD> 0.87 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD>  </TD> <TD> (0.38) </TD> <TD> (0.21) </TD> <TD> (0.26) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Finland </TD> <TD> 0.61 </TD> <TD> -0.02 </TD> <TD> 0.01 </TD> <TD> 0.8 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD>  </TD> <TD> (0.22) </TD> <TD> (0.16) </TD> <TD> (0.27) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> France </TD> <TD> 0.21 </TD> <TD> 0.19 </TD> <TD> 0.68 </TD> <TD> 0.93 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD>  </TD> <TD> (0.13) </TD> <TD> (0.11) </TD> <TD> (0.17) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> Germany </TD> <TD> 0.15 </TD> <TD> 0.02 </TD> <TD> 0.96 </TD> <TD> 0.95 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD>  </TD> <TD> (0.35) </TD> <TD> (0.1) </TD> <TD> (0.13) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> Greece </TD> <TD> 0.25 </TD> <TD> 0.19 </TD> <TD> 0.25 </TD> <TD> 0.88 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD>  </TD> <TD> (0.11) </TD> <TD> (0.15) </TD> <TD> (0.28) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 13 </TD> <TD> Ireland </TD> <TD> 0.38 </TD> <TD> 0.32 </TD> <TD> 0.4 </TD> <TD> 0.72 </TD> </TR>
  <TR> <TD align="right"> 14 </TD> <TD>  </TD> <TD> (0.17) </TD> <TD> (0.22) </TD> <TD> (0.26) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 15 </TD> <TD> Italy </TD> <TD> 0.2 </TD> <TD> 0.21 </TD> <TD> 0.58 </TD> <TD> 0.96 </TD> </TR>
  <TR> <TD align="right"> 16 </TD> <TD>  </TD> <TD> (0.08) </TD> <TD> (0.07) </TD> <TD> (0.19) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 17 </TD> <TD> Netherlands </TD> <TD> -0.25 </TD> <TD> 0.08 </TD> <TD> 1 </TD> <TD> 0.9 </TD> </TR>
  <TR> <TD align="right"> 18 </TD> <TD>  </TD> <TD> (0.54) </TD> <TD> (0.12) </TD> <TD> (0.1) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD> Portugal </TD> <TD> 0.35 </TD> <TD> 0.01 </TD> <TD> 0.23 </TD> <TD> 0.95 </TD> </TR>
  <TR> <TD align="right"> 20 </TD> <TD>  </TD> <TD> (0.13) </TD> <TD> (0.13) </TD> <TD> (0.3) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 21 </TD> <TD> Spain </TD> <TD> 0.59 </TD> <TD> 0.13 </TD> <TD> 0.19 </TD> <TD> 0.98 </TD> </TR>
  <TR> <TD align="right"> 22 </TD> <TD>  </TD> <TD> (0.15) </TD> <TD> (0.06) </TD> <TD> (0.21) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 23 </TD> <TD> Sweden </TD> <TD> 0.55 </TD> <TD> -0.17 </TD> <TD> -0.17 </TD> <TD> 0.64 </TD> </TR>
  <TR> <TD align="right"> 24 </TD> <TD>  </TD> <TD> (0.19) </TD> <TD> (0.18) </TD> <TD> (0.28) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 25 </TD> <TD> United Kingdom </TD> <TD> 0.11 </TD> <TD> -0.1 </TD> <TD> 0.84 </TD> <TD> 0.91 </TD> </TR>
  <TR> <TD align="right"> 26 </TD> <TD>  </TD> <TD> (0.2) </TD> <TD> (0.12) </TD> <TD> (0.38) </TD> <TD>  </TD> </TR>
   <A NAME=DemandByCountry></A>
</TABLE>


### Same model from 1978 only using EUR price and GDP values

```r
# Model with GDP and Price in constant EUR from 1978
dirty <- subset(chasamil_tot, Year>=1978)
# Rename GDPconstantEUR to GDPconstantUSD to avoid creating new functions 
#     for dynModelCountry() and country_series_table()
dirty$GDPconstantUSD <- dirty$GDPconstantEUR
tot_m_EUR <- dlply(dirty, .(Country), dynModelCountry)
tot_m_EUR.coefs <- ldply(tot_m_EUR, country_series_table)
print(xtable(tot_m_EUR.coefs,
             caption="Tot paper Demand Model with GDP and Price in constant EUR from 1978", 
             label="DemandByCountry"),  type = "html")
```

<!-- html table generated in R 3.0.2 by xtable 1.7-1 package -->
<!-- Thu Mar 20 16:46:14 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Tot paper Demand Model with GDP and Price in constant EUR from 1978 </CAPTION>
<TR> <TH>  </TH> <TH> Country </TH> <TH> Y </TH> <TH> P </TH> <TH> Dt_1 </TH> <TH> R2 </TH>  </TR>
  <TR> <TD align="right"> 1 </TD> <TD> Austria </TD> <TD> 0.7 </TD> <TD> -0.22 </TD> <TD> 0.46 </TD> <TD> 0.4 </TD> </TR>
  <TR> <TD align="right"> 2 </TD> <TD>  </TD> <TD> (1.02) </TD> <TD> (0.37) </TD> <TD> (0.37) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 3 </TD> <TD> Denmark </TD> <TD> 2.11 </TD> <TD> 0.14 </TD> <TD> 0.19 </TD> <TD> 0.9 </TD> </TR>
  <TR> <TD align="right"> 4 </TD> <TD>  </TD> <TD> (0.63) </TD> <TD> (0.16) </TD> <TD> (0.24) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 5 </TD> <TD> Finland </TD> <TD> 0.81 </TD> <TD> -0.13 </TD> <TD> 0.05 </TD> <TD> 0.8 </TD> </TR>
  <TR> <TD align="right"> 6 </TD> <TD>  </TD> <TD> (0.29) </TD> <TD> (0.17) </TD> <TD> (0.26) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 7 </TD> <TD> France </TD> <TD> 0.51 </TD> <TD> 0.17 </TD> <TD> 0.54 </TD> <TD> 0.94 </TD> </TR>
  <TR> <TD align="right"> 8 </TD> <TD>  </TD> <TD> (0.24) </TD> <TD> (0.1) </TD> <TD> (0.19) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 9 </TD> <TD> Germany </TD> <TD> -0.08 </TD> <TD> 0.07 </TD> <TD> 1.01 </TD> <TD> 0.95 </TD> </TR>
  <TR> <TD align="right"> 10 </TD> <TD>  </TD> <TD> (0.2) </TD> <TD> (0.11) </TD> <TD> (0.07) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 11 </TD> <TD> Greece </TD> <TD> 0.29 </TD> <TD> 0.16 </TD> <TD> 0.24 </TD> <TD> 0.88 </TD> </TR>
  <TR> <TD align="right"> 12 </TD> <TD>  </TD> <TD> (0.12) </TD> <TD> (0.15) </TD> <TD> (0.28) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 13 </TD> <TD> Ireland </TD> <TD> 0.51 </TD> <TD> 0.29 </TD> <TD> 0.38 </TD> <TD> 0.73 </TD> </TR>
  <TR> <TD align="right"> 14 </TD> <TD>  </TD> <TD> (0.22) </TD> <TD> (0.21) </TD> <TD> (0.26) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 15 </TD> <TD> Italy </TD> <TD> 0.27 </TD> <TD> 0.2 </TD> <TD> 0.52 </TD> <TD> 0.96 </TD> </TR>
  <TR> <TD align="right"> 16 </TD> <TD>  </TD> <TD> (0.11) </TD> <TD> (0.07) </TD> <TD> (0.21) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 17 </TD> <TD> Netherlands </TD> <TD> -0.38 </TD> <TD> 0.17 </TD> <TD> 0.88 </TD> <TD> 0.92 </TD> </TR>
  <TR> <TD align="right"> 18 </TD> <TD>  </TD> <TD> (0.25) </TD> <TD> (0.12) </TD> <TD> (0.11) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 19 </TD> <TD> Portugal </TD> <TD> 0.45 </TD> <TD> -0.04 </TD> <TD> 0.11 </TD> <TD> 0.95 </TD> </TR>
  <TR> <TD align="right"> 20 </TD> <TD>  </TD> <TD> (0.15) </TD> <TD> (0.13) </TD> <TD> (0.32) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 21 </TD> <TD> Spain </TD> <TD> 0.81 </TD> <TD> 0.06 </TD> <TD> 0.06 </TD> <TD> 0.98 </TD> </TR>
  <TR> <TD align="right"> 22 </TD> <TD>  </TD> <TD> (0.17) </TD> <TD> (0.05) </TD> <TD> (0.2) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 23 </TD> <TD> Sweden </TD> <TD> 0.71 </TD> <TD> -0.29 </TD> <TD> -0.08 </TD> <TD> 0.62 </TD> </TR>
  <TR> <TD align="right"> 24 </TD> <TD>  </TD> <TD> (0.26) </TD> <TD> (0.2) </TD> <TD> (0.28) </TD> <TD>  </TD> </TR>
  <TR> <TD align="right"> 25 </TD> <TD> United Kingdom </TD> <TD> 0.08 </TD> <TD> -0.14 </TD> <TD> 0.94 </TD> <TD> 0.91 </TD> </TR>
  <TR> <TD align="right"> 26 </TD> <TD>  </TD> <TD> (0.29) </TD> <TD> (0.09) </TD> <TD> (0.4) </TD> <TD>  </TD> </TR>
   <A NAME=DemandByCountry></A>
</TABLE>

```r

```


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

resid <- data.frame(r = m_fr$residuals[-1],
                    r_t_1 = m_fr$residuals[-length(m_fr$residuals)])

mr_fr <- lm(r ~ r_t_1, data=resid)
summary(mr_fr)


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
cochrane.orcutt.lm(m_fr)

# Plot autocorelation of residuals
e <- m_fr$residuals
n <- length(e)
plot(e[1:(n-1)],e[2:n])

# Same model for Fnland
m_fi = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1),
           data=subset(chasamil, Country=="Finland", 
                       select=c("Consumption","GDPconstantUSD","Price", "Consum_t_1")))
summary(m_fi)
cochrane.orcutt.lm(m_fi)


m_au = lm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1),
           data=subset(chasamil, Country=="Austria", 
                       select=c("Consumption","GDPconstantUSD","Price", "Consum_t_1")))
summary(m_au)
cochrane.orcutt.lm(m_au)
```



Correlation between model variables

```r
cor(fr)
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

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-71.png) 

```r
acf(consfr.ts,lag.max=100)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-72.png) 

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


### Pooled OLS
Estimation with pooled OLS
$$ln D_it = \delta_0 + \delta_1 ln Y + \delta_2 ln P + \delta_3 ln D_-1 + \epsilon$$


```r
pooling <- plm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), 
               data=pchasamil, model= "pooling")
```

duplicate couples (time-id)

```
## Error:
```

```r
printSummaryTable(pooling)
```

```
## Error: object 'pooling' not found
```



```r
summary(pooling)
```

```
## Error: object 'pooling' not found
```


### Fixed effects or within estimator

```r
fixed <- plm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), 
               data=pchasamil, model= "within")
```

duplicate couples (time-id)

```
## Error:
```

```r
printSummaryTable(fixed)
```

```
## Error: object 'fixed' not found
```


### Random effects estimator

```r
random <- plm(log(Consumption) ~ log(GDPconstantUSD) + log(Price) + log(Consum_t_1), 
               data=pchasamil, model= "random")
```

duplicate couples (time-id)

```
## Error:
```

```r
printSummaryTable(random)
```

```
## Error: object 'random' not found
```

The estimation of the random effect model tells me that there is an Error in swar(object, data, effect) : "the estimated variance of the individual effect is negative".

