# This script loads paper and paperboard production and trade data from FAOSTAT
# 
# Input: FAOSTAT website accessed with the R FAOSTAT package
# Output: RDATA file containing downloaded data
#
# Author: Paul Rougieux, European Forest Institute

library(FAOSTAT)

# Downloaded data will be stored in the rawdata folder
setwd("Y:/Macro/Demand Econometric Models/rawdata/")

##############################################################
# See test and example functions at the bottom of the script #
##############################################################


#######################################################################
# Function to batch download production and trade quantity and value  #
#######################################################################

#Prepare a table of country codes an names
countrycodes = subset(FAOcountryProfile,select=c(FAOST_CODE, FAO_TABLE_NAME))
names(countrycodes) =  c("FAOST_CODE", "Country")

#Function that finds an item name given its code
itemname = function(itemcode){
  i = subset(FAOmetaTable$itemTable, itemCode==itemcode&domainCode=="FO")
  agg = subset(FAOmetaTable$itemAggTable, itemCode==itemcode&domainCode=="FO")
  if(nrow(i)== 1){
    name = i$itemName }
  if(nrow(agg)== 1){
    name = agg$itemName }
  return(name)
  }

#Function that downloads data from FAOSTAT using getFAOtoSYB()
#Download all data for one product, that means one item and 5 elements
FAO.download = function(item, elem1, elem2, elem3, elem4, elem5){
  FAOquery.df = data.frame(varName = c("Production",
                           "Import_Quantity", "Import_Value",
                           "Export_Quantity", "Export_Value"),
                 domainCode = "FO",
                 itemCode = item,
                 elementCode = c(elem1, elem2, elem3, elem4, elem5),
                 stringsAsFactors = FALSE)
  FAO.lst = with(FAOquery.df,
              getFAOtoSYB(name = varName, domainCode = domainCode,
              itemCode = itemCode, elementCode = elementCode))
  #Add item name and country names to the data frames
  countrycodes$Item = itemname(item)
  FAO.lst$entity = merge(countrycodes, FAO.lst$entity)
  FAO.lst$aggregates$Item = itemname(item)
   return(FAO.lst)
}


##############################################################################
# Download production and trade data for further analysis in other R scripts #
##############################################################################

# Paper and paperboard 
newsprint = FAO.download(1671, 5510, 5610, 5622, 5910, 5922)
printingAndWritingPaper = FAO.download(1674, 5510, 5610, 5622, 5910, 5922)
otherPaperAndPaperboard = FAO.download(1675, 5510, 5610, 5622, 5910, 5922)
totalPaperAndPaperboard = FAO.download(1876, 5510, 5610, 5622, 5910, 5922)

# Aggregate all in one table, it's practical
paperAndPaperboardProducts = list(entity=rbind(newsprint$entity,
                                               printingAndWritingPaper$entity,
                                               otherPaperAndPaperboard$entity,
                                               totalPaperAndPaperboard$entity),
                                  aggregates=rbind(newsprint$aggregates,
                                                   printingAndWritingPaper$aggregates,
                                                   otherPaperAndPaperboard$aggregates,
                                                   totalPaperAndPaperboard$aggregates))


################################
# Save objects in R data files #
################################
save(paperAndPaperboardProducts, countrycodes,
     file = "Paper and paperboard.RData")


############
# Metadata #
############
#Elements in domain FO Forestry Production and Trade
subset(FAOmetaTable$elementTable, domainCode=="FO")
#Forest Products Items (products)
subset(FAOmetaTable$itemTable, domainCode=="FO")
#Forest Products Items aggregated
subset(FAOmetaTable$itemAggTable, domainCode=="FO")


##################
# Test functions #
##################
# Test if the itemname() function  returns the characters "Roundwood"
stopifnot(itemname(1861) == "Roundwood")

# Test if the production of all 3 paper products for EU27 
# is equal to the total paper and paperboard production
newsEU = subset(newsprint$aggregates,FAOST_CODE==5706)
printEU = subset(printingAndWritingPaper$aggregates, FAOST_CODE==5706)
opapEU = subset(otherPaperAndPaperboard$aggregates,FAOST_CODE==5706)
totpapEU = subset(totalPaperAndPaperboard$aggregates,FAOST_CODE==5706)
stopifnot(newsEU$Production + printEU$Production + opapEU$Production - 
          totpapEU$Production == rep(c(0),nrow(totpapEU)))

# How much time does it take to load FAOSTAT data ?
system.time(FAO.download(1876, 5510, 5610, 5622, 5910, 5922))
