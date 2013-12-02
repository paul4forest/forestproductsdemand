# This script contains functions that will be lused in other scripts
# Functions to download forest products data from FAOSTAT, using the FAOSTAT module
# Author: Paul Rougieux, European Forest Institute

require(FAOSTAT)

############
# Metadata #
############
forestrymetatable = list(elementTable= subset(FAOmetaTable$elementTable, domainCode=="FO"),
                         itemTable= subset(FAOmetaTable$itemTable, domainCode=="FO"),
                         itemAggTable = subset(FAOmetaTable$itemAggTable, domainCode=="FO"))


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

