# This script loads paper and paperboard production and trade data from FAOSTAT
# and calls the load worldbank.r script
#
# Input: FAOSTAT website accessed with the R FAOSTAT package
# Output: RDATA file containing downloaded data, stored in the rawdata folder
#         Production and trade statistics for forest products
#         As of December 2013, paper and paperboard products and sawnwood products are included
#
# Author: Paul Rougieux, European Forest Institute

# Functions (amoung them faostat functions used below)
source("code/func.r") 

# Load Worldbank data
# source("code/load WorldBank.r")

##############################################################################
# Download production and trade data for further analysis in other R scripts #
##############################################################################
# Production and trade volumes in m3 or tonnes are from the element table:
FAO$elementTable 

# Product codes are from the forestry metatables:
FAO$itemTable 
FAO$itemAggTable 

# Paper and paperboard 
newsprint = FAO$download(1671, 5510, 5610, 5622, 5910, 5922)
printingAndWritingPaper = FAO$download(1674, 5510, 5610, 5622, 5910, 5922)
otherPaperAndPaperboard = FAO$download(1675, 5510, 5610, 5622, 5910, 5922)
totalPaperAndPaperboard = FAO$download(1876, 5510, 5610, 5622, 5910, 5922)

# Aggregate all in one table, it's practical
paperAndPaperboardProducts = list(entity=rbind(newsprint$entity,
                                               printingAndWritingPaper$entity,
                                               otherPaperAndPaperboard$entity,
                                               totalPaperAndPaperboard$entity),
                                  aggregates=rbind(newsprint$aggregates,
                                                   printingAndWritingPaper$aggregates,
                                                   otherPaperAndPaperboard$aggregates,
                                                   totalPaperAndPaperboard$aggregates))

# Sawnwood
sawnwoodC = FAO$download(1632, 5516, 5616, 5622, 5916, 5922)
sawnwoodNC = FAO$download(1633, 5516, 5616, 5622, 5916, 5922)
sawnwoodTotal = FAO$download(1872, 5516, 5616, 5622, 5916, 5922)
sawnwood = list(entity = rbind(sawnwoodC$entity, 
                               sawnwoodNC$entity,
                               sawnwoodTotal$entity),
                aggregates = rbind(sawnwoodC$aggregates, 
                                   sawnwoodNC$aggregates, 
                                   sawnwoodTotal$aggregates))


roundwoodC = FAO$download(1862, 5516, 5616, 5622, 5916, 5922)
roundwoodNC = FAO$download(1863, 5516, 5616, 5622, 5916, 5922)
roundwoodTotal = FAO$download(1861, 5516, 5616, 5622, 5916, 5922)

roundwood = list(entity = rbind(roundwoodC$entity, 
                               roundwoodNC$entity,
                               roundwoodTotal$entity),
                aggregates = rbind(roundwoodC$aggregates, 
                                   roundwoodNC$aggregates, 
                                   roundwoodTotal$aggregates))


# How much time does it take to load FAOSTAT data ?
system.time(FAO$download(1876, 5510, 5610, 5622, 5910, 5922))


################################
# Save objects in R data files #
################################
save(paperAndPaperboardProducts, file = "rawdata/Paper and paperboard.RData")

save(sawnwood, file="rawdata/sawnwood.RData")

save(roundwood, file="rawdata/roundwood.Rdata")

