#
# Generate Reports for the different forest products using brew
# 
# Inspired by 
# http://botthoughts.wordpress.com/2012/05/17/generating-reports-for-different-data-sets-using-brew-and-knitr/

library(brew)
# library(tools)
library(knitr)

#############
# Load data #
#############
print(load("enddata/EU27 sawnwood demand.rdata"))
print(load("enddata/EU27 paper products demand.rdata"))


###############
# knitr alone #
###############
# setwd() is not good but didn't  find a better way
# I have asked a question about knitr's figure placement on StackOverflow:
# http://stackoverflow.com/questions/21582402/knitr-how-to-set-a-figure-path-in-knit2html-without-using-setwd
setwd("./docs/all_products/")
dtfagg = ppagg
knit2html("template.Rmd", "pp.html")
dtfagg = swdagg
knit2html("template.Rmd", "swd.html")
setwd("../..")


##################
# Brew and knitr #
##################
# setwd() is not good but didn't  find a better way
# I have asked a question about knitr's figure placement on StackOverflow:
# http://stackoverflow.com/questions/21582402/knitr-how-to-set-a-figure-path-in-knit2html-without-using-setwd
setwd("./docs/all_products/")
dtfagg = "swdagg"
brew("template.brew.Rmd", "template_swd.Rmd")
knit2html("template_swd.Rmd", "swd.html")
dtfagg = "ppagg"
brew("template.brew.Rmd", "template_pp.Rmd")
knit2html("template_pp.Rmd", "pp.html")
setwd("../..")


###################
# Without setwd() #
###################
dtfagg = "swdagg"
brew("./docs/all_products/template.brew.Rmd", "./docs/all_products/template_swd.Rmd")
# Rmd to markdown file
knit("./docs/all_products/template_test.Rmd", "./docs/all_products/bli3.md")
# Or Rmd directly to html
knit2html("./docs/all_products/template_test.Rmd", "./docs/all_products/bli3.html",  new.env() )
# The issue is that figures are storred in the root project directory 
# instead of in  .docs/all_products/figure
# What does new.env() do?


#######################################
# Put this function into .code/func.r #
#######################################
create.report <- function(dtf.name, dtfagg.name, path = "docs/all_products/"){
    rnw.file <- paste0(prepend, x, ".Rnw")
    brew('template.Rnw', rnw.file)
    knit(rnw.file)
    latex.file <- paste0(prepend, x, ".tex")
    texi2pdf(latex.file, clean = TRUE, quiet = TRUE)
    out.file <- paste0(prepend, x, ".pdf")
    return(out.file)
}



## sof
knit2html("./subdir/file.Rmd", "./subdir/file.html")

setwd("./subdir/")
knit2html("file.Rmd", "file.html")

# Create reports 
create.report()


