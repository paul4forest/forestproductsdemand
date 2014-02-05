This R program estimates demand elasticites for forest products with respect to price and revenue.


**Data sources**: FAOSTAT and World Bank downloaded with the FAOSTAT module  
**Output**: Graphs and table of estimated elasticities   
**Author**: Paul Rougieux, European Forest Institute  
<a href="http://www.efi.int"><img src="docs/efi/efi_logo_rgb_small_siw.jpg" alt="efi_logo_rgb_small_siw.jpg : 17Kb" border="0" height="54" width="50"></a>

Documents 
---------
### Paper and Paperboard Products
We started writting the program while reproducing estimates 
 of demand elasticities in a paper by Chas Amil and Buongiorno.
 This paper covered EU15 countries from 1969 to 1995.
 See our attempt at reproducing the same estimates in the folder: **./docs/ChasAmil2000**  


We then extended the data coverage up to the most recent available Year (2012)
 and to 27 contries in the European Union. For an overview of consumption, and prices of paper products in EU25 over the 50 years period. 
 
You will find descriptive statistics for paper products demand in the EU under the folder **./docs/paper_products**, in [explore.md](./docs/paper_products/explore.md) or [explore.html](./docs/paper_products/explore.html) 

### Sawnwood
You will find descriptive statistics for sawnwood demand in the European Union under [explore_sawnwood.md](./docs/sawnwood/explore_sawnwood.md) or [explore_sawnwood.html](./docs/sawnwood/explore_sawnwood.html)


Code 
----
The following R scripts are in the /code folder
* load
* load WorldBank.r
* clean EU15PaperDemand 0.6.r to prepare data for the ChasAmil 2000 estimates
* clean.r cleans world bank and FAOSTAT data to extract consumption and prices
* And estimation scripts

Tests
------
A "safety belt" is located in the /tests directory. It uses the [testthat](http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf)(external link) package.
Run the following command to run all tests.
```
    library(testthat)
    test_dir("tests")
```	
For a little more verbose messages use the Tap reporter:
```
    test_dir("tests/", reporter="Tap")
```
Remark: Because I usually run all tests with the command test_dir("tests")
the working directory is set to /tests. This is inconvenient in practice.
Therefore I begin each test file by changing the working directory to the root project directory "..".

Working directory
-----------------
[Hadley Wickham recommends](http://stat405.had.co.nz/lectures/05-shortcuts.pdf) not to setwd() in a script. Most scripts load from- or save data to- a path relative to the project path. By default, Knitr sets the working directory to the directory where the document is located. I prefer to change knitr's default directory to the project's root directory. It's possible thanks to an option added by [yihui](https://github.com/yihui/knitr/issues/277). For example if the document is localted in ./docs/explore/, I change all knitrs working directory with the option:
```
opts_knit$set(root.dir = '../..') 
```


Version Control
---------------
I'm using GIT and I followed this advice to set it up:  [create a repository](https://help.github.com/articles/create-a-repo). FAOSTAT developper Michael Kao is also using git: [source code of the R FAOSTAT package](https://github.com/mkao006/FAOSTATpackage). Commands I've used so far to upload content to [github.com/paul4forest/forestproductsdemand](https://github.com/paul4forest/forestproductsdemand):

```
git remote add origin https://github.com/paul4forest/forestproductsdemand
# Creates a remote named "origin" pointing at your GitHub repository
git pull origin master
git add <filename>
git commit -m "Message that explains my changes to the files"
git push origin master
```
Alternatively "git commit -a"" is a replacement for "git add"" and "git commit".  
More version control commands in [my blog](http://paulremote.blogspot.fr/2013/10/git-commands.html)
__Removing files__: If you intend that your next commit should record all modifications of tracked files in the working tree and record all removals of files that have been removed from the working tree with rm (as opposed to git rm), use git __commit -a__, as it will automatically notice and record all removals.

Notes
-----
Note to Paul: try an interactive map with [GoogleVis](http://rpubs.com/gallery/googleVis).

Comments 
----------
by AL
* Change Prices in EUR as effect of EUR USD exchange rate might be important after 2000
* Add consumption and trade of pulp graphs
* Check impact of local price with respect to international price when estimating demand functions

by LH 
* Structural change will be different in graffics paper:
newsprint + printing & writting paper as in packaging paper for example.
Packaging is included under other paper and paper board
When analysing demand for forest products
Deflate out GDP inpact as first start to analyse effects beyond the economic impacts
* Is sawwwod a substitue or a complement for other material?