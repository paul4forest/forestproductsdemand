This R program estimates demand elasticites for forest products with respect to price and revenue.


**Data sources**: FAOSTAT and World Bank downloaded with the FAOSTAT module  
**Output**: Graphs and table of estimated elasticities   
**Author**: Paul Rougieux, European Forest Institute  


Documents 
---------
We started writting the program while reproducing estimates 
 of demand elasticities in a paper by Chas Amil and Buongiorno.
 This paper covered EU15 countries from 1969 to 1995.
 See our attempt at reproducing the same estimates in the folder: **\Docs\ChasAmil2000**  


We then extended the data coverage up to the most recent available Year (2012)
 and to 27 contries in the European Union. For an overview of consumption, and prices of paper products in EU25 over the 50 years period. See  [explore.md](./docs/ExploreData/explore.md) or [explore.html](./docs/ExploreData/explore.html)   in the folder **\Docs\ExploreData**.

Code 
----
The following R scripts are in the /code folder
* load FAOSTAT.r
* load WorldBank.r
* clean EU15PaperDemand 0.6.r to prepare data for the ChasAmil 2000 estimates
* clean.r cleans world bank and FAOSTAT data to extract consumption and prices
* And estimation scripts

Tests
------
A "safety belt" is located in the /tests directory. It uses the [testthat](http://journal.r-project.org/archive/2011-1/RJournal_2011-1_Wickham.pdf)(external link) package.
You may run the following command to run all tests.
```
    test_dir("Y:/Macro/forestproductsdemand/tests/")
```	

Version Control
---------------
I'm using GIT and I followed this advice to set it up:  [create a repository](https://help.github.com/articles/create-a-repo). FAOSTAT developper Michael Kao is also using git: [source code of the R FAOSTAT package](https://github.com/mkao006/FAOSTATpackage). Commands I've used so far to upload content to [github.com/paul4forest/forestproductsdemand](https://github.com/paul4forest/forestproductsdemand):

```
git pull origin master
git add <filename>
git commit 
git push origin master
```
Alternatively "git commit -a"" is a replacement for "git add"" and "git commit".


Notes
=====
Note to Paul: try an interactive map with [GoogleVis](http://rpubs.com/gallery/googleVis).

Comments by AL
----------
Change Prices in EUR as effect of EUR USD exchange rate mighjt be important after 2000

Add consumption and trade of pulp graphs

Ask Antonello about impact of local price with respect to international price
when estimating demand functions

Ask Antonello to get at login for the Free Github account of the LEF

Check EDX and coursera 


Further comments by LH in the train to Lillehammer
---------------------------------------------
Structural change will be different in
  graffics paper: newsprint + printing & writting paper
  as in packaging paper for example.
  Packacging is included under other paper and paper board

When analysing demand for forest products 
Deflate out GDP inpact as first start to analyse effects beyond the economic impacts

Is sawwwod a substutue or a complement for other material?

