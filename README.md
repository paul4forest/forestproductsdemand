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
 and to 27 contries in the European Union. For an overview of consumption, and prices of paper products in EU25 over the 50 years period See [explore.html](./docs/ExploreData/explore.html) in the folder **\Docs\ExploreData**.

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
    test_dir("Y:/Macro/Demand Econometric Models/tests/")
```	

Version Control
---------------
I'm using GIT and I followed this advice to set it up:  [create a repository](https://help.github.com/articles/create-a-repo).

FAOSTAT developper Michael Kao is also using git: [source code of faostat](https://github.com/mkao006/FAOSTATpackage).

Commands I've used so far to upload content to github.com:
```
git pull origin master
git add filename
git commit
git push origin master
```
