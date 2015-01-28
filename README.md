---
title: "Forest products trade flows database"
output:
  html_document:
    toc : true
---
This package prepares data for a forest products trade flow database.
It contains functions that automatically grab data from the 
COMTRADE and COMEXT databases, feed it through algorithms to compare 
unit values, to mirror export and import values.
Data will be delivered to an online user-interface.

Examples and demonstration:

* See example use in the doc/www folder.
* look in docs/development/countries for investigations on country specific issues
* look in docs/development/cleaning for unit price calculation 
  and a procedure to prepare volume and weight data
  
## Installation
This package can be installed with the devtools package.
```
library(devtools)
install_bitbucket("paul4forest/tradeflows")
```
Load the package and configure the database with:
```
library(tradeflows)
setdatabaseconfig()
```
The database configuration file can be edited and reloaded.
`setdatabaseconfig(reload=TRUE)` displays the location of the database configuration file.

### Devtools package installation
If the devtools package is not installed on your system.
```
install.packages("devtools")
```
You may need to upgrade the libcurl library on your system shell
```
sudo apt-get install libcurl4-gnutls-dev 
```


### Installation on a server 
More details on the server configuration in this 
[google doc](https://docs.google.com/document/d/1pY6HL0kOqulsdWzgZ7ic5ibJP4wrlF0Fl1wbhV3Kxdg/edit#heading=h.4fw0eqfvx5sz)

Rmarkdown requires the latest pandoc version,
[explanation here](https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md#newer-systems-debianubuntufedora)

To include the latest required version of pandoc:

* [install pandoc from source](http://johnmacfarlane.net/pandoc/installing.html#all-platforms)
 haksell installation was rather bulky.
 and pandoc installation from source failed:
```
 setup: At least the following dependencies are missing:
 http-client >=0.3.2 && <0.4 && ==0.4.5
 cabal: Error: some packages failed to install:
 pandoc-1.13.1 failed during the configure step. The exception was:
 ExitFailure 1
```
* Or [install Rstudio server](http://www.rstudio.com/products/rstudio/download-server/)
```
md5sum rstudio-server-0.98.1091-amd64.deb
930eca2738ce335791df41c3f6302fae 
```
Rstudio server requres the installation of a recent libssl version.
md5sum should not be used anymore for security reason, 
authenticity should be checked with
```
sha256sum libssl0.9.8_0.9.8o-4squeeze14_amd64.deb 
```
Couldn't find the key. So i loaded if from 2 locations and checked that they had 
the same sum. 
To manually stop, start, and restart the server you use the following commands:
```
$ sudo rstudio-server stop
$ sudo rstudio-server start
$ sudo rstudio-server restart
```

## Input
The function `loadcomtrade` and `loadcomtrade_bycode`, 
load data from the comtrade API in JSON format 
and convert it to data frames.

* [Comtrade data extraction interface](http://comtrade.un.org/data/) beta
* [Comtrade API release notes](http://comtrade.un.org/data/doc/releasenotes/)
 first release in February 2014. 
* [Bug request of the Comtrade API](https://www.surveymonkey.com/r/?sm=2BQpdoHC1wFB3xIKcDja4TErpxm5%2b5nI5Iuz8et35wI%3d)
 * submitted the 
jsonlite::fromJSON("http://comtrade.un.org/data/cache/classificationHS.json")
Warning message:
Unexpected Content-Type: application/x-javascript 

### Raw data
Inspired by the way hadley prepares this [flight planes data](https://github.com/hadley/nycflights13/tree/master/data-raw).
The package includes a training dataset:
sawnwood bilateral trade data for European countries.

## Output
Reports and data files.

### Data output
One record contains information about one flow,
for a unique combination of 
c("reportercode", "partnercode", "productcode", "flow", "year")
For more information on this unique combination,
see the merge part of the `addpartnerflow()` function, 
Each single record can be caracterized by 6 figures:

1. weight, tradevalue and quantity as reported by the _reporter_ area
2. weight, tradevalue and quantity as reported by the _partner_ area

### Templates
Templates are placed in inst/templates, 
[location inspired by the rapport package](https://github.com/Rapporter/rapport/tree/master/inst/templates).
See also their function [rapport.ls](https://github.com/Rapporter/rapport/blob/7b459b9733a44511b4884b6d35d25d743c7a11e1/R/rp_helpers.R) that lists templates.
And their function 
[rapport.read](https://github.com/Rapporter/rapport/blob/7b459b9733a44511b4884b6d35d25d743c7a11e1/R/template.R) that reads templates from files or form 
package-bundled templates.


## Tools
### This is a package
Created based on [instructions from Hadley](http://r-pkgs.had.co.nz/).
`devtools::load_all()` or __Cmd + Shift + L__, reloads all code in the package.
Add packages to the list of required packages
`devtools::use_package("dplyr")`
`devtools::use_package("ggplot2", "suggests")`
For data I followed his recommendations in r-pkgs/data.rmd
`devtools::use_data(mtcars)`
`devtools::use_data_raw()` # To create a data-raw/ folder and add it to .Rbuildignore


### Tests
Example of testing [for the devtools package](https://github.com/hadley/devtools/blob/master/tests/testthat/test-data.r)

### Data frame manipulation with dplyr
dplyr uses non standard evaluation. See vignette("nse") 
NSE is powered by the lazyeval package
```
# standard evaluation
sawnwood %>% select_(.dots = c("yr", "rtCode" )) %>% head
# is the same as
# lazy evaluation
sawnwood %>% select(yr, rtCode ) %>% head
```

### Error catching with tryCatch

* see `demo(error.catching)`.
* See also Hadley's article: [beyond-exception-handling](http://adv-r.had.co.nz/beyond-exception-handling.html).

### Documentation in long form
[How to create package vignettes](http://r-pkgs.had.co.nz/vignettes.html).

> You can build all vignettes from 
> the console with devtools::build_vignettes()

> RStudio’s “Build & reload” does not build vignettes to save time. Similarly, devtools::install_github() (and friends) will not build vignettes by default because they’re time consuming and may require additional packages. You can force building with devtools::install_github(build_vignettes = TRUE). This will also install all suggested packages.


### Function documentation using roxygen2
Export documentation in a pdf document at the command line in the man folder run
R CMD Rd2pdf *

You should be able to see the documentation of exported functions by placing a 
question mark before the function name at the R command prompt.

inspired by the documentation of roxygenize
https://github.com/yihui/roxygen2/blob/master/R/roxygenize.R
`vignette("namespace", package = "roxygen2")` says:

> If you are using just a few functions from another package, the recommended option is to note the package name in the Imports: field of the DESCRIPTION file and call the function(s) explicitly using ::, e.g., pkg::fun(). Alternatively, though no longer recommended due to its poorer readability, use @importFrom, e.g., @importFrom pgk fun, and call the function(s) without ::.
> If you are using many functions from another package, use @import package to import them all and make available without using ::.

But Hadley says:

> Alternatively, if you are repeatedly using many functions from another package, you can import them in one command with @import package. This is the least recommended solution: it makes your code harder to read (because you can’t tell where a function is coming from), and if you @import many packages, the chance of a conflicting function names increases.

calling packages might have to be changed to follow Hadley's recommendations
on how package namespaces: http://r-pkgs.had.co.nz/namespace.html
see also vignette("namespace", package = "roxygen2")
require(RJSONIO)
require(dplyr)


### Version tracking system with git
The .git repository is backed on bitbucket.
Use devtools::install_bitbucket() to install the package.


### Shiny
A demonstration with time series plot and bar chart will be made
with shiny and the ggplot2 package, based on the diamond example using.

### Screen server tool
Use screen to keep a long process running on a server after you close the ssh session. I started a screen session with:

        screen -S sessionname

In order to find the screen session later you might want to rename it using sessionname. Or on the first screen invocation use the s flag -S sessionname

I started the R software in this screen session, started a long running process. Then detached the session with:

        CTRL-A-D

I could re-attach the session later with:

        screen -r sessionname

If the session was not detached properly, it might be necessary to detach it and re attach it:

       screen -d -r sessionname

## Ongoing work

* [Oct 2014] Use a client side table sorter, 
such as the [JQuery tablesorter](http://tablesorter.com/docs/)
The plugin requires the thead and tbody tags, which are generated by Rmarkdown. 
Calling the javascript files should be possible within a YAML document, see 
[HTML document format](http://rmarkdown.rstudio.com/html_document_format.html).
    + I linked to the necessary javascript in docs/www/include/in_header.html
    + It basically works, CSS styles need to be added so that we can see little sorting arrows. See sample table in docs/www/index.html

### Work for programmers of the production system
* Automate data load from comtrade
    * Use loadcomtrade_bycode() as a function
    * Replace loadcomtrade_bycode() by a software that loads json files from comtrade, and places the content in a raw comtrade table in a SQL database
* Automate clean mechanism
    * Call R functions in clean.R from the server to clean the raw data
  from the database. Put cleaned data in the database.

### Bugs
20141208 A bug in Lyx prevents me from generating pdfs 
    when the text contains a euro € sign.


### TODO by order of ease / importance
* Change the docs/development folder into vignettes.
* use an environmennt variable for the yearbegin and yearend of the
function that chooses between reporter and partner volume
* Create a trac system with externam accesss to track progress.
* in checkdbcolumns() Use sqlquery <- "SELECT `COLUMN_NAME` 
FROM `INFORMATION_SCHEMA`.`COLUMNS` 
WHERE `TABLE_SCHEMA`='tradeflows' 
AND `TABLE_NAME`='validated_flow_yearly';"
instead of loading a data frame to check the column names.
Because loading a data frame doesn't work when the table is
empty.
* create a function to display product codes available
or a function that returns an uncollected tbl dplyr object
on which to run arbitrary dplyr statements. 
--> This might not actually be needed.
* calculate regional prices as a ponderation of 
import and export prices as done in Chasamil2000
* discrepancy report
in the server function,
add a parameter to the loadcomtrade_bycode function to render this optional
log validataion status of jsonfiles with
fileConn<-file("output.txt")
writeLines(c("Hello","World"), fileConn)
close(fileConn)
Change the docs/development folder into vignettes.

* Calculate median prices by region patner and see how 
  using partner prices for conversion impacts the 
  world trade flows
* Floating table of content for html reports with a custom csss
  http://rpubs.com/stevepowell99/floating-css
* Use aesthetic to make points more transparent on a coutry * contry grid 
 display trade volume as alpha level.
 see also docs/development/ggplot2
* use package options, inspired by the devtools or knitr package 
  "Devtools uses the following options to configure behaviour:..."
* Time plot of HS by quantity, weight or value data (ggplot gant chart) to 
visualise missing data
* load from EUROSTAT comext at 10 digit level
* Javascript visualisation: [Add googleVis and Rcharts to Markdown documents](http://al2na.github.io/Rmarkdown_JSviz/)

### Notes to EFI developpers
Change this to a vignette installation and configuration
Which directories I want to read at
https://bitbucket.org/paul4forest/tradeflows/?
You want to look at files in the R folders.

* database.R is doing the database interaction
* clean.R is cleaning the datai

The configuration table columnnames located in config/column_names.csv
now contains 2 column specifying which columns names
are used in the trade flows database:
"raw_flow" and "validated_flow"
Database configuration file and column names are located under:
```
system.file("config", package="tradeflows")
```

The function cleandb() will feed data into the database table(s) validated_flow
updates will be done on a product basis, at the 6 digit level. The cleaning script will:

1. Delete all flows for a product
  (between all reporter and partner countries in all years),
2. Enter All validated flows for that product.

The main clean instruction 
can also run from a system shell directly
```
Rscript -e 'library(tradeflows)' -e 'cleandbproduct(440799, tableread =  "raw_flow_yearly", tablewrite = "validated_flow_yearly")
```
