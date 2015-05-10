---
title: "01-set-up"
author: "gmlang"
date: "April 3, 2015"
output: md_document
---

# Set Up

1. Install [R](http://www.r-project.org) and [Rstudio](http://www.rstudio.com/products/rstudio/download/).
2. Install a set of development tools:
* On Windows, download and install [Rtools](http://cran.r-project.org/bin/windows/Rtools/). 
* On Mac, install the [Xcode command line tools](https://developer.apple.com/downloads). 
* On Linux, install the R development package, usually called **r-devel** or **r-base-dev**.
3. Install devtools by running 

A>
```r
install.packages("devtools")
```
4. Use devtools to install the following R packages.

A> 
```r
devtools::install_github("gmlang/ezplot")
devtools::install_github("gmlang/loans")
```
