# Set up

1. Install [R](http://www.r-project.org) and [Rstudio](http://www.rstudio.com/products/rstudio/download/).
2. Install a set of development tools:
* On Windows, download and install [Rtools](http://cran.r-project.org/bin/windows/Rtools/). 
* On Mac, install the [Xcode command line tools](https://developer.apple.com/downloads). 
* On Linux, install the R development package, usually called **r-devel** or **r-base-dev**.
3. Install the following R packages.

A>
```r
install.packages("devtools")
devtools::install_github("gmlang/ezplot")
devtools::install_github("gmlang/loans")
install.packages("rJava")
install.packages("glmulti")
install.packages("dplyr")
install.packages("tidyr")
```
