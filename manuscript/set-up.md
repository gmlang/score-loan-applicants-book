# Set Up

Install and load the following packages.

A> {linenos=off}
```r
install.packages("devtools")
devtools::install_github("gmlang/ezplot")
devtools::install_github("gmlang/loans")
library(ezplot)
library(loans)
```

Create a directory called *score-loan-applicants* under your home directory. Use it as the project folder that will store all files related with our analysis, which include code, processed data, intermediate results, figures, and etc.

A> {linenos=off}
```r
proj_path = "~/score-loan-applicants"
dir.create(proj_path, showWarnings=FALSE)
```
