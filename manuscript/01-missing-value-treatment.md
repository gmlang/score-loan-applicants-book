
Install and load the following packages.

```r
# install.packages("devtools")
# devtools::install_github("gmlang/ezplot")
# devtools::install_github("gmlang/loans")

library(ezplot)
library(loans)
```

Create a directory called *score-loan-applicants* under your home directory. Set it as the working directory where we'll run the analysis. 

```r
proj_path = "~/score-loan-applicants"
dir.create(proj_path, showWarnings=FALSE)
setwd(proj_path)
getwd()
```

```
[1] "/Users/gmlang/score-loan-applicants"
```

Examine the unsecured personal loans (upl) data.

```r
str(upl)
```

```
'data.frame':	7250 obs. of  17 variables:
 $ purpose            : Factor w/ 3 levels "0","1","unknown": 1 1 1 2 3 1 2 1 1 1 ...
 $ age                : num  38.3 40.3 21.7 37.5 43.8 ...
 $ marital            : Factor w/ 2 levels "0","1": 1 2 1 2 1 1 1 2 2 2 ...
 $ employment         : Factor w/ 6 levels "1","2","3","4",..: 1 1 1 3 2 1 5 1 1 1 ...
 $ annual_income      : num  225523 93072 66236 45626 79104 ...
 $ debt_to_income     : num  0.393 0.357 0.868 1.574 1.167 ...
 $ market_value       : num  1540881 1159186 0 1069064 582007 ...
 $ own_property       : Factor w/ 2 levels "0","1": 2 2 1 2 2 1 1 2 2 2 ...
 $ late_repayments    : Factor w/ 2 levels "0","1": 1 1 1 2 2 1 2 1 1 1 ...
 $ repossess          : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 2 1 1 ...
 $ conviction         : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
 $ bankruptcy         : Factor w/ 3 levels "0","1","unknown": 1 1 1 1 1 1 1 1 1 1 ...
 $ unspent_convictions: Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 1 1 1 ...
 $ credit_applications: num  2 2 3 7 3 4 4 2 2 2 ...
 $ credit_line_age    : num  77.5 72.8 15.7 6.9 14 ...
 $ exist_customer     : Factor w/ 2 levels "0","1": 1 1 1 1 1 2 1 1 2 2 ...
 $ bad                : Factor w/ 2 levels "0","1": 1 1 1 2 2 2 1 1 1 1 ...
 - attr(*, "codepage")= int 65001
```

We see the data contains 7250 observations and 17 variables. You can find out the definitions of the variables by typing `?upl` in R.

Check variable types and make changes if necessary

```r
str(upl) # all vars are numeric
```

```
'data.frame':	7250 obs. of  17 variables:
 $ purpose            : Factor w/ 3 levels "0","1","unknown": 1 1 1 2 3 1 2 1 1 1 ...
 $ age                : num  38.3 40.3 21.7 37.5 43.8 ...
 $ marital            : Factor w/ 2 levels "0","1": 1 2 1 2 1 1 1 2 2 2 ...
 $ employment         : Factor w/ 6 levels "1","2","3","4",..: 1 1 1 3 2 1 5 1 1 1 ...
 $ annual_income      : num  225523 93072 66236 45626 79104 ...
 $ debt_to_income     : num  0.393 0.357 0.868 1.574 1.167 ...
 $ market_value       : num  1540881 1159186 0 1069064 582007 ...
 $ own_property       : Factor w/ 2 levels "0","1": 2 2 1 2 2 1 1 2 2 2 ...
 $ late_repayments    : Factor w/ 2 levels "0","1": 1 1 1 2 2 1 2 1 1 1 ...
 $ repossess          : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 2 1 1 ...
 $ conviction         : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
 $ bankruptcy         : Factor w/ 3 levels "0","1","unknown": 1 1 1 1 1 1 1 1 1 1 ...
 $ unspent_convictions: Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 1 1 1 ...
 $ credit_applications: num  2 2 3 7 3 4 4 2 2 2 ...
 $ credit_line_age    : num  77.5 72.8 15.7 6.9 14 ...
 $ exist_customer     : Factor w/ 2 levels "0","1": 1 1 1 1 1 2 1 1 2 2 ...
 $ bad                : Factor w/ 2 levels "0","1": 1 1 1 2 2 2 1 1 1 1 ...
 - attr(*, "codepage")= int 65001
```

```r
# the following predictors need to be coded as factors. Change them to characters first.
iv_cat = c("Bankruptcy", "Purpose", "Exist_Customer", "Unspent_Convictions", 
           "Conviction", "Repossess", "Own_Property", "Late_Repayments", 
           "Marital", "Employment")
iv_cat = tolower(iv_cat)
for (var in iv_cat) upl[[var]] = as.character(upl[[var]])
str(upl[, iv_cat])
```

```
'data.frame':	7250 obs. of  10 variables:
 $ bankruptcy         : chr  "0" "0" "0" "0" ...
 $ purpose            : chr  "0" "0" "0" "1" ...
 $ exist_customer     : chr  "0" "0" "0" "0" ...
 $ unspent_convictions: chr  "1" "0" "0" "0" ...
 $ conviction         : chr  "0" "0" "0" "0" ...
 $ repossess          : chr  "0" "0" "0" "0" ...
 $ own_property       : chr  "1" "1" "0" "1" ...
 $ late_repayments    : chr  "0" "0" "0" "1" ...
 $ marital            : chr  "0" "1" "0" "1" ...
 $ employment         : chr  "1" "1" "1" "3" ...
```

Check which variables have missing values.

```r
n = nrow(upl) # count total number of observations
vars = names(upl)
varsNA = pctNA = c()
for (var in vars) {
        cntNA = sum(is.na(upl[[var]]))
        if (cntNA > 0) {
                varsNA = c(varsNA, var)
                pctNA = c(pctNA, cntNA/n)
        }
}
pctNA = paste0(round(pctNA*100, 2), "%")
pct_missing = data.frame(vars=varsNA, percent_missing = pctNA)
```

```
Error in data.frame(vars = varsNA, percent_missing = pctNA): arguments imply differing number of rows: 0, 1
```

```r
print(pct_missing)
```

```
            vars percent_missing
1        purpose          15.13%
2 debt_to_income           6.03%
3   market_value           9.17%
4     bankruptcy           3.97%
```

Distribution of the target variable

The data set contains 7250 observations, of which 82% are good customers while 18% are bad ones as shown below. The imbalanced distribution of the target variable implies that we can't merely use the overall classification accuracy to measure model performance. For example, suppose we build a model, and it gives us an accuracy of 82%. We wouldn't consider it to be a good model here because we can achieve the same 82% accuracy by simply predicting every observation as good without fitting any model. We want to build models that can correctly identify the bad customers. Therefore, we need to use more granular measures such as sensitivity and specificity to assess model performances. 

```r
## BEGIN Define Function

pct_good_n_bad = function(dat, yvar, xvar = ""){
        if (xvar == "") tbl = data.frame(table(dat[[yvar]]))
        else tbl = data.frame(table(dat[[yvar]][is.na(dat[[xvar]])]))
        tbl$percent = tbl$Freq / sum(tbl$Freq)
        tbl$Freq = NULL
        names(tbl) = c(yvar, "percent") 
        tbl
}

## END Define Function

# change the target to factor, right now, it's coded as numeric
upl$bad = as.factor(upl$bad)

# calculate the distribution of each level in target and plot it
tbl = pct_good_n_bad(upl, "bad")
plt = mk_barplot(tbl)
p = plt("bad", "percent", fillby="bad", xlab="0 - Good, 1 - Bad", legend=F,
        main = "Proportion of Good and Bad Customers")
p = scale_axis(p, "y", use_pct=T, pct_jump=0.2)
print(p)
```

![Target Distribution](figure/unnamed-chunk-7-1.pdf) 

Explore the relationship between missing values and the target

Bankruptcy has few missings (< 4%), Purpose has heavy missings (> 15%), Debt_to_Income has few missings (~ 6%), and Market_Value has 9% missings

```r
# # Bankruptcy has few missings (< 4%)
# tbl = pct_good_n_bad(dat, "bad", "bankruptcy")
# plt = mk_barplot(tbl)
# p = plt("bad", "percent", fillby="bad", xlab="0 - Good, 1 - Bad", legend=F,
#         main = "Proportion of good and bad customers amongst customers with
#                 missing values for bankruptcy")
# p = scale_axis(p, "y", use_pct=T, pct_jump=0.2)
# print(p)
# 
# # Purpose has heavy missings (> 15%)
# tbl = pct_good_n_bad(dat, "bad", "purpose")
# plt = mk_barplot(tbl)
# p = plt("bad", "percent", fillby="bad", xlab="0 - Good, 1 - Bad", legend=F,
#         main = "Proportion of good and bad customers amongst customers with
#                 missing values for purpose")
# p = scale_axis(p, "y", use_pct=T, pct_jump=0.2) 
# print(p)
# 
# # Debt_to_Income has few missings (~ 6%)
# tbl = pct_good_n_bad(dat, "bad", "debt_to_income")
# plt = mk_barplot(tbl)
# p = plt("bad", "percent", fillby="bad", xlab="0 - Good, 1 - Bad", legend=F,
#         main = "Proportion of good and bad customers amongst customers
#                 with missing values for debt_to_income")
# p = scale_axis(p, "y", use_pct=T, pct_jump=0.2)
# print(p)
# 
# # Market_Value has 9% missings
# tbl = pct_good_n_bad(dat, "bad", "market_value")
# plt = mk_barplot(tbl)
# p = plt("bad", "percent", fillby="bad", xlab="0 - Good, 1 - Bad", legend=F,
#         main = "Proportion of good and bad customers amongst customers with
#                 missing values for market_value")
# p = scale_axis(p, "y", use_pct=T, pct_jump=0.2)
# print(p)

# write a for loop to do the same thing. DRY
for (var in varsNA) {
        tbl = pct_good_n_bad(upl, "bad", var)
        title = paste("Percent of good and bad customers \namongst applicants with missing \nvalues for", var)
        plt = mk_barplot(tbl)
        p = plt("bad", "percent", fillby="bad", xlab="0 - Good, 1 - Bad", 
                main = title, legend=F)
        p = scale_axis(p, "y", use_pct=T, pct_jump=0.2)
        print(p)
        cat('\r\n\r\n')
}
```

Deal with missing values.

For categorical vars, change missing values to "unkown".

For continuous vars except market_value, fill the missing values with the grand median. 

For market_value, because it measures similar info with own_property, we fill the missings in market_value based on the values of own_property first. In particular, for records with own_property = 0 and missing market_value, we fill the missings with zeros. For records with own_property = 1 and missing market_value, we fill the missings with the grand median.

```r
print(varsNA)
```

```
NULL
```

```r
# if own_property = 0, market_value should also be 0
upl$market_value[upl$own_property == 0 & is.na(upl$market_value)] = 0

# # method 1
# upl = within(upl, {
#         bankruptcy[is.na(bankruptcy)] = "unknown"
#         purpose[is.na(purpose)] = "unknown"
#         debt_to_income[is.na(debt_to_income)] = median(debt_to_income, na.rm=T)
#         market_value[is.na(market_value)] = median(market_value, na.rm=T)
# })

# method 2
for (var in varsNA) {
        if (class(upl[[var]]) == "character") {
                print(var)
                upl[[var]][is.na(upl[[var]])] = "unknown"
                print(table(upl[[var]]))
        } else {
                print(var)
                upl[[var]][is.na(upl[[var]])] = median(upl[[var]], na.rm=T)                
                print(summary(upl[[var]]))
        }
}
```

Change the character predictors to factors


```r
for (var in iv_cat) upl[[var]] = as.factor(upl[[var]])
```

Create a *data* subfolder under proj_path to save the processed data for future use. 

```r
data_path = file.path(proj_path, "data")
dir.create(data_path, showWarnings=F)
save(upl, iv_cat, file=file.path(data_path, "cleaned-01.rda"))
```

