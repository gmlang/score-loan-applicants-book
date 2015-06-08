## Simple Logit Models

First, let's load the data we saved from section 3.2.

A>
```r
proj_path = "~/score-loan-applicants"
data_path = file.path(proj_path, 'data')
file_path = file.path(data_path, 'cleaned-02.rda')
load(file_path)
```

Next, we fit a simple logit model regressing the target against each predictor separately and extract the parameter estimates and their p-values. We do that using a for loop.

A>
```r
predictors = c(iv_cat_strong, iv_cat_weak, iv_cat_none, 
               iv_con_strong, iv_con_weak, iv_con_none)
simple_est = data.frame()
for (var in predictors) {
        f = formula(paste0("bad ~ ", var))
        fit = glm(f, data=upl, family=binomial)
        simple_est = rbind(simple_est, rbind(summary(fit)$coef[2, c("Estimate", "Pr(>|z|)")]))
} 
simple_est = cbind(var=predictors, simple_est)
simple_est$Significant = ifelse(simple_est[["Pr(>|z|)"]] < 0.05, "*", " ")
print(simple_est)
```

A>{linenos=off}
```
                   var       Estimate    Pr(>|z|) Significant
1           bankruptcy -0.12067570796  7.8672e-01            
2           conviction  0.72992473492  5.7607e-04           *
3            repossess -0.88834871922  2.3738e-04           *
4         own_property  2.00955624792 4.2291e-102           *
5      late_repayments  1.05650903419  2.6028e-53           *
6     market_value_cat  2.57523010938 3.6626e-126           *
7              purpose  0.13176201859  6.2791e-02            
8              marital -0.16125475122  8.9267e-03           *
9           employment -0.13361636031  2.3655e-01            
10      exist_customer  0.06669536130  2.9758e-01            
11 unspent_convictions  0.02492247562  8.2639e-01            
12  log_debt_to_income  1.14922553664 1.3272e-106           *
13   log_annual_income -1.63352153666 1.0175e-151           *
14     credit_line_age -0.02320293478  2.1985e-97           *
15        market_value  0.00000067324  1.0416e-44           *
16 credit_applications  0.38084436288  2.1742e-77           *
17                 age -0.00345785109  3.0503e-01            
```

We then separate the predictors into two groups based on if their p-values are greater than 0.05 or not.

A>
```r
iv_sig = as.character(simple_est$var[simple_est[["Pr(>|z|)"]] <= 0.05])
print(iv_sig)
```

A>{linenos=off}
```
 [1] "conviction"          "repossess"           "own_property"        "late_repayments"    
 [5] "market_value_cat"    "marital"             "log_debt_to_income"  "log_annual_income"  
 [9] "credit_line_age"     "market_value"        "credit_applications"
```

A>
```r
iv_non_sig = as.character(simple_est$var[simple_est[["Pr(>|z|)"]] > 0.05])
print(iv_non_sig)
```

A>{linenos=off}
```
[1] "bankruptcy"          "purpose"             "employment"          "exist_customer"     
[5] "unspent_convictions" "age"                
```

Finally, we save them for ensuing analysis.
A>
```r
save(iv_sig, iv_non_sig, file=file.path(data_path, 'cleaned-04.rda'))
```
