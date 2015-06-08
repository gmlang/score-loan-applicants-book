## Pre-select Predictors

In this section, we'll pre-select predictors that will go into the final modeling building process based on the results we got from descriptive analysis, information value and simple logit models in the previous sections. First, we load the relavant data.
        
A>
```r
proj_path = "~/score-loan-applicants"
data_path = file.path(proj_path, 'data')
load(file.path(data_path, 'cleaned-02.rda'))
load(file.path(data_path, 'cleaned-03.rda'))
load(file.path(data_path, 'cleaned-04.rda'))
```

We want to drop predictors that meet all of the following criteria:
* potentially none or weakly predictive predictors found using distribution plots in the descriptive analysis section
* potentially none-predictive predictors based on information value.
* potentially none-significant predictors identified by the simple logit models.

A>
```r
drop_cat1 = intersect(intersect(iv_cat_none, IV_none), iv_non_sig)
drop_con1 = intersect(intersect(iv_con_none, IV_none), iv_non_sig)
drop_cat2 = intersect(intersect(iv_cat_weak, IV_none), iv_non_sig)
drop_con2 = intersect(intersect(iv_con_weak, IV_none), iv_non_sig)
drop_cat = c(drop_cat1, drop_cat2)
drop_con = c(drop_con1, drop_con2)
print(drop_cat)
```

A>{linenos=off}
```
[1] "exist_customer"      "unspent_convictions" "purpose"             "employment"         
```

A>
```r
print(drop_con)
```

A>{linenos=off}
```
[1] "age"
```

We drop these predictors and update data for ensuing analysis.

A>
```r
iv_cat = c(iv_cat_none, iv_cat_weak, iv_cat_strong)
iv_cat = iv_cat[!iv_cat %in% drop_cat]
print(iv_cat)
```

A>{linenos=off}
```
[1] "marital"          "bankruptcy"       "conviction"       "repossess"        "own_property"    
[6] "late_repayments"  "market_value_cat"
```

A>
```r
iv_con = c(iv_con_none, iv_con_weak, iv_con_strong)
iv_con = iv_con[!iv_con %in% drop_con]
print(iv_con)
```

A>{linenos=off}
```
[1] "market_value"        "credit_applications" "log_debt_to_income"  "log_annual_income"  
[5] "credit_line_age"    
```

A>
```r
save(upl, iv_cat, iv_con, file=file.path(data_path, "cleaned-05.rda"))
```
