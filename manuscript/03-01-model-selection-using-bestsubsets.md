## Model Selection Using the Best Subsets Algorithm

First, we load the cleaned data and split it into training and testing subsets. We do this because we'll use the training set for model selection, and we'll use the testing set for backtesting the performance of the chosen model.

A>
```r
proj_path = "~/score-loan-applicants"
data_path = file.path(proj_path, 'data')
file_path = file.path(data_path, 'cleaned-06.rda')
load(file_path)
# split dat into training (60%) and testing (40%) sets
set.seed(123294)
train = sample(1:nrow(upl), round(nrow(upl)*0.6))
dat_train = upl[train, ]
dat_test = upl[-train, ]
```

Just a quick recap, these are the predictors we'll be working with.

A>
```r
print(predictors)
```

A>{linenos=off}
```
[1] "credit_applications" "log_annual_income"   "credit_line_age"     
[4] "marital"             "bankruptcy"          "conviction"          
[7] "repossess"           "late_repayments"     "market_value_cat"   
```

Next, we build a logit model regressing the target variable against each subset of the predictors. We do that for all subsets of the predictors, and we only use the training dataset. Each model will have an [AIC](http://en.wikipedia.org/wiki/Akaike_information_criterion) value. The one with the smallest AIC will be the best model. This can be easily done using the `glmulti` package.

A>
```r
library(glmulti)
t0 = proc.time() # record starting time
f = as.formula(paste0("bad ~ ", paste(predictors, collapse=" + ")))
bestsub_logit = glmulti(f, data = dat_train, 
                        level = 1, # no interaction considered
                        method = "h", # exhaustive approach
                        crit = "aic", # AIC as criteria
                        confsetsize = 5, # keep 5 best models
                        plotty = F, report = F, # no plot or interim reports
                        fitfunction = "glm", # glm function
                        family = binomial) # binomial family for logit model
cat("Run time: ")
print(proc.time() - t0) # calculating time it took to run the models
```

A>{linenos=off}
```
Run time: 
   user  system elapsed 
 13.033   2.033  15.356 
```

The top 5 best models are

A>
```r
bestsub_logit@formulas # use @ instead of $ since bestsub_logit is a s4 object
```

A>
```
bad ~ 1 + marital + bankruptcy + market_value_cat + log_annual_income + 
	  credit_line_age
bad ~ 1 + marital + bankruptcy + conviction + market_value_cat + 
      log_annual_income + credit_line_age
bad ~ 1 + marital + bankruptcy + repossess + market_value_cat + 
      log_annual_income + credit_line_age
bad ~ 1 + marital + bankruptcy + late_repayments + market_value_cat + 
      log_annual_income + credit_line_age
bad ~ 1 + marital + bankruptcy + market_value_cat + credit_applications + 
      log_annual_income + credit_line_age
```

The best model is

A>
```r
summary(bestsub_logit@objects[[1]])
```

A>{linenos=off}
```
Call:
fitfunc(formula = as.formula(x), family = ..1, data = data)
Deviance Residuals: 
   Min      1Q  Median      3Q     Max  
-2.475  -0.578  -0.302  -0.108   3.133  
Coefficients:
                                        Estimate Std. Error z value Pr(>|z|)    
(Intercept)                             25.33599    1.58346   16.00  < 2e-16 ***
marital1                                -0.18820    0.09355   -2.01    0.044 *  
bankruptcy1                             -0.61627    0.66155   -0.93    0.352    
bankruptcyunknown                        0.81843    0.20325    4.03 0.000057 ***
market_value_cat$1 - $910,600            2.41009    0.16009   15.05  < 2e-16 ***
market_value_cat$910,601 - $1,290,000    3.01516    0.16261   18.54  < 2e-16 ***
market_value_cat$1,290,001 - $2,680,000  3.49140    0.19470   17.93  < 2e-16 ***
log_annual_income                       -2.46533    0.14573  -16.92  < 2e-16 ***
credit_line_age                         -0.00458    0.00214   -2.13    0.033 *  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
(Dispersion parameter for binomial family taken to be 1)
    Null deviance: 4140.3  on 4349  degrees of freedom
Residual deviance: 2991.0  on 4341  degrees of freedom
AIC: 3009
Number of Fisher Scoring iterations: 6
```

We can extract the main effects of the best model.

A>
```r
temp = as.character(bestsub_logit@formulas[[1]])[3]
main_effects = strsplit(temp, " \\+ ")[[1]][-1]
print(main_effects)
```

A>{linenos=off}
```
[1] "marital"           "bankruptcy"        "market_value_cat"  
[4] "log_annual_income" "credit_line_age"  
```

If you recall, in section 3.6, we identified potentially correlated predictors. We also need to consider the effect of their interactions on the target. We create variables in R to hold them.

A>
```r
interact_terms1 = c("marital:bankruptcy", 
                    "bankruptcy:market_value_cat")
interact_terms2 = c("credit_line_age:bankruptcy",
                    "credit_line_age:market_value_cat",
                    "log_annual_income:bankruptcy",
                    "log_annual_income:market_value_cat")
interactions = c(interact_terms1, interact_terms2)
```

We can then make model formulas for all combinations of the main effects given by the best model we just found and these interaction terms.

A>
```r
base_model = paste0("bad ~ ", paste(main_effects, collapse=" + "))
# create list of models
list_of_models = lapply(seq_along(interactions), function(n) {
        left = base_model
        right = apply(combn(interactions, n), 2, paste, collapse = " + ")
        paste(left, right, sep = " + ")
})
# convert to vector
vec_of_models = unlist(list_of_models)
vec_of_models = c(base_model, vec_of_models)
```

Finally, we loop through each of the new model fomula, fit a logit model on the training set, and select the final best model using AIC. 

A>
```r
list_of_fits = lapply(vec_of_models, function(x) {
        formula = as.formula(x)
        fit = glm(formula, data=dat_train, family=binomial)
        result_AIC = extractAIC(fit)
        data.frame(predictor_cnt = result_AIC[1],
                   AIC = result_AIC[2], model = x, stringsAsFactors=F)
})
result = do.call(rbind, list_of_fits) # collapse to a data frame
result = result[order(result$AIC),] # sort
```

The final best model is

A>
```r
fbest = as.formula(result$model[1])
fbest
```

A>{linenos=off}
```
bad ~ marital + bankruptcy + market_value_cat + log_annual_income + credit_line_age + 	  log_annual_income:bankruptcy + log_annual_income:market_value_cat
```

Using this formula, we refit the final best model on the training set.

A>
```r
bestfit = glm(fbest, data=dat_train, family=binomial)
coef(bestfit)
```

A>{linenos=off}
```
                                              (Intercept) 
                                               20.7806749 
                                                 marital1 
                                               -0.1694319 
                                              bankruptcy1 
                                               -2.1402012 
                                        bankruptcyunknown 
                                               11.4584570 
                            market_value_cat$1 - $910,600 
                                               -0.5009762 
                    market_value_cat$910,601 - $1,290,000 
                                               10.7874772 
                  market_value_cat$1,290,001 - $2,680,000 
                                               30.3147072 
                                        log_annual_income 
                                               -2.0574576 
                                          credit_line_age 
                                               -0.0050016 
                            bankruptcy1:log_annual_income 
                                                0.1326843 
                      bankruptcyunknown:log_annual_income 
                                               -0.9153788 
          market_value_cat$1 - $910,600:log_annual_income 
                                                0.2534780 
  market_value_cat$910,601 - $1,290,000:log_annual_income 
                                               -0.6824139 
market_value_cat$1,290,001 - $2,680,000:log_annual_income 
                                               -2.2484841 
```

You can run `summary(bestfit)` to find out the standard errors and p-values associated with these parameter coefficients.

