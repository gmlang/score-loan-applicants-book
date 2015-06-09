---
title: "03-01-model-selection-using-bestsubsets"
author: "gmlang"
date: "April 3, 2015"
output: pdf_document
---


```r
library(knitr)
opts_chunk$set(comment = "", warning = FALSE, message = FALSE, tidy = FALSE,
               echo = TRUE, fig.width = 6, fig.height = 6, dev = 'png')
options(width = 100, scipen = 5, digits = 5)
```

## Model Selection Using the Best Subsets Algorithm

First, we load the cleaned data and split it into training and testing subsets. We do this because we'll use the training set for model selection, and we'll use the testing set for backtesting the performance of the chosen model.


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


```r
print(predictors)
```

```
 [1] "market_value"        "credit_applications" "log_annual_income"   "credit_line_age"    
 [5] "marital"             "bankruptcy"          "conviction"          "repossess"          
 [9] "late_repayments"     "market_value_cat"   
```

Next, we build a logit model regressing the target variable against each subset of the predictors. We do that for all subsets of the predictors, and we only use the training dataset. Each model will have an [AIC](http://en.wikipedia.org/wiki/Akaike_information_criterion) value. The one with the smallest AIC will be the best model. This can be easily done using the `glmulti` package.


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
```

```
Run time: 
```

```r
print(proc.time() - t0) # calculating time it took to run the models
```

```
   user  system elapsed 
 24.808   4.283  29.314 
```

The top 5 best models are


```r
bestsub_logit@formulas # use @ instead of $ since bestsub_logit is a s4 object
```

```
[[1]]
bad ~ 1 + marital + bankruptcy + market_value_cat + market_value + 
    log_annual_income + credit_line_age
<environment: 0x1148b5990>

[[2]]
bad ~ 1 + marital + bankruptcy + repossess + market_value_cat + 
    market_value + log_annual_income + credit_line_age
<environment: 0x1148b5990>

[[3]]
bad ~ 1 + marital + bankruptcy + conviction + market_value_cat + 
    market_value + log_annual_income + credit_line_age
<environment: 0x1148b5990>

[[4]]
bad ~ 1 + marital + bankruptcy + late_repayments + market_value_cat + 
    market_value + log_annual_income + credit_line_age
<environment: 0x1148b5990>

[[5]]
bad ~ 1 + marital + bankruptcy + market_value_cat + market_value + 
    log_annual_income
<environment: 0x1148b5990>
```

The best model is


```r
summary(bestsub_logit@objects[[1]])
```

```

Call:
fitfunc(formula = as.formula(x), family = ..1, data = data)

Deviance Residuals: 
   Min      1Q  Median      3Q     Max  
-2.530  -0.578  -0.306  -0.094   3.211  

Coefficients:
                                            Estimate   Std. Error z value  Pr(>|z|)    
(Intercept)                             28.199951211  1.739535768   16.21   < 2e-16 ***
marital1                                -0.200295436  0.093837348   -2.13   0.03280 *  
bankruptcy1                             -0.626182432  0.668194425   -0.94   0.34869    
bankruptcyunknown                        0.843667065  0.204828699    4.12 0.0000381 ***
market_value_cat$1 - $910,600            1.241672912  0.301871494    4.11 0.0000390 ***
market_value_cat$910,601 - $1,290,000    1.360111697  0.397045751    3.43   0.00061 ***
market_value_cat$1,290,001 - $2,680,000  1.282895830  0.523735010    2.45   0.01430 *  
market_value                             0.000001614  0.000000355    4.54 0.0000055 ***
log_annual_income                       -2.725048914  0.159953255  -17.04   < 2e-16 ***
credit_line_age                         -0.004129667  0.002154689   -1.92   0.05529 .  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 4140.3  on 4349  degrees of freedom
Residual deviance: 2971.1  on 4340  degrees of freedom
AIC: 2991

Number of Fisher Scoring iterations: 6
```

We can extract the main effects of the best model.


```r
temp = as.character(bestsub_logit@formulas[[1]])[3]
main_effects = strsplit(temp, " \\+ ")[[1]][-1]
print(main_effects)
```

```
[1] "marital"           "bankruptcy"        "market_value_cat"  "market_value"     
[5] "log_annual_income" "credit_line_age"  
```

If you recall, in section 3.6, we identified potentially correlated predictors. We also need to consider the effect of their interactions on the target. We create variables in R to hold them.


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
head(result)
```

```
   predictor_cnt    AIC
7             13 2927.1
22            15 2927.5
19            15 2930.1
16            19 2930.6
12            15 2931.0
41            17 2931.2
                                                                                                                                                                                                 model
7                                                              bad ~ marital + bankruptcy + market_value_cat + market_value + log_annual_income + credit_line_age + log_annual_income:market_value_cat
22                              bad ~ marital + bankruptcy + market_value_cat + market_value + log_annual_income + credit_line_age + log_annual_income:bankruptcy + log_annual_income:market_value_cat
19                                bad ~ marital + bankruptcy + market_value_cat + market_value + log_annual_income + credit_line_age + credit_line_age:bankruptcy + log_annual_income:market_value_cat
16                               bad ~ marital + bankruptcy + market_value_cat + market_value + log_annual_income + credit_line_age + bankruptcy:market_value_cat + log_annual_income:market_value_cat
12                                        bad ~ marital + bankruptcy + market_value_cat + market_value + log_annual_income + credit_line_age + marital:bankruptcy + log_annual_income:market_value_cat
41 bad ~ marital + bankruptcy + market_value_cat + market_value + log_annual_income + credit_line_age + credit_line_age:bankruptcy + log_annual_income:bankruptcy + log_annual_income:market_value_cat
```

The final best model is


```r
fbest = as.formula(result$model[1])
fbest
```

```
bad ~ marital + bankruptcy + market_value_cat + market_value + 
    log_annual_income + credit_line_age + log_annual_income:market_value_cat
```

Using this formula, we refit the final best model on the training set.


```r
bestfit = glm(fbest, data=dat_train, family=binomial)
summary(bestfit)
```

```

Call:
glm(formula = fbest, family = binomial, data = dat_train)

Deviance Residuals: 
   Min      1Q  Median      3Q     Max  
-2.732  -0.536  -0.277  -0.127   3.017  

Coefficients:
                                                              Estimate   Std. Error z value
(Intercept)                                               21.377844848  2.387273825    8.95
marital1                                                  -0.196586703  0.095516781   -2.06
bankruptcy1                                               -0.618257965  0.670917299   -0.92
bankruptcyunknown                                          0.845954299  0.208715602    4.05
market_value_cat$1 - $910,600                              2.841600640  3.248407873    0.87
market_value_cat$910,601 - $1,290,000                     12.190680885  3.896596242    3.13
market_value_cat$1,290,001 - $2,680,000                   39.494934872  5.303760328    7.45
market_value                                               0.000002878  0.000000425    6.78
log_annual_income                                         -2.108088299  0.216390684   -9.74
credit_line_age                                           -0.004762600  0.002178980   -2.19
market_value_cat$1 - $910,600:log_annual_income           -0.236149537  0.296003091   -0.80
market_value_cat$910,601 - $1,290,000:log_annual_income   -1.079128230  0.345081624   -3.13
market_value_cat$1,290,001 - $2,680,000:log_annual_income -3.370458153  0.466557789   -7.22
                                                          Pr(>|z|)    
(Intercept)                                                < 2e-16 ***
marital1                                                    0.0396 *  
bankruptcy1                                                 0.3568    
bankruptcyunknown                                          5.1e-05 ***
market_value_cat$1 - $910,600                               0.3817    
market_value_cat$910,601 - $1,290,000                       0.0018 ** 
market_value_cat$1,290,001 - $2,680,000                    9.6e-14 ***
market_value                                               1.2e-11 ***
log_annual_income                                          < 2e-16 ***
credit_line_age                                             0.0288 *  
market_value_cat$1 - $910,600:log_annual_income             0.4250    
market_value_cat$910,601 - $1,290,000:log_annual_income     0.0018 ** 
market_value_cat$1,290,001 - $2,680,000:log_annual_income  5.0e-13 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 4140.3  on 4349  degrees of freedom
Residual deviance: 2901.1  on 4337  degrees of freedom
AIC: 2927

Number of Fisher Scoring iterations: 6
```

