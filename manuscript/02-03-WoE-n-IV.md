## Weight of Evidence and Information Value

First, let's load the data we saved from the previous section.

A>
```r
proj_path = "~/score-loan-applicants"
data_path = file.path(proj_path, 'data')
file_path = file.path(data_path, 'cleaned-02.rda')
load(file_path)
```

Next, we write a function to calculate the Weight of Evidence (WoE) and Information Value (IV) of any predictor given a binary target. The WoE and IV, like the distribution plots in the last section, will help us identify potential predicitve or useless predcitors. You can read more about them and see how they are defined mathematically at this [website](http://documentation.statsoft.com/STATISTICAHelp.aspx?path=WeightofEvidence/WeightofEvidenceWoEIntroductoryOverview).

A>
```r
WoE_n_IV = function(dat, yvar, xvar) {
        # dat: data frame
        # yvar, xvar: strings
        woe = iv = NULL
        if (class(dat[[xvar]]) == "factor") {
                tbl = table(dat[[yvar]], dat[[xvar]])
                good = drop(tbl["0", ])
                bad = drop(tbl["1", ])
                # calculate percent of good and bad customers
                good_pct = good / sum(good)
                bad_pct = bad  / sum(bad)
                # calculate WOE and IV for each level
                woe = log(good_pct / bad_pct)
                iv = (good_pct - bad_pct) * woe                
        } 
        if (class(dat[[xvar]]) == "numeric") {
                # sort dat[[xvar]] 
                sorted_idx = order(dat[[xvar]])
                y = as.numeric(dat[[yvar]][sorted_idx])
                # split the continuous xvar into 10 bins
                ttl_num = length(dat[[xvar]])
                bin = 10
                n = ttl_num %/% bin
                # count the number of good and bad for each bin
                good = rep(0, bin)
                bad = rep(0, bin)
                for (i in 1:bin) { # calculate PSI for ith bin
                        if (i != bin) {
                                good[i] = sum(y[((i-1)*n+1):(n*i)])
                                bad[i] = n-good[i]
                        } else {
                                good[i] = sum(y[((i-1)*n+1):ttl_num])
                                bad[i] = ttl_num - n*(i-1) - good[i]
                        }
                }
                # calculate WOE and IV for each bin
                good_pct = good / sum(good)
                bad_pct = bad / sum(bad)
                woe = rep(0, bin)
                iv = rep(0, bin) 
                for (i in 1:bin) {
                        woe[i] = log(good_pct[i] / bad_pct[i])
                        iv[i] = (good_pct[i] - bad_pct[i]) * woe[i]
                }
        }
        # put results in a data frame and return it
        out = data.frame(WOE = woe, IV = iv)
        out = cbind(x=row.names(out), out)
        names(out)[1] = xvar
        rownames(out) = NULL
        out
}
```

Now, armed with this function, we can go ahead calculating the Weight of Evidence and Information Value for each predictor.

A>
```r
predictors = c(iv_cat_strong, iv_cat_weak, iv_cat_none, iv_con_strong,
               iv_con_weak, iv_con_none)
IV = rep(0, length(predictors))
for (i in 1:length(predictors)) {
        var = predictors[i]
        WOE_IV = WoE_n_IV(upl, "bad", var)
        print(kable(WOE_IV, row.names = FALSE, format = "pandoc", caption=var))
        cat("\n")        
        # sum up the IV components for each predictor
        IV[i] = sum(WOE_IV$IV)
}
```

A>{linenos=off}
```
Table: bankruptcy

bankruptcy         WOE        IV
-----------  ---------  --------
0              0.04255   0.00171
1              0.16323   0.00013
unknown       -0.81760   0.03344



Table: conviction

conviction         WOE        IV
-----------  ---------  --------
0              0.01313   0.00017
1             -0.71680   0.00923



Table: repossess

repossess         WOE        IV
----------  ---------  --------
0            -0.02053   0.00041
1             0.86782   0.01738



Table: own_property

own_property         WOE        IV
-------------  ---------  --------
0                1.49076   0.53989
1               -0.51879   0.18788



Table: late_repayments

late_repayments         WOE        IV
----------------  ---------  --------
0                   0.24901   0.04662
1                  -0.80750   0.15117



Table: market_value_cat

market_value_cat                WOE        IV
------------------------  ---------  --------
$0                          1.49076   0.53989
$1 - $910,600              -1.08447   0.22559
$910,601 - $1,290,000      -0.69860   0.12189
$1,290,001 - $2,680,000     0.08354   0.00170



Table: purpose

purpose         WOE        IV
--------  ---------  --------
0           0.03300   0.00064
1          -0.09876   0.00259
unknown     0.04602   0.00032



Table: marital

marital         WOE        IV
--------  ---------  --------
0          -0.09395   0.00368
1           0.06731   0.00264



Table: employment

employment         WOE        IV
-----------  ---------  --------
1             -0.01754   0.00013
2              0.11608   0.00124
3             -0.00006   0.00000
4              0.09158   0.00076
5              0.18044   0.00147
6             -0.12792   0.00254



Table: exist_customer

exist_customer         WOE        IV
---------------  ---------  --------
0                  0.02295   0.00035
1                 -0.04375   0.00066



Table: unspent_convictions

unspent_convictions         WOE        IV
--------------------  ---------  --------
0                       0.00194   0.00000
1                      -0.02299   0.00004



Table: log_debt_to_income

log_debt_to_income         WOE        IV
-------------------  ---------  --------
1                      1.34909   0.08811
2                      0.55342   0.02184
3                      0.50287   0.01855
4                      0.37796   0.01126
5                     -0.03099   0.00010
6                     -0.04327   0.00019
7                      0.06830   0.00045
8                     -0.13459   0.00199
9                     -0.32505   0.01342
10                    -0.66630   0.07614



Table: log_annual_income

log_annual_income         WOE        IV
------------------  ---------  --------
1                    -0.72043   0.09396
2                    -0.47547   0.03253
3                    -0.23882   0.00677
4                    -0.04327   0.00019
5                     0.08986   0.00076
6                     0.19104   0.00322
7                     0.29784   0.00733
8                     0.52776   0.02015
9                     1.25775   0.07970
10                    2.77344   0.22233



Table: credit_line_age

credit_line_age         WOE        IV
----------------  ---------  --------
1                  -0.71004   0.09031
2                  -0.33309   0.01418
3                  -0.26588   0.00857
4                   0.06830   0.00045
5                   0.34697   0.00966
6                   0.52776   0.02015
7                   0.42121   0.01363
8                   0.20786   0.00377
9                   0.45522   0.01561
10                  0.66467   0.02967



Table: market_value

market_value         WOE        IV
-------------  ---------  --------
1                1.12325   0.06755
2                1.38167   0.09113
3                1.41543   0.09427
4                1.05118   0.06118
5               -0.63825   0.06799
6               -0.51247   0.03903
7               -0.42235   0.02453
8               -0.30453   0.01159
9               -0.09039   0.00087
10               0.51522   0.01934



Table: credit_applications

credit_applications         WOE        IV
--------------------  ---------  --------
1                       0.25156   0.00538
2                       0.44372   0.01493
3                       0.35717   0.01017
4                       0.34697   0.00966
5                       0.21641   0.00407
6                       0.15846   0.00226
7                       0.22505   0.00438
8                      -0.13993   0.00216
9                      -0.34102   0.01496
10                     -0.63100   0.06599



Table: age

age         WOE        IV
----  ---------  --------
1       0.03370   0.00011
2      -0.07308   0.00056
3      -0.03099   0.00010
4      -0.01850   0.00003
5      -0.06721   0.00047
6       0.04735   0.00022
7       0.00065   0.00000
8       0.01369   0.00002
9       0.00065   0.00000
10      0.11204   0.00117
```

To judge a predictor's predictive power, people often use the following list as a guidance:

* IV < 0.02 => Useless for prediction
* 0.02 ~ 0.1 => Weak predictor
* 0.1 ~ 0.3 => Medium predictor
* 0.3 ~ 0.5 => Strong predictor
* IV > 0.5 => Suspicious or too good to be true

If we apply this list to our case, we get the following results.

A>
```r
IV = data.frame(predictor = predictors, IV=IV)
IV$predictive_power[IV$IV < 0.02] = "Useless"
IV$predictive_power[0.02 <= IV$IV & IV$IV < 0.1] = "Weak"
IV$predictive_power[0.1 <= IV$IV & IV$IV < 0.3] = "Medium"
IV$predictive_power[0.3 <= IV$IV & IV$IV < 0.5] = "Strong"
IV$predictive_power[0.5 <= IV$IV] = "Too good to be true"
kable(IV, row.names = FALSE, format = "pandoc", caption="Information Value")
```


A>{linenos=off}
```
Table: Information Value

predictor                   IV  predictive_power    
--------------------  --------  --------------------
bankruptcy             0.03528  Weak                
conviction             0.00940  Useless             
repossess              0.01779  Useless             
own_property           0.72777  Too good to be true 
late_repayments        0.19779  Medium              
market_value_cat       0.88906  Too good to be true 
purpose                0.00354  Useless             
marital                0.00632  Useless             
employment             0.00614  Useless             
exist_customer         0.00100  Useless             
unspent_convictions    0.00004  Useless             
log_debt_to_income     0.23204  Medium              
log_annual_income      0.46695  Strong              
credit_line_age        0.20600  Medium              
market_value           0.47746  Strong              
credit_applications    0.13396  Medium              
age                    0.00268  Useless             
```

We see that while many of the conclusions are consistence with what we got using the visual aids in the last section, there're some discrepancies. For example, the distribution plot showed bankruptcy is a strong predictor, but the IV says it's a weak predictor. Remember that neither distribution plots or IVs are definitively. Their purpose is to help us be smart about variable selection when we build models, which we'll start doing in the next section. For now, let's collect the predictors into different groups based on the potential predictive power suggested by the Information Values.

A>
```r
IV_none = as.character(with(IV, predictor[predictive_power=="Useless"]))
IV_weak = as.character(with(IV, predictor[predictive_power=="Weak"]))
IV_medium = as.character(with(IV, predictor[predictive_power=="Medium"]))
IV_strong = as.character(with(IV, predictor[predictive_power=="Strong"]))
IV_extra_strong = as.character(with(IV, predictor[predictive_power=="Too good to be true"]))
save(IV_none, IV_weak, IV_medium, IV_strong, IV_extra_strong, 
     file = file.path(data_path, 'cleaned-03.rda'))
```
