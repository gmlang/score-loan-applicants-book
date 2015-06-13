## Cross Validation

We use the method of 10-fold Cross Validation on the training set to choose the threshold that leads to ~80% TPR and ~80% Accuracy.

A>
```r
# define a function that outputs ids for each of the k folds.
get_folds = function(k, dat) sample(rep(1:k, length=nrow(dat)))
# set seed
set.seed(292303)
K = 10 # number of folds
folds = get_folds(K, dat_train) # get ids for each of the K folds.
# initialization
cutoffs = seq(0, 1, 0.01)
cv_opt_cutoff = rep(NA, K)
cv_tpr = rep(NA, K)
cv_accuracy = rep(NA, K)
# run 10-fold Cross Validation
for (i in 1:K) {
        # fit model using K-i folds and predict on the ith fold
        subtrain = dat_train[folds!=i, ]
        subtest  = dat_test[folds==i, ]
        fit = glm(fbest, data=subtrain, family=binomial)
        prob = predict(fit, subtest, type="response")
        # find optimal threshold for fold i
        target_tpr = target_accuracy = 0.8
        epsilon = 0.05
        opt_threshold = result_tpr = result_accuracy = c()
        for (threshold in cutoffs) {
                pred = ifelse(prob > threshold, 1, 0)
                tbl = mk_tbl(pred, subtest$bad)
                tpr = calc_tpr(tbl)
                accuracy = calc_accuracy(tbl)
                if (abs(tpr - target_tpr) < epsilon && 
                            abs(accuracy - target_accuracy) < epsilon) { 
                        result_tpr = c(result_tpr, tpr)
                        result_accuracy = c(result_accuracy, accuracy)
                        opt_threshold = c(opt_threshold, threshold)
                }
        }
        cv_opt_cutoff[i] = mean(opt_threshold)
        cv_tpr[i] = mean(result_tpr)
        cv_accuracy[i] = mean(result_accuracy)
}
```

We can look at the CV results.

A>
```r
print(cv_tpr)
```

A>{linenos=off}
```
 [1] 0.75439 0.81696 0.82353 0.78571 0.79365 0.79545 0.78889 0.79032 0.78205 0.78857
```

A>
```r
print(cv_accuracy)
```

A>{linenos=off}
```
 [1] 0.76389 0.78821 0.83874 0.75658 0.81379 0.75887 0.77562 0.82076 0.76423 0.78799
```

Note that each fold has an optimal CV cutoff. 

A>
```r
print(cv_opt_cutoff)
```

A>{linenos=off}
```
 [1] 0.220 0.205 0.255 0.150 0.210 0.180 0.225 0.230 0.200 0.190
```

We'll take the average of these optimal CV cutoffs as the optimal threshold.

A>
```r
opt_threshold = mean(cv_opt_cutoff)
print(opt_threshold)
```

A>{linenos=off}
```
[1] 0.2065
```

Using this optimal threshold, we can re-calculate the Accuracy, TPR, FPR, Precision, and F-measure on the test set.

A>
```r
prob = predict(bestfit, dat_test, type="response")
pred = ifelse(prob > opt_threshold, 1, 0)
tbl_logit = table(pred, truth = dat_test$bad)
data.frame(Accuracy = print_format(calc_accuracy(tbl_logit)),
           TPR = print_format(calc_tpr(tbl_logit)),
           FPR = print_format(calc_fpr(tbl_logit)),
           Precision = print_format(calc_precision(tbl_logit)),
           F = print_format(calc_F(tbl_logit)))
```

A>{linenos=off}
```
  Accuracy    TPR   FPR Precision      F
1   78.72% 78.83% 21.3%    44.42% 56.82%
```

We see that at the threshold value of 0.2065, we achieve a True Positive Rate of 78.83% and an Accuracy of 78.72% at the cost of a 21.3% False Positive Rate. This says that out of 100 good customers, we can expect the model to correctly identify 79 of them. And out of 100 bad customers, we can expect the model to erroneously identify 21 of them. What's the impact on our profit and loss if we are to use this model to score loan applicants? Recall that the loss caused by a bad customer is 5 times the profit we make from a good customer. Suppose the profit per good customer is $1, then the expected profit we'll make using the model is 0.787 * $1 = $0.787, and the expected loss we'll suffer is 0.212 * $5 = $1.06. Suppose the percent of bad customers in the population is Pb, we can then expect the profit/loss to be `0.787(1 – Pb) - 1.06Pb = 0.787 - 1.847Pb`. Now our sample has 18% bad customers. Suppose 18% is a good estimate of Pb, plug it in and we get $0.45, which is not bad.
