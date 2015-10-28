## Backtesting

How well will `bestfit` perform on the test set? To answer this question, we first need to decide on a performance measure. It turns out there're 6 common ones: Accuracy, True Positive Rate (TPR), False Positive Rate (FPR), Specificity, Precision and the F-measure. We'll calculate each of them on the test set. Before we do that, there's a technical detail we need to take care of. Because a logit model outputs probabilities instead of class labels, we need to choose a threshold and determine the class labels by comparing the predicted probabilities with the threshold. A common choice is 0.5. If a predicted probability is greater than 0.5, we'll label the applicant 1 (bad customer), and 0 (good customer) otherwise.

A>
```r
threshold = 0.5
```

Now, we can calculate those model performance measures on the test set. Let's define some terms first:

* TP = Number of True Positives
* FP = Number of False Positives
* P = Number of Positives = TP + FN
* TN = Number of True Negatives
* FN = Number of False Negatives
* N = Number of Negatives = TN + FP

where the word "positive" means "bad customer" in our case since "bad customer" is coded as 1, and "negative" means "good customer" since "good customer" is coded as 0.

### Accuracy

First, we calculate the **Accuracy** of our chosen logit model, defined as `Accuracy = (TP + TN) / (P + N)`. It measures the overall percent of correct classifications. 

A>
```r
prob = predict(bestfit, dat_test, type="response")
pred = ifelse(prob > threshold, 1, 0)
tbl_logit = table(pred, truth = dat_test$bad)
print(tbl_logit)
```

A>{linenos=off}
```
    truth
pred    0    1
   0 2296  331
   1   89  184
```

A>
```r
test_accuracy = sum(pred == dat_test$bad) / length(pred)
paste0(round(test_accuracy*100, 2), "%")
```

A>{linenos=off}
```
[1] "85.52%"
```

To see if our chosen logit model is any good, we need to compare its performance with the benchmark model, which labels every applicant as a good customer. We now calculate the Accuracy of the benchmark.

A>
```r
pred = rep(0, length(dat_test$bad))
tbl_bench = table(pred, truth = dat_test$bad)
tbl_bench = rbind(tbl_bench, c(0, 0))
row.names(tbl_bench) = c("0", "1")
print(tbl_bench)
```

A>{linenos=off}
```
     0   1
0 2385 515
1    0   0
```

A>
```r
test_accuracy = sum(pred == dat_test$bad) / length(pred)
paste0(round(test_accuracy*100, 2), "%")
```

A>{linenos=off}
```
[1] "82.24%"
```

We see our logit model chosen by the best subsets method beats the benchmark by about 3% in Accuracy.

### True Positive Rate (TPR) or Sensitivity or Recall

Next, we calculate the **True Positive Rate (TPR)**, which is also called **Sensitivity** or **Recall**. It is defined as `TPR = TP / P = TP / (TP + FN)`, so it measures the probability of predicting the loan applicant to be a bad customer, given that the applicant is a bad customer. The bigger the TPR, the better.

A>
```r
# calculate test TPR under the logit model
TP = tbl_logit["1", "1"]
FN = tbl_logit["0", "1"]
TPR = TP / (TP + FN)
paste0(round(TPR*100, 2), "%")
```

A>{linenos=off}
```
[1] "35.73%"
```

A>
```r
# calculate test TPR under the benchmark
TP = tbl_bench[2,2]
FN = tbl_bench[1,2]
TPR = TP / (TP + FN)
paste0(round(TPR*100, 2), "%")
```

A>{linenos=off}
```
[1] "0%"
```

We see our logit model is much better at detecting the truely bad customers than our benchmark, although in absolute terms, 36% of TPR is still too small since a bad customer is really costly. We want to increase the TPR, and we'll see how to do that in the next section by varying the threshold and creating something called ROC curve. But first of all, let's be more efficient with our code. If you look at the code above carefully, you'll see repetitions. We can write helper functions to avoid repeating ourselves.

A>
```r
# make confusion table (also called contingency table or error matrix)
mk_tbl = function(pred, truth) {
        # pred: a vector of the predicted class labels
        # truth: a vector of the true labels
        tbl = table(prediction = pred, truth = truth)
        rownames = row.names(tbl)
        if (length(rownames) == 1) {
                if (rownames == "0")
                        tbl = rbind(tbl, c(0, 0))                
                if (rownames == "1")
                        tbl = rbind(c(0, 0), tbl)
                row.names(tbl) = c("0", "1")
        }
        tbl
}
```

A>
```r
calc_accuracy = function(tbl) {
        # tbl: a confusion table with predictions along the rows and 
        #      truth along the columns
        sum(diag(tbl)) / sum(tbl)
}
```

A>
```r
calc_tpr = function(tbl) {
        # tbl: a confusion table with predictions along the rows and 
        #      truth along the columns
        TP = tbl["1", "1"]
        FN = tbl["0", "1"]
        TP / (TP + FN)
}
```

A>
```r
print_format = function(num) paste0(round(num*100, 2), "%")
```

Let's redo the calculations using these helper functions.

A>
```r
# get confusion table for benchmark
pred = rep(0, length(dat_test$bad))
tbl_bench = mk_tbl(pred, dat_test$bad)
tbl_bench
```

A>{linenos=off}
```
     0   1
0 2385 515
1    0   0
```

A>
```r
# get confusion table for logit model
prob = predict(bestfit, dat_test, type="response")
pred = ifelse(prob > threshold, 1, 0)
tbl_logit = mk_tbl(pred, dat_test$bad)
tbl_logit
```

A>{linenos=off}
```
          truth
prediction    0    1
         0 2296  331
         1   89  184
```

A>
```r
# calculate test accuracy for benchmark and logit model respectively
print_format(calc_accuracy(tbl_bench))
```

A>{linenos=off}
```
[1] "82.24%"
```

A>
```r
print_format(calc_accuracy(tbl_logit))
```

A>{linenos=off}
```
[1] "85.52%"
```

A>
```r
# calculate TPR for benchmark and logit model respectively
print_format(calc_tpr(tbl_bench))
```

A>{linenos=off}
```
[1] "0%"
```

A>
```r
print_format(calc_tpr(tbl_logit))
```

A>{linenos=off}
```
[1] "35.73%"
```

### False Positive Rate (FPR)

Next, we calculate the **False Positive Rate (FPR)**, defined as `FPR = FP / N = FP / (FP + TN)`. It measures the probability of predicting the applicant to be a bad customer, given that he or she is a good customer. The smaller the FPR, the better. 

A>
```r
calc_fpr = function(tbl) {
        # tbl: a confusion table with predictions along the rows and 
        #      truth along the columns
        FP = tbl["1", "0"]
        TN = tbl["0", "0"]
        FP / (FP + TN)
}
```

A>
```r
# calculate FPR for benchmark and logit model respectively
print_format(calc_fpr(tbl_bench))
```

A>{linenos=off}
```
[1] "0%"
```

A>
```r
print_format(calc_fpr(tbl_logit))
```

A>{linenos=off}
```
[1] "3.73%"
```

### Specificity or True Negative Rate (TNR)

It's also common to calculate something called **Specificity** by subtracting FPR from 1. Because `1 - FPR = TN / N`, Specificity is just the **True Negative Rate (TNR)**. It measures the probability of predicting the applicant to be a good customer, given that the applicant is a good customer.

### Precision or Positive Predictive Value

Next, we calculate the **Precision**, defined as `Precision = TP / (TP + FP)`. It's also called **Positive Predictive Value** because it measures how good the model is for detecting the positives (bad customers in our case). The bigger the Precision, the better.

A>
```r
calc_precision = function(tbl) {
        # tbl: a confusion table with predictions along the rows and 
        #      truth along the columns
        TP = tbl["1", "1"]
        FP = tbl["1", "0"]
        TP / (TP + FP)
}
```

A>
```r
# calculate Precision for benchmark and logit model respectively
print_format(calc_precision(tbl_bench))
```

A>{linenos=off}
```
[1] "NaN%"
```

A>
```r
print_format(calc_precision(tbl_logit))
```

A>{linenos=off}
```
[1] "67.4%"
```

### The F-measure

Next, we calculate the **F-measure**, defined as `F = 2 / (1/precision + 1/recall)`.

A>
```r
calc_F = function(tbl) {
        # tbl: a confusion table with predictions along the rows and 
        #      truth along the columns
        precision = calc_precision(tbl)
        recall = calc_tpr(tbl)
        2 / (1/precision + 1/recall)
}
```

A>
```r
# calculate Precision for benchmark and logit model respectively
print_format(calc_F(tbl_bench))
```

A>{linenos=off}
```
[1] "NaN%"
```

A>
```r
print_format(calc_F(tbl_logit))
```

A>{linenos=off}
```
[1] "46.7%"
```

