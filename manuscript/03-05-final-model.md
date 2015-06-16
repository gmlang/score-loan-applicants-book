## Final Model

To obtain a final model for use in production, we take the best model formula found by the best subsets method, , and fit a logit model using the whole data set available to us. When in production, we just send new data as input to the model, and the model will output predicted probabilities indicating the likelihood of a bad customer. We'll use 0.2065, the optimal threshold discovered by 10-fold Cross Validation to make classifications in that if the predicted probability is greater than 0.2065, we'll classify the applicant as "bad"; otherwise, we'll classify it as "good".

A>
```r
finalfit = glm(fbest, data=upl, family=binomial)
round(coef(finalfit), 3)
```

{title = "Table 5: Coefficient Estimates of Model in Production", width = "wide"}
|                                             Predictor   |Coef Est|
|---------------------------------------------------------|--------|
|                                              (Intercept)| 24.002 |
|                                                 marital1| -0.190 |
|                                              bankruptcy1|  4.563 |
|                                        bankruptcyunknown|  4.087 |
|                            market_value_cat$1 - $910,600| -2.266 |
|                    market_value_cat$910,601 - $1,290,000| 11.241 |
|                  market_value_cat$1,290,001 - $2,680,000| 26.821 |
|                                        log_annual_income| -2.357 |
|                                          credit_line_age| -0.004 |
|                            bankruptcy1:log_annual_income| -0.423 |
|                      bankruptcyunknown:log_annual_income| -0.272 |
|          market_value_cat$1 - $910,600:log_annual_income|  0.425 |
|  market_value_cat$910,601 - $1,290,000:log_annual_income| -0.701 |
|market_value_cat$1,290,001 - $2,680,000:log_annual_income| -1.939 |                                          
|---------------------------------------------------------|--------|


To see if this model makes sense, we need to look at the Odds Ratios (OR), which are listed in the following table. 

A>
```r
odds_ratio = exp(coef(finalfit)[-1])
round(odds_ratio, 3)
```

{title = "Table 6: Odds Ratios", width = "wide"}
|                                             Predictor   |Odds Ratio|
|---------------------------------------------------------|----------|
|                                                 marital1|  0.827   |
|                                              bankruptcy1| 95.834   |
|                                        bankruptcyunknown| 59.583   | 
|                            market_value_cat$1 - $910,600|  0.104   |
|                    market_value_cat$910,601 - $1,290,000|76181.645 | 
|                  market_value_cat$1,290,001 - $2,680,000|  4.448e11| 
|                                        log_annual_income|  0.095   |
|                                          credit_line_age|  0.996   |
|                            bankruptcy1:log_annual_income|  0.655   |
|                      bankruptcyunknown:log_annual_income|  0.762   |
|          market_value_cat$1 - $910,600:log_annual_income|  1.529   |
|  market_value_cat$910,601 - $1,290,000:log_annual_income|  0.496   |
|market_value_cat$1,290,001 - $2,680,000:log_annual_income|  0.144   |
|---------------------------------------------------------|----------|


The odds of becoming a bad customer for an applicant who's married is 17.3% lower than someone who's single. This makes sense because married people on average tend to have more stable jobs and higher income, hence they are more likely to make loan payments on time.

The odds of becoming a bad customer for an applicant who has declared bankruptcy is 94.8 times higher than someone who's solvant. Our model strongly echos common sense here. Be wary when lending money to people who's bankrupted. When an applicant doesn't tell us his or her bankruptcy info, we should try our best to find out because the odds of becoming a bad customer for someone with bankruptcy info missing is still 58.6 higher than someone who's solvant. A possible explanation is that these people are in the process of filing bankruptcy or they are experiencing financial distress, otherwise, they wouldn't need to hide this piece of information.

The odds of becoming a bad customer for an applicant who owns property with market value of below $910,600 is 89.6% lower than someone without any property. However, as market value increases, the odds of becoming a bad customer increases dramatically. This also makes a lot of sense. People who don't own any property are less affluent on average, and hence it's more likely for them to miss payments. People with properties that worth a lot of money tend to have more debt obligations and more expensive life style on average. As a result, it's very easy for them to miss a payment or two if they don't manage their cash flow carefully.  

If the annual income of an applicant increase by 1.7 times, while keeping all other predictors fixed, we expect to see a 90.5% drop in the odds of being a bad customer. This 90.5% decrease does not depend on the value that annual income is held at. This makes sense because the more income a person makes, the eaiser it is for him to make on time payments. Plus, people with higher incomes tend to be financially savy and will likely try to avoid any damage to their credit scores by making timely payments.

Finally, for a 1-month increase in the age of longest credit line, while keeping the other predictors fixed, we expect to see a 0.4% drop in the odds of being a bad customer. This translates to an annual decrease of 4.5%.

We've interpreted the main effects above. Now, it's your turn to interpret the interaction effects along with the main effects. Do this as an exercise to see if you really understand the logit model. To understand more about odds ratios, you can check out this [website](http://www.ats.ucla.edu/stat/mult_pkg/faq/general/odds_ratio.htm).
