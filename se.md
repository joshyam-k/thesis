## Standard Errors

Big Question: how do we estimate the standard errors of the estimates for our small area parameters of interest?

Posterior Predictive Distribution
- Can't use standard errors of posterior predictive distribution for a small area of interest because this distribution strongly depends on how properly specified the model is.
- These should be considered predictive accuracy intervals and used as a way to check your models, but the standard errors of these posterior predictive distributions should not be treated as the standard error of the model for that small area of interest.
- Gelman says:  we can check a model by external validation using the model to make predictions about future data, and then collecting those data and comparing to their predictions. Posterior means should be correct on average, 50% intervals should contain the true values half the time, and so forth. 


