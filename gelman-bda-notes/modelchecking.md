## Posterior Predictive Checking

A basic technique for checking the fit of a model to data is to draw simulated values from the joint posterior predictive distribution and compare those samples to the observed data.

Bayes Rules! describes how to do this in more detail, essentially you
- for each MCMC parameter set (of which there are usually 20,000) we predict an outcome using one of our observed data points
- we do this process for every single observed data point
- if we had 500 data points this would give us 500 different predictions for each parameter set
- we could plot this posterior predictive distribution alongside the actual distribution (since we predicted on observed data) to do a posterior predictive check
- in other words, if our assumptions are correct, our posterior predictive model should be able to simulate data thats similar the observed response data
