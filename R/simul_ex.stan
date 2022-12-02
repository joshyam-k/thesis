data {
  int<lower=0> n; // number of observations (eg plots)
  int<lower=0> p; // number of predictors in models
  vector[n] y; // response vector for Y model
  matrix[n, p+1] x; // design matrix for Z model (needs intercept)
  int z[n]; // response vector for Z model
  real<lower=0> tau_2; // variance of Y model when Z = 0 
}

parameters {
  vector[p+1] beta;    // coefficients for linear model
  vector[p+1] gamma;     // coefficients for logistic regression
  real<lower=0> tau_1;
}
model {
  tau_1 ~ exponential(0.1);
  
  for (i in 1:(p+1)){
    beta[i] ~ normal(0, 1000);
    gamma[i] ~ normal(0, 1000);
  }

  
  for (i in 1:n) {
    z[i] ~ bernoulli_logit(x[i] * gamma );
    y[i] ~ normal(z[i]*(x[i] * beta),
                  z[i]*tau_1 + (1 - z[i])*tau_2);
  }
}
