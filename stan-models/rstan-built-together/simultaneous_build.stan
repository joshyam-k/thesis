// this code fits a two-stage model with AOI level random effects 
data {
  int<lower=0> n; // number of observations (eg plots)
  int<lower=0> j; // number of AOIs (for random effects)
  int<lower=0> p; // number of predictors in models
  vector[n] y; // response vector for Y model
  matrix[n, p+1] x; // design matrix for Z model (needs intercept)
  int z[n]; // response vector for Z model
  real<lower=0> tau_2; // variance of Y model when Z = 0 
  int rfid[n]; // index for area of interest (must be consecutive integers starting at 1)
}

parameters {
  vector[p+1] beta;    // coefficients for linear model
  vector[p+1] eta;     // coefficients for logistic regression
  real<lower=0> alpha; // shape parameter
  vector[j] u;         // random effect vector for linear model
  vector[j] v;         // random effect vector for logistic regression
  real<lower=0> sigma_u; // sd for random effect of linear model
  real<lower=0> sigma_v; // sd for random effect of logistic regression
}

model {
  sigma_u ~ exponential(0.01);
  sigma_v ~ exponential(0.01);
  alpha ~ gamma(0.01, 0.01);
  
  for (i in 1:j) {
    u[i] ~ normal(0, sigma_u);
    v[i] ~ normal(0, sigma_v);
  }
  
  for (i in 1:n) {
    z[i] ~ bernoulli_logit(x[i]*eta + v[rfid[i]]);
    y[i] ~ gamma(alpha, alpha/(z[i]*exp(x[i]*beta + u[rfid[i]]) + (1-z[i])*tau_2));
  }
}
