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
  vector[p+1] beta;
  vector[p+1] eta;
  real<lower=0> alpha; // shape parameter
  vector[j] u;
  vector[j] v;
  real<lower=0> sigma_u;
  real<lower=0> sigma_v;
}

model {
  vector[n] mu_y;
  vector[n] mu_p;
  sigma_u ~ normal(0, 1000);
  sigma_v ~ normal(0, 1000);
  
  for (i in 1:j) {
    beta[i] ~ normal(0, sigma_u);
    eta[i] ~ normal(0, sigma_v);
  }
  
  mu_p = x*eta + v[rfid];
  z ~ bernoulli_logit(mu_p);
  
  mu_y = exp(x*beta + u[rfid]);

  for (i in 1:n) {
    y[i] ~ gamma(alpha, alpha/(z[i]*mu_y[i] + (1-z[i])*tau_2));
  }
}
