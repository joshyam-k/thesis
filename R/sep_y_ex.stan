data {
  int<lower=0> n;   // number of data items
  int<lower=1> p;   // number of predictors
  matrix[n, p + 1] x;   // predictor matrix (need to include a column for the intercept term)
  vector[n] y;      // outcome vector
}
parameters {
  vector[p + 1] beta;       // coefficients for predictors
  real<lower = 0> sigma_e;  // model error sd
}
model {
  vector[n] mu;
  // hyper priors
  sigma_e ~ exponential(0.1);
  
  // adding priors to fixed effects
  for(i in 1:(p + 1)){
    beta[i] ~ normal(0, 1000);
  }
  
  mu = x*beta;
  y ~ normal(mu, sigma_e);  
  
}