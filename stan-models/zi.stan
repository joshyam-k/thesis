data {
  int<lower=0> n; // number of data points
  int<lower=1> p; // number of predictors
  int<lower=1> j; // number of random effects
  matrix[n, p] x; // predictor matrix
  vector[n] y; // outcome vector
}
parameters {
  vector[3] beta; // fixed effects
  vector[j] u; //vector of random effects
  real<lower=0> sigma_u; // random effect sd
  real<lower=0> sigma_e; // error sd
}
model {
  y ~ normal(mu, sigma);
}

