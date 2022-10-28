data {
  int<lower=0> n;   // number of data items
  int<lower=1> p;   // number of predictors
  int<lower = 1> j; // number of random effects
  int<lower = 1, upper = j> rfid[n]; // random effect id vector
  matrix[n, p] x;   // predictor matrix
  vector[n] y;      // outcome vector
}
parameters {
  vector[p] beta;       // coefficients for predictors
  real<lower=0> sigma_e;  // model error sd
  real<lower = 0> sigma_u; // random effect sd
  vector[j] u; // rf vector
}
model {
  vector[n] mu;
  
  //priors
  u ~ normal(0, sigma_u);

  mu = x*beta + u[rfid];
  y ~ normal(mu, sigma_e);  // likelihood
}