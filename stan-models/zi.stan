data {
  int<lower = 0> n; // number of data points
  int<lower = 1> p; // number of predictors
  int<lower = 1> j; // number of random effects
  int<lower = 1, upper = j> rfid[n]; // random effect id vector
  matrix[n, p] x; // predictor matrix
  vector[n] y; // outcome vector
}
parameters {
  vector[p] beta;
  real<lower = 0> sigma_u; // random effect sd
  real<lower = 0> sigma_e; // error sd
  vector[j] u;
}
transformed parameters {
  vector[n] mu;
  for (i in 1:n)
    mu[i] = u[rfid[i]] + x[i]*beta; //
}
model {
  u ~ normal(0, sigma_u);
  y ~ normal(mu, sigma_e);
}


