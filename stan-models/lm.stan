data {
  int<lower=0> n;   // number of data items
  int<lower=1> p;   // number of predictors
  matrix[n, p] x;   // predictor matrix
  vector[n] y;      // outcome vector
}
parameters {
  vector[p] beta;       // coefficients for predictors
  real<lower=0> sigma;  // error scale
}
model {
  y ~ normal(x * beta, sigma);  // likelihood
}