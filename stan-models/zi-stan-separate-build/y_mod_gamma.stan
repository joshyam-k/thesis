data {
  int<lower=0> n;   // number of data items
  int<lower=1> p;   // number of predictors
  matrix[n, p + 1] x;   // predictor matrix (need to include a column for the intercept term)
  vector[n] y;      // outcome vector
}
parameters {
  vector[p + 1] betas;       // coefficients for predictors
  real<lower = 0> alpha;       // shape parameter
}
model {
  vector[n] mu;
  alpha ~ gamma(0.01, 0.01);
  
  for(i in 1:(p + 1)){
    betas[i] ~ cauchy(0, 10);
  }
  mu = exp(x*betas);
  y ~ gamma(alpha, mu/alpha);
}
