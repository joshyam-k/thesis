data {
  int<lower=0> n;   // number of data items
  int<lower=1> p;   // number of predictors
  int<lower = 1> j; // number of areas of interest
  int<lower = 1, upper = j> rfid[n]; // random effect id vector
  matrix[n, p + 1] x;   // predictor matrix (need to include a column for the intercept term)
  vector[n] y;      // outcome vector
}
parameters {
  vector[p + 1] betas;       // coefficients for predictors
  real<lower = 0> alpha;       // shape parameter
  // real<lower = 0> sigma_u; // random effect sd
  // vector[j] u; 
}
transformed parameters {
  vector[n] mu;
  mu = exp(x*betas);
}
model {
  alpha ~ gamma(0.01, 0.01);
  
  for(i in 1:(p + 1)){
    betas[i] ~ cauchy(0, 10);
  }
  y ~ gamma(mu, alpha);
}
