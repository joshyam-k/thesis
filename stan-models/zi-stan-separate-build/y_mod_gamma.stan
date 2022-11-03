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
  real<lower = 0> phi;  // variance parameter
  real<lower = 0> sigma_u; // random effect sd
  vector[j] u;
}
model {
  vector[n] mu;
  vector[n] alpha;
  vector[n] beta;
  
  sigma_u ~ exponential(1);
  phi ~ exponential(1);
  
  for(i in 1:(p + 1)){
    betas[i] ~ cauchy(0, 10);
  }
  
  mu = exp(x*betas + u[rfid]);
  alpha = mu .* mu/phi;
  beta = mu/phi;
  
  y ~ gamma(alpha, beta);
}
