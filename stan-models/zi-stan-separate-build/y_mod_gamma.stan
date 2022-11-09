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
  real<lower = 0> sigma_u; // random effect sd
  vector[j] u; // rf vector
}
model {
  vector[n] mu;
  alpha ~ gamma(0.01, 0.01);
  sigma_u ~ exponential(1);
  
  for(grp in 1:j){
    u[grp] ~ normal(0, sigma_u);
  }
  
  for(i in 1:(p + 1)){
    betas[i] ~ cauchy(0, 10);
  }
  
  mu = exp(x*betas + u[rfid]);
  
  for(i in 1:n)
    y[i] ~ gamma(alpha, (alpha / mu[i])); 
}
