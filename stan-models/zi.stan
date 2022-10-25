data {
  int<lower = 0> n; // number of data points
  int<lower = 1> p; // number of predictors
  int<lower = 1> j; // number of random effects
  int<lower = 1, upper = j> rfid[n]; // random effect id vector
  matrix[n, p] x; // predictor matrix
  vector[n] y; // outcome vector
  int pr[n]; // log reg outcome vector
  real<lower = 0> tau2; // error sd for logistic model (set to a small number)
}
parameters {
  vector[p] beta; // for normal model
  vector[p] gamma; // for logistic model
  real<lower = 0> sigma_u_y; // random effect sd for normal model
  real<lower = 0> sigma_u_p; // random effect sd for logistic model
  real<lower = 0> tau1; // error sd for normal model
  vector[j] u_y; // rf for normal model
  vector[j] u_p; // rf for logistic model
}
transformed parameters {
  vector[n] mu_y;
  vector[n] mu_p;
  for (i in 1:n)
    mu_y[i] = u_y[rfid[i]] + x[i]*beta; 
  for (i in 1:n)
    mu_p[i] = u_p[rfid[i]] + x[i]*gamma;
}
model {
  for(i in 1:n) {
    u_p[i] ~ normal(0, sigma_u_p);
    u_y[i] ~ normal(0, sigma_u_y);
    pr[i] ~ bernoulli_logit(mu_p[i]);
    y[i] ~ normal(pr[i]*mu_y[i], pr[i]*tau1 + (1 - pr[i])*tau2);
  }  
}


