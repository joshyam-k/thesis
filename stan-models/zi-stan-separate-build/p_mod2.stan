data {
  int<lower=0> n;   // number of data items
  int<lower=1> p;   // number of predictors
  int<lower = 1> j; // number of random effects
  int<lower = 1, upper = j> rfid[n]; // random effect id vector
  matrix[n, p] x;   // predictor matrix
  int z[n];      // outcome vector
}
parameters {
  vector[p] gamma;       // coefficients for predictors
  real<lower = 0> tau_u; // random effect sd
  vector[j] v; // rf vector
}
model {
  vector[n] mu;
  
  //priors go here

  mu = x*gamma + v[rfid];
  z ~ bernoulli_logit(mu);
}