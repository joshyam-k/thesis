data {
  int<lower=0> n;   // number of data items
  int<lower=1> p;   // number of predictors
  int<lower = 1> j; // number of random effects
  int<lower = 1, upper = j> rfid[n]; // random effect id vector
  matrix[n, p + 1] x;   // predictor matrix
  int z[n];      // outcome vector
}
parameters {
  vector[p + 1] gamma;       // coefficients for predictors
  real<lower = 0> tau_u; // random effect sd
  vector[j] v; // rf vector
}
model {
  vector[n] mu;
  
  // phyper rior
  tau_u ~ exponential(1);
  
  // fixed effect prior
  for(i in 1:(p + 1)){
    gamma[i] ~ normal(0, 10000);
  }
  
  for(grp in 1:j){
    v[grp] ~ normal(0, tau_u);
  }
  
  // all vectorized
  mu = x*gamma + v[rfid];
  z ~ bernoulli_logit(mu);
}