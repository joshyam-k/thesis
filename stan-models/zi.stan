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
  vector[j] u; //vector of random effects
  real<lower = 0> sigma_u; // random effect sd
  real<lower = 0> sigma_e; // error sd
}
model {
  for(grp in 1:j){
    u[grp] ~ normal();
  }
  
  for(i in 1:n){
    y[i] ~ normal(x[i]*beta + u[], sigma_e);
  }
}

