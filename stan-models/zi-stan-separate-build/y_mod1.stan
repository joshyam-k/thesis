data {
  int<lower=0> n;   // number of data items
  int<lower=1> p;   // number of predictors
  int<lower = 1> j; // number of areas of interest
  int<lower = 1, upper = j> rfid[n]; // random effect id vector
  matrix[n, p + 1] x;   // predictor matrix (need to include a column for the intercept term)
  vector[n] y;      // outcome vector
}
parameters {
  vector[p + 1] beta;       // coefficients for predictors
  real<lower = 0> sigma_e;  // model error sd
  real<lower = 0> sigma_u; // random effect sd
  vector[j] u; // rf vector
}
model {
  vector[n] mu;
  // hyper priors
  sigma_e ~ exponential(0.1);
  sigma_u ~ exponential(0.1);
  
  // adding priors to fixed effects
  for(i in 1:(p + 1)){
    beta[i] ~ normal(0, 1000);
  }
  
  // model building
  for(grp in 1:j){
    u[grp] ~ normal(0, sigma_u);
  }
  
  // vectorized since
  // x is a n x (p + 1) matrix and beta is a (p + 1) vector
  // so x*beta gives a n-vector of predictions
  // next, rfid is an n-vector  
  // taking u[rfid] returns an n-vector where each entry
  // is the random effect associated with that data point 
  
  mu = x*beta + u[rfid];
  y ~ normal(mu, sigma_e);  
  
}