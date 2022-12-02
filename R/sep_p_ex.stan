data {
  int<lower=0> n;  
  int<lower=1> p;   
  matrix[n, p + 1] x;   
  int z[n];      
}
parameters {
  vector[p + 1] gamma;
}
model {
  vector[n] mu;

  for(i in 1:(p + 1)){
    gamma[i] ~ normal(0, 1000);
  }

  mu = x*gamma;
  z ~ bernoulli_logit(mu);
}
