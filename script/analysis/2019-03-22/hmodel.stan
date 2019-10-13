// .stan file for hierarchical model 
data { // data block 
  int<lower=1> N;                       // sample size
  int<lower=1> K;                       // number of individual-level covars
  int<lower=1> R;  // number of county-year-level covars (excluding rainfall variables)
  int<lower=1> n_county_year;           // number of county-year levels
  row_vector[K] X[N];                // individual-level covariates
  row_vector[R] U[n_county_year];    // county-year level covariates
  vector[n_county_year] day7;           // rainfall on election day 
  vector[n_county_year] day6;           // rainfall on day before election
  vector[n_county_year] day3;           // rainfall on 5th day before election
  vector[n_county_year] day1;           // rainfall on week before election
  int county_year_id[N];                // vector of county-year id
  int y[N];                          // response vector
  
  // values to feed into priors
  real alpha0_mean;             // mean of alpha0
  real alpha0_var;              // variance of alpha0             
  real beta_mean;               // mean of beta
  real beta_var;                // variance of beta
  real a;                       // shape of tau
  real b;                       // scale of tau
  
  
}
parameters { // parameter block
  vector[K] beta;                         // coefficient for individual-level vars
  vector[R] gamma;                       // coefficient for year-level vars
  vector[n_county_year] alpha0; // mean conty-year level effect if gamma=0
  real tau;                     // variance of county-year level effect
  
  real rain1;                   // coefficient for day1
  real rain3;                   // coefficient for day3
  real rain6;                   // coefficient for day6
  real rain7;                   // coefficient for day7
}
transformed parameters{
  vector[N] mu;                 // linear predictor at individual level
  vector[n_county_year] alpha;  // linear predictor at county-year level
  
  for(j in 1:n_county_year){
          alpha[j] = U[j] * gamma + day1[j] * rain1 + day3[j] * rain3 + day6[j] * rain6 + day7[j] * rain7;   
  }
  for(i in 1:N){
          mu[i] = X[i] * beta + alpha[county_year_id[i]];
  }
}
model { // model block
  // priors
  rain1 ~ normal(0, 1);
  rain3 ~ normal(0, 1);
  rain6 ~ normal(0, 1);
  rain7 ~ normal(-0.833, 0.001);
  alpha0 ~ normal(alpha0_mean, alpha0_var);
  tau ~ inv_gamma(a, b);
  alpha ~ normal(alpha0, tau);
  for (k in 1:K){
          beta[k] ~ normal(beta_mean, beta_var);
  }
  
  
  // logit 
  for(i in 1:N){
          y[i] ~ bernoulli_logit(mu[i]);
  }
}
generated quantities {
  // log-likelihood of each obs
  vector[N] log_lik;
  // probability
  vector[N] pi;
  for (i in 1:N) {
    pi[i] = inv_logit(mu[i]);
    log_lik[i] = bernoulli_logit_lpmf(y[i] | mu[i]);
  }
}
