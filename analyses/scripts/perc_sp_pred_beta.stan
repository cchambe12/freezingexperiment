// Stan Model for Buds analysis - first stab, use buds as reps and have one level of hierarchy
// 1st is individual
// Based off Danf's and Lizzie's stan models for chilling experiment
// For Beta Distribution model in Percent Budburst

data {
  int<lower=0> N;
  
  // Dependent variable
  vector[N] perc;
  vector[N] tx;
  vector[N] sp;
  
}

parameters {
  vector[N] b_tx;
  vector[N] b_sp;
  
  real mu_tx;
  real mu_sp;
  
  real sigma_b_tx;
  real sigma_b_sp;
  
  real sigma_y;

}

transformed parameters {
  vector[N] y_hat;
  
  for(i in 1:N){
    y_hat[i] = b_tx[i] * tx[i] +
    b_sp[i] * sp[i]
    ;
    
  }
}

model {
  mu_tx ~ normal(0, 1);
  mu_sp ~ normal(0, 1);
  
  sigma_b_tx ~ normal(0, 1);
  sigma_b_sp ~ normal(0, 1);
  
  b_tx ~ normal(mu_tx, sigma_b_tx);
  b_sp ~ normal(mu_sp, sigma_b_sp);
  
	perc ~ beta_binomial(b_sp, b_tx);
	  
}



