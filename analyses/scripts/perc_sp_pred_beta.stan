// Stan Model for Buds analysis - first stab, use buds as reps and have one level of hierarchy
// 1st is individual
// Based off Danf's and Lizzie's stan models for chilling experiment
// For Beta Distribution model in Percent Budburst

data {
  int<lower=0> N;
  int<lower=0> n_sp;
  int<lower=0.001, upper=1> sp[N];
  vector[N] perc;
  vector[N] tx;
  vector[N] sp;
  
}

parameters {
  vector[n_sp] a_sp;
  vector[n_sp] b_tx;

  real mu_a; 
  real mu_b_tx;

  real<lower=0> sigma_b_tx;

  real<lower=0> sigma_a;
    
  real<lower=0> sigma_y; 
  
}


transformed parameters { 
		vector[N] y_hat;
		
	for(i in 1:N){
		y_hat[i] = a_sp[sp[i]] + 
		b_tx[sp[i]] * tx[i] 
		;
				
		}
	
}

model {
	// Priors. Make them flat
	mu_b_tx ~ beta_binomial(0, 1); 
	
	sigma_b_tx ~ beta_binomial(0, 1);

	a_sp ~ beta_binomial(mu_a, sigma_a);  
	
	b_tx ~ beta_binomial(mu_b_tx, sigma_b_tx);
	
	perc ~ beta_binomial(y_hat, sigma_y);

}

