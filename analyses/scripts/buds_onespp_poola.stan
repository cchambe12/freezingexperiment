// Stan Model for Buds analysis - first stab, use buds as reps and have two levels of hierarchy
// 1st is individual and 2nd is species
// Based off Danf's and Lizzie's stan models for chilling experiment

data {
  int<lower=0> N;
  int<lower=0> n_ind;
  int<lower=1, upper=n_ind> ind[N];
  vector[N] dvr;
  vector[N] tx;
}

parameters {
  vector[n_ind] a_ind;
  vector[n_ind] b_tx;

  real mu_a; 
  real mu_b_tx;

  real<lower=0> sigma_b_tx;

  real<lower=0> sigma_a;
    
  real<lower=0> sigma_y; 
  }


transformed parameters { // Vectorize: Won't save time probably here (no scalar x vector)
		vector[N] y_hat;
		
	for(i in 1:N){
		y_hat[i] = a_ind[ind[i]] + 
		b_tx[ind[i]] * tx[i] 
		;
				
		}
	
}

model {
	// Priors. Make them flat
	mu_b_tx ~ normal(0, 15); 
	
	sigma_b_tx ~ normal(0, 5);

	a_ind ~ normal(mu_a, sigma_a);  // SHOULD ADD PRIORS!
	
	b_tx ~ normal(mu_b_tx, sigma_b_tx);
	
	dvr ~ normal(y_hat, sigma_y);

}

