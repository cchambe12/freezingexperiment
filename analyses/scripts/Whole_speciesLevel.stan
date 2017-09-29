// Cat's whole dvr analysis
// Looking at Duration of Vegetative risk at the species level
// Started 279September 2017
// 2 level model for duration of vegetative risk by bud number and frost treatment at the individual level
// Level: Individual on INTERCEPTS and SLOPES

data {
	int<lower=1> N;
	int<lower=1> n_sp;
	int<lower=1, upper=n_sp> sp[N];
	vector[N] y; 		// response
	vector[N] tx; 	// predictor
		
	}

parameters {
  real mu_a_sp;   
  real mu_b_tx_sp;
  real<lower=0> sigma_a_sp; 
  real<lower=0> sigma_b_tx_sp;
  real<lower=0> sigma_y; 

  real a_sp[n_sp]; // intercept for species
  real b_tx[n_sp]; // slope of tx effect
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_sp[sp[i]] + // indexed with species
		b_tx[sp[i]] * tx[i]; 
			     	}

	}

model {

	a_sp ~ normal(mu_a_sp, sigma_a_sp); 
	b_tx ~ normal(mu_b_tx_sp, sigma_b_tx_sp);

        mu_b_tx_sp ~ normal(0, 30);
        sigma_b_tx_sp ~ normal(0, 10);
	//b_force ~ normal(0, 10);
	//b_photo ~ normal(0, 10);
	//b_chill ~ normal(0, 30);
	
	y ~ normal(yhat, sigma_y);

}
