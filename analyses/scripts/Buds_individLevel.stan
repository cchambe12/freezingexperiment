// Cat's buds analysis
// Looking at Duration of Vegetative risk at the bud level
// Started 27 September 2017
// 2 level model for duration of vegetative risk by bud number and frost treatment at the individual level
// Level: Individual on INTERCEPTS and SLOPES

data {
	int<lower=1> N;
	int<lower=1> n_ind;
	int<lower=1, upper=n_ind> ind[N];
	vector[N] y; 		// response
	vector[N] tx; 	// predictor
	vector[N] bud; 	// predictor
		
	}

parameters {
  real mu_a_ind;   
  real mu_b_tx_ind;   
  real mu_b_bud_ind;
  real<lower=0> sigma_a_ind; 
  real<lower=0> sigma_b_tx_ind; 
  real<lower=0> sigma_b_bud_ind;
  real<lower=0> sigma_y; 

  real a_ind[n_ind]; // intercept for species
  real b_tx[n_ind]; // slope of tx effect 
  real b_bud[n_ind]; // slope of bud effect
	}

transformed parameters {
   real yhat[N];
       	for(i in 1:N){
            yhat[i] = a_ind[ind[i]] + // indexed with species
		b_tx[ind[i]] * tx[i] + 
	      	b_bud[ind[i]] * bud[i]; 
			     	}

	}

model {

	a_ind ~ normal(mu_a_ind, sigma_a_ind); 
	b_tx ~ normal(mu_b_tx_ind, sigma_b_tx_ind); 
	b_bud ~ normal(mu_b_bud_ind, sigma_b_bud_ind);

        mu_b_tx_ind ~ normal(0, 30);
        sigma_b_tx_ind ~ normal(0, 10);
        mu_b_bud_ind ~ normal(0, 30);
        sigma_b_bud_ind ~ normal(0, 10);
	//b_force ~ normal(0, 10);
	//b_photo ~ normal(0, 10);
	//b_chill ~ normal(0, 30);
	
	y ~ normal(yhat, sigma_y);

}
