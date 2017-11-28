// Stan Model for Buds analysis - first stab, use buds as reps and have one level of hierarchy
// 1st is individual
// Based off Danf's and Lizzie's stan models for chilling experiment
// For Beta Distribution model in Percent Budburst

data {
  int<lower=0> N; 
  int<lower=0, upper=1> perc[N]; 
  
}

parameters {
  real<lower=0, upper=1> tx; 

}

model {
  tx ~ beta(1,1);
  for (i in 1:N)
  perc[i] ~ beta(tx);
  
}



