data {
  int<lower=0> n;//sample size
  int<lower=0> q;//#variables
  real y[n]; //continuous outcome
  int<lower=0> age[n];//individual input
  int<lower=0> eth[n];
  int<lower=0> edu[n];
  int<lower=0> age_eth[n];//two-way interaction
  int<lower=0> age_edu[n];
  int<lower=0> eth_edu[n];
  int<lower=0> age_eth_edu[n];//three-way interaction
  int<lower=0> J_age; //levels
  int<lower=0> J_eth;
  int<lower=0> J_edu;
  int<lower=0> J_age_eth;
  int<lower=0> J_age_edu;
  int<lower=0> J_eth_edu;
  int<lower=0> J_age_eth_edu;
}

parameters {
  real alpha; //intercept
  vector[J_age] alpha_age;
  vector[J_eth] alpha_eth;
  vector[J_edu] alpha_edu;
  vector[J_age_eth] alpha_age_eth;
  vector[J_age_edu] alpha_age_edu;
  vector[J_eth_edu] alpha_eth_edu;
  vector[J_age_eth_edu] alpha_age_eth_edu;

  real<lower=0> lambda_m;
  //real log_sigma2_m[q];
  real<lower=0> sigma_m[q];
  real<lower=0> lambda_inter[2];
  real<lower=0> sigma_y;

}
//transformed parameters{
//  real<lower=0> sigma_m[q];
//  for (q0 in 1:q)
//    sigma_m[q0] <- sqrt(exp(log_sigma2_m[q0]));//Jeffrey's prior, log-transformation is uniform
//}
       
model {
  vector[n] alpha_v_age;
  vector[n] alpha_v_eth;
  vector[n] alpha_v_edu;
  vector[n] alpha_v_age_eth;
  vector[n] alpha_v_age_edu;  
  vector[n] alpha_v_eth_edu;
  vector[n] alpha_v_age_eth_edu;


  alpha_age ~ normal(0,1);
  alpha_eth ~ normal(0,1);
  alpha_edu ~ normal(0,1);
  alpha_age_eth ~ normal(0,1);
  alpha_age_edu ~ normal(0,1);
  alpha_eth_edu ~ normal(0,1);
  alpha_age_eth_edu ~ normal(0,1);

  lambda_m ~ cauchy(0,1);
  sigma_m ~ cauchy(0,1);
  sigma_y ~ cauchy(0,1);
  lambda_inter ~ cauchy(0,1);

for (i in 1:n) {
  alpha_v_age[i] <- alpha_age[age[i]];
  alpha_v_eth[i] <- alpha_eth[eth[i]];
  alpha_v_edu[i] <- alpha_edu[edu[i]];
  alpha_v_age_eth[i] <- alpha_age_eth[age_eth[i]];
  alpha_v_age_edu[i] <- alpha_age_edu[age_edu[i]];  
  alpha_v_eth_edu[i] <- alpha_eth_edu[eth_edu[i]];
  alpha_v_age_eth_edu[i] <- alpha_age_eth_edu[age_eth_edu[i]];
}

y ~ normal(alpha +
	   alpha_v_age * lambda_m * sigma_m[1] +	   
	   alpha_v_eth * lambda_m * sigma_m[2] +
	   alpha_v_edu * lambda_m * sigma_m[3] +
	   alpha_v_age_eth * lambda_inter[1] * sigma_m[1] * sigma_m[2] +
	   alpha_v_age_edu * lambda_inter[1] * sigma_m[1] * sigma_m[3] +    
	   alpha_v_eth_edu * lambda_inter[1] * sigma_m[2] * sigma_m[3] +
	   alpha_v_age_eth_edu * lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[3], sigma_y);
}
