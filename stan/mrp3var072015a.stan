data {
  int<lower=0> n;//sample size
  int<lower=0> J;//#cells
  int<lower=0> q;//#variables
  vector[J] N_cell;
  vector[J] n_cell;
  vector[J] y_cell; //continuous outcome
  real<lower=0> ss_cell; //sum of variance in cells
  int<lower=0> cell_str[J,q]; //variable values allocated to cells
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

  //real log_sigma2_m[q];
  real<lower=0> sigma_m[q];
  real<lower=0> lambda_inter[4];
  real<lower=0> sigma_y;

}
transformed parameters{
  vector[J] mu_cell;
  vector[J] sigma_y_cell;

 for (j in 1:J){
    mu_cell[j] <- alpha + alpha_age[cell_str[j,1]] * sigma_m[1] + 
      alpha_eth[cell_str[j,2]] * sigma_m[2] + 
      alpha_edu[cell_str[j,3]] * sigma_m[3] +
      alpha_age_eth[(cell_str[j,1]-1) * J_eth + cell_str[j,2]] * lambda_inter[1] * sigma_m[1] * sigma_m[2]+
      alpha_age_edu[(cell_str[j,1]-1) * J_edu + cell_str[j,3]] * lambda_inter[2] * sigma_m[1] * sigma_m[3]+
      alpha_eth_edu[(cell_str[j,2]-1) * J_edu + cell_str[j,3]] * lambda_inter[3] * sigma_m[2] * sigma_m[3]+
      alpha_age_eth_edu[(cell_str[j,1]-1) * J_eth * J_edu + (cell_str[j,2]-1) * J_edu + cell_str[j,3]] * lambda_inter[4] * sigma_m[1] * sigma_m[2] * sigma_m[3];
  }

  for (j in 1:J){
    sigma_y_cell[j] <- sigma_y / sqrt(n_cell[j]);
  }
//  real<lower=0> sigma_m[q];
//  for (q0 in 1:q)
//    sigma_m[q0] <- sqrt(exp(log_sigma2_m[q0]));//Jeffrey's prior, log-transformation is uniform
}
       
model {
  alpha_age ~ normal(0,1);
  alpha_eth ~ normal(0,1);
  alpha_edu ~ normal(0,1);
  alpha_age_eth ~ normal(0,1);
  alpha_age_edu ~ normal(0,1);
  alpha_eth_edu ~ normal(0,1);
  alpha_age_eth_edu ~ normal(0,1);

  sigma_m ~ cauchy(0,1);
  sigma_y ~ cauchy(0,1);
  lambda_inter ~ cauchy(0,1);
  
  y_cell ~ normal(mu_cell,sigma_y_cell);

  ss_cell / pow(sigma_y,2) ~ chi_square(n-1);
  increment_log_prob(log(2) - 3 * log(sigma_y));

}
generated quantities{
  vector[J] y_cell_new;
  vector[J] w_new;
  vector[J] ps_w;
  real pri_var;

  pri_var <- pow(sigma_m[1],2) + 
    pow(sigma_m[2],2) + 
    pow(sigma_m[3],2) + 
    pow(lambda_inter[1] * sigma_m[1] * sigma_m[2],2)+
    pow(lambda_inter[2] * sigma_m[1] * sigma_m[2],2) +
    pow(lambda_inter[3] * sigma_m[2] * sigma_m[3],2) +
    pow(lambda_inter[4] * sigma_m[1] * sigma_m[2] * sigma_m[3],2);


  for (j in 1:J){
    y_cell_new[j] <- normal_rng(mu_cell[j],sigma_y_cell[j]);
    ps_w[j] <- pow(sigma_y_cell[j],-2)/(pow(sigma_y_cell[j],-2) + 1/pri_var);
    w_new[j] <- ps_w[j] * N_cell[j]/sum(N_cell)/n_cell[j]*n + 1-ps_w[j];
  }



}