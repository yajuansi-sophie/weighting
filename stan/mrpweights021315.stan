data {
  int<lower=0> n;
  int<lower=0> q;
  //int<lower=0> k_grp;
  //int<lower=0> cell_id[n];
  //int<lower=0> J;
  real y[n]; 
  int<lower=0> age[n];
  int<lower=0> sex[n];
  int<lower=0> eth[n];
  int<lower=0> edu[n];
  int<lower=0> inc[n];
  int<lower=0> eldx[n];
  int<lower=0> childx[n];
  int<lower=0> wax[n];
  int<lower=0> age_sex[n];
  int<lower=0> age_eth[n];
  int<lower=0> age_edu[n];
  int<lower=0> age_inc[n];
  int<lower=0> age_eldx[n];
  int<lower=0> age_childx[n];
  int<lower=0> age_wax[n];  
  int<lower=0> sex_eth[n];
  int<lower=0> sex_edu[n];                  
  int<lower=0> sex_inc[n];
  int<lower=0> sex_eldx[n];
  int<lower=0> sex_childx[n];
  int<lower=0> sex_wax[n];
  int<lower=0> eth_edu[n];
  int<lower=0> eth_inc[n];
  int<lower=0> eth_eldx[n];
  int<lower=0> eth_childx[n];
  int<lower=0> eth_wax[n];
  int<lower=0> edu_inc[n];
  int<lower=0> edu_eldx[n];
  int<lower=0> edu_childx[n];
  int<lower=0> edu_wax[n];
  int<lower=0> inc_eldx[n];
  int<lower=0> inc_childx[n];
  int<lower=0> inc_wax[n];
  int<lower=0> eldx_childx[n];
  int<lower=0> eldx_wax[n];
  int<lower=0> childx_wax[n];
  int<lower=0> age_sex_eth[n];     
  int<lower=0> age_sex_edu[n];
  int<lower=0> age_sex_inc[n];
  int<lower=0> age_sex_eldx[n];
  int<lower=0> age_sex_childx[n];
  int<lower=0> age_sex_wax[n];
  int<lower=0> age_eth_edu[n];
  int<lower=0> age_eth_inc[n];
  int<lower=0> age_eth_eldx[n];
  int<lower=0> age_eth_childx[n];
  int<lower=0> age_eth_wax[n];
  int<lower=0> age_edu_inc[n];
  int<lower=0> age_edu_eldx[n];
  int<lower=0> age_edu_childx[n];
  int<lower=0> age_edu_wax[n];
  int<lower=0> age_inc_eldx[n];
  int<lower=0> age_inc_childx[n];
  int<lower=0> age_inc_wax[n];
  int<lower=0> age_eldx_childx[n];
  int<lower=0> age_eldx_wax[n];
  int<lower=0> age_childx_wax[n];
  int<lower=0> sex_eth_edu[n];
  int<lower=0> sex_eth_inc[n];
  int<lower=0> sex_eth_eldx[n];
  int<lower=0> sex_eth_childx[n];
  int<lower=0> sex_eth_wax[n];
  int<lower=0> sex_edu_inc[n];
  int<lower=0> sex_edu_eldx[n];
  int<lower=0> sex_edu_childx[n];
  int<lower=0> sex_edu_wax[n];
  int<lower=0> sex_inc_eldx[n];
  int<lower=0> sex_inc_childx[n];
  int<lower=0> sex_inc_wax[n];
  int<lower=0> sex_eldx_childx[n];
  int<lower=0> sex_eldx_wax[n];
  int<lower=0> sex_childx_wax[n];
  int<lower=0> eth_edu_inc[n];
  int<lower=0> eth_edu_eldx[n];
  int<lower=0> eth_edu_childx[n];
  int<lower=0> eth_edu_wax[n];
  int<lower=0> eth_inc_eldx[n];
  int<lower=0> eth_inc_childx[n];
  int<lower=0> eth_inc_wax[n];
  int<lower=0> eth_eldx_childx[n];
  int<lower=0> eth_eldx_wax[n];
  int<lower=0> eth_childx_wax[n];
  int<lower=0> edu_inc_eldx[n];
  int<lower=0> edu_inc_childx[n];
  int<lower=0> edu_inc_wax[n];
  int<lower=0> edu_eldx_childx[n];
  int<lower=0> edu_eldx_wax[n];
  int<lower=0> edu_childx_wax[n];
  int<lower=0> inc_eldx_childx[n];
  int<lower=0> inc_eldx_wax[n];
  int<lower=0> inc_childx_wax[n];
  int<lower=0> eldx_childx_wax[n];
  int<lower=0> J_age;
  int<lower=0> J_sex;
  int<lower=0> J_eth;
  int<lower=0> J_edu;
  int<lower=0> J_inc;
  int<lower=0> J_eldx;
  int<lower=0> J_childx;
  int<lower=0> J_wax;
  int<lower=0> J_age_sex;
  int<lower=0> J_age_eth;
  int<lower=0> J_age_edu;
  int<lower=0> J_age_inc;
  int<lower=0> J_age_eldx;
  int<lower=0> J_age_childx;
  int<lower=0> J_age_wax; 
  int<lower=0> J_sex_eth;
  int<lower=0> J_sex_edu;                 
  int<lower=0> J_sex_inc;
  int<lower=0> J_sex_eldx;
  int<lower=0> J_sex_childx;
  int<lower=0> J_sex_wax;
  int<lower=0> J_eth_edu;
  int<lower=0> J_eth_inc;
  int<lower=0> J_eth_eldx;
  int<lower=0> J_eth_childx;
  int<lower=0> J_eth_wax;
  int<lower=0> J_edu_inc;
  int<lower=0> J_edu_eldx;
  int<lower=0> J_edu_childx;
  int<lower=0> J_edu_wax;
  int<lower=0> J_inc_eldx;
  int<lower=0> J_inc_childx;
  int<lower=0> J_inc_wax;
  int<lower=0> J_eldx_childx;
  int<lower=0> J_eldx_wax;
  int<lower=0> J_childx_wax;
  int<lower=0> J_age_sex_eth;    
  int<lower=0> J_age_sex_edu;
  int<lower=0> J_age_sex_inc;
  int<lower=0> J_age_sex_eldx;
  int<lower=0> J_age_sex_childx;
  int<lower=0> J_age_sex_wax;
  int<lower=0> J_age_eth_edu;
  int<lower=0> J_age_eth_inc;
  int<lower=0> J_age_eth_eldx;
  int<lower=0> J_age_eth_childx;
  int<lower=0> J_age_eth_wax;
  int<lower=0> J_age_edu_inc;
  int<lower=0> J_age_edu_eldx;
  int<lower=0> J_age_edu_childx;
  int<lower=0> J_age_edu_wax;
  int<lower=0> J_age_inc_eldx;
  int<lower=0> J_age_inc_childx;
  int<lower=0> J_age_inc_wax;
  int<lower=0> J_age_eldx_childx;
  int<lower=0> J_age_eldx_wax;
  int<lower=0> J_age_childx_wax;
  int<lower=0> J_sex_eth_edu;
  int<lower=0> J_sex_eth_inc;
  int<lower=0> J_sex_eth_eldx;
  int<lower=0> J_sex_eth_childx;
  int<lower=0> J_sex_eth_wax;
  int<lower=0> J_sex_edu_inc;
  int<lower=0> J_sex_edu_eldx;
  int<lower=0> J_sex_edu_childx;
  int<lower=0> J_sex_edu_wax;
  int<lower=0> J_sex_inc_eldx;
  int<lower=0> J_sex_inc_childx;
  int<lower=0> J_sex_inc_wax;
  int<lower=0> J_sex_eldx_childx;
  int<lower=0> J_sex_eldx_wax;
  int<lower=0> J_sex_childx_wax;
  int<lower=0> J_eth_edu_inc;
  int<lower=0> J_eth_edu_eldx;
  int<lower=0> J_eth_edu_childx;
  int<lower=0> J_eth_edu_wax;
  int<lower=0> J_eth_inc_eldx;
  int<lower=0> J_eth_inc_childx;
  int<lower=0> J_eth_inc_wax;
  int<lower=0> J_eth_eldx_childx;
  int<lower=0> J_eth_eldx_wax;
  int<lower=0> J_eth_childx_wax;
  int<lower=0> J_edu_inc_eldx;
  int<lower=0> J_edu_inc_childx;
  int<lower=0> J_edu_inc_wax;
  int<lower=0> J_edu_eldx_childx;
  int<lower=0> J_edu_eldx_wax;
  int<lower=0> J_edu_childx_wax;
  int<lower=0> J_inc_eldx_childx;
  int<lower=0> J_inc_eldx_wax;
  int<lower=0> J_inc_childx_wax;
  int<lower=0> J_eldx_childx_wax;
  real<lower=0> lambda_m;
  real log_sigma2_m[q];
  real<lower=0> lambda_inter[2];
  real<lower=0> sigma_y;

}
transformed data{
  real<lower=0> sigma_m[q];
  for (q0 in 1:q)
    sigma_m[q0] <- sqrt(exp(log_sigma2_m[q0]));
}

parameters {
  real alpha; //intercept
  vector[J_age] alpha_age;
  vector[J_sex] alpha_sex;
  vector[J_eth] alpha_eth;
  vector[J_edu] alpha_edu;
  vector[J_inc] alpha_inc;
  vector[J_eldx] alpha_eldx;
  vector[J_childx] alpha_childx;
  vector[J_wax] alpha_wax;
  vector[J_age_sex] alpha_age_sex;
  vector[J_age_eth] alpha_age_eth;
  vector[J_age_edu] alpha_age_edu;
  vector[J_age_inc] alpha_age_inc;
  vector[J_age_eldx] alpha_age_eldx;
  vector[J_age_childx] alpha_age_childx;
  vector[J_age_wax] alpha_age_wax;
  vector[J_sex_eth] alpha_sex_eth;
  vector[J_sex_edu] alpha_sex_edu;                 
  vector[J_sex_inc] alpha_sex_inc;
  vector[J_sex_eldx] alpha_sex_eldx;
  vector[J_sex_childx] alpha_sex_childx;
  vector[J_sex_wax] alpha_sex_wax;
  vector[J_eth_edu] alpha_eth_edu;
  vector[J_eth_inc] alpha_eth_inc;
  vector[J_eth_eldx] alpha_eth_eldx;
  vector[J_eth_childx] alpha_eth_childx;
  vector[J_eth_wax] alpha_eth_wax;
  vector[J_edu_inc] alpha_edu_inc;
  vector[J_edu_eldx] alpha_edu_eldx;
  vector[J_edu_childx] alpha_edu_childx;
  vector[J_edu_wax] alpha_edu_wax;
  vector[J_inc_eldx] alpha_inc_eldx;
  vector[J_inc_childx] alpha_inc_childx;
  vector[J_inc_wax] alpha_inc_wax;
  vector[J_eldx_childx] alpha_eldx_childx;
  vector[J_eldx_wax] alpha_eldx_wax;
  vector[J_childx_wax] alpha_childx_wax;
  vector[J_age_sex_eth] alpha_age_sex_eth;    
  vector[J_age_sex_edu] alpha_age_sex_edu;
  vector[J_age_sex_inc] alpha_age_sex_inc;
  vector[J_age_sex_eldx] alpha_age_sex_eldx;
  vector[J_age_sex_childx] alpha_age_sex_childx;
  vector[J_age_sex_wax] alpha_age_sex_wax;
  vector[J_age_eth_edu] alpha_age_eth_edu;
  vector[J_age_eth_inc] alpha_age_eth_inc;
  vector[J_age_eth_eldx] alpha_age_eth_eldx;
  vector[J_age_eth_childx] alpha_age_eth_childx;
  vector[J_age_eth_wax] alpha_age_eth_wax;
  vector[J_age_edu_inc] alpha_age_edu_inc;
  vector[J_age_edu_eldx] alpha_age_edu_eldx;
  vector[J_age_edu_childx] alpha_age_edu_childx;
  vector[J_age_edu_wax] alpha_age_edu_wax;
  vector[J_age_inc_eldx] alpha_age_inc_eldx;
  vector[J_age_inc_childx] alpha_age_inc_childx;
  vector[J_age_inc_wax] alpha_age_inc_wax;
  vector[J_age_eldx_childx] alpha_age_eldx_childx;
  vector[J_age_eldx_wax] alpha_age_eldx_wax;
  vector[J_age_childx_wax] alpha_age_childx_wax;
  vector[J_sex_eth_edu] alpha_sex_eth_edu;
  vector[J_sex_eth_inc] alpha_sex_eth_inc;
  vector[J_sex_eth_eldx] alpha_sex_eth_eldx;
  vector[J_sex_eth_childx] alpha_sex_eth_childx;
  vector[J_sex_eth_wax] alpha_sex_eth_wax;
  vector[J_sex_edu_inc] alpha_sex_edu_inc;
  vector[J_sex_edu_eldx] alpha_sex_edu_eldx;
  vector[J_sex_edu_childx] alpha_sex_edu_childx;
  vector[J_sex_edu_wax] alpha_sex_edu_wax;
  vector[J_sex_inc_eldx] alpha_sex_inc_eldx;
  vector[J_sex_inc_childx] alpha_sex_inc_childx;
  vector[J_sex_inc_wax] alpha_sex_inc_wax;
  vector[J_sex_eldx_childx] alpha_sex_eldx_childx;
  vector[J_sex_eldx_wax] alpha_sex_eldx_wax;
  vector[J_sex_childx_wax] alpha_sex_childx_wax;
  vector[J_eth_edu_inc] alpha_eth_edu_inc;
  vector[J_eth_edu_eldx] alpha_eth_edu_eldx;
  vector[J_eth_edu_childx] alpha_eth_edu_childx;
  vector[J_eth_edu_wax] alpha_eth_edu_wax;
  vector[J_eth_inc_eldx] alpha_eth_inc_eldx;
  vector[J_eth_inc_childx] alpha_eth_inc_childx;
  vector[J_eth_inc_wax] alpha_eth_inc_wax;
  vector[J_eth_eldx_childx] alpha_eth_eldx_childx;
  vector[J_eth_eldx_wax] alpha_eth_eldx_wax;
  vector[J_eth_childx_wax] alpha_eth_childx_wax;
  vector[J_edu_inc_eldx] alpha_edu_inc_eldx;
  vector[J_edu_inc_childx] alpha_edu_inc_childx;
  vector[J_edu_inc_wax] alpha_edu_inc_wax;
  vector[J_edu_eldx_childx] alpha_edu_eldx_childx;
  vector[J_edu_eldx_wax] alpha_edu_eldx_wax;
  vector[J_edu_childx_wax] alpha_edu_childx_wax;
  vector[J_inc_eldx_childx] alpha_inc_eldx_childx;
  vector[J_inc_eldx_wax] alpha_inc_eldx_wax;
  vector[J_inc_childx_wax] alpha_inc_childx_wax;
  vector[J_eldx_childx_wax] alpha_eldx_childx_wax;

  real<lower=0> lambda_m;
  real log_sigma2_m[q];
  real<lower=0> lambda_inter[2];
  real<lower=0> sigma_y;

}
transformed parameters{
  real<lower=0> sigma_m[q];
  for (q0 in 1:q)
    sigma_m[q0] <- sqrt(exp(log_sigma2_m[q0]));
}
       
model {
vector[n] alpha_v_age;
vector[n] alpha_v_sex;
vector[n] alpha_v_eth;
vector[n] alpha_v_edu;
vector[n] alpha_v_inc;
vector[n] alpha_v_eldx;
vector[n] alpha_v_childx;
vector[n] alpha_v_wax;
vector[n] alpha_v_age_sex;
vector[n] alpha_v_age_eth;
vector[n] alpha_v_age_edu;
vector[n] alpha_v_age_inc;
vector[n] alpha_v_age_eldx;
vector[n] alpha_v_age_childx;
vector[n] alpha_v_age_wax;
vector[n] alpha_v_sex_eth;
vector[n] alpha_v_sex_edu;
vector[n] alpha_v_sex_inc;
vector[n] alpha_v_sex_eldx;
vector[n] alpha_v_sex_childx;
vector[n] alpha_v_sex_wax;     
vector[n] alpha_v_eth_edu;
vector[n] alpha_v_eth_inc;
vector[n] alpha_v_eth_eldx;
vector[n] alpha_v_eth_childx;
vector[n] alpha_v_eth_wax;
vector[n] alpha_v_edu_inc;
vector[n] alpha_v_edu_eldx;
vector[n] alpha_v_edu_childx;
vector[n] alpha_v_edu_wax;
vector[n] alpha_v_inc_eldx;
vector[n] alpha_v_inc_childx;
vector[n] alpha_v_inc_wax;
vector[n] alpha_v_eldx_childx;
vector[n] alpha_v_eldx_wax;
vector[n] alpha_v_childx_wax;
vector[n] alpha_v_age_sex_eth;
vector[n] alpha_v_age_sex_inc;
vector[n] alpha_v_age_sex_edu;
vector[n] alpha_v_age_sex_eldx;
vector[n] alpha_v_age_sex_childx;
vector[n] alpha_v_age_sex_wax;
vector[n] alpha_v_age_eth_edu;
vector[n] alpha_v_age_eth_inc;
vector[n] alpha_v_age_eth_eldx;
vector[n] alpha_v_age_eth_childx;
vector[n] alpha_v_age_eth_wax;
vector[n] alpha_v_age_edu_inc;
vector[n] alpha_v_age_edu_eldx;
vector[n] alpha_v_age_edu_childx;
vector[n] alpha_v_age_edu_wax;
vector[n] alpha_v_age_inc_eldx;
vector[n] alpha_v_age_inc_childx;
vector[n] alpha_v_age_inc_wax;
vector[n] alpha_v_age_eldx_childx;
vector[n] alpha_v_age_eldx_wax;
vector[n] alpha_v_age_childx_wax;
vector[n] alpha_v_sex_eth_edu;
vector[n] alpha_v_sex_eth_inc;
vector[n] alpha_v_sex_eth_eldx;
vector[n] alpha_v_sex_eth_childx;
vector[n] alpha_v_sex_eth_wax;
vector[n] alpha_v_sex_edu_inc;
vector[n] alpha_v_sex_edu_eldx;
vector[n] alpha_v_sex_edu_childx;
vector[n] alpha_v_sex_edu_wax;
vector[n] alpha_v_sex_inc_eldx;
vector[n] alpha_v_sex_inc_childx;
vector[n] alpha_v_sex_inc_wax;
vector[n] alpha_v_sex_eldx_childx;
vector[n] alpha_v_sex_eldx_wax;
vector[n] alpha_v_sex_childx_wax;
  vector[n] alpha_v_eth_edu_inc;
  vector[n] alpha_v_eth_edu_eldx;
  vector[n] alpha_v_eth_edu_childx;
  vector[n] alpha_v_eth_edu_wax;
  vector[n] alpha_v_eth_inc_eldx;
  vector[n] alpha_v_eth_inc_childx;
  vector[n] alpha_v_eth_inc_wax;
  vector[n] alpha_v_eth_eldx_childx;
  vector[n] alpha_v_eth_eldx_wax;
  vector[n] alpha_v_eth_childx_wax;
  vector[n] alpha_v_edu_inc_eldx;
  vector[n] alpha_v_edu_inc_childx;
  vector[n] alpha_v_edu_inc_wax;
  vector[n] alpha_v_edu_eldx_childx;
  vector[n] alpha_v_edu_eldx_wax;
  vector[n] alpha_v_edu_childx_wax;
  vector[n] alpha_v_inc_eldx_childx;
  vector[n] alpha_v_inc_eldx_wax;
  vector[n] alpha_v_inc_childx_wax;
  vector[n] alpha_v_eldx_childx_wax;

  alpha_age ~ normal(0,1);
  alpha_sex ~ normal(0,1);
  alpha_eth ~ normal(0,1);
  alpha_edu ~ normal(0,1);
  alpha_inc ~ normal(0,1);
  alpha_eldx ~ normal(0,1);
  alpha_childx ~ normal(0,1);
  alpha_wax ~ normal(0,1);
  alpha_age_sex ~ normal(0,1);
  alpha_age_eth ~ normal(0,1);
  alpha_age_edu ~ normal(0,1);
  alpha_age_inc ~ normal(0,1);
  alpha_age_eldx ~ normal(0,1);
  alpha_age_childx ~ normal(0,1);
  alpha_age_wax ~ normal(0,1);
  alpha_sex_eth ~ normal(0,1);
  alpha_sex_edu ~ normal(0,1);                 
  alpha_sex_inc ~ normal(0,1);
  alpha_sex_eldx ~ normal(0,1);
  alpha_sex_childx ~ normal(0,1);
  alpha_sex_wax ~ normal(0,1);
  alpha_eth_edu ~ normal(0,1);
  alpha_eth_inc ~ normal(0,1);
  alpha_eth_eldx ~ normal(0,1);
  alpha_eth_childx ~ normal(0,1);
  alpha_eth_wax ~ normal(0,1);
  alpha_edu_inc ~ normal(0,1);
  alpha_edu_eldx ~ normal(0,1);
  alpha_edu_childx ~ normal(0,1);
  alpha_edu_wax ~ normal(0,1);
  alpha_inc_eldx ~ normal(0,1);
  alpha_inc_childx ~ normal(0,1);
  alpha_inc_wax ~ normal(0,1);
  alpha_eldx_childx ~ normal(0,1);
  alpha_eldx_wax ~ normal(0,1);
  alpha_childx_wax ~ normal(0,1);
  alpha_age_sex_eth ~ normal(0,1);    
  alpha_age_sex_edu ~ normal(0,1);
  alpha_age_sex_inc ~ normal(0,1);
  alpha_age_sex_eldx ~ normal(0,1);
  alpha_age_sex_childx ~ normal(0,1);
  alpha_age_sex_wax ~ normal(0,1);
  alpha_age_eth_edu ~ normal(0,1);
  alpha_age_eth_inc ~ normal(0,1);
  alpha_age_eth_eldx ~ normal(0,1);
  alpha_age_eth_childx ~ normal(0,1);
  alpha_age_eth_wax ~ normal(0,1);
  alpha_age_edu_inc ~ normal(0,1);
  alpha_age_edu_eldx ~ normal(0,1);
  alpha_age_edu_childx ~ normal(0,1);
  alpha_age_edu_wax ~ normal(0,1);
  alpha_age_inc_eldx ~ normal(0,1);
  alpha_age_inc_childx ~ normal(0,1);
  alpha_age_inc_wax ~ normal(0,1);
  alpha_age_eldx_childx ~ normal(0,1);
  alpha_age_eldx_wax ~ normal(0,1);
  alpha_age_childx_wax ~ normal(0,1);
  alpha_sex_eth_edu ~ normal(0,1);
  alpha_sex_eth_inc ~ normal(0,1);
  alpha_sex_eth_eldx ~ normal(0,1);
  alpha_sex_eth_childx ~ normal(0,1);
  alpha_sex_eth_wax ~ normal(0,1);
  alpha_sex_edu_inc ~ normal(0,1);
  alpha_sex_edu_eldx ~ normal(0,1);
  alpha_sex_edu_childx ~ normal(0,1);
  alpha_sex_edu_wax ~ normal(0,1);
  alpha_sex_inc_eldx ~ normal(0,1);
  alpha_sex_inc_childx ~ normal(0,1);
  alpha_sex_inc_wax ~ normal(0,1);
  alpha_sex_eldx_childx ~ normal(0,1);
  alpha_sex_eldx_wax ~ normal(0,1);
  alpha_sex_childx_wax ~ normal(0,1);
  alpha_eth_edu_inc ~ normal(0,1);
  alpha_eth_edu_eldx ~ normal(0,1);
  alpha_eth_edu_childx ~ normal(0,1);
  alpha_eth_edu_wax ~ normal(0,1);
  alpha_eth_inc_eldx ~ normal(0,1);
  alpha_eth_inc_childx ~ normal(0,1);
  alpha_eth_inc_wax ~ normal(0,1);
  alpha_eth_eldx_childx ~ normal(0,1);
  alpha_eth_eldx_wax ~ normal(0,1);
  alpha_eth_childx_wax ~ normal(0,1);
  alpha_edu_inc_eldx ~ normal(0,1);
  alpha_edu_inc_childx ~ normal(0,1);
  alpha_edu_inc_wax ~ normal(0,1);
  alpha_edu_eldx_childx ~ normal(0,1);
  alpha_edu_eldx_wax ~ normal(0,1);
  alpha_edu_childx_wax ~ normal(0,1);
  alpha_inc_eldx_childx ~ normal(0,1);
  alpha_inc_eldx_wax ~ normal(0,1);
  alpha_inc_childx_wax ~ normal(0,1);
  alpha_eldx_childx_wax ~ normal(0,1);

  sigma_y ~ cauchy(0,10);
  lambda_inter ~ cauchy(0,5);

for (i in 1:n) {
 alpha_v_age[i] <- alpha_age[age[i]];
 alpha_v_sex[i] <- alpha_sex[sex[i]];
 alpha_v_eth[i] <- alpha_eth[eth[i]];
 alpha_v_edu[i] <- alpha_edu[edu[i]];
 alpha_v_inc[i] <- alpha_inc[inc[i]];
 alpha_v_eldx[i] <- alpha_eldx[eldx[i]];
 alpha_v_childx[i] <- alpha_childx[childx[i]];
 alpha_v_wax[i] <- alpha_wax[wax[i]];
 alpha_v_age_sex[i] <- alpha_age_sex[age_sex[i]];
 alpha_v_age_eth[i] <- alpha_age_eth[age_eth[i]];
 alpha_v_age_edu[i] <- alpha_age_edu[age_edu[i]];
 alpha_v_age_inc[i] <- alpha_age_inc[age_inc[i]];
 alpha_v_age_eldx[i] <- alpha_age_eldx[age_eldx[i]];
 alpha_v_age_childx[i] <- alpha_age_childx[age_childx[i]];
 alpha_v_age_wax[i] <- alpha_age_wax[age_wax[i]];
 alpha_v_sex_eth[i] <- alpha_sex_eth[sex_eth[i]];
 alpha_v_sex_edu[i] <- alpha_sex_edu[sex_edu[i]];
 alpha_v_sex_inc[i] <- alpha_sex_inc[sex_inc[i]];
 alpha_v_sex_eldx[i] <- alpha_sex_eldx[sex_eldx[i]];
 alpha_v_sex_childx[i] <- alpha_sex_childx[sex_childx[i]];
 alpha_v_sex_wax[i] <- alpha_sex_wax[sex_wax[i]];     
 alpha_v_eth_edu[i] <- alpha_eth_edu[eth_edu[i]];
 alpha_v_eth_inc[i] <- alpha_eth_inc[eth_inc[i]];
 alpha_v_eth_eldx[i] <- alpha_eth_eldx[eth_eldx[i]];
 alpha_v_eth_childx[i] <- alpha_eth_childx[eth_childx[i]];
 alpha_v_eth_wax[i] <- alpha_eth_wax[eth_wax[i]];
 alpha_v_edu_inc[i] <- alpha_edu_inc[edu_inc[i]];
 alpha_v_edu_eldx[i] <- alpha_edu_eldx[edu_eldx[i]];
 alpha_v_edu_childx[i] <- alpha_edu_childx[edu_childx[i]];
 alpha_v_edu_wax[i] <- alpha_edu_wax[edu_wax[i]];
 alpha_v_inc_eldx[i] <- alpha_inc_eldx[inc_eldx[i]];
 alpha_v_inc_childx[i] <- alpha_inc_childx[inc_childx[i]];
 alpha_v_inc_wax[i] <- alpha_inc_wax[inc_wax[i]];
 alpha_v_eldx_childx[i] <- alpha_eldx_childx[eldx_childx[i]];
 alpha_v_eldx_wax[i] <- alpha_eldx_wax[eldx_wax[i]];
 alpha_v_childx_wax[i] <- alpha_childx_wax[childx_wax[i]];
 alpha_v_age_sex_eth[i] <- alpha_age_sex_eth[age_sex_eth[i]];
 alpha_v_age_sex_inc[i] <- alpha_age_sex_inc[age_sex_inc[i]];
 alpha_v_age_sex_edu[i] <- alpha_age_sex_edu[age_sex_edu[i]];
 alpha_v_age_sex_eldx[i] <- alpha_age_sex_eldx[age_sex_eldx[i]];
 alpha_v_age_sex_childx[i] <- alpha_age_sex_childx[age_sex_childx[i]];
 alpha_v_age_sex_wax[i] <- alpha_age_sex_wax[age_sex_wax[i]];
 alpha_v_age_eth_edu[i] <- alpha_age_eth_edu[age_eth_edu[i]];
 alpha_v_age_eth_inc[i] <- alpha_age_eth_inc[age_eth_inc[i]];
 alpha_v_age_eth_eldx[i] <- alpha_age_eth_eldx[age_eth_eldx[i]];
 alpha_v_age_eth_childx[i] <- alpha_age_eth_childx[age_eth_childx[i]];
 alpha_v_age_eth_wax[i] <- alpha_age_eth_wax[age_eth_wax[i]];
 alpha_v_age_edu_inc[i] <- alpha_age_edu_inc[age_edu_inc[i]];
 alpha_v_age_edu_eldx[i] <- alpha_age_edu_eldx[age_edu_eldx[i]];
 alpha_v_age_edu_childx[i] <- alpha_age_edu_childx[age_edu_childx[i]];
 alpha_v_age_edu_wax[i] <- alpha_age_edu_wax[age_edu_wax[i]];;
 alpha_v_age_inc_eldx[i] <- alpha_age_inc_eldx[age_inc_eldx[i]];
 alpha_v_age_inc_childx[i] <- alpha_age_inc_childx[age_inc_childx[i]];
 alpha_v_age_inc_wax[i] <- alpha_age_inc_wax[age_inc_wax[i]];
 alpha_v_age_eldx_childx[i] <- alpha_age_eldx_childx[age_eldx_childx[i]];
 alpha_v_age_eldx_wax[i] <- alpha_age_eldx_wax[age_eldx_wax[i]];
 alpha_v_age_childx_wax[i] <- alpha_age_childx_wax[age_childx_wax[i]];;
 alpha_v_sex_eth_edu[i] <- alpha_sex_eth_edu[sex_eth_edu[i]];
 alpha_v_sex_eth_inc[i] <- alpha_sex_eth_inc[sex_eth_inc[i]];
 alpha_v_sex_eth_eldx[i] <- alpha_sex_eth_eldx[sex_eth_eldx[i]];
 alpha_v_sex_eth_childx[i] <- alpha_sex_eth_childx[sex_eth_childx[i]];
 alpha_v_sex_eth_wax[i] <- alpha_sex_eth_wax[sex_eth_wax[i]];
 alpha_v_sex_edu_inc[i] <- alpha_sex_edu_inc[sex_edu_inc[i]];
 alpha_v_sex_edu_eldx[i] <- alpha_sex_edu_eldx[sex_edu_eldx[i]];
 alpha_v_sex_edu_childx[i] <- alpha_sex_edu_childx[sex_edu_childx[i]];
 alpha_v_sex_edu_wax[i] <- alpha_sex_edu_wax[sex_edu_wax[i]];
 alpha_v_sex_inc_eldx[i] <- alpha_sex_inc_eldx[sex_inc_eldx[i]];
 alpha_v_sex_inc_childx[i] <- alpha_sex_inc_childx[sex_inc_childx[i]];
 alpha_v_sex_inc_wax[i] <- alpha_sex_inc_wax[sex_inc_wax[i]];
 alpha_v_sex_eldx_childx[i] <- alpha_sex_eldx_childx[sex_eldx_childx[i]];
 alpha_v_sex_eldx_wax[i] <- alpha_sex_eldx_wax[sex_eldx_wax[i]];
 alpha_v_sex_childx_wax[i] <- alpha_sex_childx_wax[sex_childx_wax[i]];
 alpha_v_eth_edu_inc[i] <- alpha_eth_edu_inc[eth_edu_inc[i]];
 alpha_v_eth_edu_eldx[i] <- alpha_eth_edu_eldx[eth_edu_eldx[i]];
 alpha_v_eth_edu_childx[i] <- alpha_eth_edu_childx[eth_edu_childx[i]];
 alpha_v_eth_edu_wax[i] <- alpha_eth_edu_wax[eth_edu_wax[i]];
 alpha_v_eth_inc_eldx[i] <- alpha_eth_inc_eldx[eth_inc_eldx[i]];
 alpha_v_eth_inc_childx[i] <- alpha_eth_inc_childx[eth_inc_childx[i]];
 alpha_v_eth_inc_wax[i] <- alpha_eth_inc_wax[eth_inc_wax[i]];
 alpha_v_eth_eldx_childx[i] <- alpha_eth_eldx_childx[eth_eldx_childx[i]];
 alpha_v_eth_eldx_wax[i] <- alpha_eth_eldx_wax[eth_eldx_wax[i]];
 alpha_v_eth_childx_wax[i] <- alpha_eth_childx_wax[eth_childx_wax[i]];
 alpha_v_edu_inc_eldx[i] <- alpha_edu_inc_eldx[edu_inc_eldx[i]];
 alpha_v_edu_inc_childx[i] <- alpha_edu_inc_childx[edu_inc_childx[i]];
 alpha_v_edu_inc_wax[i] <- alpha_edu_inc_wax[edu_inc_wax[i]];
 alpha_v_edu_eldx_childx[i] <- alpha_edu_eldx_childx[edu_eldx_childx[i]];
 alpha_v_edu_eldx_wax[i] <- alpha_edu_eldx_wax[edu_eldx_wax[i]];
 alpha_v_edu_childx_wax[i] <- alpha_edu_childx_wax[edu_childx_wax[i]];
 alpha_v_inc_eldx_childx[i] <- alpha_inc_eldx_childx[inc_eldx_childx[i]];
 alpha_v_inc_eldx_wax[i] <- alpha_inc_eldx_wax[inc_eldx_wax[i]];
 alpha_v_inc_childx_wax[i] <- alpha_inc_childx_wax[inc_childx_wax[i]];
 alpha_v_eldx_childx_wax[i] <- alpha_eldx_childx_wax[eldx_childx_wax[i]];
}

y ~ normal(alpha +
alpha_v_age * lambda_m * sigma_m[1] +
alpha_v_sex * lambda_m * sigma_m[2] +
 alpha_v_eth * lambda_m * sigma_m[3] +
 alpha_v_edu * lambda_m * sigma_m[4] +
 alpha_v_inc * lambda_m * sigma_m[5] +
 alpha_v_eldx * lambda_m * sigma_m[6] +
 alpha_v_childx * lambda_m * sigma_m[7] +
 alpha_v_wax * lambda_m * sigma_m[8] +
 alpha_v_age_sex * lambda_inter[1] * sigma_m[1] * sigma_m[2] +
 alpha_v_age_eth * lambda_inter[1] * sigma_m[1] * sigma_m[3] +
 alpha_v_age_edu * lambda_inter[1] * sigma_m[1] * sigma_m[4] +
 alpha_v_age_inc * lambda_inter[1] * sigma_m[1] * sigma_m[5] +
 alpha_v_age_eldx * lambda_inter[1] * sigma_m[1] * sigma_m[6] +
 alpha_v_age_childx * lambda_inter[1] * sigma_m[1] * sigma_m[7] +
 alpha_v_age_wax * lambda_inter[1] * sigma_m[1] * sigma_m[8] +
 alpha_v_sex_eth * lambda_inter[1] * sigma_m[2] * sigma_m[3] +
 alpha_v_sex_edu * lambda_inter[1] * sigma_m[2] * sigma_m[4] +
 alpha_v_sex_inc * lambda_inter[1] * sigma_m[2] * sigma_m[5] +
 alpha_v_sex_eldx * lambda_inter[1] * sigma_m[2] * sigma_m[6] +
 alpha_v_sex_childx * lambda_inter[1] * sigma_m[2] * sigma_m[7] +
 alpha_v_sex_wax * lambda_inter[1] * sigma_m[2] * sigma_m[8] +      
 alpha_v_eth_edu * lambda_inter[1] * sigma_m[3] * sigma_m[4] +
 alpha_v_eth_inc * lambda_inter[1] * sigma_m[3] * sigma_m[5] +
 alpha_v_eth_eldx * lambda_inter[1] * sigma_m[3] * sigma_m[6] +
 alpha_v_eth_childx * lambda_inter[1] * sigma_m[3] * sigma_m[7] +
 alpha_v_eth_wax * lambda_inter[1] * sigma_m[3] * sigma_m[8] +
 alpha_v_edu_inc * lambda_inter[1] * sigma_m[4] * sigma_m[5] +
 alpha_v_edu_eldx * lambda_inter[1] * sigma_m[4] * sigma_m[6] +
 alpha_v_edu_childx * lambda_inter[1] * sigma_m[4] * sigma_m[7] +
 alpha_v_edu_wax * lambda_inter[1] * sigma_m[4] * sigma_m[8] +
 alpha_v_inc_eldx * lambda_inter[1] * sigma_m[5] * sigma_m[6] +
 alpha_v_inc_childx * lambda_inter[1] * sigma_m[5] * sigma_m[7] +
 alpha_v_inc_wax * lambda_inter[1] * sigma_m[5] * sigma_m[8] +
 alpha_v_eldx_childx * lambda_inter[1] * sigma_m[6] * sigma_m[7] +
 alpha_v_eldx_wax * lambda_inter[1] * sigma_m[6] * sigma_m[8] +
 alpha_v_childx_wax * lambda_inter[1] * sigma_m[7] * sigma_m[8] +
 alpha_v_age_sex_eth * lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[3] +
 alpha_v_age_sex_inc * lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[4] +
 alpha_v_age_sex_edu * lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[5] +
 alpha_v_age_sex_eldx * lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[6] +
 alpha_v_age_sex_childx * lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[7] +
 alpha_v_age_sex_wax * lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[8] +
 alpha_v_age_eth_edu * lambda_inter[2] * sigma_m[1] * sigma_m[3] * sigma_m[4] +
 alpha_v_age_eth_inc * lambda_inter[2] * sigma_m[1] * sigma_m[3] * sigma_m[5] +
 alpha_v_age_eth_eldx * lambda_inter[2] * sigma_m[1] * sigma_m[3] * sigma_m[6] +
 alpha_v_age_eth_childx * lambda_inter[2] * sigma_m[1] * sigma_m[3] * sigma_m[7] +
 alpha_v_age_eth_wax * lambda_inter[2] * sigma_m[1] * sigma_m[3] * sigma_m[8] +
 alpha_v_age_edu_inc * lambda_inter[2] * sigma_m[1] * sigma_m[4] * sigma_m[5] +
 alpha_v_age_edu_eldx * lambda_inter[2] * sigma_m[1] * sigma_m[4] * sigma_m[6] +
 alpha_v_age_edu_childx * lambda_inter[2] * sigma_m[1] * sigma_m[4] * sigma_m[7] +
 alpha_v_age_edu_wax * lambda_inter[2] * sigma_m[1] * sigma_m[4] * sigma_m[8] + 
 alpha_v_age_inc_eldx * lambda_inter[2] * sigma_m[1] * sigma_m[5] * sigma_m[6] +
 alpha_v_age_inc_childx * lambda_inter[2] * sigma_m[1] * sigma_m[5] * sigma_m[7] +
 alpha_v_age_inc_wax * lambda_inter[2] * sigma_m[1] * sigma_m[5] * sigma_m[8] +
 alpha_v_age_eldx_childx * lambda_inter[2] * sigma_m[1] * sigma_m[6] * sigma_m[7] +
 alpha_v_age_eldx_wax * lambda_inter[2] * sigma_m[1] * sigma_m[6] * sigma_m[8] +
 alpha_v_age_childx_wax * lambda_inter[2] * sigma_m[1] * sigma_m[7] * sigma_m[8] +
 alpha_v_sex_eth_edu * lambda_inter[2] * sigma_m[2] * sigma_m[3] * sigma_m[4] +
 alpha_v_sex_eth_inc * lambda_inter[2] * sigma_m[2] * sigma_m[3] * sigma_m[5] +
 alpha_v_sex_eth_eldx * lambda_inter[2] * sigma_m[2] * sigma_m[3] * sigma_m[6] +
 alpha_v_sex_eth_childx * lambda_inter[2] * sigma_m[2] * sigma_m[3] * sigma_m[7] +
 alpha_v_sex_eth_wax * lambda_inter[2] * sigma_m[2] * sigma_m[3] * sigma_m[8] + 
 alpha_v_sex_edu_inc * lambda_inter[2] * sigma_m[2] * sigma_m[4] * sigma_m[5] +
 alpha_v_sex_edu_eldx * lambda_inter[2] * sigma_m[2] * sigma_m[4] * sigma_m[6] +
 alpha_v_sex_edu_childx * lambda_inter[2] * sigma_m[2] * sigma_m[4] * sigma_m[7] +
 alpha_v_sex_edu_wax * lambda_inter[2] * sigma_m[2] * sigma_m[4] * sigma_m[8] +
 alpha_v_sex_inc_eldx * lambda_inter[2] * sigma_m[2] * sigma_m[5] * sigma_m[6] +
 alpha_v_sex_inc_childx * lambda_inter[2] * sigma_m[2] * sigma_m[5] * sigma_m[7] +
 alpha_v_sex_inc_wax * lambda_inter[2] * sigma_m[2] * sigma_m[5] * sigma_m[8] +
 alpha_v_sex_eldx_childx * lambda_inter[2] * sigma_m[2] * sigma_m[6] * sigma_m[7] +
 alpha_v_sex_eldx_wax * lambda_inter[2] * sigma_m[2] * sigma_m[6] * sigma_m[8] +
 alpha_v_sex_childx_wax * lambda_inter[2] * sigma_m[2] * sigma_m[7] * sigma_m[8] +
 alpha_v_eth_edu_inc * lambda_inter[2] * sigma_m[3] * sigma_m[4] * sigma_m[5] +
 alpha_v_eth_edu_eldx * lambda_inter[2] * sigma_m[3] * sigma_m[4] * sigma_m[6] +
 alpha_v_eth_edu_childx * lambda_inter[2] * sigma_m[3] * sigma_m[4] * sigma_m[7] +
 alpha_v_eth_edu_wax * lambda_inter[2] * sigma_m[3] * sigma_m[4] * sigma_m[8] +
 alpha_v_eth_inc_eldx * lambda_inter[2] * sigma_m[3] * sigma_m[5] * sigma_m[6] +
 alpha_v_eth_inc_childx * lambda_inter[2] * sigma_m[3] * sigma_m[5] * sigma_m[7] +
 alpha_v_eth_inc_wax * lambda_inter[2] * sigma_m[3] * sigma_m[5] * sigma_m[8] +
 alpha_v_eth_eldx_childx * lambda_inter[2] * sigma_m[3] * sigma_m[6] * sigma_m[7] +
 alpha_v_eth_eldx_wax * lambda_inter[2] * sigma_m[3] * sigma_m[6] * sigma_m[8] +
 alpha_v_eth_childx_wax * lambda_inter[2] * sigma_m[3] * sigma_m[7] * sigma_m[8] +
 alpha_v_edu_inc_eldx * lambda_inter[2] * sigma_m[4] * sigma_m[5] * sigma_m[6] +
 alpha_v_edu_inc_childx * lambda_inter[2] * sigma_m[4] * sigma_m[5] * sigma_m[7] + 
 alpha_v_edu_inc_wax * lambda_inter[2] * sigma_m[4] * sigma_m[5] * sigma_m[8] +
 alpha_v_edu_eldx_childx * lambda_inter[2] * sigma_m[4] * sigma_m[6] * sigma_m[7] + 
 alpha_v_edu_eldx_wax * lambda_inter[2] * sigma_m[4] * sigma_m[6] * sigma_m[8] + 
 alpha_v_edu_childx_wax * lambda_inter[2] * sigma_m[4] * sigma_m[7] * sigma_m[8] + 
 alpha_v_inc_eldx_childx * lambda_inter[2] * sigma_m[5] * sigma_m[6] * sigma_m[7] + 
 alpha_v_inc_eldx_wax * lambda_inter[2] * sigma_m[5] * sigma_m[6] * sigma_m[8] + 
 alpha_v_inc_childx_wax * lambda_inter[2] * sigma_m[5] * sigma_m[7] * sigma_m[8] + 
	   alpha_v_eldx_childx_wax * lambda_inter[2] * sigma_m[6] * sigma_m[7] * sigma_m[8], sigma_y);
}
