data {
  int<lower=0> n;//sample size
  int<lower=0> J;//#cells
  int<lower=0> J_true; //#cells in population
  int<lower=0> q;//#variables
  int<lower=0> J_use[J]; //occupied cells
  vector[J] N_cell;
  vector[J_true] N_cell_true;
  vector[J] n_cell;
  vector[J] y_cell; //continuous outcome
  real<lower=0> ss_cell; //sum of variance in cells
  int<lower=0> cell_str[J_true,q]; //variable values allocated to cells
  int<lower=0> J_age; //levels
  int<lower=0> J_sex;
  int<lower=0> J_eth;
  int<lower=0> J_edu;
  int<lower=0> J_inc;
}
transformed data {
  int<lower=0> J_age_eth;
  int<lower=0> J_age_edu;
  int<lower=0> J_age_sex;
  int<lower=0> J_age_inc;
  int<lower=0> J_eth_edu;
  int<lower=0> J_eth_sex;
  int<lower=0> J_eth_inc;
  int<lower=0> J_edu_sex;
  int<lower=0> J_edu_inc;
  int<lower=0> J_sex_inc;
  int<lower=0> J_age_eth_edu;
  int<lower=0> J_age_eth_sex;
  int<lower=0> J_age_eth_inc;
  int<lower=0> J_age_edu_sex;
  int<lower=0> J_age_edu_inc;
  int<lower=0> J_age_sex_inc;
  int<lower=0> J_eth_edu_sex;
  int<lower=0> J_eth_edu_inc;
  int<lower=0> J_eth_sex_inc;
  int<lower=0> J_edu_sex_inc;
  int<lower=0> J_age_eth_edu_sex;
  int<lower=0> J_age_eth_edu_inc;
  int<lower=0> J_age_eth_sex_inc;
  int<lower=0> J_age_edu_sex_inc;
  int<lower=0> J_eth_edu_sex_inc;
  int<lower=0> J_age_eth_edu_sex_inc;

  J_age_sex <- J_age * J_sex;
  J_age_eth <- J_age * J_eth;
  J_age_edu <- J_age * J_edu;
  J_age_inc <- J_age * J_inc;
  J_eth_sex <- J_sex * J_eth;
  J_edu_sex <- J_sex * J_edu;
  J_sex_inc <- J_sex * J_inc;
  J_eth_edu <- J_eth * J_edu;
  J_eth_inc <- J_eth * J_inc;
  J_edu_inc <- J_edu * J_inc;
  J_age_eth_sex <- J_age * J_sex * J_eth;
  J_age_edu_sex <- J_age * J_sex * J_edu;
  J_age_sex_inc <- J_age * J_sex * J_inc;
  J_age_eth_edu <- J_age * J_eth * J_edu;
  J_age_eth_inc <- J_age * J_eth * J_inc;
  J_age_edu_inc <- J_age * J_edu * J_inc;
  J_eth_edu_sex <- J_sex * J_eth * J_edu;
  J_eth_sex_inc <- J_sex * J_eth * J_inc;
  J_edu_sex_inc <- J_sex * J_edu * J_inc;
  J_eth_edu_inc <- J_eth * J_edu * J_inc;
  J_age_eth_edu_sex <- J_age * J_sex * J_eth * J_edu;
  J_age_eth_sex_inc <- J_age * J_sex * J_eth * J_inc;
  J_age_edu_sex_inc <- J_age * J_sex * J_edu * J_inc;
  J_age_eth_edu_inc <- J_age * J_eth * J_edu * J_inc;
  J_eth_edu_sex_inc <- J_sex * J_eth * J_edu * J_inc;
  J_age_eth_edu_sex_inc <- J_age * J_sex * J_eth * J_edu * J_inc;

}

parameters {
  real alpha; //intercept
  vector[J_age] alpha_age;
  vector[J_eth] alpha_eth;
  vector[J_edu] alpha_edu;
  vector[J_sex] alpha_sex;
  vector[J_inc] alpha_inc;
  vector[J_age_eth] alpha_age_eth;
  vector[J_age_edu] alpha_age_edu;
  vector[J_age_sex] alpha_age_sex;
  vector[J_age_inc] alpha_age_inc;
  vector[J_eth_edu] alpha_eth_edu;
  vector[J_eth_sex] alpha_eth_sex;
  vector[J_eth_inc] alpha_eth_inc;
  vector[J_edu_sex] alpha_edu_sex;
  vector[J_edu_inc] alpha_edu_inc;
  vector[J_sex_inc] alpha_sex_inc;
  vector[J_age_eth_edu] alpha_age_eth_edu;
  vector[J_age_eth_sex] alpha_age_eth_sex;
  vector[J_age_eth_inc] alpha_age_eth_inc;
  vector[J_age_edu_sex] alpha_age_edu_sex;
  vector[J_age_edu_inc] alpha_age_edu_inc;
  vector[J_age_sex_inc] alpha_age_sex_inc;
  vector[J_eth_edu_sex] alpha_eth_edu_sex;
  vector[J_eth_edu_inc] alpha_eth_edu_inc;
  vector[J_eth_sex_inc] alpha_eth_sex_inc;
  vector[J_edu_sex_inc] alpha_edu_sex_inc;
  vector[J_age_eth_edu_sex] alpha_age_eth_edu_sex;
  vector[J_age_eth_edu_inc] alpha_age_eth_edu_inc;
  vector[J_age_eth_sex_inc] alpha_age_eth_sex_inc;
  vector[J_age_edu_sex_inc] alpha_age_edu_sex_inc;
  vector[J_eth_edu_sex_inc] alpha_eth_edu_sex_inc;
  vector[J_age_eth_edu_sex_inc] alpha_age_eth_edu_sex_inc;

  real<lower=0.001> sigma_m;
  real<lower=0.001> lambda_m[q];
  real<lower=0.001> lambda_inter[4];
  real<lower=0.001> sigma_y;

}
transformed parameters{
  vector[J] mu_cell;
  vector[J] sigma_y_cell;
  vector[q] lambda_m_vec;

  for (i in 1:q){
    lambda_m_vec[i] <- lambda_m[i];
  }

 for (j in 1:J){
   mu_cell[j] <- alpha + 
    alpha_age[cell_str[J_use[j],1]] * lambda_m[1] * sigma_m+ 
    alpha_eth[cell_str[J_use[j],2]] * lambda_m[2] * sigma_m+ 
    alpha_edu[cell_str[J_use[j],3]] * lambda_m[3] * sigma_m+
    alpha_sex[cell_str[J_use[j],4]] * lambda_m[4] * sigma_m+ 
    alpha_inc[cell_str[J_use[j],5]] * lambda_m[5] * sigma_m+
    alpha_age_eth[(cell_str[J_use[j],1]-1) * J_eth + cell_str[J_use[j],2]] * lambda_inter[1] * lambda_m[1] * lambda_m[2]* sigma_m+
    alpha_age_edu[(cell_str[J_use[j],1]-1) * J_edu + cell_str[J_use[j],3]] * lambda_inter[1] * lambda_m[1] * lambda_m[3]* sigma_m+
    alpha_age_sex[(cell_str[J_use[j],1]-1) * J_sex + cell_str[J_use[j],4]] * lambda_inter[1] * lambda_m[1] * lambda_m[4]* sigma_m+
    alpha_age_inc[(cell_str[J_use[j],1]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[1] * lambda_m[1] * lambda_m[5]* sigma_m+
    alpha_eth_edu[(cell_str[J_use[j],2]-1) * J_edu + cell_str[J_use[j],3]] * lambda_inter[1] * lambda_m[2] * lambda_m[3]* sigma_m+
    alpha_eth_sex[(cell_str[J_use[j],2]-1) * J_sex + cell_str[J_use[j],4]] * lambda_inter[1] * lambda_m[2] * lambda_m[4]* sigma_m+
    alpha_eth_inc[(cell_str[J_use[j],2]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[1] * lambda_m[2] * lambda_m[5]* sigma_m+
    alpha_edu_sex[(cell_str[J_use[j],3]-1) * J_sex + cell_str[J_use[j],4]] * lambda_inter[1] * lambda_m[3] * lambda_m[4]* sigma_m+
    alpha_edu_inc[(cell_str[J_use[j],3]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[1] * lambda_m[3] * lambda_m[5]* sigma_m+
    alpha_sex_inc[(cell_str[J_use[j],4]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[1] * lambda_m[4] * lambda_m[5]* sigma_m+
    alpha_age_eth_edu[(cell_str[J_use[j],1]-1) * J_eth * J_edu + (cell_str[J_use[j],2]-1) * J_edu + cell_str[J_use[j],3]] * lambda_inter[2] * lambda_m[1] * lambda_m[2] * lambda_m[3]* sigma_m+
    alpha_age_eth_sex[(cell_str[J_use[j],1]-1) * J_eth * J_sex + (cell_str[J_use[j],2]-1) * J_sex + cell_str[J_use[j],4]] * lambda_inter[2] * lambda_m[1] * lambda_m[2] * lambda_m[4]* sigma_m+
    alpha_age_eth_inc[(cell_str[J_use[j],1]-1) * J_eth * J_inc + (cell_str[J_use[j],2]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[2] * lambda_m[1] * lambda_m[2] * lambda_m[5]* sigma_m+
    alpha_age_edu_sex[(cell_str[J_use[j],1]-1) * J_edu * J_sex + (cell_str[J_use[j],3]-1) * J_sex + cell_str[J_use[j],4]] * lambda_inter[2] * lambda_m[1] * lambda_m[3] * lambda_m[4]* sigma_m+
    alpha_age_edu_inc[(cell_str[J_use[j],1]-1) * J_edu * J_inc + (cell_str[J_use[j],3]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[2] * lambda_m[1] * lambda_m[3] * lambda_m[5]* sigma_m+
    alpha_age_sex_inc[(cell_str[J_use[j],1]-1) * J_sex * J_inc + (cell_str[J_use[j],4]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[2] * lambda_m[1] * lambda_m[4] * lambda_m[5]* sigma_m+
    alpha_eth_edu_sex[(cell_str[J_use[j],2]-1) * J_edu * J_sex + (cell_str[J_use[j],3]-1) * J_sex + cell_str[J_use[j],4]] * lambda_inter[2] * lambda_m[2] * lambda_m[3] * lambda_m[4]* sigma_m+
    alpha_eth_edu_inc[(cell_str[J_use[j],2]-1) * J_edu * J_inc + (cell_str[J_use[j],3]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[2] * lambda_m[2] * lambda_m[3] * lambda_m[5]* sigma_m+
    alpha_eth_sex_inc[(cell_str[J_use[j],2]-1) * J_sex * J_inc + (cell_str[J_use[j],4]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[2] * lambda_m[2] * lambda_m[4] * lambda_m[5]* sigma_m+
    alpha_edu_sex_inc[(cell_str[J_use[j],3]-1) * J_sex * J_inc + (cell_str[J_use[j],4]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[2] * lambda_m[3] * lambda_m[4] * lambda_m[5]* sigma_m+
    alpha_age_eth_edu_sex[(cell_str[J_use[j],1]-1) * J_eth * J_edu * J_sex + (cell_str[J_use[j],2]-1) * J_edu * J_sex + (cell_str[J_use[j],3]-1) * J_sex + cell_str[J_use[j],4]] * lambda_inter[3] * lambda_m[1] * lambda_m[2] * lambda_m[3]* lambda_m[4]* sigma_m+
    alpha_age_eth_edu_inc[(cell_str[J_use[j],1]-1) * J_eth * J_edu * J_inc + (cell_str[J_use[j],2]-1) * J_edu * J_inc + (cell_str[J_use[j],3]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[3] * lambda_m[1] * lambda_m[2] * lambda_m[3]* lambda_m[5]* sigma_m+
    alpha_age_eth_sex_inc[(cell_str[J_use[j],1]-1) * J_eth * J_sex * J_inc + (cell_str[J_use[j],2]-1) * J_sex * J_inc + (cell_str[J_use[j],4]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[3] * lambda_m[1] * lambda_m[2] * lambda_m[4]* lambda_m[5]* sigma_m+
    alpha_age_edu_sex_inc[(cell_str[J_use[j],1]-1) * J_edu * J_sex * J_inc + (cell_str[J_use[j],3]-1) * J_sex * J_inc + (cell_str[J_use[j],4]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[3] * lambda_m[1] * lambda_m[3] * lambda_m[4]* lambda_m[5]* sigma_m+
    alpha_eth_edu_sex_inc[(cell_str[J_use[j],2]-1) * J_edu * J_sex * J_inc + (cell_str[J_use[j],3]-1) * J_sex * J_inc + (cell_str[J_use[j],4]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[3] * lambda_m[2] * lambda_m[3] * lambda_m[4]* lambda_m[5]* sigma_m+
    alpha_age_eth_edu_sex_inc[(cell_str[J_use[j],1]-1) * J_eth * J_edu * J_sex * J_inc + (cell_str[J_use[j],2]-1) * J_edu * J_sex * J_inc + (cell_str[J_use[j],3]-1) * J_sex * J_inc + (cell_str[J_use[j],4]-1) * J_inc + cell_str[J_use[j],5]] * lambda_inter[4] * lambda_m[1] * lambda_m[2] * lambda_m[3]* lambda_m[4]* lambda_m[5]* sigma_m;
  }

  for (j in 1:J){
    sigma_y_cell[j] <- sigma_y / sqrt(n_cell[j]);
  }

}
       
model {
  alpha ~ normal(0,100);
  alpha_age ~ normal(0,1);
  alpha_sex ~ normal(0,1);
  alpha_eth ~ normal(0,1);
  alpha_edu ~ normal(0,1);
  alpha_inc ~ normal(0,1);
  alpha_age_eth ~ normal(0,1);
  alpha_age_edu ~ normal(0,1);
  alpha_age_sex ~ normal(0,1);
  alpha_age_inc ~ normal(0,1);
  alpha_eth_edu ~ normal(0,1);
  alpha_eth_sex ~ normal(0,1);
  alpha_eth_inc ~ normal(0,1);
  alpha_edu_sex ~ normal(0,1);
  alpha_edu_inc ~ normal(0,1);
  alpha_sex_inc ~ normal(0,1);
  alpha_age_eth_edu ~ normal(0,1);
  alpha_age_eth_sex ~ normal(0,1);
  alpha_age_eth_inc ~ normal(0,1);
  alpha_age_edu_sex ~ normal(0,1);
  alpha_age_edu_inc ~ normal(0,1);
  alpha_age_sex_inc ~ normal(0,1);
  alpha_eth_edu_sex ~ normal(0,1);
  alpha_eth_edu_inc ~ normal(0,1);
  alpha_eth_sex_inc ~ normal(0,1);
  alpha_edu_sex_inc ~ normal(0,1);
  alpha_age_eth_edu_sex ~ normal(0,1);
  alpha_age_eth_edu_inc ~ normal(0,1);
  alpha_age_eth_sex_inc ~ normal(0,1);
  alpha_age_edu_sex_inc ~ normal(0,1);
  alpha_eth_edu_sex_inc ~ normal(0,1);
  alpha_age_eth_edu_sex_inc ~ normal(0,1);


  sigma_m ~ normal(0,2.5);
  sigma_y ~ normal(0,2.5);
  lambda_m ~ normal(0,1);
  lambda_inter ~ normal(0,1);
  
  y_cell ~ normal(mu_cell,sigma_y_cell);

  ss_cell / pow(sigma_y,2) ~ chi_square(n-1);
  increment_log_prob(log(2) - 3 * log(sigma_y));

}
generated quantities{
  vector[J] y_cell_new;
  vector[J] w_new;
  vector[J] ps_w;
  real pri_var;
  vector[J_true] mu_cell_pred;
  real theta_pred;

  pri_var <- lambda_m_vec' * lambda_m_vec * pow(sigma_m,2) +
 (lambda_inter[1] * lambda_m[1] * lambda_m[2] * sigma_m)^2 +
  (lambda_inter[1] * lambda_m[1] * lambda_m[3]* sigma_m)^2 + (lambda_inter[1] * lambda_m[1] * lambda_m[4]* sigma_m)^2 +
  (lambda_inter[1] * lambda_m[1] * lambda_m[5]* sigma_m)^2+ (lambda_inter[1] * lambda_m[2] * lambda_m[3]* sigma_m)^2 +
  (lambda_inter[1] * lambda_m[2] * lambda_m[4]* sigma_m)^2 + (lambda_inter[1] * lambda_m[2] * lambda_m[5]* sigma_m)^2+
  (lambda_inter[1] * lambda_m[3] * lambda_m[4]* sigma_m)^2+ (lambda_inter[1] * lambda_m[3] * lambda_m[5]* sigma_m)^2+
  (lambda_inter[1] * lambda_m[4] * lambda_m[5]* sigma_m)^2+
  (lambda_inter[2] * lambda_m[1] * lambda_m[2]* lambda_m[3]* sigma_m)^2 + 
  (lambda_inter[2] * lambda_m[1] * lambda_m[2]* lambda_m[4]* sigma_m)^2 + 
  (lambda_inter[2] * lambda_m[1] * lambda_m[2]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[2] * lambda_m[1] * lambda_m[3]* lambda_m[4]* sigma_m)^2 + 
  (lambda_inter[2] * lambda_m[1] * lambda_m[3]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[2] * lambda_m[1] * lambda_m[4]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[2] * lambda_m[2] * lambda_m[3]* lambda_m[4]* sigma_m)^2 + 
  (lambda_inter[2] * lambda_m[2] * lambda_m[3]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[2] * lambda_m[2] * lambda_m[4]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[2] * lambda_m[3] * lambda_m[4]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[3] * lambda_m[1] * lambda_m[2]* lambda_m[3]* lambda_m[4]* sigma_m)^2 + 
  (lambda_inter[3] * lambda_m[1] * lambda_m[2]* lambda_m[3]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[3] * lambda_m[1] * lambda_m[2]* lambda_m[4]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[3] * lambda_m[1] * lambda_m[3]* lambda_m[4]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[3] * lambda_m[2] * lambda_m[3]* lambda_m[4]* lambda_m[5]* sigma_m)^2 + 
  (lambda_inter[4] * lambda_m[1] * lambda_m[2]* lambda_m[3]* lambda_m[4]* lambda_m[5]* sigma_m)^2;


  for (j in 1:J){
    y_cell_new[j] <- normal_rng(mu_cell[j],sigma_y_cell[j]);
    ps_w[j] <- pow(sigma_y_cell[j],-2)/(pow(sigma_y_cell[j],-2) + 1/pri_var);
    w_new[j] <- ps_w[j] * N_cell[j]/sum(N_cell)/n_cell[j]*n + 1-ps_w[j];
  }

for (j in 1:J_true){
  mu_cell_pred[j] <- alpha + 
    alpha_age[cell_str[j,1]] * lambda_m[1] * sigma_m+ 
    alpha_eth[cell_str[j,2]] * lambda_m[2] * sigma_m+ 
    alpha_edu[cell_str[j,3]] * lambda_m[3] * sigma_m+
    alpha_sex[cell_str[j,4]] * lambda_m[4] * sigma_m+ 
    alpha_inc[cell_str[j,5]] * lambda_m[5] * sigma_m+
    alpha_age_eth[(cell_str[j,1]-1) * J_eth + cell_str[j,2]] * lambda_inter[1] * lambda_m[1] * lambda_m[2]* sigma_m+
    alpha_age_edu[(cell_str[j,1]-1) * J_edu + cell_str[j,3]] * lambda_inter[1] * lambda_m[1] * lambda_m[3]* sigma_m+
    alpha_age_sex[(cell_str[j,1]-1) * J_sex + cell_str[j,4]] * lambda_inter[1] * lambda_m[1] * lambda_m[4]* sigma_m+
    alpha_age_inc[(cell_str[j,1]-1) * J_inc + cell_str[j,5]] * lambda_inter[1] * lambda_m[1] * lambda_m[5]* sigma_m+
    alpha_eth_edu[(cell_str[j,2]-1) * J_edu + cell_str[j,3]] * lambda_inter[1] * lambda_m[2] * lambda_m[3]* sigma_m+
    alpha_eth_sex[(cell_str[j,2]-1) * J_sex + cell_str[j,4]] * lambda_inter[1] * lambda_m[2] * lambda_m[4]* sigma_m+
    alpha_eth_inc[(cell_str[j,2]-1) * J_inc + cell_str[j,5]] * lambda_inter[1] * lambda_m[2] * lambda_m[5]* sigma_m+
    alpha_edu_sex[(cell_str[j,3]-1) * J_sex + cell_str[j,4]] * lambda_inter[1] * lambda_m[3] * lambda_m[4]* sigma_m+
    alpha_edu_inc[(cell_str[j,3]-1) * J_inc + cell_str[j,5]] * lambda_inter[1] * lambda_m[3] * lambda_m[5]* sigma_m+
    alpha_sex_inc[(cell_str[j,4]-1) * J_inc + cell_str[j,5]] * lambda_inter[1] * lambda_m[4] * lambda_m[5]* sigma_m+
    alpha_age_eth_edu[(cell_str[j,1]-1) * J_eth * J_edu + (cell_str[j,2]-1) * J_edu + cell_str[j,3]] * lambda_inter[2] * lambda_m[1] * lambda_m[2] * lambda_m[3]* sigma_m+
    alpha_age_eth_sex[(cell_str[j,1]-1) * J_eth * J_sex + (cell_str[j,2]-1) * J_sex + cell_str[j,4]] * lambda_inter[2] * lambda_m[1] * lambda_m[2] * lambda_m[4]* sigma_m+
    alpha_age_eth_inc[(cell_str[j,1]-1) * J_eth * J_inc + (cell_str[j,2]-1) * J_inc + cell_str[j,5]] * lambda_inter[2] * lambda_m[1] * lambda_m[2] * lambda_m[5]* sigma_m+
    alpha_age_edu_sex[(cell_str[j,1]-1) * J_edu * J_sex + (cell_str[j,3]-1) * J_sex + cell_str[j,4]] * lambda_inter[2] * lambda_m[1] * lambda_m[3] * lambda_m[4]* sigma_m+
    alpha_age_edu_inc[(cell_str[j,1]-1) * J_edu * J_inc + (cell_str[j,3]-1) * J_inc + cell_str[j,5]] * lambda_inter[2] * lambda_m[1] * lambda_m[3] * lambda_m[5]* sigma_m+
    alpha_age_sex_inc[(cell_str[j,1]-1) * J_sex * J_inc + (cell_str[j,4]-1) * J_inc + cell_str[j,5]] * lambda_inter[2] * lambda_m[1] * lambda_m[4] * lambda_m[5]* sigma_m+
    alpha_eth_edu_sex[(cell_str[j,2]-1) * J_edu * J_sex + (cell_str[j,3]-1) * J_sex + cell_str[j,4]] * lambda_inter[2] * lambda_m[2] * lambda_m[3] * lambda_m[4]* sigma_m+
    alpha_eth_edu_inc[(cell_str[j,2]-1) * J_edu * J_inc + (cell_str[j,3]-1) * J_inc + cell_str[j,5]] * lambda_inter[2] * lambda_m[2] * lambda_m[3] * lambda_m[5]* sigma_m+
    alpha_eth_sex_inc[(cell_str[j,2]-1) * J_sex * J_inc + (cell_str[j,4]-1) * J_inc + cell_str[j,5]] * lambda_inter[2] * lambda_m[2] * lambda_m[4] * lambda_m[5]* sigma_m+
    alpha_edu_sex_inc[(cell_str[j,3]-1) * J_sex * J_inc + (cell_str[j,4]-1) * J_inc + cell_str[j,5]] * lambda_inter[2] * lambda_m[3] * lambda_m[4] * lambda_m[5]* sigma_m+
    alpha_age_eth_edu_sex[(cell_str[j,1]-1) * J_eth * J_edu * J_sex + (cell_str[j,2]-1) * J_edu * J_sex + (cell_str[j,3]-1) * J_sex + cell_str[j,4]] * lambda_inter[3] * lambda_m[1] * lambda_m[2] * lambda_m[3]* lambda_m[4]* sigma_m+
    alpha_age_eth_edu_inc[(cell_str[j,1]-1) * J_eth * J_edu * J_inc + (cell_str[j,2]-1) * J_edu * J_inc + (cell_str[j,3]-1) * J_inc + cell_str[j,5]] * lambda_inter[3] * lambda_m[1] * lambda_m[2] * lambda_m[3]* lambda_m[5]* sigma_m+
    alpha_age_eth_sex_inc[(cell_str[j,1]-1) * J_eth * J_sex * J_inc + (cell_str[j,2]-1) * J_sex * J_inc + (cell_str[j,4]-1) * J_inc + cell_str[j,5]] * lambda_inter[3] * lambda_m[1] * lambda_m[2] * lambda_m[4]* lambda_m[5]* sigma_m+
    alpha_age_edu_sex_inc[(cell_str[j,1]-1) * J_edu * J_sex * J_inc + (cell_str[j,3]-1) * J_sex * J_inc + (cell_str[j,4]-1) * J_inc + cell_str[j,5]] * lambda_inter[3] * lambda_m[1] * lambda_m[3] * lambda_m[4]* lambda_m[5]* sigma_m+
    alpha_eth_edu_sex_inc[(cell_str[j,2]-1) * J_edu * J_sex * J_inc + (cell_str[j,3]-1) * J_sex * J_inc + (cell_str[j,4]-1) * J_inc + cell_str[j,5]] * lambda_inter[3] * lambda_m[2] * lambda_m[3] * lambda_m[4]* lambda_m[5]* sigma_m+
    alpha_age_eth_edu_sex_inc[(cell_str[j,1]-1) * J_eth * J_edu * J_sex * J_inc + (cell_str[j,2]-1) * J_edu * J_sex * J_inc + (cell_str[j,3]-1) * J_sex * J_inc + (cell_str[j,4]-1) * J_inc + cell_str[j,5]] * lambda_inter[4] * lambda_m[1] * lambda_m[2] * lambda_m[3]* lambda_m[4]* lambda_m[5]* sigma_m;
  }

 theta_pred <- mu_cell_pred' * N_cell_true / sum(N_cell_true);
}