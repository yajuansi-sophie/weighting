data {
  int<lower=0> n;
  int<lower=0> q;
  int<lower=0> k_grp;
  int<lower=0> cell_id[n];
  int<lower=0> J;
  int<lower=0> y[n]; 
  int<lower=0> age[n];
  int<lower=0>  sex[n];
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
  int<lower=0> eth_wax[n];,
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
  int<lower=0> J_age_eldx_wa;
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
  real<lower=0> sigma_m[q];
  real<lower=0> lambda_iter[q-1];


real<lower=0> sigma_alpha[n_grp];
real<lower=0> sigma_sigma_alpha;
real<lower=0> sigma_beta[3];
real<lower=0> sigma_sigma_beta;
}
transformed parameters{
  real<lower=0> sigma_2way[k_grp_2];
  real<lower=0> sigma_3way[k_grp_3];

}
model {
vector[n] alpha_v_sex;
vector[n] alpha_v_eth;
vector[n] alpha_v_inc;
vector[n] alpha_v_mar;
vector[n] alpha_v_edu;
vector[n] alpha_v_age;
vector[n] alpha_v_pid;
vector[n] alpha_v_prmd;
vector[n] alpha_v_votes;
vector[n] alpha_v_sttgrp;
vector[n] alpha_v_reg;
vector[n] alpha_v_stt;
vector[n] alpha_v_sex_eth;
vector[n] alpha_v_sex_inc;
vector[n] alpha_v_sex_mar;
vector[n] alpha_v_sex_edu;
vector[n] alpha_v_sex_age;
vector[n] alpha_v_sex_pid;
vector[n] alpha_v_sex_prmd;
vector[n] alpha_v_sex_votes;
vector[n] alpha_v_sex_sttgrp;
vector[n] alpha_v_sex_reg;
vector[n] alpha_v_sex_stt;
vector[n] alpha_v_eth_inc;
vector[n] alpha_v_eth_mar;
vector[n] alpha_v_eth_edu;
vector[n] alpha_v_eth_age;
vector[n] alpha_v_eth_pid;
vector[n] alpha_v_eth_prmd;
vector[n] alpha_v_eth_votes;
vector[n] alpha_v_eth_sttgrp;
vector[n] alpha_v_eth_reg;
vector[n] alpha_v_eth_stt;
vector[n] alpha_v_inc_mar;
vector[n] alpha_v_inc_edu;
vector[n] alpha_v_inc_age;
vector[n] alpha_v_inc_pid;
vector[n] alpha_v_inc_prmd;
vector[n] alpha_v_inc_votes;
vector[n] alpha_v_inc_sttgrp;
vector[n] alpha_v_inc_reg;
vector[n] alpha_v_inc_stt;
vector[n] alpha_v_mar_edu;
vector[n] alpha_v_mar_age;
vector[n] alpha_v_mar_pid;
vector[n] alpha_v_mar_prmd;
vector[n] alpha_v_mar_votes;
vector[n] alpha_v_mar_sttgrp;
vector[n] alpha_v_mar_reg;
vector[n] alpha_v_mar_stt;
vector[n] alpha_v_edu_age;
vector[n] alpha_v_edu_pid;
vector[n] alpha_v_edu_prmd;
vector[n] alpha_v_edu_votes;
vector[n] alpha_v_edu_sttgrp;
vector[n] alpha_v_edu_reg;
vector[n] alpha_v_edu_stt;
vector[n] alpha_v_age_pid;
vector[n] alpha_v_age_prmd;
vector[n] alpha_v_age_votes;
vector[n] alpha_v_age_sttgrp;
vector[n] alpha_v_age_reg;
vector[n] alpha_v_age_stt;
vector[n] alpha_v_pid_prmd;
vector[n] alpha_v_pid_votes;
vector[n] alpha_v_pid_sttgrp;
vector[n] alpha_v_pid_reg;
vector[n] alpha_v_pid_stt;
vector[n] alpha_v_prmd_votes;
vector[n] alpha_v_prmd_sttgrp;
vector[n] alpha_v_prmd_reg;
vector[n] alpha_v_prmd_stt;
vector[n] alpha_v_votes_sttgrp;
vector[n] alpha_v_votes_reg;
vector[n] alpha_v_votes_stt;
vector[n] beta_v;

alpha_sex ~ normal(0,1);
alpha_eth ~ normal(0,1);
alpha_inc ~ normal(0,1);
alpha_mar ~ normal(0,1);
alpha_edu ~ normal(0,1);
alpha_age ~ normal(0,1);
alpha_pid ~ normal(0,1);
alpha_prmd ~ normal(0,1);
alpha_votes ~ normal(0,1);
alpha_sttgrp ~ normal(0,1);
alpha_reg ~ normal(0,1);
alpha_stt ~ normal(0,1);
alpha_sex_eth ~ normal(0,1);
alpha_sex_inc ~ normal(0,1);
alpha_sex_mar ~ normal(0,1);
alpha_sex_edu ~ normal(0,1);
alpha_sex_age ~ normal(0,1);
alpha_sex_pid ~ normal(0,1);
alpha_sex_prmd ~ normal(0,1);
alpha_sex_votes ~ normal(0,1);
alpha_sex_sttgrp ~ normal(0,1);
alpha_sex_reg ~ normal(0,1);
alpha_sex_stt ~ normal(0,1);
alpha_eth_inc ~ normal(0,1);
alpha_eth_mar ~ normal(0,1);
alpha_eth_edu ~ normal(0,1);
alpha_eth_age ~ normal(0,1);
alpha_eth_pid ~ normal(0,1);
alpha_eth_prmd ~ normal(0,1);
alpha_eth_votes ~ normal(0,1);
alpha_eth_sttgrp ~ normal(0,1);
alpha_eth_reg ~ normal(0,1);
alpha_eth_stt ~ normal(0,1);
alpha_inc_mar ~ normal(0,1);
alpha_inc_edu ~ normal(0,1);
alpha_inc_age ~ normal(0,1);
alpha_inc_pid ~ normal(0,1);
alpha_inc_prmd ~ normal(0,1);
alpha_inc_votes ~ normal(0,1);
alpha_inc_sttgrp ~ normal(0,1);
alpha_inc_reg ~ normal(0,1);
alpha_inc_stt ~ normal(0,1);
alpha_mar_edu ~ normal(0,1);
alpha_mar_age ~ normal(0,1);
alpha_mar_pid ~ normal(0,1);
alpha_mar_prmd ~ normal(0,1);
alpha_mar_votes ~ normal(0,1);
alpha_mar_sttgrp ~ normal(0,1);
alpha_mar_reg ~ normal(0,1);
alpha_mar_stt ~ normal(0,1);
alpha_edu_age ~ normal(0,1);
alpha_edu_pid ~ normal(0,1);
alpha_edu_prmd ~ normal(0,1);
alpha_edu_votes ~ normal(0,1);
alpha_edu_sttgrp ~ normal(0,1);
alpha_edu_reg ~ normal(0,1);
alpha_edu_stt ~ normal(0,1);
alpha_age_pid ~ normal(0,1);
alpha_age_prmd ~ normal(0,1);
alpha_age_votes ~ normal(0,1);
alpha_age_sttgrp ~ normal(0,1);
alpha_age_reg ~ normal(0,1);
alpha_age_stt ~ normal(0,1);
alpha_pid_prmd ~ normal(0,1);
alpha_pid_votes ~ normal(0,1);
alpha_pid_sttgrp ~ normal(0,1);
alpha_pid_reg ~ normal(0,1);
alpha_pid_stt ~ normal(0,1);
alpha_prmd_votes ~ normal(0,1);
alpha_prmd_sttgrp ~ normal(0,1);
alpha_prmd_reg ~ normal(0,1);
alpha_prmd_stt ~ normal(0,1);
alpha_votes_sttgrp ~ normal(0,1);
alpha_votes_reg ~ normal(0,1);
alpha_votes_stt ~ normal(0,1);

beta_eth ~ normal(0,1);
beta_pid ~ normal(0,1);
beta_prmd ~ normal(0,1);
sigma_alpha ~ student_t(8,0,1);
sigma_beta ~ student_t(8,0,1);

for (i_n in 1:n) {
alpha_v_sex[i_n] <- alpha_sex[sex[i_n]];
alpha_v_eth[i_n] <- alpha_eth[eth[i_n]];
alpha_v_inc[i_n] <- alpha_inc[inc[i_n]];
alpha_v_mar[i_n] <- alpha_mar[mar[i_n]];
alpha_v_edu[i_n] <- alpha_edu[edu[i_n]];
alpha_v_age[i_n] <- alpha_age[age[i_n]];
alpha_v_pid[i_n] <- alpha_pid[pid[i_n]];
alpha_v_prmd[i_n] <- alpha_prmd[prmd[i_n]];
alpha_v_votes[i_n] <- alpha_votes[votes[i_n]];
alpha_v_sttgrp[i_n] <- alpha_sttgrp[sttgrp[i_n]];
alpha_v_reg[i_n] <- alpha_reg[reg[i_n]];
alpha_v_stt[i_n] <- alpha_stt[stt[i_n]];
alpha_v_sex_eth[i_n] <- alpha_sex_eth[sex_eth[i_n]];
alpha_v_sex_inc[i_n] <- alpha_sex_inc[sex_inc[i_n]];
alpha_v_sex_mar[i_n] <- alpha_sex_mar[sex_mar[i_n]];
alpha_v_sex_edu[i_n] <- alpha_sex_edu[sex_edu[i_n]];
alpha_v_sex_age[i_n] <- alpha_sex_age[sex_age[i_n]];
alpha_v_sex_pid[i_n] <- alpha_sex_pid[sex_pid[i_n]];
alpha_v_sex_prmd[i_n] <- alpha_sex_prmd[sex_prmd[i_n]];
alpha_v_sex_votes[i_n] <- alpha_sex_votes[sex_votes[i_n]];
alpha_v_sex_sttgrp[i_n] <- alpha_sex_sttgrp[sex_sttgrp[i_n]];
alpha_v_sex_reg[i_n] <- alpha_sex_reg[sex_reg[i_n]];
alpha_v_sex_stt[i_n] <- alpha_sex_stt[sex_stt[i_n]];
alpha_v_eth_inc[i_n] <- alpha_eth_inc[eth_inc[i_n]];
alpha_v_eth_mar[i_n] <- alpha_eth_mar[eth_mar[i_n]];
alpha_v_eth_edu[i_n] <- alpha_eth_edu[eth_edu[i_n]];
alpha_v_eth_age[i_n] <- alpha_eth_age[eth_age[i_n]];
alpha_v_eth_pid[i_n] <- alpha_eth_pid[eth_pid[i_n]];
alpha_v_eth_prmd[i_n] <- alpha_eth_prmd[eth_prmd[i_n]];
alpha_v_eth_votes[i_n] <- alpha_eth_votes[eth_votes[i_n]];
alpha_v_eth_sttgrp[i_n] <- alpha_eth_sttgrp[eth_sttgrp[i_n]];
alpha_v_eth_reg[i_n] <- alpha_eth_reg[eth_reg[i_n]];
alpha_v_eth_stt[i_n] <- alpha_eth_stt[eth_stt[i_n]];
alpha_v_inc_mar[i_n] <- alpha_inc_mar[inc_mar[i_n]];
alpha_v_inc_edu[i_n] <- alpha_inc_edu[inc_edu[i_n]];
alpha_v_inc_age[i_n] <- alpha_inc_age[inc_age[i_n]];
alpha_v_inc_pid[i_n] <- alpha_inc_pid[inc_pid[i_n]];
alpha_v_inc_prmd[i_n] <- alpha_inc_prmd[inc_prmd[i_n]];
alpha_v_inc_votes[i_n] <- alpha_inc_votes[inc_votes[i_n]];
alpha_v_inc_sttgrp[i_n] <- alpha_inc_sttgrp[inc_sttgrp[i_n]];
alpha_v_inc_reg[i_n] <- alpha_inc_reg[inc_reg[i_n]];
alpha_v_inc_stt[i_n] <- alpha_inc_stt[inc_stt[i_n]];
alpha_v_mar_edu[i_n] <- alpha_mar_edu[mar_edu[i_n]];
alpha_v_mar_age[i_n] <- alpha_mar_age[mar_age[i_n]];
alpha_v_mar_pid[i_n] <- alpha_mar_pid[mar_pid[i_n]];
alpha_v_mar_prmd[i_n] <- alpha_mar_prmd[mar_prmd[i_n]];
alpha_v_mar_votes[i_n] <- alpha_mar_votes[mar_votes[i_n]];
alpha_v_mar_sttgrp[i_n] <- alpha_mar_sttgrp[mar_sttgrp[i_n]];
alpha_v_mar_reg[i_n] <- alpha_mar_reg[mar_reg[i_n]];
alpha_v_mar_stt[i_n] <- alpha_mar_stt[mar_stt[i_n]];
alpha_v_edu_age[i_n] <- alpha_edu_age[edu_age[i_n]];
alpha_v_edu_pid[i_n] <- alpha_edu_pid[edu_pid[i_n]];
alpha_v_edu_prmd[i_n] <- alpha_edu_prmd[edu_prmd[i_n]];
alpha_v_edu_votes[i_n] <- alpha_edu_votes[edu_votes[i_n]];
alpha_v_edu_sttgrp[i_n] <- alpha_edu_sttgrp[edu_sttgrp[i_n]];
alpha_v_edu_reg[i_n] <- alpha_edu_reg[edu_reg[i_n]];
alpha_v_edu_stt[i_n] <- alpha_edu_stt[edu_stt[i_n]];
alpha_v_age_pid[i_n] <- alpha_age_pid[age_pid[i_n]];
alpha_v_age_prmd[i_n] <- alpha_age_prmd[age_prmd[i_n]];
alpha_v_age_votes[i_n] <- alpha_age_votes[age_votes[i_n]];
alpha_v_age_sttgrp[i_n] <- alpha_age_sttgrp[age_sttgrp[i_n]];
alpha_v_age_reg[i_n] <- alpha_age_reg[age_reg[i_n]];
alpha_v_age_stt[i_n] <- alpha_age_stt[age_stt[i_n]];
alpha_v_pid_prmd[i_n] <- alpha_pid_prmd[pid_prmd[i_n]];
alpha_v_pid_votes[i_n] <- alpha_pid_votes[pid_votes[i_n]];
alpha_v_pid_sttgrp[i_n] <- alpha_pid_sttgrp[pid_sttgrp[i_n]];
alpha_v_pid_reg[i_n] <- alpha_pid_reg[pid_reg[i_n]];
alpha_v_pid_stt[i_n] <- alpha_pid_stt[pid_stt[i_n]];
alpha_v_prmd_votes[i_n] <- alpha_prmd_votes[prmd_votes[i_n]];
alpha_v_prmd_sttgrp[i_n] <- alpha_prmd_sttgrp[prmd_sttgrp[i_n]];
alpha_v_prmd_reg[i_n] <- alpha_prmd_reg[prmd_reg[i_n]];
alpha_v_prmd_stt[i_n] <- alpha_prmd_stt[prmd_stt[i_n]];
alpha_v_votes_sttgrp[i_n] <- alpha_votes_sttgrp[votes_sttgrp[i_n]];
alpha_v_votes_reg[i_n] <- alpha_votes_reg[votes_reg[i_n]];
alpha_v_votes_stt[i_n] <- alpha_votes_stt[votes_stt[i_n]];
beta_v[i_n] <- beta + 
beta_eth[eth[i_n]] * sigma_sigma_beta * sigma_beta[1] + 
beta_pid[pid[i_n]] * sigma_sigma_beta * sigma_beta[2] +
beta_prmd[prmd[i_n]] * sigma_sigma_beta * sigma_beta[3];
}

y ~ bernoulli_logit(alpha +
alpha_v_sex * (sigma_alpha[1] * sigma_sigma_alpha) +
alpha_v_eth * (sigma_alpha[2] * sigma_sigma_alpha) +
alpha_v_inc * (sigma_alpha[3] * sigma_sigma_alpha) +
alpha_v_mar * (sigma_alpha[4] * sigma_sigma_alpha) +
alpha_v_edu * (sigma_alpha[5] * sigma_sigma_alpha) +
alpha_v_age * (sigma_alpha[6] * sigma_sigma_alpha) +
alpha_v_pid * (sigma_alpha[7] * sigma_sigma_alpha) +
alpha_v_prmd * (sigma_alpha[8] * sigma_sigma_alpha) +
alpha_v_votes * (sigma_alpha[9] * sigma_sigma_alpha) +
alpha_v_sttgrp * (sigma_alpha[10] * sigma_sigma_alpha) +
alpha_v_reg * (sigma_alpha[11] * sigma_sigma_alpha) +
alpha_v_stt * (sigma_alpha[12] * sigma_sigma_alpha) +
alpha_v_sex_eth * (sigma_alpha[13] * sigma_sigma_alpha) +
alpha_v_sex_inc * (sigma_alpha[14] * sigma_sigma_alpha) +
alpha_v_sex_mar * (sigma_alpha[15] * sigma_sigma_alpha) +
alpha_v_sex_edu * (sigma_alpha[16] * sigma_sigma_alpha) +
alpha_v_sex_age * (sigma_alpha[17] * sigma_sigma_alpha) +
alpha_v_sex_pid * (sigma_alpha[18] * sigma_sigma_alpha) +
alpha_v_sex_prmd * (sigma_alpha[19] * sigma_sigma_alpha) +
alpha_v_sex_votes * (sigma_alpha[20] * sigma_sigma_alpha) +
alpha_v_sex_sttgrp * (sigma_alpha[21] * sigma_sigma_alpha) +
alpha_v_sex_reg * (sigma_alpha[22] * sigma_sigma_alpha) +
alpha_v_sex_stt * (sigma_alpha[23] * sigma_sigma_alpha) +
alpha_v_eth_inc * (sigma_alpha[24] * sigma_sigma_alpha) +
alpha_v_eth_mar * (sigma_alpha[25] * sigma_sigma_alpha) +
alpha_v_eth_edu * (sigma_alpha[26] * sigma_sigma_alpha) +
alpha_v_eth_age * (sigma_alpha[27] * sigma_sigma_alpha) +
alpha_v_eth_pid * (sigma_alpha[28] * sigma_sigma_alpha) +
alpha_v_eth_prmd * (sigma_alpha[29] * sigma_sigma_alpha) +
alpha_v_eth_votes * (sigma_alpha[30] * sigma_sigma_alpha) +
alpha_v_eth_sttgrp * (sigma_alpha[31] * sigma_sigma_alpha) +
alpha_v_eth_reg * (sigma_alpha[32] * sigma_sigma_alpha) +
alpha_v_eth_stt * (sigma_alpha[33] * sigma_sigma_alpha) +
alpha_v_inc_mar * (sigma_alpha[34] * sigma_sigma_alpha) +
alpha_v_inc_edu * (sigma_alpha[35] * sigma_sigma_alpha) +
alpha_v_inc_age * (sigma_alpha[36] * sigma_sigma_alpha) +
alpha_v_inc_pid * (sigma_alpha[37] * sigma_sigma_alpha) +
alpha_v_inc_prmd * (sigma_alpha[38] * sigma_sigma_alpha) +
alpha_v_inc_votes * (sigma_alpha[39] * sigma_sigma_alpha) +
alpha_v_inc_sttgrp * (sigma_alpha[40] * sigma_sigma_alpha) +
alpha_v_inc_reg * (sigma_alpha[41] * sigma_sigma_alpha) +
alpha_v_inc_stt * (sigma_alpha[42] * sigma_sigma_alpha) +
alpha_v_mar_edu * (sigma_alpha[43] * sigma_sigma_alpha) +
alpha_v_mar_age * (sigma_alpha[44] * sigma_sigma_alpha) +
alpha_v_mar_pid * (sigma_alpha[45] * sigma_sigma_alpha) +
alpha_v_mar_prmd * (sigma_alpha[46] * sigma_sigma_alpha) +
alpha_v_mar_votes * (sigma_alpha[47] * sigma_sigma_alpha) +
alpha_v_mar_sttgrp * (sigma_alpha[48] * sigma_sigma_alpha) +
alpha_v_mar_reg * (sigma_alpha[49] * sigma_sigma_alpha) +
alpha_v_mar_stt * (sigma_alpha[50] * sigma_sigma_alpha) +
alpha_v_edu_age * (sigma_alpha[51] * sigma_sigma_alpha) +
alpha_v_edu_pid * (sigma_alpha[52] * sigma_sigma_alpha) +
alpha_v_edu_prmd * (sigma_alpha[53] * sigma_sigma_alpha) +
alpha_v_edu_votes * (sigma_alpha[54] * sigma_sigma_alpha) +
alpha_v_edu_sttgrp * (sigma_alpha[55] * sigma_sigma_alpha) +
alpha_v_edu_reg * (sigma_alpha[56] * sigma_sigma_alpha) +
alpha_v_edu_stt * (sigma_alpha[57] * sigma_sigma_alpha) +
alpha_v_age_pid * (sigma_alpha[58] * sigma_sigma_alpha) +
alpha_v_age_prmd * (sigma_alpha[59] * sigma_sigma_alpha) +
alpha_v_age_votes * (sigma_alpha[60] * sigma_sigma_alpha) +
alpha_v_age_sttgrp * (sigma_alpha[61] * sigma_sigma_alpha) +
alpha_v_age_reg * (sigma_alpha[62] * sigma_sigma_alpha) +
alpha_v_age_stt * (sigma_alpha[63] * sigma_sigma_alpha) +
alpha_v_pid_prmd * (sigma_alpha[64] * sigma_sigma_alpha) +
alpha_v_pid_votes * (sigma_alpha[65] * sigma_sigma_alpha) +
alpha_v_pid_sttgrp * (sigma_alpha[66] * sigma_sigma_alpha) +
alpha_v_pid_reg * (sigma_alpha[67] * sigma_sigma_alpha) +
alpha_v_pid_stt * (sigma_alpha[68] * sigma_sigma_alpha) +
alpha_v_prmd_votes * (sigma_alpha[69] * sigma_sigma_alpha) +
alpha_v_prmd_sttgrp * (sigma_alpha[70] * sigma_sigma_alpha) +
alpha_v_prmd_reg * (sigma_alpha[71] * sigma_sigma_alpha) +
alpha_v_prmd_stt * (sigma_alpha[72] * sigma_sigma_alpha) +
alpha_v_votes_sttgrp * (sigma_alpha[73] * sigma_sigma_alpha) +
alpha_v_votes_reg * (sigma_alpha[74] * sigma_sigma_alpha) +
alpha_v_votes_stt * (sigma_alpha[75] * sigma_sigma_alpha) +
beta_v .* z_dem2wayprev +
Z * beta_z);
}
