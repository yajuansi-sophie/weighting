data {
int<lower=1> N;
vector[N] age;
vector[N] sex;
vector[N] race;
int<lower=0> z[N];
int<lower=0> n[N];
}
transformed data {
vector[N] mu;
mu <- rep_vector(0, N);
}
parameters {
vector[N] y;
real<lower=0> eta_sq_age;
real<lower=0> rho_sq_age;
real<lower=0> rho_sq_sex;
real<lower=0> rho_sq_race;
}
model {
matrix[N,N] Sigma;
matrix[N,N] L;
for (i in 1:(N-1)) {
for (j in (i+1):N) {
Sigma[i,j] <- eta_sq_age * exp(-rho_sq_age * square(age[i] - age[j]) -rho_sq_sex * (sex[i] != sex[j]) -rho_sq_race * (race[i] != race[j]));
Sigma[j,i] <- Sigma[i,j];
}
Sigma[i,i] <- eta_sq_age;
}
Sigma[N,N] <- eta_sq_age;
L <- cholesky_decompose(Sigma);
y ~ multi_normal_cholesky(mu,L);
eta_sq_age ~ exponential(5);//cauchy(0,5);
rho_sq_age ~ exponential(5);//cauchy(0,5);
rho_sq_sex ~ exponential(5);//cauchy(0,5);
rho_sq_race ~ exponential(5);//cauchy(0,5);
z ~ binomial_logit(n, y);
}
//generated quantities {
//  vector[N] log_lik;
//  for (i in 1:N) {
//    log_lik[i] <- binomial_logit_log(z[i],n[i],y[i]);
//  }
//}
