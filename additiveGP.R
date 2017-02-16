

model <- '
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
'

setwd("/Users/evanblumgart/Desktop/Gaussian Process Research")

library(rstan)
df <- read.csv('naes04.csv')
df <- df[-is.na(df$race),]
df$gender <- as.numeric(df$gender)-1 # female = 0, male = 1
df$race <- as.numeric(df$race)-1 # Black = 0, Hispanic = 1, Other = 2, White = 3
#df$gayFavorFederalMarriage <- as.numeric(df$gayFavorFederalMarriage)-1 # No = 0, Yes = 1 
df$gayFavorStateMarriage <- as.numeric(df$gayFavorStateMarriage)-1 # No = 0, Yes = 1
df$gayKnowSomeone <- as.numeric(df$gayKnowSomeone)-1 # No = 0, Yes = 1
# remove missing data
miss <- attributes(na.omit(df[,1:4]))[4]
miss <- as.numeric(unlist(miss))
df <- df[-miss,]
# sort data
df <- df[with(df,order(df$gayKnowSomeone)),]

df <- df[1:min(which(is.na(df$gayKnowSomeone))),]
df <- df[sample(1:dim(df)[1],dim(df)[1],replace=T),]

# run model
z <- age <- n <- tmp <- rep(seq(20,90,by=5),8)
sex <- c(rep(0,length(tmp)/2),rep(1,length(tmp)/2))
race <- rep(c(rep(0,length(tmp)/8),rep(1,length(tmp)/8),rep(2,length(tmp)/8),rep(3,length(tmp)/8)),2)
for (i in 1:(length(tmp)/2)) {
  n[i] <- length(which((df$age==(tmp[i]-2)|df$age==(tmp[i]-1)|df$age==(tmp[i])|df$age==(tmp[i]+1)|df$age==(tmp[i]+2)) & (df$gender==0) & (df$race==race[i])))
  z[i] <- sum(df$gayFavorStateMarriage[which((df$age==(tmp[i]-2)|df$age==(tmp[i]-1)|df$age==(tmp[i])|df$age==(tmp[i]+1)|df$age==(tmp[i]+2)) & (df$gender==0) & (df$race==race[i]))])
  age[i] <- mean(df$age[which((df$age==(tmp[i]-2)|df$age==(tmp[i]-1)|df$age==(tmp[i])|df$age==(tmp[i]+1)|df$age==(tmp[i]+2)) & (df$gender==0) & (df$race==race[i]))])
}
for (i in (length(tmp)/2+1):length(tmp)) {
  for (j in 0:3) {
    n[i] <- length(which((df$age==(tmp[i]-2)|df$age==(tmp[i]-1)|df$age==(tmp[i])|df$age==(tmp[i]+1)|df$age==(tmp[i]+2)) & (df$gender==1) & (df$race==race[i])))
    z[i] <- sum(df$gayFavorStateMarriage[which((df$age==(tmp[i]-2)|df$age==(tmp[i]-1)|df$age==(tmp[i])|df$age==(tmp[i]+1)|df$age==(tmp[i]+2)) & (df$gender==1) & (df$race==race[i]))])
    age[i] <- mean(df$age[which((df$age==(tmp[i]-2)|df$age==(tmp[i]-1)|df$age==(tmp[i])|df$age==(tmp[i]+1)|df$age==(tmp[i]+2)) & (df$gender==1) & (df$race==race[i]))])
  }
}
N <- length(tmp)

rem <- which(n==0)
age <- age[-rem]
sex <- sex[-rem]
race <- race[-rem]
z <- z[-rem]
n <- n[-rem]
N <- N - length(rem)

dat <- list(N=N, age=age, sex=sex, race=race, z=z, n=n)

fit2 <- stan(model_code = model, data = dat, 
             iter = 100, chains = 1)

out <- extract(fit2)
library(boot)
library(matrixStats)
mean <- colMeans(out$y)
title <- c('Black','Hispanic','Other','White')
par(mfrow=c(2,4))
for (i in 0:3) {
  plot(age[which(sex==0&race==i)],100*inv.logit(mean)[which(sex==0&race==i)],ylim=c(0,100),pch=4,type='l',col='blue',
       xlab='Age (years)',ylab='Support for Gay Marriage (%)',main=title[i+1])
  lines(age[which(sex==1&race==i)],100*inv.logit(mean)[which(sex==1&race==i)],ylim=c(0,100),pch=4,col='red')
  gamma <- (n[which(sex==0&race==i)]-min(n))/(max(n)-min(n))
  points(age[which(sex==0&race==i)],100*z[which(sex==0&race==i)]/n[which(sex==0&race==i)],pch=1,col='blue',cex=5*gamma+0.2*(1-gamma))
  gamma <- (n[which(sex==1&race==i)]-min(n))/(max(n)-min(n))
  points(age[which(sex==1&race==i)],100*z[which(sex==1&race==i)]/n[which(sex==1&race==i)],pch=1,col='red',cex=5*gamma+0.2*(1-gamma))
  legend(60,85,c("female","male"),lty=c(1,1),lwd=c(1,1),col=c("blue","red"))
}



# calculate WAIC
colVars <- function(a) {
  vars <- c();
  for (n in 1:ncol(a)) vars[n] <- var(a[,n]);
  return(vars);
}
waic <- function(fit) {
  log_lik <- extract(fit, "log_lik")$log_lik
  dim(log_lik) <- if (length(dim(log_lik))==1) c(length(log_lik),1) else
    c(dim(log_lik)[1], prod(dim(log_lik)[2:length(dim(log_lik))]))
  lppd <- sum(log(colMeans(exp(log_lik))))
  p_waic_1 <- 2*sum(log(colMeans(exp(log_lik))) - colMeans(log_lik))
  p_waic_2 <- sum(colVars(log_lik))
  waic_2 <- -2*lppd + 2*p_waic_2
  return(list(waic=waic_2, p_waic=p_waic_2, lppd=lppd, p_waic_1=p_waic_1))
}