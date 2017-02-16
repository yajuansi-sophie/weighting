###---------------MRP to Construct Survey Weights--------------###
###Author: YS
###Latest Edit date: 04/20/2015
###clear
remove(list=objects())
###code
#recode arm:rescale
#----------required packages-----------------#
#require(arm)
#require(car)
require(foreign)
require(rstan)
require(survey)
require(foreach)
require(doMC)
source("http://mc-stan.org/rstan/stan.R")
set.seed(20150420)

#-----------function defination-------------#
ci_50 <- function(x){quantile(x,c(0.25,0.75))}
ci_80 <- function(x){quantile(x,c(0.1,0.9))}
ci_95 <- function(x){quantile(x,c(0.025,0.975))}
#-----------computation--------------#
load("simulateddata.RData")
###cell id
n<-dim(dat)[1] #sample size
q<-3 
###model on the cross-tabbed cells 
J_age <- length(unique(dat$age))
J_eth <- length(unique(dat$eth))
J_edu <- length(unique(dat$edu))

cell_id<-rep(0,n)
cell_str<-matrix(0,J_age*J_eth*J_edu,q)
 j<-0
for (i3 in unique(dat$edu)){
  for (i2 in unique(dat$eth)){
    for (i1 in unique(dat$age)){
                j<- (i3-1) * J_eth * J_age + (i2-1) * J_age + i1
                cell_id[dat$age==i1& dat$eth==i2 & dat$edu==i3]<-j
                cell_str[j,]<-c(i1,i2,i3)
    }
  }
}
J<-length(unique(cell_id))
n_cell <- as.numeric(table(cell_id))   
N_cell<-acs_N

R<-1

mu_cell<-rep(0,J)
sigma_y_cell<-rep(0,J)
cover_priorvar50<-rep(0,R)
cover_lambda50<-matrix(0,R,q-1)
cover_sigma50<-matrix(0,R,q)
cover_sigmay50<-rep(0,R)
cover_priorvar80<-rep(0,R)
cover_lambda80<-matrix(0,R,q-1)
cover_sigma80<-matrix(0,R,q)
cover_sigmay80<-rep(0,R)
cover_priorvar95<-rep(0,R)
cover_lambda95<-matrix(0,R,q-1)
cover_sigma95<-matrix(0,R,q)
cover_sigmay95<-rep(0,R)
cover_mu50<-matrix(0,R,J)
cover_mu80<-matrix(0,R,J)
cover_mu95<-matrix(0,R,J)

sd_bias<-matrix(0,R,2*q+1)
mu_bias<-matrix(0,R,J)

lambda_m <- 1

for (r in 1:R){
  sigma_y <- abs(rt(1,1,0))
  sigma_m <- abs(rt(q,1,0))
  lambda_inter <-abs(rt(q-1,1,0))

  pri_var <- sum(lambda_m^2 * sigma_m^2) + (lambda_inter[1] * sigma_m[1] * sigma_m[2])^2 +
    (lambda_inter[1] * sigma_m[1] * sigma_m[3])^2 + (lambda_inter[1] * sigma_m[2] * sigma_m[3])^2 +
    (lambda_inter[2] * sigma_m[1] * sigma_m[2]* sigma_m[3])^2
  
  alpha<-rnorm(1)
  alpha_age <-rnorm(J_age); alpha_eth <-rnorm(J_eth); alpha_edu <- rnorm(J_edu);
  alpha_age_eth<-rnorm(J_age * J_eth); alpha_age_edu <-rnorm(J_age * J_edu);
  alpha_eth_edu <-rnorm(J_eth * J_edu); alpha_age_eth_edu <-rnorm(J_age * J_eth * J_edu)
  
  for (j in 1:J){
    mu_cell[j] <- alpha + alpha_age[cell_str[j,1]] * lambda_m * sigma_m[1] + 
      alpha_eth[cell_str[j,2]] * lambda_m * sigma_m[2] + 
      alpha_edu[cell_str[j,3]] * lambda_m * sigma_m[3] +
      alpha_age_eth[(cell_str[j,1]-1) * J_eth + cell_str[j,2]] * lambda_inter[1] * sigma_m[1] * sigma_m[2]+
      alpha_age_edu[(cell_str[j,1]-1) * J_edu + cell_str[j,3]] * lambda_inter[1] * sigma_m[1] * sigma_m[3]+
      alpha_eth_edu[(cell_str[j,2]-1) * J_edu + cell_str[j,3]] * lambda_inter[1] * sigma_m[2] * sigma_m[3]+
      alpha_age_eth_edu[(cell_str[j,1]-1) * J_eth * J_edu + (cell_str[j,2]-1) * J_edu + cell_str[j,3]] * lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[3];
  }
  
  for (j in 1:J){
    sigma_y_cell[j] <- sigma_y / sqrt(n_cell[j]);
  }

  y_cell <-rnorm(J,mu_cell, sigma_y_cell);
  ss_cell <-rchisq(1, n-1) * sigma_y^2;
  

###-----------------STAN--------------------------###

stan.data_cell <- list(n=nrow(dat),q=q,
                  J=J, n_cell=n_cell, y_cell=y_cell, ss_cell=ss_cell,
                  cell_str=cell_str,N_cell=N_cell,
                  J_age=length(unique(dat$age)),
                  J_eth=length(unique(dat$eth)),
                  J_edu=length(unique(dat$edu)),
                  J_age_eth=length(unique(dat$age)) * length(unique(dat$eth)),
                  J_age_edu=length(unique(dat$age)) * length(unique(dat$edu)),
                  J_eth_edu=length(unique(dat$eth)) * length(unique(dat$edu)),
                  J_age_eth_edu=length(unique(dat$age)) * length(unique(dat$eth)) * length(unique(dat$edu))
)

set_cppo("debug")
S.compile_cell <- stan(file='mrpweights041715a.stan',
                       data=stan.data_cell,
                       iter=2, chains=1)
stan.seed <- round(runif(1,0,9999999))
n.chains <- 3
registerDoMC(n.chains)
st <- system.time(sflist1 <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
  S <- stan(fit=S.compile_cell,
            data=stan.data_cell,
            iter=500, chains=1, seed=stan.seed, chain_id=i.cores)
  return(S)
})[3]
S <- sflist2stanfit(sflist1)

# print(S,digits=3)
output<-extract(S, permuted=TRUE)

if (ci_50(output$pri_var)[1] <= pri_var & pri_var <= ci_50(output$pri_var)[2]){
  cover_priorvar50[r]<-1
}

if (ci_50(output$lambda_inter[,1])[1] <= lambda_inter[1] & lambda_inter[1] <= ci_50(output$lambda_inter[,1])[2]){
  cover_lambda50[r,1]<-1
}
if (ci_50(output$lambda_inter[,2])[1] <= lambda_inter[2] & lambda_inter[2] <= ci_50(output$lambda_inter[,2])[2]){
  cover_lambda50[r,2]<-1
}
for (j in 1:q){
  if (ci_50(output$sigma_m[,j])[1] <= sigma_m[j] & sigma_m[j] <= ci_50(output$sigma_m[,j])[2]){
    cover_sigma50[r,j]<-1
  }
}
if (ci_50(output$sigma_y)[1] <= sigma_y & sigma_y <= ci_50(output$sigma_y)[2]){
  cover_sigmay50[r]<-1
}

if (ci_80(output$pri_var)[1] <= pri_var & pri_var <= ci_80(output$pri_var)[2]){
  cover_priorvar80[r]<-1
}

if (ci_80(output$lambda_inter[,1])[1] <= lambda_inter[1] & lambda_inter[1] <= ci_80(output$lambda_inter[,1])[2]){
  cover_lambda80[r,1]<-1
}
if (ci_80(output$lambda_inter[,2])[1] <= lambda_inter[2] & lambda_inter[2] <= ci_80(output$lambda_inter[,2])[2]){
  cover_lambda80[r,2]<-1
}
for (j in 1:q){
  if (ci_80(output$sigma_m[,j])[1] <= sigma_m[j] & sigma_m[j] <= ci_80(output$sigma_m[,j])[2]){
    cover_sigma80[r,j]<-1
  }
}
if (ci_80(output$sigma_y)[1] <= sigma_y & sigma_y <= ci_80(output$sigma_y)[2]){
  cover_sigmay80[r]<-1
}
if (ci_95(output$pri_var)[1] <= pri_var & pri_var <= ci_95(output$pri_var)[2]){
  cover_priorvar95[r]<-1
}

if (ci_95(output$lambda_inter[,1])[1] <= lambda_inter[1] & lambda_inter[1] <= ci_95(output$lambda_inter[,1])[2]){
  cover_lambda95[r,1]<-1
}
if (ci_95(output$lambda_inter[,2])[1] <= lambda_inter[2] & lambda_inter[2] <= ci_95(output$lambda_inter[,2])[2]){
  cover_lambda95[r,2]<-1
}
for (j in 1:q){
  if (ci_95(output$sigma_m[,j])[1] <= sigma_m[j] & sigma_m[j] <= ci_95(output$sigma_m[,j])[2]){
    cover_sigma95[r,j]<-1
  }
}
if (ci_95(output$sigma_y)[1] <= sigma_y & sigma_y <= ci_95(output$sigma_y)[2]){
  cover_sigmay95[r]<-1
}

for (j in 1:J){
  if (apply(output$mu_cell,2,ci_50)[1,j] <= mu_cell[j] & mu_cell[j] <= apply(output$mu_cell,2,ci_50)[2,j]){
    cover_mu50[r,j]<-1
  }
  if (apply(output$mu_cell,2,ci_80)[1,j] <= mu_cell[j] & mu_cell[j] <= apply(output$mu_cell,2,ci_80)[2,j]){
    cover_mu80[r,j]<-1
  }
  if (apply(output$mu_cell,2,ci_95)[1,j] <= mu_cell[j] & mu_cell[j] <= apply(output$mu_cell,2,ci_95)[2,j]){
    cover_mu95[r,j]<-1
  }  
}

sd_bias[r,]<-c(mean(output$sigma_y)-sigma_y, apply(output$sigma_m,2,mean)-sigma_m, 
               apply(output$lambda_inter,2,mean)-lambda_inter,mean(output$pri_var)-pri_var)
mu_bias[r,]<-apply(output$mu_cell,2,mean)-mu_cell
}

save.image("check_mrp_20150420a.RData")

#load("check_mrp_20150420a.RData")
# apply(cover_priorvar50,2,mean)
# apply(cover_lambda50,2,mean)
# apply(cover_sigma50,2,mean)
# mean(cover_sigmay50)
# apply(cover_priorvar80,2,mean)
# apply(cover_lambda80,2,mean)
# apply(cover_sigma80,2,mean)
# mean(cover_sigmay80)
# apply(cover_priorvar95,2,mean)
# apply(cover_lambda95,2,mean)
# apply(cover_sigma95,2,mean)
# mean(cover_sigmay95)
# apply(cover_mu50,2,mean)
# apply(cover_mu80,2,mean)
# apply(cover_mu95,2,mean)
# 
# apply(mu_bias,2,mean)
# apply(sd_bias,2,mean)
############################################################################################################
