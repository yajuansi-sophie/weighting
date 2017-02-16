###---------------MRP to Construct Survey Weights--------------###
###Author: YS
###Latest Edit date: 07/19/2015
#setwd("/Users/Shared/GoogleDriveFolder/working/weighting/code")
#setwd("~/Google Drive/working/weighting/code")
setwd("~/Documents/weighting/code")
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
set.seed(20150213)

#----------data simulation-----------------#
data<-read.dta("data/SRBIandAGENCYbaselinew1w2w3datawithwghts072114.dta")
SRBIdata<-data[data$sample=="SRBI",]
###ACS data
acs_pop<-read.dta("data/acs_nyc_2011_wpov1.dta",convert.factors = FALSE)
acs_ad <- acs_pop[as.numeric(acs_pop$age)>=18,] 
##age
age_tr<-acs_ad$age #at least 18
##
acs_ad$age_dc<-age_tr #discretization
acs_ad$age_dc[age_tr<=34]<-1 #18-34
acs_ad$age_dc[age_tr<=64 & age_tr>34]<-2 #35-64
acs_ad$age_dc[age_tr>=64]<-3 #65+

SRBIdata$age_dc<-SRBIdata$age
SRBIdata$age_dc[SRBIdata$age<=34]<-1
SRBIdata$age_dc[SRBIdata$age<=64 & SRBIdata$age > 34]<-2
SRBIdata$age_dc[SRBIdata$age > 64]<-3

###sex
acs_ad$sex<-as.numeric(acs_ad$sex)
SRBIdata$sex<-as.numeric(as.factor(SRBIdata$r_gender))

###race
acs_ad$race_dc<-acs_ad$racex
acs_ad$race_dc[acs_ad$racex==4]<-3
acs_ad$race_dc[acs_ad$racex==5]<-4

SRBIdata$race_dc<-SRBIdata$race
SRBIdata$race_dc[SRBIdata$race==4]<-3
SRBIdata$race_dc[SRBIdata$race==5]<-4

#edu
#educat 4
#recode qi5 (1/2=1) (3/4=2) (5/6=3) (7/8=4) (98/99=.), gen(educat)
SRBIdata$educat<-SRBIdata$edu

#tot.income
#poverty gap: 1 Under 50%   2 50-100%  3 100-200%  4 200-300%     5 300%+
acs_ad$opmres_x<-acs_ad$povgap
SRBIdata$opmres_x<-1
SRBIdata$opmres_x[SRBIdata$povgap=="3 100-200%"]<-2
SRBIdata$opmres_x[SRBIdata$povgap=="4 200-300%"]<-3
SRBIdata$opmres_x[SRBIdata$povgap=="5 300%+"]<-3

#eldx
acs_ad$eldx_ca<-acs_ad$eldx # 0 1 2+
acs_ad$eldx_ca[acs_ad$eldx>1]<-2
SRBIdata$eldx_ca<-SRBIdata$eldx
SRBIdata$eldx_ca[SRBIdata$eldx>1]<-2


#childx 0 1 2 3
acs_ad$childx_ca<-acs_ad$childx
acs_ad$childx_ca[acs_ad$childx_ca>2]<-3
SRBIdata$childx_ca<-SRBIdata$childx
SRBIdata$childx_ca[SRBIdata$childx_ca>2]<-3

#wax 0 1 2
acs_ad$wax_ca<-acs_ad$wax
acs_ad$wax_ca[acs_ad$wax>1]<-2
SRBIdata$wax_ca<-SRBIdata$wax
SRBIdata$wax_ca[SRBIdata$wax_ca>1]<-2

#personx 1-4

acs_ad$personx_ca<-acs_ad$personx
acs_ad$personx_ca[acs_ad$personx>4]<-4
SRBIdata$personx_ca<-SRBIdata$personx
SRBIdata$personx_ca[SRBIdata$personx_ca>4]<-4

#use THE WEIGHTS OF ACS ITSELF
acs_ds_ad <-svydesign(id=~1, weights=~perwt, data=acs_ad)

acs_N<-as.numeric(svytable(~age_dc+race_dc+educat,acs_ds_ad))
 
X<-data.frame(age=SRBIdata$age_dc,eth=SRBIdata$race_dc,edu=SRBIdata$educat)

dat <- data.frame(X)
remove(list=objects()[!(objects() %in% c("dat","acs_N"))])
gc()

#-----------function defination-------------#
ci_50 <- function(x){quantile(x,c(0.25,0.75))}
ci_80 <- function(x){quantile(x,c(0.1,0.9))}
ci_95 <- function(x){quantile(x,c(0.025,0.975))}
#-----------computation--------------#
###cell id
n<-dim(dat)[1] #sample size
q<-3 #8
#names(table(dat$age, dat$eth, dat$edu))
J_age <- length(unique(dat$age))
J_eth <- length(unique(dat$eth))
J_edu <- length(unique(dat$edu))

cell_id<-rep(0,n)
cell_str<-matrix(0,J_age*J_eth*J_edu,q)
 j<-0
for (i3 in 1:J_edu){
  for (i2 in 1:J_eth){
    for (i1 in 1:J_age){
                j<- (i3-1) * J_eth * J_age + (i2-1) * J_age + i1
                cell_id[dat$age==i1& dat$eth==i2 & dat$edu==i3]<-j
                cell_str[j,]<-c(i1,i2,i3)
    }
  }
}
J<-length(unique(cell_id))
n_cell <- as.numeric(table(cell_id))   
N_cell<-acs_N[as.numeric(names(table(cell_id)))]
#X_n<-as.numeric(table(dat$age,dat$eth,dat$edu))
plot(N_cell/n_cell)
hist(N_cell/n_cell)
summary(N_cell/n_cell)
sd(N_cell/n_cell/sum(N_cell)*n)



R<-100

mu_cell<-rep(0,J)
sigma_y_cell<-rep(0,J)

cover_lambda50<-matrix(0,R,q-1)
cover_sigma50<-matrix(0,R,q)
cover_sigmay50<-rep(0,R)
cover_lambda80<-matrix(0,R,q-1)
cover_sigma80<-matrix(0,R,q)
cover_sigmay80<-rep(0,R)
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
  sigma_y <- abs(rcauchy(1,0,1))
  sigma_m <- abs(rcauchy(q,0,1))
  lambda_inter <-abs(rcauchy(q-1,0,1))

  pri_var <- sum(lambda_m^2 * sigma_m^2) + (lambda_inter[1] * sigma_m[1] * sigma_m[2])^2 +
    (lambda_inter[1] * sigma_m[1] * sigma_m[3])^2 + (lambda_inter[1] * sigma_m[2] * sigma_m[3])^2 +
    (lambda_inter[2] * sigma_m[1] * sigma_m[2]* sigma_m[3])^2
  
  alpha<-rnorm(1)
  alpha_age <-rnorm(J_age); alpha_eth <-rnorm(J_eth); alpha_edu <- rnorm(J_edu);
  alpha_age_eth<-rnorm(J_age * J_eth); alpha_age_edu <-rnorm(J_age * J_edu);
  alpha_eth_edu <-rnorm(J_eth * J_edu); alpha_age_eth_edu <-rnorm(J_age * J_eth * J_edu)
 #x_value 3-dim vector 
  mu_mean<-function(x_value){
    alpha + alpha_age[x_value[1]] * lambda_m * sigma_m[1] + 
      alpha_eth[x_value[2]] * lambda_m * sigma_m[2] + 
      alpha_edu[x_value[3]] * lambda_m * sigma_m[3] +
      alpha_age_eth[(x_value[1]-1) * J_eth + x_value[2]] * lambda_inter[1] * sigma_m[1] * sigma_m[2]+
      alpha_age_edu[(x_value[1]-1) * J_edu + x_value[3]] * lambda_inter[1] * sigma_m[1] * sigma_m[3]+
      alpha_eth_edu[(x_value[2]-1) * J_edu + x_value[3]] * lambda_inter[1] * sigma_m[2] * sigma_m[3]+
      alpha_age_eth_edu[(x_value[1]-1) * J_eth * J_edu + (x_value[2]-1) * J_edu + 
                          x_value[3]] * lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[3];
  }
 
 mu_rake_mean<-function(x_value){
   alpha + alpha_age[x_value[1]] * lambda_m * sigma_m[1] + 
     alpha_eth[x_value[2]] * lambda_m * sigma_m[2] + 
     alpha_edu[x_value[3]] * lambda_m * sigma_m[3] 
 }
  for (j in 1:J){
    mu_cell[j] <- mu_mean(cell_str[j,])
    
    alpha + alpha_age[cell_str[j,1]] * lambda_m * sigma_m[1] + 
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
S.compile_cell <- stan(file='stan/mrpweights031115a.stan',
                       data=stan.data_cell,
                       iter=2, chains=1)
#print(S.compile_cell)
stan.seed <- round(runif(1,0,9999999))
n.chains <- 3
registerDoMC(n.chains)
st <- system.time(sflist1 <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
  S <- stan(fit=S.compile_cell,
            data=stan.data_cell,
            iter=1000, chains=1, seed=stan.seed, chain_id=i.cores)
  return(S)
})[3]
S <- sflist2stanfit(sflist1)

# print(S,digits=3)
output<-extract(S, permuted=TRUE)

# plot(sqrt(output$pri_var))
# summary(sqrt(output$pri_var))
# sqrt(pri_var)
#   apply(output$lambda_inter,2,mean)
# lambda_inter
#    apply(output$sigma_m,2,mean)
# sigma_m 
# # mean(output$pri_var)
# # anova(lm(apply(output$mu_cell,2,mean)~1))
# plot(n_cell, apply(output$w_new,2,mean))
# plot(n_cell, apply(output$ps_w,2,mean))
# plot((apply(output$ps_w,2,mean)-sigma_y_cell^(-2)/(sigma_y_cell^(-2) + pri_var^(-1)))/
#        sigma_y_cell^(-2)/(sigma_y_cell^(-2) + pri_var^(-1)))
# 
# sd(N_cell/n_cell *n /sum(N_cell))
# sd(apply(output$w_new,2,mean))
# 
#  plot(output$lambda_inter[,1])
#  plot(output$lambda_inter[,2])
# plot(output$lambda_inter[,2]* output$sigma_m[,1] * output$sigma_m[,2]*output$sigma_m[,3])
# mean(output$lambda_inter[,2]* output$sigma_m[,1] * output$sigma_m[,2]*output$sigma_m[,3])
# lambda_inter[2] * sigma_m[1] * sigma_m[2] * sigma_m[3]
# mean(output$lambda_inter[,1]* output$sigma_m[,1] * output$sigma_m[,2])
# lambda_inter[1] * sigma_m[1] * sigma_m[2]
# mean(output$lambda_inter[,1]* output$sigma_m[,1] * output$sigma_m[,3])
# lambda_inter[1] * sigma_m[1] * sigma_m[3]
# mean(output$lambda_inter[,1]* output$sigma_m[,2] * output$sigma_m[,3])
# lambda_inter[1] * sigma_m[2] * sigma_m[3]
# 
# plot(output$sigma_m[,1])
# plot(output$sigma_m[,2])
# plot(output$sigma_m[,3])
# plot(output$lambda_inter[,1])
# plot(output$lambda_inter[,2])
# plot(output$lambda_inter[,1] * output$sigma_m[,1] * output$sigma_m[,2])
# plot(output$lambda_inter[,1] * output$sigma_m[,1] * output$sigma_m[,3])
# plot(output$lambda_inter[,1] * output$sigma_m[,2] * output$sigma_m[,3])
# 
# 
# plot(apply(output$mu_cell,2,mean)-mu_cell)
# plot((apply(output$mu_cell,2,mean)-mu_cell)/mu_cell)
# plot(n_cell,apply(output$mu_cell,2,mean)-mu_cell)
# plot(n_cell,(apply(output$mu_cell,2,mean)-mu_cell)/mu_cell)

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

#if (ci_80(output$lambda_m)[1] <= lambda_m & lambda_m <= ci_80(output$lambda_m)[2]){
#  cover_lambda80[r,1]<-1
#}
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

#if (ci_95(output$lambda_m)[1] <= lambda_m & lambda_m <= ci_95(output$lambda_m)[2]){
#  cover_lambda95[r,1]<-1
#}
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

sd_bias[r,]<-c(mean(output$sigma_y)-sigma_y, apply(output$sigma_m,2,mean)-sigma_m, mean(output$lambda_m)-lambda_m,
               apply(output$lambda_inter,2,mean)-lambda_inter)
mu_bias[r,]<-apply(output$mu_cell,2,mean)-mu_cell
}

save.image("output/check_mrp_20150719.RData")
# load("check_mrp_20150311a.RData")
# load("check_mrp_20150311b.RData")
# load("check_mrp_20150311c.RData")
# apply(cover_lambda50,2,mean)
# apply(cover_sigma50,2,mean)
# mean(cover_sigmay50)
# apply(cover_lambda80,2,mean)
# apply(cover_sigma80,2,mean)
# mean(cover_sigmay80)
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
