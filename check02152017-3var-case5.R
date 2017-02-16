###---------------MRP to Construct Survey Weights--------------###
###Author: YS
###Latest Edit date: 02/15/2017
#setwd("/Users/Shared/ysi/boxsync/Box Sync/projects/weighting/code")
#setwd("~/Documents/weighting/code")
###clear
remove(list=objects())
###code
#recode arm:rescale

#----------required packages-----------------#
#require(arm)
#require(car)
require(foreign)
require(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
require(survey)
require(foreach)
require(doMC)
require(ggplot2)

set.seed(20150213)
#-----------function defination-------------#

#----------data simulation-----------------#
###ACS data
acs_pop<-read.dta("data/acs_nyc_2011_wpov1.dta",convert.factors = FALSE)
acs_ad <- acs_pop[as.numeric(acs_pop$age)>=18,] 
##age
age_tr<-acs_ad$age #at least 18
acs_ad$age_dc<-age_tr #discretization
acs_ad$age_dc[age_tr<=34]<-1 #18-34
acs_ad$age_dc[age_tr<=44 & age_tr>34]<-2
acs_ad$age_dc[age_tr<=54 & age_tr>44]<-3
acs_ad$age_dc[age_tr<=64 & age_tr>54]<-4
acs_ad$age_dc[age_tr>64]<-5

###sex
acs_ad$sex<-as.numeric(acs_ad$sex)
###race
# race 1 "1 White Non-Hispanic" 2 "2 Black Non-Hispanic" 3 "3 Asian" 4 "4 Other" 5 "5 Hispanic"
acs_ad$race_dc<-acs_ad$racex

#educat 4
#recode qi5 (1/2=1) (3/4=2) (5/6=3) (7/8=4) (98/99=.), gen(educat)

#tot.income
#poverty gap: 1 Under 50%   2 50-100%  3 100-200%  4 200-300%     5 300%+
acs_ad$opmres_x<-1
#acs_ad$poverty
acs_ad$opmres_x[acs_ad$poverty<=100 & acs_ad$poverty>50]<-2
acs_ad$opmres_x[acs_ad$poverty<=200 & acs_ad$poverty>100]<-3
acs_ad$opmres_x[acs_ad$poverty<=300 & acs_ad$poverty>200]<-4
acs_ad$opmres_x[acs_ad$poverty>300]<-5

#eldx
acs_ad$eldx_ca<-acs_ad$eldx # 0 1 2+
acs_ad$eldx_ca[acs_ad$eldx>1]<-2
acs_ad$eldx_ca<-acs_ad$eldx_ca+1 #change to positive integer
#childx 0 1 2 3
acs_ad$childx_ca<-acs_ad$childx
acs_ad$childx_ca[acs_ad$childx_ca>2]<-3
acs_ad$childx_ca<-acs_ad$childx_ca+1
#wax 0 1 2
#acs_ad$wax_ca<-acs_ad$wax
#acs_ad$wax_ca[acs_ad$wax>1]<-2
#personx 1-4
acs_ad$personx_ca<-acs_ad$personx
acs_ad$personx_ca[acs_ad$personx>4]<-4

q<-3

J_age <- length(unique(acs_ad$age_dc))
J_eth <- length(unique(acs_ad$race_dc))
J_edu <- length(unique(acs_ad$educat))

###########################################
N<-dim(acs_ad)[1]

acs_ad$age_dc<-as.factor(acs_ad$age_dc)
acs_ad$race_dc<-as.factor(acs_ad$race_dc)
acs_ad$educat<-as.factor(acs_ad$educat)

options("contrasts")
age<-model.matrix(~age_dc, acs_ad)
race<-model.matrix(~race_dc,acs_ad)
edu<-model.matrix(~educat,acs_ad)

age_race<-model.matrix(~age_dc:race_dc, acs_ad)[,-1]
age_edu<-model.matrix(~age_dc:educat, acs_ad)[,-1]
race_edu<-model.matrix(~race_dc:educat, acs_ad)[,-1]

age_race_edu<-model.matrix(~age_dc:race_dc:educat, acs_ad)[,-1]

###simulate Y

#case 1)
# betaY_age<-matrix(seq(0.5, 4,length=dim(age)[2]),dim(age)[2],1)
# betaY_race<-matrix(rep(1,dim(race)[2]) + c(2,rep(0,dim(race)[2]-1)),dim(race)[2],1)
# betaY_edu<-matrix(seq(3, 0,length=dim(edu)[2]),dim(edu)[2],1)
# 
# betaY_age_race<-matrix(sample(1:4,dim(age_race)[2],replace=T),dim(age_race)[2],1)
# betaY_age_edu<-matrix(sample(-2:2,dim(age_edu)[2],replace=T),dim(age_edu)[2],1)
# betaY_race_edu<-matrix(sample(-3:1,dim(race_edu)[2],replace=T),dim(race_edu)[2],1)
# betaY_age_race_edu<-matrix(sample(seq(-1,1,by=0.5),dim(age_race_edu)[2],replace=T),dim(age_race_edu)[2],1)

# cat("betaY_age",betaY_age,"\n")
# cat("betaY_race",betaY_race,"\n")
# cat("betaY_edu",betaY_edu,"\n")
# cat("betaY_age_race",betaY_age_race,"\n")
# cat("betaY_age_edu",betaY_age_edu,"\n")
# cat("betaY_race_edu",betaY_race_edu,"\n")
# cat("betaY_age_race_edu",betaY_age_race_edu,"\n")

# #case 2)
# betaY_age<-matrix(seq(0.5, 4,length=dim(age)[2]),dim(age)[2],1)
# betaY_race<-matrix(rep(1,dim(race)[2]) + c(2,rep(0,dim(race)[2]-1)),dim(race)[2],1)
# betaY_edu<-matrix(seq(3, 0,length=dim(edu)[2]),dim(edu)[2],1)
# 
# betaY_age_race<-matrix(0,dim(age_race)[2],1)
# betaY_age_edu<-matrix(0,dim(age_edu)[2],1)
# betaY_race_edu<-matrix(0,dim(race_edu)[2],1)
# betaY_age_race_edu<-matrix(0,dim(age_race_edu)[2],1)
# 
# #case 3) RACE
betaY_age<-matrix(seq(0.5, 4,length=dim(age)[2]),dim(age)[2],1)
betaY_race<-matrix(0,dim(race)[2],1)
betaY_edu<-matrix(seq(3, 0,length=dim(edu)[2]),dim(edu)[2],1)

betaY_age_race<-matrix(0,dim(age_race)[2],1)

betaY_age_edu<-matrix(sample(-2:2,dim(age_edu)[2],replace=T),dim(age_edu)[2],1)

betaY_race_edu<-matrix(0,dim(race_edu)[2],1)
betaY_age_race_edu<-matrix(0,dim(age_race_edu)[2],1)

muY <- age%*%betaY_age + race %*% betaY_race + edu%*%betaY_edu + 
  age_race%*%betaY_age_race + age_edu%*%betaY_age_edu + race_edu%*%betaY_race_edu + 
  age_race_edu%*%betaY_age_race_edu 

Y<-rnorm(N) + muY

###simuate I

#case 0)
# betaI_age<-matrix(seq(-2, -1,length=dim(age)[2]),dim(age)[2],1)
# betaI_race<-matrix(seq(-1, 2,length=dim(race)[2]),dim(race)[2],1)
# betaI_edu<-matrix(seq(0, 2,length=dim(edu)[2]),dim(edu)[2],1)
# 
# betaI_age_race<-matrix(sample(-1:1,dim(age_race)[2],replace=T),dim(age_race)[2],1)
# betaI_age_edu<-matrix(sample(-1:1,dim(age_edu)[2],replace=T),dim(age_edu)[2],1)
# betaI_race_edu<-matrix(sample(-1:1,dim(race_edu)[2],replace=T),dim(race_edu)[2],1)
# betaI_age_race_edu<-matrix(sample(seq(-1,1,by=0.2),dim(age_race_edu)[2],replace=T),dim(age_race_edu)[2],1)

# # #case 1)
betaI_age<-matrix(seq(-2, 0,length=dim(age)[2]),dim(age)[2],1)
betaI_race<-matrix(seq(-1, 1,length=dim(race)[2]),dim(race)[2],1)
betaI_edu<-matrix(seq(0, 3,length=dim(edu)[2]),dim(edu)[2],1)

betaI_age_race<-matrix(0,dim(age_race)[2],1)
betaI_age_edu<-matrix(0,dim(age_edu)[2],1)
betaI_race_edu<-matrix(0,dim(race_edu)[2],1)
betaI_age_race_edu<-matrix(0,dim(age_race_edu)[2],1)

# case 2)
# betaI_age<-matrix(seq(0, 2,length=dim(age)[2]),dim(age)[2],1)
# betaI_race<-matrix(seq(-2, 0,length=dim(race)[2]),dim(race)[2],1)
# betaI_edu<-matrix(0,dim(edu)[2],1)
# betaI_age_race<-matrix(sample(-1:1,dim(age_race)[2],replace=T),dim(age_race)[2],1)
# betaI_age_edu<-matrix(0,dim(age_edu)[2],1)
# betaI_race_edu<-matrix(0,dim(race_edu)[2],1)
# betaI_age_race_edu<-matrix(0,dim(age_race_edu)[2],1)

sel_prob <- 1/(1+exp(-(-2+age%*%betaI_age + race %*% betaI_race + edu%*%betaI_edu + 
  age_race%*%betaI_age_race + age_edu%*%betaI_age_edu + race_edu%*%betaI_race_edu + 
  age_race_edu%*%betaI_age_race_edu)))

#hist(sel_prob)
#sum((runif(N)<=sel_prob))
pop_cell_id<-rep(0,N)
cell_str<-matrix(0,J_age*J_eth*J_edu,q)
j<-0
for (i3 in 1:J_edu){
  for (i2 in 1:J_eth){
    for (i1 in 1:J_age){
      j<- (i3-1) * J_eth * J_age + (i2-1) * J_age + i1
      pop_cell_id[acs_ad$age_dc==i1& acs_ad$race_dc==i2 & acs_ad$educat==i3]<-j
      cell_str[j,]<-c(i1,i2,i3)
    }
  }
}
J_true<-J_age*J_eth*J_edu
N_cell_true <- as.numeric(table(pop_cell_id))

pop_data<-data.frame(cbind(pop_cell_id,Y))
mu_cell_true<-aggregate(.~pop_cell_id,data=pop_data,mean)$V2
###------compiling------###
I<-(runif(N)<=sel_prob)

dat<-data.frame(Y=Y[I],age=acs_ad$age_dc[I],eth=acs_ad$race_dc[I],edu=acs_ad$educat[I])
#-----------computation--------------#
###cell id
n<-dim(dat)[1] #sample size

cell_id<-pop_cell_id[I]

J<-length(unique(cell_id))
J_use<-as.numeric(names(table(cell_id)))
n_cell <- as.numeric(table(cell_id))   

N_cell<-as.numeric(table(pop_cell_id))[as.numeric(names(table(cell_id)))]

mu_cell_use<-mu_cell_true[J_use]

y_cell <- aggregate(.~cell_id,data=dat,mean)$Y
ss_cell<-0
for (j in 1:J){
  ss_cell<-ss_cell+sum((dat$Y[cell_id==J_use[j]]-y_cell[j])^2)
}

###-----------------STAN with structural prior--------------------------###

stan.data_cell <- list(n=nrow(dat),q=q,
                       J=J, n_cell=n_cell, y_cell=y_cell, ss_cell=ss_cell,
                       cell_str=cell_str,N_cell=N_cell,
                       J_true=J_true,J_use=J_use,
                       N_cell_true=N_cell_true,
                       J_age=length(unique(dat$age)),
                       J_eth=length(unique(dat$eth)),
                       J_edu=length(unique(dat$edu)),
                       J_age_eth=length(unique(dat$age)) * length(unique(dat$eth)),
                       J_age_edu=length(unique(dat$age)) * length(unique(dat$edu)),
                       J_eth_edu=length(unique(dat$eth)) * length(unique(dat$edu)),
                       J_age_eth_edu=length(unique(dat$age)) * length(unique(dat$eth)) * length(unique(dat$edu))
)


S.compile_cell_sp <- stan(file='stan/mrpweights02072017-3var.stan',
                          data=stan.data_cell,
                          iter=2, chains=1)

S.compile_cell_1 <- stan(file='stan/mrpweights05262016-3var-iid.stan',
                         data=stan.data_cell,
                         iter=2, chains=1)
###------repeated sampling process------###
R<-200

bias_mu_pred<-rep(0,R)
sd_mu_pred<-rep(0,R)
cr_mu_pred<-rep(0,R)
bias_mu_sample<-rep(0,R)
sd_mu_sample<-rep(0,R)
cr_mu_sample<-rep(0,R)
bias_mu_popcell<-matrix(0,R,J_true)
sd_mu_popcell<-matrix(0,R,J_true)
cr_mu_popcell<-matrix(0,R,J_true)
bias_mu_cell<-matrix(0,R,J_true)
sd_mu_cell<-matrix(0,R,J_true)
cr_mu_cell<-matrix(0,R,J_true)

bias_mu_pred_iid<-rep(0,R)
sd_mu_pred_iid<-rep(0,R)
cr_mu_pred_iid<-rep(0,R)
bias_mu_sample_iid<-rep(0,R)
sd_mu_sample_iid<-rep(0,R)
cr_mu_sample_iid<-rep(0,R)
bias_mu_popcell_iid<-matrix(0,R,J_true)
sd_mu_popcell_iid<-matrix(0,R,J_true)
cr_mu_popcell_iid<-matrix(0,R,J_true)
bias_mu_cell_iid<-matrix(0,R,J_true)
sd_mu_cell_iid<-matrix(0,R,J_true)
cr_mu_cell_iid<-matrix(0,R,J_true)

bias_mu_ps<-rep(0,R)
sd_mu_ps<-rep(0,R)
cr_mu_ps<-rep(0,R)
bias_mu_ips<-rep(0,R)
sd_mu_ips<-rep(0,R)
cr_mu_ips<-rep(0,R)
bias_mu_rake<-rep(0,R)
sd_mu_rake<-rep(0,R)
cr_mu_rake<-rep(0,R)

wght_sd_rt<-matrix(0,R,2)
wght_sd_rt_ps<-matrix(0,R,2)
wght_sd_rt_ips<-matrix(0,R,2)
wght_sd_rt_rake<-matrix(0,R,2)
wght_sd_rt_iid<-matrix(0,R,2)

for (r in 1:R){
  set.seed(20150213+r);
###sampling
I<-(runif(N)<=sel_prob)

dat<-data.frame(Y=Y[I],age=acs_ad$age_dc[I],eth=acs_ad$race_dc[I],edu=acs_ad$educat[I])
#-----------computation--------------#
###cell id
n<-dim(dat)[1] #sample size

cell_id<-pop_cell_id[I]

J<-length(unique(cell_id))
J_use<-as.numeric(names(table(cell_id)))
n_cell <- as.numeric(table(cell_id))   

N_cell<-as.numeric(table(pop_cell_id))[as.numeric(names(table(cell_id)))]

mu_cell_use<-mu_cell_true[J_use]

y_cell <- aggregate(.~cell_id,data=dat,mean)$Y
ss_cell<-0
for (j in 1:J){
  ss_cell<-ss_cell+sum((dat$Y[cell_id==J_use[j]]-y_cell[j])^2)
}

###-----------------STAN with structural prior--------------------------###

stan.data_cell <- list(n=nrow(dat),q=q,
                       J=J, n_cell=n_cell, y_cell=y_cell, ss_cell=ss_cell,
                       cell_str=cell_str,N_cell=N_cell,
                       J_true=J_true,J_use=J_use,
                       N_cell_true=N_cell_true,
                       J_age=length(unique(dat$age)),
                       J_eth=length(unique(dat$eth)),
                       J_edu=length(unique(dat$edu)),
                       J_age_eth=length(unique(dat$age)) * length(unique(dat$eth)),
                       J_age_edu=length(unique(dat$age)) * length(unique(dat$edu)),
                       J_eth_edu=length(unique(dat$eth)) * length(unique(dat$edu)),
                       J_age_eth_edu=length(unique(dat$age)) * length(unique(dat$eth)) * length(unique(dat$edu))
)

stan.seed <- round(runif(1,0,9999999))
n.chains <- 3
registerDoMC(n.chains)
st <- system.time(sflist <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
  S <- stan(fit=S.compile_cell_sp,
            data=stan.data_cell,
            iter=500, chains=1, seed=stan.seed, chain_id=i.cores)
  return(S)
})[3]
S <- sflist2stanfit(sflist)

   output<-extract(S, permuted=TRUE)
# ###---look at one sample output---###

# print(S,digits=3)
# summary(output$pri_var)
# hist(output$pri_var)
# plot(output$pri_var)
# 
# hist(output$sigma_m)
# summary(output$sigma_m)
# plot(output$sigma_m)
# 
# hist(output$lambda_inter[,1])
# hist(output$lambda_inter[,2])
# plot(output$lambda_inter[,1])
# plot(output$lambda_inter[,2])
# summary(output$lambda_inter)
# 
# summary(output$lambda_m)
# plot(output$lambda_m[,1])
# plot(output$lambda_m[,2])
# plot(output$lambda_m[,3])
# 
# summary(output$sigma_y)
# plot(output$sigma_y)
# 
# 
# dim(output$ps_w)
# apply(output$ps_w,2,mean)
# 
# plot(apply(output$y_cell_new,2,mean)-y_cell) #compare with observed data
# 
# plot(apply(output$mu_cell_pred,2,mean)-mu_cell_true)
# plot(n_cell,apply(output$mu_cell,2,mean)-mu_cell_use)
# 
# summary(output$theta_pred)
# quantile(output$theta_pred,c(0.025,0.975))
# mean(Y)
# ###-----------------------###
###overal all
#prediction also using zero cells
bias_mu_pred[r]<-mean(output$theta_pred)-mean(Y)
sd_mu_pred[r]<-sd(output$theta_pred)
if (quantile(output$theta_pred,0.025)<= mean(Y) & mean(Y)<=quantile(output$theta_pred,0.975)){
  cr_mu_pred[r]<-1
}
#prediction using only nonzero cells
bias_mu_sample[r]<-mean(output$theta_sample)-mean(Y)
sd_mu_sample[r]<-sd(output$theta_sample)
if (quantile(output$theta_sample,0.025)<= mean(Y) & mean(Y)<=quantile(output$theta_sample,0.975)){
  cr_mu_sample[r]<-1
}

###population cell mean
bias_mu_popcell[r,]<-apply(output$mu_cell_pred,2,mean)-mu_cell_true
sd_mu_popcell[r,]<-apply(output$mu_cell_pred,2,sd)
cr_mu_popcell[r,]<-as.numeric(apply(output$mu_cell_pred,2,quantile,0.025) <= mu_cell_true & 
                                    mu_cell_true <= apply(output$mu_cell_pred,2,quantile,0.975))

###sampled cell mean
bias_mu_cell[r,J_use]<-apply(output$mu_cell,2,mean)-mu_cell_use
sd_mu_cell[r,J_use]<-apply(output$mu_cell,2,sd)
cr_mu_cell[r,J_use]<-as.numeric(apply(output$mu_cell,2,quantile,0.025) <= mu_cell_use & 
                                      mu_cell_use <= apply(output$mu_cell,2,quantile,0.975))

# apply(output$w_new,2,mean)
w_unit<-rep(0,n)
for (i in 1:n){
  w_unit[i]<-apply(output$w_new,2,mean)[J_use==cell_id[i]]
}
w_unit<-w_unit/mean(w_unit)
# summary(w_unit)
wght_sd_rt[r,]<-c(sd(w_unit),max(w_unit)/min(w_unit))

###------PS weighting------###
# plot(N_cell/n_cell/sum(N_cell)*n)
# hist(N_cell/n_cell/sum(N_cell)*n)
# summary(N_cell/n_cell/sum(N_cell)*n)
# sd(N_cell/n_cell/sum(N_cell)*n)
# plot(n_cell,N_cell/n_cell/sum(N_cell)*n)
w_ps<-rep(1,n)
for (i in 1:n){
  w_ps[i]<-(N_cell /n_cell)[J_use==cell_id[i]]
}
w_ps<-w_ps/mean(w_ps)
wght_sd_rt_ps[r,]<-c(sd(w_ps),max(w_ps)/min(w_ps))

ps.dat.design<-svydesign(id=~1,data=dat,weights=w_ps)

mu_ps<-svymean(~Y,ps.dat.design)[1]
bias_mu_ps[r]<-mu_ps-mean(Y)
sd_mu_ps[r]<-SE(svymean(~Y,ps.dat.design))
cr_mu_ps[r]<- as.numeric(mu_ps-1.96*sd_mu_ps[r]<=mean(Y) & 
                             mean(Y)<=mu_ps+1.96*sd_mu_ps[r])
###------inverse-prob weighted estimator------###
#sum(dat$Y/sel_prob[I])/sum(1/sel_prob[I])
w_ips<-1/sel_prob[I]/mean(1/sel_prob[I])

wght_sd_rt_ips[r,]<-c(sd(w_ips),max(w_ips)/min(w_ips))
#summary(w_ips)
#sd(w_ips)
ips.dat.design<-svydesign(id=~1,data=dat,weights=w_ips)

mu_ips<-svymean(~Y,ips.dat.design)[1]
bias_mu_ips[r]<-mu_ips-mean(Y)
sd_mu_ips[r]<-SE(svymean(~Y,ips.dat.design))
cr_mu_ips[r]<- as.numeric(mu_ips-1.96*sd_mu_ips[r]<=mean(Y) & 
                             mean(Y)<=mu_ips+1.96*sd_mu_ips[r])
###------raking estimator------###
dat.design<-svydesign(id=~1,data=dat)

pop.age<-data.frame(1:J_age,Freq=as.numeric(table(acs_ad$age_dc)))
names(pop.age)<-c("age","Freq")
pop.eth<-data.frame(1:J_eth,Freq=as.numeric(table(acs_ad$race_dc)))
names(pop.eth)<-c("eth","Freq")
pop.edu<-data.frame(1:J_edu,Freq=as.numeric(table(acs_ad$educat)))
names(pop.edu)<-c("edu","Freq")
dat_rake<-rake(dat.design,list(~age,~eth,~edu),list(pop.age,pop.eth,pop.edu))
#weights(dat_rake)

#plot(weights(dat_rake))
#summary(weights(dat_rake))
#length(unique(weights(dat_rake)))
w_rake<-weights(dat_rake)/mean(weights(dat_rake))


wght_sd_rt_rake[r,]<-c(sd(w_rake),max(w_rake)/min(w_rake))

rake.dat.design<-svydesign(id=~1,data=dat,weights=w_rake)

mu_rake<-svymean(~Y,rake.dat.design)[1]
bias_mu_rake[r]<-mu_rake-mean(Y)
sd_mu_rake[r]<-SE(svymean(~Y,rake.dat.design))
cr_mu_rake[r]<- as.numeric(mu_rake-1.96*sd_mu_rake[r]<=mean(Y) & 
                             mean(Y)<=mu_rake+1.96*sd_mu_rake[r])
#summary(w_rake) 
#sd(w_rake)

###-----------------STAN with independent prior--------------------------###

registerDoMC(n.chains)
st <- system.time(sflist1 <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
  S1 <- stan(fit=S.compile_cell_1,
            data=stan.data_cell,
            iter=500, chains=1, seed=stan.seed, chain_id=i.cores)
  return(S1)
})[3]
S1 <- sflist2stanfit(sflist1)

output_iid<-extract(S1, permuted=TRUE)

###---look at one sample output---###
# print(S1,digits=3)
# summary(output_iid$pri_var)
# hist(output_iid$pri_var)
# plot(output_iid$pri_var)
# 
# 
# summary(output_iid$sigma_m)
# j=2
# hist(output_iid$sigma_m[,j])
# plot(output_iid$sigma_m[,j])
# 
# 
# summary(output_iid$sigma_y)
# plot(output_iid$sigma_y)
# 
# 
# dim(output_iid$ps_w)
# apply(output_iid$ps_w,2,mean)
# 
# 
# plot(apply(output_iid$y_cell_new,2,mean))
# 
# 
# plot(apply(output_iid$mu_cell_pred,2,mean)-mu_cell_true)
# plot(n_cell,apply(output_iid$mu_cell,2,mean)-mu_cell_use)
###-------------------------------------###

###overal all
#prediction also using zero cells
bias_mu_pred_iid[r]<-mean(output_iid$theta_pred)-mean(Y)
sd_mu_pred_iid[r]<-sd(output_iid$theta_pred)
if (quantile(output_iid$theta_pred,0.025)<= mean(Y) & mean(Y)<=quantile(output_iid$theta_pred,0.975)){
  cr_mu_pred_iid[r]<-1
}
#prediction using only nonzero cells
bias_mu_sample_iid[r]<-mean(output_iid$theta_sample)-mean(Y)
sd_mu_sample_iid[r]<-sd(output_iid$theta_sample)
if (quantile(output_iid$theta_sample,0.025)<= mean(Y) & mean(Y)<=quantile(output_iid$theta_sample,0.975)){
  cr_mu_sample_iid[r]<-1
}
###population cell mean
bias_mu_popcell_iid[r,]<-apply(output_iid$mu_cell_pred,2,mean)-mu_cell_true
sd_mu_popcell_iid[r,]<-apply(output_iid$mu_cell_pred,2,sd)
cr_mu_popcell_iid[r,]<-as.numeric(apply(output_iid$mu_cell_pred,2,quantile,0.025) <= mu_cell_true & 
                                    mu_cell_true <=apply(output_iid$mu_cell_pred,2,quantile,0.975))

###sampled cell mean
bias_mu_cell_iid[r,J_use]<-apply(output_iid$mu_cell,2,mean)-mu_cell_use
sd_mu_cell_iid[r,J_use]<-apply(output_iid$mu_cell,2,sd)
cr_mu_cell_iid[r,J_use]<-as.numeric(apply(output_iid$mu_cell,2,quantile,0.025) <= mu_cell_use & 
                                    mu_cell_use <=apply(output_iid$mu_cell,2,quantile,0.975))

# apply(output_iid$w_new,2,mean)
w_unit_iid<-rep(0,n)
for (i in 1:n){
  w_unit_iid[i]<-apply(output_iid$w_new,2,mean)[J_use==cell_id[i]]
}
w_unit_iid<-w_unit_iid/mean(w_unit_iid)
# summary(w_unit_iid)
# sd(w_unit_iid)

wght_sd_rt_iid[r,]<-c(sd(w_unit_iid),max(w_unit_iid)/min(w_unit_iid))
}
save.image("output/check_3var_20170215_case5_Y2var_Imain.RData")

############################################################################################################
