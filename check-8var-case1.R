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

q<-8
#names(table(dat$age, dat$eth, dat$edu))
J_age <- length(unique(acs_ad$age_dc))
J_eth <- length(unique(acs_ad$race_dc))
J_edu <- length(unique(acs_ad$educat))
J_sex <- length(unique(acs_ad$sex))
J_inc <- length(unique(acs_ad$opmres_x))
J_eld <- length(unique(acs_ad$eldx_ca))
J_cld <- length(unique(acs_ad$childx_ca))
J_ps <- length(unique(acs_ad$personx_ca))
###########################################
N<-dim(acs_ad)[1]

acs_ad$age_dc<-as.factor(acs_ad$age_dc)
acs_ad$race_dc<-as.factor(acs_ad$race_dc)
acs_ad$educat<-as.factor(acs_ad$educat)
acs_ad$sex<-as.factor(acs_ad$sex)
acs_ad$opmres_x<-as.factor(acs_ad$opmres_x)
acs_ad$eldx_ca<-as.factor(acs_ad$eldx_ca)
acs_ad$childx_ca<-as.factor(acs_ad$childx_ca)
acs_ad$psx_ca<-as.factor(acs_ad$personx_ca)

options("contrasts")
age<-model.matrix(~age_dc, acs_ad)
race<-model.matrix(~race_dc,acs_ad)
edu<-model.matrix(~educat,acs_ad)
sex<-model.matrix(~sex,acs_ad)
inc<-model.matrix(~opmres_x,acs_ad)
eldx<-model.matrix(~eldx_ca,acs_ad)
cldx<-model.matrix(~childx_ca,acs_ad)
psx<-model.matrix(~psx_ca,acs_ad)

age_race<-model.matrix(~age_dc:race_dc, acs_ad)[,-1]
age_edu<-model.matrix(~age_dc:educat, acs_ad)[,-1]
race_edu<-model.matrix(~race_dc:educat, acs_ad)[,-1]

race_inc<-model.matrix(~race_dc:opmres_x, acs_ad)[,-1]

age_inc<-model.matrix(~age_dc:opmres_x,acs_ad)[,-1]

inc_psx<-model.matrix(~opmres_x:psx_ca,acs_ad)[,-1]
inc_cldx<-model.matrix(~opmres_x:childx_ca,acs_ad)[,-1]
inc_eldx<-model.matrix(~opmres_x:eldx_ca,acs_ad)[,-1]

age_race_edu<-model.matrix(~age_dc:race_dc:educat, acs_ad)[,-1]
age_race_inc<-model.matrix(~age_dc:race_dc:opmres_x, acs_ad)[,-1]
###simulate Y

#case 1)
betaY_age<-matrix(seq(0.5, 4,length=dim(age)[2]),dim(age)[2],1)
betaY_race<-matrix(rep(1,dim(race)[2]) + c(2,rep(0,dim(race)[2]-1)),dim(race)[2],1)
betaY_edu<-matrix(seq(3, 0,length=dim(edu)[2]),dim(edu)[2],1)
betaY_sex<-matrix(seq(1, 0,length=dim(sex)[2]),dim(sex)[2],1)
betaY_inc<-matrix(seq(-3, 0,length=dim(inc)[2]),dim(inc)[2],1)
betaY_cldx<-matrix(seq(0, 2,length=dim(cldx)[2]),dim(cldx)[2],1)
betaY_eldx<-matrix(seq(2, 0,length=dim(eldx)[2]),dim(eldx)[2],1)
betaY_psx<-matrix(seq(4, 0,length=dim(psx)[2]),dim(psx)[2],1)

betaY_age_race<-matrix(sample(-1:1,dim(age_race)[2],replace=T),dim(age_race)[2],1)
betaY_age_edu<-matrix(sample(-1:1,dim(age_edu)[2],replace=T),dim(age_edu)[2],1)
betaY_race_edu<-matrix(sample(-1:1,dim(race_edu)[2],replace=T),dim(race_edu)[2],1)

betaY_race_inc<-matrix(sample(-1:1,dim(race_inc)[2],replace=T),dim(race_inc)[2],1)
betaY_age_inc<-matrix(sample(-1:1,dim(age_inc)[2],replace=T),dim(age_inc)[2],1)
betaY_inc_psx<-matrix(sample(-1:1,dim(inc_psx)[2],replace=T),dim(inc_psx)[2],1)
betaY_inc_eldx<-matrix(sample(-1:1,dim(inc_eldx)[2],replace=T),dim(inc_eldx)[2],1)
betaY_inc_cldx<-matrix(sample(-1:1,dim(inc_cldx)[2],replace=T),dim(inc_cldx)[2],1)

betaY_age_race_edu<-matrix(sample(seq(-1,1,by=0.2),dim(age_race_edu)[2],replace=T),dim(age_race_edu)[2],1)
betaY_age_race_inc<-matrix(sample(seq(-1,1,by=0.2),dim(age_race_inc)[2],replace=T),dim(age_race_inc)[2],1)

#case 2)
# betaY_age<-matrix(seq(0.5, 4,length=dim(age)[2]),dim(age)[2],1)
# betaY_race<-matrix(rep(1,dim(race)[2]) + c(2,rep(0,dim(race)[2]-1)),dim(race)[2],1)
# betaY_edu<-matrix(seq(3, 0,length=dim(edu)[2]),dim(edu)[2],1)
# betaY_sex<-matrix(seq(1, 0,length=dim(sex)[2]),dim(sex)[2],1)
# betaY_inc<-matrix(seq(-3, 0,length=dim(inc)[2]),dim(inc)[2],1)
# betaY_cldx<-matrix(seq(0, 2,length=dim(cldx)[2]),dim(cldx)[2],1)
# betaY_eldx<-matrix(seq(2, 0,length=dim(eldx)[2]),dim(eldx)[2],1)
# 
# betaY_psx<-matrix(0,dim(psx)[2],1)
# 
# betaY_age_race<-matrix(0,dim(age_race)[2],1)
# betaY_age_edu<-matrix(0,dim(age_edu)[2],1)
# betaY_race_edu<-matrix(0,dim(race_edu)[2],1)
# 
# betaY_race_inc<-matrix(0,dim(race_inc)[2],1)
# betaY_age_inc<-matrix(0,dim(age_inc)[2],1)
# betaY_inc_psx<-matrix(0,dim(inc_psx)[2],1)
# betaY_inc_eldx<-matrix(0,dim(inc_eldx)[2],1)
# betaY_inc_cldx<-matrix(0,dim(inc_cldx)[2],1)
# 
# betaY_age_race_edu<-matrix(0,dim(age_race_edu)[2],1)
# betaY_age_race_inc<-matrix(0,dim(age_race_inc)[2],1)


muY <- age%*%betaY_age + race %*% betaY_race + edu%*%betaY_edu + 
  sex%*%betaY_sex + inc %*% betaY_inc + psx%*%betaY_psx + 
  eldx%*%betaY_eldx + cldx %*% betaY_cldx + 
  age_race%*%betaY_age_race + age_edu%*%betaY_age_edu + race_edu%*%betaY_race_edu + 
  race_inc%*%betaY_race_inc + age_inc%*%betaY_age_inc + inc_psx%*%betaY_inc_psx + 
  inc_eldx%*%betaY_inc_eldx + inc_cldx%*%betaY_inc_cldx +  
  age_race_edu%*%betaY_age_race_edu +
  age_race_inc%*%betaY_age_race_inc

Y<-rnorm(N) * 5 + muY

###simuate I

#case 1)
betaI_age<-matrix(seq(-1, 1,length=dim(age)[2]),dim(age)[2],1)
betaI_race<-matrix(seq(-1, 1,length=dim(race)[2]),dim(race)[2],1)
betaI_edu<-matrix(seq(-1, 1,length=dim(edu)[2]),dim(edu)[2],1)
betaI_sex<-matrix(seq(-1, 0,length=dim(sex)[2]),dim(sex)[2],1)
betaI_inc<-matrix(seq(0, 1,length=dim(inc)[2]),dim(inc)[2],1)
betaI_cldx<-matrix(seq(0, 1,length=dim(cldx)[2]),dim(cldx)[2],1)
betaI_eldx<-matrix(seq(0, 1,length=dim(eldx)[2]),dim(eldx)[2],1)
betaI_psx<-matrix(seq(-1, 0,length=dim(psx)[2]),dim(psx)[2],1)

betaI_age_race<-matrix(sample(-1:1,dim(age_race)[2],replace=T),dim(age_race)[2],1)
betaI_age_edu<-matrix(sample(-1:1,dim(age_edu)[2],replace=T),dim(age_edu)[2],1)
betaI_race_edu<-matrix(sample(-1:1,dim(race_edu)[2],replace=T),dim(race_edu)[2],1)

betaI_race_inc<-matrix(sample(-1:1,dim(race_inc)[2],replace=T),dim(race_inc)[2],1)
betaI_age_inc<-matrix(sample(-1:1,dim(age_inc)[2],replace=T),dim(age_inc)[2],1)
betaI_inc_psx<-matrix(sample(-1:1,dim(inc_psx)[2],replace=T),dim(inc_psx)[2],1)
betaI_inc_eldx<-matrix(sample(-1:1,dim(inc_eldx)[2],replace=T),dim(inc_eldx)[2],1)
betaI_inc_cldx<-matrix(sample(-1:1,dim(inc_cldx)[2],replace=T),dim(inc_cldx)[2],1)

betaI_age_race_edu<-matrix(sample(seq(-1,1,by=0.2),dim(age_race_edu)[2],replace=T),dim(age_race_edu)[2],1)
betaI_age_race_inc<-matrix(sample(seq(-1,1,by=0.2),dim(age_race_inc)[2],replace=T),dim(age_race_inc)[2],1)

sel_prob <- 1/(1+exp(-(-2+age%*%betaI_age + race %*% betaI_race + edu%*%betaI_edu + 
                         sex%*%betaI_sex + inc %*% betaI_inc + psx%*%betaI_psx + 
                         eldx%*%betaI_eldx + cldx %*% betaI_cldx + 
                         age_race%*%betaI_age_race + age_edu%*%betaI_age_edu + race_edu%*%betaI_race_edu + 
                         race_inc%*%betaI_race_inc + age_inc%*%betaI_age_inc + inc_psx%*%betaI_inc_psx + 
                         inc_eldx%*%betaI_inc_eldx + inc_cldx%*%betaI_inc_cldx +  
                         age_race_edu%*%betaI_age_race_edu +
                         age_race_inc%*%betaI_age_race_inc)))

# #case 2)
# betaI_age<-matrix(seq(0, 2,length=dim(age)[2]),dim(age)[2],1)
# betaI_race<-matrix(seq(-1, 1,length=dim(race)[2]),dim(race)[2],1)
# betaI_edu<-matrix(seq(0, 3,length=dim(edu)[2]),dim(edu)[2],1)
# betaI_sex<-matrix(seq(-1, 0,length=dim(sex)[2]),dim(sex)[2],1)
# betaI_inc<-matrix(seq(0, 4,length=dim(inc)[2]),dim(inc)[2],1)
# betaI_cldx<-matrix(seq(-1, 1,length=dim(cldx)[2]),dim(cldx)[2],1)
# betaI_eldx<-matrix(seq(-3, 0,length=dim(eldx)[2]),dim(eldx)[2],1)
# betaI_psx<-matrix(seq(-1, 0,length=dim(psx)[2]),dim(psx)[2],1)
# 
# betaI_age_race<-matrix(0,dim(age_race)[2],1)
# betaI_age_edu<-matrix(0,dim(age_edu)[2],1)
# betaI_race_edu<-matrix(0,dim(race_edu)[2],1)
# 
# betaI_race_inc<-matrix(0,dim(race_inc)[2],1)
# betaI_age_inc<-matrix(0,dim(age_inc)[2],1)
# betaI_inc_psx<-matrix(0,dim(inc_psx)[2],1)
# betaI_inc_eldx<-matrix(0,dim(inc_eldx)[2],1)
# betaI_inc_cldx<-matrix(0,dim(inc_cldx)[2],1)
# 
# betaI_age_race_edu<-matrix(0,dim(age_race_edu)[2],1)
# betaI_age_race_inc<-matrix(0,dim(age_race_inc)[2],1)



# sel_prob <- 1/(1+exp(-(-2+age%*%betaI_age + race %*% betaI_race + edu%*%betaI_edu + 
#                          sex%*%betaI_sex + inc %*% betaI_inc + psx%*%betaI_psx + 
#                          eldx%*%betaI_eldx + cldx %*% betaI_cldx + 
#                          age_race%*%betaI_age_race + age_edu%*%betaI_age_edu + race_edu%*%betaI_race_edu + 
#                          race_inc%*%betaI_race_inc + age_inc%*%betaI_age_inc + inc_psx%*%betaI_inc_psx + 
#                          inc_eldx%*%betaI_inc_eldx + inc_cldx%*%betaI_inc_cldx +  
#                          age_race_edu%*%betaI_age_race_edu +
#                          age_race_inc%*%betaI_age_race_inc)))

hist(sel_prob)
sum((runif(N)<=sel_prob))

pop_cell_id<-rep(0,N)
J_sup<-J_age*J_eth*J_edu*J_sex*J_inc*J_eld*J_cld*J_ps
cell_str<-matrix(0,J_sup,q)
j<-0
for (i8 in 1:J_ps){
  for (i7 in 1:J_cld){
    for (i6 in 1:J_eld){
      for (i5 in 1:J_inc){
        for (i4 in 1:J_sex){
          for (i3 in 1:J_edu){
            for (i2 in 1:J_eth){
              for (i1 in 1:J_age){
                j<- (i8-1) * J_cld * J_eld * J_inc* J_sex * J_edu * J_eth * J_age +
                    (i7-1) * J_eld * J_inc* J_sex * J_edu * J_eth * J_age +
                    (i6-1)  * J_inc* J_sex * J_edu * J_eth * J_age +
                    (i5-1) * J_sex * J_edu * J_eth * J_age + (i4-1) * J_edu * J_eth * J_age + 
                    (i3-1) * J_eth * J_age + (i2-1) * J_age + i1
                pop_cell_id[acs_ad$age_dc==i1& acs_ad$race_dc==i2 & acs_ad$educat==i3&
                              acs_ad$sex==i4& acs_ad$opmres_x==i5 & acs_ad$eldx_ca==i6&
                              acs_ad$childx_ca==i7& acs_ad$psx_ca==i8]<-j
                cell_str[j,]<-c(i1,i2,i3,i4,i5,i6,i7,i8)
              }
            }
          }
        }
      }
    }
  }
}


N_cell_true <- as.numeric(table(pop_cell_id))
J_true<-length(N_cell_true)

pop_data<-data.frame(cbind(pop_cell_id,Y))
mu_cell_true<-aggregate(.~pop_cell_id,data=pop_data,mean)$V2
#sum(aggregate(.~pop_cell_id,data=pop_data,mean)$pop_cell_id==as.numeric(names(table(pop_cell_id))))

###------compiling------###
I<-(runif(N)<=sel_prob)

dat<-data.frame(Y=Y[I],age=acs_ad$age_dc[I],eth=acs_ad$race_dc[I],edu=acs_ad$educat[I],
                sex=acs_ad$sex[I],inc=acs_ad$opmres_x[I],eldx=acs_ad$eldx_ca[I],
                cldx=acs_ad$childx_ca[I],psx=acs_ad$psx_ca[I])
#-----------computation--------------#
###cell id
n<-dim(dat)[1] #sample size
cell_id<-pop_cell_id[I]

J<-length(unique(cell_id))
J_use<-as.numeric(names(table(cell_id)))
n_cell <- as.numeric(table(cell_id))   

J_pop<-as.numeric(names(table(pop_cell_id)))
N_cell<-as.numeric(table(pop_cell_id))[as.numeric(names(table(pop_cell_id)))%in%as.numeric(names(table(cell_id)))]

mu_cell_use<-mu_cell_true[as.numeric(names(table(pop_cell_id)))%in%as.numeric(names(table(cell_id)))]

y_cell <- aggregate(.~cell_id,data=dat,mean)$Y
ss_cell<-0
for (j in 1:J){
  ss_cell<-ss_cell+sum((dat$Y[cell_id==J_use[j]]-y_cell[j])^2)
}

###-----------------STAN with structural prior--------------------------###

stan.data_cell <- list(n=nrow(dat),q=q,
                       J=J, n_cell=n_cell, y_cell=y_cell, ss_cell=ss_cell,
                       cell_str=cell_str,N_cell=N_cell,
                       J_true=J_true,J_use=J_use,J_pop=J_pop,J_sup=J_sup,
                       N_cell_true=N_cell_true,
                       J_age=J_age,J_eth=J_eth,J_edu=J_edu,J_sex=J_sex,J_inc=J_inc,
                       J_eld=J_eld,J_cld=J_cld,J_ps=J_ps,
                       J_age_eth=J_age * J_eth,J_age_edu=J_age * J_edu,J_eth_edu=J_eth * J_edu,
                       J_eth_inc=J_eth * J_inc,J_age_inc=J_age * J_inc,J_inc_psx=J_inc * J_ps,
                       J_inc_eldx=J_inc*J_eld, J_inc_cldx=J_inc*J_cld,
                       J_age_eth_edu=J_age * J_eth * J_edu, J_age_eth_inc=J_age*J_eth*J_inc
)

S.compile_cell_sp <- stan(file='stan/mrpweights-8var.stan',
                          data=stan.data_cell,
                          iter=2, chains=1)

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

#print(S,digits=3)
# 
summary(output$pri_var)
hist(output$pri_var)
plot(output$pri_var)

hist(output$sigma_m)
summary(output$sigma_m)
plot(output$sigma_m)

hist(output$lambda_inter[,1])
hist(output$lambda_inter[,2])
plot(output$lambda_inter[,1])
plot(output$lambda_inter[,2])
summary(output$lambda_inter)

summary(output$lambda_m)
plot(output$lambda_m[,1])
plot(output$lambda_m[,2])
plot(output$lambda_m[,3])

summary(output$sigma_y)
plot(output$sigma_y)

summary(output$lambda_m[,1]*output$sigma_m)
summary(output$lambda_m[,2]*output$sigma_m)
summary(output$lambda_m[,3]*output$sigma_m)

summary(output$lambda_inter[,1]*output$lambda_m[,1]*output$lambda_m[,2]*output$sigma_m)
summary(output$lambda_inter[,1]*output$lambda_m[,1]*output$lambda_m[,3]*output$sigma_m)
summary(output$lambda_inter[,1]*output$lambda_m[,2]*output$lambda_m[,3]*output$sigma_m)

summary(output$lambda_inter[,2]*output$lambda_m[,1]*output$lambda_m[,2]*output$lambda_m[,3]*output$sigma_m)



dim(output$ps_w)
apply(output$ps_w,2,mean)
apply(output$w_new,2,mean)

length(unique(apply(output$w_new,2,mean)))
plot(apply(output$y_cell_new,2,mean)-y_cell) #compare with observed data

plot(apply(output$mu_cell_pred,2,mean)-mu_cell_true)
plot(n_cell,apply(output$mu_cell,2,mean)-mu_cell_use)

#age eth edu sex inc eldx cldx psx
age1 <- as.numeric(names(table(pop_cell_id))) %in% (1:J_sup)[cell_str[,1]==1]
sum(mu_cell_true[age1] * N_cell_true[age1])/sum(N_cell_true[age1])

sum(apply(output$mu_cell_pred,2,mean)[age1] * N_cell_true[age1])/sum(N_cell_true[age1])

sex1 <- as.numeric(names(table(pop_cell_id))) %in% (1:J_sup)[cell_str[,5]==1]
sum(mu_cell_true[sex1] * N_cell_true[sex1])/sum(N_cell_true[sex1])

sum(apply(output$mu_cell_pred,2,mean)[sex1] * N_cell_true[sex1])/sum(N_cell_true[sex1])

#poor<-as.numeric(names(table(pop_cell_id))) %in% (1:J_sup)[cell_str[,4]==1]

summary(output$theta_pred)
quantile(output$theta_pred,c(0.025,0.975))
mean(Y)

w1<-apply(output$w_new,2,mean)
w_unit<-rep(0,n)
for (i in 1:n){
  w_unit[i]<-apply(output$w_new,2,mean)[J_use==cell_id[i]]
}
w_unit<-w_unit/mean(w_unit)
summary(w_unit)
sd(w_unit)

sum(w_unit*dat$Y)/sum(w_unit)

st.dat.design<-svydesign(id=~1,data=dat,weights=w_unit)

svymean(~Y,st.dat.design)


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
summary(w_ps)
sd(w_ps)
ps.dat.design<-svydesign(id=~1,data=dat,weights=w_ps)

svymean(~Y,ps.dat.design)

###------inverse-prob weighted estimator------###
#sum(dat$Y/sel_prob[I])/sum(1/sel_prob[I])
w_ips<-1/sel_prob[I]/mean(1/sel_prob[I])
summary(w_ips)
sd(w_ips)
ips.dat.design<-svydesign(id=~1,data=dat,weights=w_ips)

svymean(~Y,ips.dat.design)
###------raking estimator------###
dat.design<-svydesign(id=~1,data=dat)

pop.age<-data.frame(1:J_age,Freq=as.numeric(table(acs_ad$age_dc)))
names(pop.age)<-c("age","Freq")
pop.eth<-data.frame(1:J_eth,Freq=as.numeric(table(acs_ad$race_dc)))
names(pop.eth)<-c("eth","Freq")
pop.edu<-data.frame(1:J_edu,Freq=as.numeric(table(acs_ad$educat)))
names(pop.edu)<-c("edu","Freq")
pop.sex<-data.frame(1:J_sex,Freq=as.numeric(table(acs_ad$sex)))
names(pop.sex)<-c("sex","Freq")
pop.inc<-data.frame(1:J_inc,Freq=as.numeric(table(acs_ad$opmres_x)))
names(pop.inc)<-c("inc","Freq")
pop.cld<-data.frame(1:J_cld,Freq=as.numeric(table(acs_ad$childx_ca)))
names(pop.cld)<-c("cldx","Freq")
pop.eld<-data.frame(1:J_eld,Freq=as.numeric(table(acs_ad$eldx_ca)))
names(pop.eld)<-c("eldx","Freq")
pop.ps<-data.frame(1:J_ps,Freq=as.numeric(table(acs_ad$psx_ca)))
names(pop.ps)<-c("psx","Freq")

dat_rake<-rake(dat.design,list(~age,~eth,~edu,~sex,~inc,~cldx,~eldx,~psx),
               list(pop.age,pop.eth,pop.edu,pop.sex,pop.inc,pop.cld,pop.eld,pop.ps))
#Warning message:
#  In rake(dat.design, list(~age, ~eth, ~edu, ~sex, ~inc, ~cldx, ~eldx,  :
#                             Raking did not converge after 10 iterations.

#weights(dat_rake)

plot(weights(dat_rake))
hist(weights(dat_rake))
summary(weights(dat_rake))
#length(unique(weights(dat_rake)))
w_rake<-weights(dat_rake)/mean(weights(dat_rake))
rake.dat.design<-svydesign(id=~1,data=dat,weights=w_rake)

svymean(~Y,rake.dat.design)

summary(w_rake) 
sd(w_rake)

###-----------------STAN with independent prior--------------------------###
S.compile_cell_1 <- stan(file='stan/mrpweights-8var-iid.stan',
                         data=stan.data_cell,
                         iter=2, chains=1)

stan.seed <- round(runif(1,0,9999999))
n.chains <- 3
registerDoMC(n.chains)
st <- system.time(sflist1 <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
  S1 <- stan(fit=S.compile_cell_1,
            data=stan.data_cell,
            iter=500, chains=1, seed=stan.seed, chain_id=i.cores)
  return(S1)
})[3]
S1 <- sflist2stanfit(sflist1)
#print(S1,digits=3)
output_iid<-extract(S1, permuted=TRUE)

summary(output_iid$pri_var)
hist(output_iid$pri_var)
plot(output_iid$pri_var)

hist(output_iid$sigma_m[,1])
summary(output_iid$sigma_m)
plot(output_iid$sigma_m[,1])


summary(output_iid$sigma_y)
plot(output_iid$sigma_y)


dim(output_iid$ps_w)
apply(output_iid$ps_w,2,mean)


plot(apply(output_iid$y_cell_new,2,mean))


plot(apply(output_iid$mu_cell_pred,2,mean)-mu_cell_true)
plot(n_cell,apply(output_iid$mu_cell,2,mean)-mu_cell_use)


# apply(output_iid$w_new,2,mean)
w_unit_iid<-rep(0,n)
for (i in 1:n){
  w_unit_iid[i]<-apply(output_iid$w_new,2,mean)[J_use==cell_id[i]]
}
w_unit_iid<-w_unit_iid/mean(w_unit_iid)
# summary(w_unit_iid)
# sd(w_unit_iid)

sum(w_unit_iid*dat$Y)/sum(w_unit_iid)

id.dat.design<-svydesign(id=~1,data=dat,weights=w_unit_iid)

svymean(~Y,id.dat.design)

save.image("output/simulation_8var_20160530_case1_all.RData")

############################################################################################################
