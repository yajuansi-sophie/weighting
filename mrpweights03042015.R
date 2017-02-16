###---------------MRP to Construct Survey Weights--------------###
###Author: YS
###Latest Edit date: 03/04/2015
setwd("/Users/Shared/GoogleDriveFolder/working/weighting/code")
setwd("Google Drive/working/weighting/code")
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
#get the weighted frequency
# #main effects
# tot.age<-data.frame(cbind(1:3, as.numeric(svytable(~age_dc,acs_ds_ad))))
# tot.sex<-data.frame(cbind(1:2, as.numeric(svytable(~sex,acs_ds_ad))))
# tot.race<-data.frame(cbind(1:4, as.numeric(svytable(~race_dc,acs_ds_ad))))
# tot.edu<-data.frame(cbind(1:4, as.numeric(svytable(~educat,acs_ds_ad))))
# tot.opinc<-data.frame(cbind(1:3,as.numeric(svytable(~opmres_x,acs_ds_ad))))
# tot.eld<-data.frame(cbind(0:2,as.numeric(svytable(~eldx_ca,acs_ds_ad))))
# tot.cld<-data.frame(cbind(0:3,as.numeric(svytable(~childx_ca,acs_ds_ad))))
# tot.wa<-data.frame(cbind(0:2,as.numeric(svytable(~wax_ca,acs_ds_ad))))
# #interaction btw poverty and age
# tbl.age.op<-svytable(~age_dc+opmres_x,acs_ds_ad)
acs_N<-as.numeric(svytable(~age_dc+race_dc+educat,acs_ds_ad))
###
X<-data.frame(age=SRBIdata$age_dc,sex=SRBIdata$sex,eth=SRBIdata$race_dc,edu=SRBIdata$educat,
              inc=SRBIdata$opmres_x, eldx=SRBIdata$eldx_ca+1,childx=SRBIdata$childx_ca+1,
              wax=SRBIdata$wax_ca+1)

X<-data.frame(age=SRBIdata$age_dc,eth=SRBIdata$race_dc,edu=SRBIdata$educat)

dat <- data.frame(X)
remove(list=objects()[!(objects() %in% c("dat","acs_N"))])
gc()
#eldx childx wax +1 
#"imp_incret" "imp_incdis"  "imp_incwelf" "imp_incui"                    
# "imp_incsnap"  "imp_increg"  "imp_incoth"                   
# "imp_earnhd"   "imp_earnsp"  "imp_incothhh"   "imp_health"
#Y<-log(SRBIdata$opmres+0.5)

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
#X_n<-as.numeric(table(dat$age,dat$eth,dat$edu))

# interactions
#two-way
dat$age_eth <- (dat$age - 1) * J_eth + dat$eth
dat$age_edu <- (dat$age - 1) * J_edu + dat$edu
dat$eth_edu <- (dat$eth - 1) * J_edu + dat$edu

#three-way
dat$age_eth_edu <- (dat$age - 1) * J_eth * J_edu + (dat$eth - 1) * J_edu + dat$edu

#simulate Y
beta<-rnorm(J,seq(-10,10,length.out=J),1)
#Xdata<-matrix(0,n,8)
#Xdata[,1]<-rep(1,n)
#for (j in 1:7){
#Xdata[,j]<-dat[,j]
#}
#Y<-rnorm(n, Xdata %*% matrix(beta,8,1),2)
Y<-rnorm(n,beta[cell_id],1)

dat$Y<-Y
dat$age <- as.factor(dat$age)
dat$eth <- as.factor(dat$eth)
dat$edu <- as.factor(dat$edu)
contrasts(dat$age) <- contr.treatment(3)
contrasts(dat$eth) <- contr.treatment(4)
contrasts(dat$edu) <- contr.treatment(4)

summary(lm(Y ~ age:eth:edu, data=dat))


y_cell <- tapply(Y,cell_id,mean)
ss_cell<-rep(0,J)
for (j in 1:J){
  ss_cell[j] <- sum((Y[cell_id==j]-y_cell[j])^2)
}

###-----------------STAN--------------------------###

stan.data_cell <- list(n=nrow(dat),q=3,
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
source("http://mc-stan.org/rstan/stan.R")
set_cppo("debug")
S.compile_cell <- stan(file='stan/mrpweights030315a.stan',
                  data=stan.data_cell,
                  iter=2, chains=1)
print(S.compile_cell)
stan.seed <- round(runif(1,0,9999999))
S_cell <- stan(fit=S.compile_cell,
          data=stan.data_cell,
          iter=500, chains=3, seed=stan.seed)

print(S_cell,digits=3)

output<-extract(S_cell, permuted=TRUE)

plot(output$lambda_m)
hist(output$lambda_m)

plot(y_cell-apply(output$y_cell_new,2,mean))
plot(y_cell-apply(output$mu_cell,2,mean))
plot(n_cell,y_cell-apply(output$y_cell_new,2,mean))

plot(output$mu_cell[,1])

plot(output$sigma_m[,1])
plot(output$sigma_m[,2])
plot(output$sigma_m[,3])
plot(output$lambda_inter[,1])
plot(output$lambda_inter[,2])

plot(1/output$pri_var)
plot(1/(output$sigma_y_cell[,1])^2)

mean(output$sigma_y)
apply(output$sigma_y_cell,2,mean)
apply(output$sigma_y_cell,2,function(x){mean(x)^(-2)})

plot(output$ps_w[,1])
plot(N_cell/n_cell/sum(N_cell)*n/sum(N_cell/n_cell/sum(N_cell)*n)- apply(output$w_new,2,mean)/sum(apply(output$w_new,2,mean)))


gc()

n.chains <- 3

registerDoMC(n.chains)
st <- system.time(sflist1 <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
  S <- stan(fit=S.compile_cell,
            data=stan.data_cell,
            iter=500, chains=1, seed=stan.seed, chain_id=i.cores)
  return(S)
})[3]
S <- sflist2stanfit(sflist1)
print(S,digits=3)

save.image("model_2012_20130520.RData")

############################################################################################################
