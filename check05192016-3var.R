###---------------MRP to Construct Survey Weights--------------###
###Author: YS
###Latest Edit date: 05/22/2016
#setwd("/Users/Shared/ysi/GoogleDriveFolder/working/weighting/code")
#setwd("~/Google Drive/working/weighting/code")
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
require(survey)
require(foreach)
require(doMC)
require(ggplot2)

set.seed(20150213)
#----------data simulation-----------------#
data<-read.dta("data/SRBIandAGENCYbaselinew1w2w3datawithwghts072114.dta")
SRBIdata<-data[data$sample=="SRBI",]
# bestlife=qd7
#la var bestlife "Life Satisfaction, 10=best possibl
table(SRBIdata[,names(SRBIdata)=="bestlife"])
Y_sys<-as.numeric(SRBIdata[,names(SRBIdata)=="bestlife"])
Y_sys[is.na(Y_sys)]<-sample(Y_sys[!is.na(Y_sys)],sum(is.na(Y_sys)),replace=T)


###ACS data
acs_pop<-read.dta("data/acs_nyc_2011_wpov1.dta",convert.factors = FALSE)
acs_ad <- acs_pop[as.numeric(acs_pop$age)>=18,] 
##age
age_tr<-acs_ad$age #at least 18
##
# acs_ad$age_dc<-age_tr #discretization
# acs_ad$age_dc[age_tr<=34]<-1 #18-34
# acs_ad$age_dc[age_tr<=64 & age_tr>34]<-2 #35-64
# acs_ad$age_dc[age_tr>=64]<-3 #65+
# 
# SRBIdata$age_dc<-SRBIdata$age
# SRBIdata$age_dc[SRBIdata$age<=34]<-1
# SRBIdata$age_dc[SRBIdata$age<=64 & SRBIdata$age > 34]<-2
# SRBIdata$age_dc[SRBIdata$age > 64]<-3

acs_ad$age_dc<-age_tr #discretization
acs_ad$age_dc[age_tr<=34]<-1 #18-34
acs_ad$age_dc[age_tr<=44 & age_tr>34]<-2
acs_ad$age_dc[age_tr<=54 & age_tr>44]<-3
acs_ad$age_dc[age_tr<=64 & age_tr>54]<-4
acs_ad$age_dc[age_tr>64]<-5

SRBIdata$age_dc<-SRBIdata$age
SRBIdata$age_dc[SRBIdata$age<=34]<-1
SRBIdata$age_dc[SRBIdata$age<=44 & SRBIdata$age > 34]<-2
SRBIdata$age_dc[SRBIdata$age<=54 & SRBIdata$age > 44]<-3
SRBIdata$age_dc[SRBIdata$age<=64 & SRBIdata$age > 54]<-4
SRBIdata$age_dc[SRBIdata$age > 64]<-5

###sex
acs_ad$sex<-as.numeric(acs_ad$sex)
SRBIdata$sex<-as.numeric(as.factor(SRBIdata$r_gender))

###race
# race 1 "1 White Non-Hispanic" 2 "2 Black Non-Hispanic" 3 "3 Asian" 4 "4 Other" 5 "5 Hispanic"

acs_ad$race_dc<-acs_ad$racex
SRBIdata$race_dc<-SRBIdata$race

#edu
#educat 4
#recode qi5 (1/2=1) (3/4=2) (5/6=3) (7/8=4) (98/99=.), gen(educat)
SRBIdata$educat<-SRBIdata$edu

#tot.income
#poverty gap: 1 Under 50%   2 50-100%  3 100-200%  4 200-300%     5 300%+
acs_ad$opmres_x<-1
#acs_ad$poverty
acs_ad$opmres_x[acs_ad$poverty<=100 & acs_ad$poverty>50]<-2
acs_ad$opmres_x[acs_ad$poverty<=200 & acs_ad$poverty>100]<-3
acs_ad$opmres_x[acs_ad$poverty<=300 & acs_ad$poverty>200]<-4
acs_ad$opmres_x[acs_ad$poverty>300]<-5
SRBIdata$opmres_x<-1
#(1/100=1) (101-200=2) (201/501=3
SRBIdata$opmres_x[SRBIdata$povgap=="2 50-100%"]<-2
SRBIdata$opmres_x[SRBIdata$povgap=="3 100-200%"]<-3
SRBIdata$opmres_x[SRBIdata$povgap=="4 200-300%"]<-4
SRBIdata$opmres_x[SRBIdata$povgap=="5 300%+"]<-5

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

# J<-length(unique(cell_id))
# n_cell <- as.numeric(table(cell_id))   
# N_cell<-acs_N[as.numeric(names(table(cell_id)))]
J<-length(unique(cell_id))
J_true<-J_age*J_eth*J_edu
J_use<-as.numeric(names(table(cell_id)))
n_cell <- as.numeric(table(cell_id))   
N_cell<-acs_N[as.numeric(names(table(cell_id)))]
#prediction
#J_diff<-J_use-J
#N_cell_empty<-acs_N[-as.numeric(names(table(cell_id)))]
#J_empty<-(1:J_true)[-as.numeric(names(table(cell_id)))]
N_cell_true <- acs_N
# #X_n<-as.numeric(table(dat$age,dat$eth,dat$edu))
# plot(N_cell/n_cell/sum(N_cell)*n)
# hist(N_cell/n_cell/sum(N_cell)*n)
# summary(N_cell/n_cell/sum(N_cell)*n)
# sd(N_cell/n_cell/sum(N_cell)*n)
# plot(n_cell,N_cell/n_cell/sum(N_cell)*n)


R<-1

mu_cell<-rep(0,J)
theta_cell<-rep(0,J)
sigma_y_cell<-rep(0,J)

cover_lambda50<-matrix(0,R,q+2)
cover_sigma50<-rep(0,R)
cover_sigmay50<-rep(0,R)
cover_lambda80<-matrix(0,R,q+2)
cover_sigma80<-rep(0,R)
cover_sigmay80<-rep(0,R)
cover_lambda95<-matrix(0,R,q+2)
cover_sigma95<-rep(0,R)
cover_sigmay95<-rep(0,R)
cover_mu50<-matrix(0,R,J)
cover_mu80<-matrix(0,R,J)
cover_mu95<-matrix(0,R,J)

sd_bias<-matrix(0,R,2*q+1)
mu_bias<-matrix(0,R,J)


for (r in 1:R){
  set.seed(20150213+r);

##################################################
# the same error variance, different scale
#################################################
sigma_y <- abs(rnorm(1,0,1))
sigma_m <- abs(rnorm(1,0,1))
lambda_m <- abs(rnorm(q,0,1))
lambda_inter <- abs(rnorm(2,0,1))
tau_cell <-abs(rnorm(J,0,1))

pri_var <- sum(lambda_m^2 * sigma_m^2) + (lambda_inter[1] * lambda_m[1] * lambda_m[2] * sigma_m)^2 +
  (lambda_inter[1] * lambda_m[1] * lambda_m[3] * sigma_m)^2 + 
  (lambda_inter[1] * lambda_m[2] * lambda_m[3] * sigma_m)^2 +
  (lambda_inter[2] * lambda_m[1] * lambda_m[2] * lambda_m[3] * sigma_m)^2
  
alpha<-3
alpha_age <-rnorm(J_age); alpha_eth <-rnorm(J_eth); alpha_edu <- rnorm(J_edu);
alpha_age_eth<-rnorm(J_age * J_eth); alpha_age_edu <-rnorm(J_age * J_edu);
alpha_eth_edu <-rnorm(J_eth * J_edu); alpha_age_eth_edu <-rnorm(J_age * J_eth * J_edu)


mu_mean<-function(x_value){
  alpha + alpha_age[x_value[1]] * lambda_m[1] * sigma_m + 
    alpha_eth[x_value[2]] * lambda_m[2] * sigma_m + 
    alpha_edu[x_value[3]] * lambda_m[3] * sigma_m +
    alpha_age_eth[(x_value[2]-1) * J_age + x_value[1]] * lambda_inter[1] * lambda_m[1] * lambda_m[2] * sigma_m +
    alpha_age_edu[(x_value[3]-1) * J_age + x_value[1]] * lambda_inter[1] * lambda_m[1] * lambda_m[3] * sigma_m +
    alpha_eth_edu[(x_value[3]-1) * J_eth + x_value[2]] * lambda_inter[1] * lambda_m[2] * lambda_m[3] * sigma_m +
    alpha_age_eth_edu[(x_value[3]-1) * J_eth * J_age + (x_value[2]-1) * J_age + 
                        x_value[1]] * lambda_inter[2] * lambda_m[1] * lambda_m[2] * lambda_m[3] * sigma_m;
}


for (j in 1:J){
  mu_cell[j] <- mu_mean(cell_str[J_use[j],]) 
  theta_cell[j]<-mu_cell[j] + rnorm(1) * sigma_y/tau_cell[j] 
}


for (j in 1:J){
  sigma_y_cell[j] <- sigma_y / sqrt(n_cell[j]);
}

y_cell <-rnorm(J,theta_cell, sigma_y_cell);
ss_cell <-rchisq(1, n-1) * sigma_y^2;



###-----------------STAN--------------------------###

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


S.compile_cell_2 <- stan(file='stan/mrpweights05192016-3var.stan',
                         data=stan.data_cell,
                         iter=2, chains=1)

stan.seed <- round(runif(1,0,9999999))
n.chains <- 3
registerDoMC(n.chains)
st <- system.time(sflist1 <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
  S <- stan(fit=S.compile_cell_2,
            data=stan.data_cell,
            iter=500, chains=1, seed=stan.seed, chain_id=i.cores)
  return(S)
})[3]
S <- sflist2stanfit(sflist1)

S.compile_cell_iid <- stan(file='stan/mrpweights05192016-3var-iid.stan',
                         data=stan.data_cell,
                         iter=2, chains=1)

stan.seed <- round(runif(1,0,9999999))
n.chains <- 3
registerDoMC(n.chains)
st <- system.time(sflist_iid <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
  S_iid <- stan(fit=S.compile_cell_iid,
            data=stan.data_cell,
            iter=500, chains=1, seed=stan.seed, chain_id=i.cores)
  return(S_iid)
})[3]
S_iid <- sflist2stanfit(sflist_iid)

print(S,digits=3)
print(S_iid,digits=3)
#plot(S)
#pairs(S, pars = c("pri_var", "lambda_inter", "lp__"))

output<-extract(S_iid, permuted=TRUE)

summary(output$pri_var)
hist(output$pri_var)
plot(output$pri_var)
pri_var

hist(output$sigma_m)
summary(output$sigma_m)
sigma_m

hist(output$lambda_inter)
summary(output$lambda_inter)
plot(output$lambda_inter[,1])
plot(output$lambda_inter[,2])
lambda_inter

summary(output$lambda_m)
plot(output$lambda_m[,1])
plot(output$lambda_m[,2])
plot(output$lambda_m[,3])
lambda_m
summary(output$sigma_y)
sigma_y
plot(output$sigma_y)

summary(output$tau_cell)
plot(tau_cell/apply(output$tau_cell,2,median))

dim(output$ps_w)
plot(n_cell,apply(output$ps_w,2,median))

plot(n_cell,(apply(output$y_cell_new,2,mean)-y_cell)/y_cell)#compare with observed data

plot(apply(output$mu_cell_pred,2,mean)-mu_cell)
plot(n_cell,(apply(output$mu_cell,2,mean)-mu_cell)/mu_cell)

summary(output$theta_pred)
plot(output$theta_pred)
hist(output$theta_pred)
quantile(output$theta_pred,c(0.025,0.975))



# 
# pdf("plot/sum_var10202015.pdf")
p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=pri_var)) + geom_point()
p + geom_hline(yintercept=pri_var)
# scale_y_continuous(expression(paste("Variance of",theta, "(sum of variances)"))) + 
#   scale_x_continuous(name="Iterations after warm-up")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# 
# pdf("plot/w1_10202015.pdf")
# p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=w_new.1)) + geom_point()
# p + geom_hline(yintercept=N_cell[1]/n_cell[1]/sum(N_cell)*n)+
#   scale_y_continuous("Model-based weights for a cell with size 3") + 
#   scale_x_continuous(name="Iterations after warm-up")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# pdf("plot/ps_w1_10202015.pdf")
# p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=ps_w.1)) + geom_point()
# p + geom_hline(yintercept=1)+
#   scale_y_continuous("Allocation proprotion of fully poststratification weights for a cell with size 3") + 
#   scale_x_continuous(name="Iterations after warm-up")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# 
# 
# plot(output$pri_var)
# 
# pri_var
# 
# hist(sqrt(output$pri_var))
# summary(sqrt(output$pri_var))
# sqrt(pri_var)
#   apply(output$lambda_inter,2,mean)
# lambda_inter
#    apply(output$sigma_m,2,mean)
# sigma_m 
# 
# 
# # anova(lm(apply(output$mu_cell,2,mean)~1))
# plot(n_cell, apply(output$w_new,2,mean))
# plot(n_cell, apply(output$ps_w,2,mean))
# summary(apply(output$w_new,2,mean))
# pdf("plot/model-wght10202015.pdf")
# p <- ggplot() + geom_point(aes(x=n_cell,y=apply(output$w_new,2,mean)))
# p + 
#   scale_y_continuous("Model-based cell weights") + 
#   scale_x_continuous(name="Sample cell sizes")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# pdf("plot/model_wgh_prop10202015.pdf")
# p <- ggplot() + geom_point(aes(x=n_cell,y=apply(output$ps_w,2,mean)))
# p + 
#   scale_y_continuous("Allocation proportion for fully poststratification weights") + 
#   scale_x_continuous(name="Sample cell sizes")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# 
# plot((apply(output$ps_w,2,mean)-sigma_y_cell^(-2)/(sigma_y_cell^(-2) + pri_var^(-1)))/
#        sigma_y_cell^(-2)/(sigma_y_cell^(-2) + pri_var^(-1)))
# 
# summary(N_cell/n_cell *n /sum(N_cell))
# summary(apply(output$w_new,2,mean))
# 
# 
# 
# plot(output$lambda_inter[,2])
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
# pdf("plot/lambda_inter1_10202015.pdf")
# p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=lambda_inter.1)) + geom_point()
# p + geom_hline(yintercept=lambda_inter[1])+
#   scale_y_continuous(expression(paste("Relative scale for two-way interactions"))) + 
#   scale_x_continuous(name="Iterations after warm-up")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# 
# pdf("plot/lambda_inter2_10202015.pdf")
# p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=lambda_inter.2)) + geom_point()
# p + geom_hline(yintercept=lambda_inter[2])+
#   scale_y_continuous(expression(paste("Relative scale for three-way interactions"))) + 
#   scale_x_continuous(name="Iterations after warm-up")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# pdf("plot/sigma_1_10202015.pdf")
# p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=sigma_m.1)) + geom_point()
# p + geom_hline(yintercept=sigma_m[1])+
#   scale_y_continuous(expression(paste("local scale for age"))) + 
#   scale_x_continuous(name="Iterations after warm-up")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# pdf("plot/sigma_2_10202015.pdf")
# p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=sigma_m.2)) + geom_point()
# p + geom_hline(yintercept=sigma_m[2])+
#   scale_y_continuous(expression(paste("local scale for ethnicity"))) + 
#   scale_x_continuous(name="Iterations after warm-up")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# pdf("plot/sigma_3_10202015.pdf")
# p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=sigma_m.3)) + geom_point()
# p + geom_hline(yintercept=sigma_m[3])+
#   scale_y_continuous(expression(paste("local scale for education"))) + 
#   scale_x_continuous(name="Iterations after warm-up")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# pdf("plot/sigma_y_10202015.pdf")
# p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=sigma_y)) + geom_point()
# p + geom_hline(yintercept=sigma_y)+
#   scale_y_continuous(expression(paste("within cell scale"))) + 
#   scale_x_continuous(name="Iterations after warm-up")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# 
# plot(apply(output$mu_cell,2,mean)-mu_cell)
# plot((apply(output$mu_cell,2,mean)-mu_cell)/mu_cell)
# plot(n_cell,apply(output$mu_cell,2,mean)-mu_cell)
# 
# plot(n_cell,(apply(output$mu_cell,2,mean)-mu_cell)/mu_cell)
# 
# pdf("plot/mu_bias10202015.pdf")
# p <- ggplot() + geom_point(aes(x=n_cell,y=apply(output$mu_cell,2,mean)-mu_cell))
# p + 
#   scale_y_continuous("Bias of cell mean estimation") + 
#   scale_x_continuous(name="Sample cell sizes")+
#   theme(
#     legend.position="",
#     legend.direction="horizontal",
#     legend.text = element_text(size = 0, face = 'bold'),
#     legend.title = element_blank()
#   )+theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()
#     ,panel.background = element_blank()
#   )+
#   theme(axis.line = element_line(color = 'black'))+
#   theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
#         axis.text.y = element_text(colour="grey20",size=8,angle=0,hjust=1,vjust=0,face="plain"),  
#         axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
#         axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))
# dev.off()
# 
# plot(output$alpha)
# hist(output$alpha)
# summary(output$alpha)
# alpha
# j=2
# plot(output$alpha_age[,j])
# hist(output$alpha_age[,j])
# apply(output$alpha_age,2,summary)
# alpha_age
# 
# j=3
# plot(output$alpha_eth[,j])
# hist(output$alpha_eth[,j])
# apply(output$alpha_eth,2,summary)
# alpha_eth
# 
# apply(output$alpha_age_eth,2,summary)
# alpha_age_eth

if (ci_50(output$lambda_inter[,1])[1] <= lambda_inter[1] & lambda_inter[1] <= ci_50(output$lambda_inter[,1])[2]){
  cover_lambda50[r,1]<-1
}
if (ci_50(output$lambda_inter[,2])[1] <= lambda_inter[2] & lambda_inter[2] <= ci_50(output$lambda_inter[,2])[2]){
  cover_lambda50[r,2]<-1
}
for (j in 1:q){
  if (ci_50(output$lambda_m[,j])[1] <= lambda_m[j] & lambda_m[j] <= ci_50(output$lambda_m[,j])[2]){
    cover_lambda50[r,2+j]<-1
  }
}
if (ci_50(output$sigma_m)[1] <= sigma_m & sigma_m <= ci_50(output$sigma_m)[2]){
  cover_sigma50[r]<-1
}

if (ci_50(output$sigma_y)[1] <= sigma_y & sigma_y <= ci_50(output$sigma_y)[2]){
  cover_sigmay50[r]<-1
}

if (ci_80(output$lambda_inter[,1])[1] <= lambda_inter[1] & lambda_inter[1] <= ci_80(output$lambda_inter[,1])[2]){
  cover_lambda80[r,1]<-1
}
if (ci_80(output$lambda_inter[,2])[1] <= lambda_inter[2] & lambda_inter[2] <= ci_80(output$lambda_inter[,2])[2]){
  cover_lambda80[r,2]<-1
}
for (j in 1:q){
  if (ci_80(output$lambda_m[,j])[1] <= lambda_m[j] & lambda_m[j] <= ci_80(output$lambda_m[,j])[2]){
    cover_lambda80[r,2+j]<-1
  }
}
if (ci_80(output$sigma_m)[1] <= sigma_m & sigma_m <= ci_80(output$sigma_m)[2]){
  cover_sigma80[r]<-1
}

if (ci_80(output$sigma_y)[1] <= sigma_y & sigma_y <= ci_80(output$sigma_y)[2]){
  cover_sigmay80[r]<-1
}

if (ci_95(output$lambda_inter[,1])[1] <= lambda_inter[1] & lambda_inter[1] <= ci_95(output$lambda_inter[,1])[2]){
  cover_lambda95[r,1]<-1
}
if (ci_95(output$lambda_inter[,2])[1] <= lambda_inter[2] & lambda_inter[2] <= ci_95(output$lambda_inter[,2])[2]){
  cover_lambda95[r,2]<-1
}
for (j in 1:q){
  if (ci_95(output$lambda_m[,j])[1] <= lambda_m[j] & lambda_m[j] <= ci_95(output$lambda_m[,j])[2]){
    cover_lambda95[r,2+j]<-1
  }
}
if (ci_95(output$sigma_m)[1] <= sigma_m & sigma_m <= ci_95(output$sigma_m)[2]){
  cover_sigma95[r]<-1
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

sd_bias[r,]<-c(mean(output$sigma_y)-sigma_y, mean(output$sigma_m)-sigma_m, apply(output$lambda_m,2,mean)-lambda_m,
               apply(output$lambda_inter,2,mean)-lambda_inter)
mu_bias[r,]<-apply(output$mu_cell,2,mean)-mu_cell
}

save.image("output/check_mrp_20160211.RData")
# load("check_mrp_20150311a.RData")
# load("check_mrp_20150311b.RData")
# load("check_mrp_20150311c.RData")
#load("output/check_mrp_20150719.RData")
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
