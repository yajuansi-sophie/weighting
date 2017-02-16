###---------------MRP to Construct Survey Weights--------------###
###Author: YS
###Latest Edit date: 04/13/2016
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
betaY_age<-matrix(seq(0.5, 4,length=dim(age)[2]),dim(age)[2],1)
betaY_race<-matrix(rep(1,dim(race)[2]) + c(2,rep(0,dim(race)[2]-1)),dim(race)[2],1)
betaY_edu<-matrix(seq(3, 0,length=dim(edu)[2]),dim(edu)[2],1)
betaY_age_race<-matrix(sample(1:4,dim(age_race)[2],replace=T),dim(age_race)[2],1)
betaY_age_edu<-matrix(sample(-2:2,dim(age_edu)[2],replace=T),dim(age_edu)[2],1)
betaY_race_edu<-matrix(sample(-3:1,dim(race_edu)[2],replace=T),dim(race_edu)[2],1)
betaY_age_race_edu<-matrix(sample(seq(-1,1,by=0.2),dim(age_race_edu)[2],replace=T),dim(age_race_edu)[2],1)


muY <- age%*%betaY_age + race %*% betaY_race + edu%*%betaY_edu + 
  age_race%*%betaY_age_race + age_edu%*%betaY_age_edu + race_edu%*%betaY_race_edu + 
  age_race_edu%*%betaY_age_race_edu 

Y<-rnorm(N) + muY

###simuate I
betaI_age<-matrix(seq(-2, -1,length=dim(age)[2]),dim(age)[2],1)
betaI_race<-matrix(seq(-1, 0,length=dim(race)[2]),dim(race)[2],1)
betaI_edu<-matrix(seq(1, 2,length=dim(edu)[2]),dim(edu)[2],1)
betaI_age_race<-matrix(sample(-1:1,dim(age_race)[2],replace=T),dim(age_race)[2],1)
betaI_age_edu<-matrix(sample(-1:1,dim(age_edu)[2],replace=T),dim(age_edu)[2],1)
betaI_race_edu<-matrix(sample(-1:1,dim(race_edu)[2],replace=T),dim(race_edu)[2],1)
betaI_age_race_edu<-matrix(sample(seq(-1,1,by=0.2),dim(age_race_edu)[2],replace=T),dim(age_race_edu)[2],1)


sel_prob <- 1/(1+exp(-(-1.5+age%*%betaI_age + race %*% betaI_race + edu%*%betaI_edu + 
  age_race%*%betaI_age_race + age_edu%*%betaI_age_edu + race_edu%*%betaI_race_edu + 
  age_race_edu%*%betaI_age_race_edu)))
hist(sel_prob)

I<-(runif(N)<=sel_prob)

dat<-data.frame(Y=Y[I],age=acs_ad$age_dc[I],eth=acs_ad$race_dc[I],edu=acs_ad$educat[I])

#remove(list=objects()[!(objects() %in% c("dat","acs_N"))])
#gc()

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
pop_cell_id<-rep(0,N)

cell_str<-matrix(0,J_age*J_eth*J_edu,q)
j<-0
for (i3 in 1:J_edu){
  for (i2 in 1:J_eth){
    for (i1 in 1:J_age){
      j<- (i3-1) * J_eth * J_age + (i2-1) * J_age + i1
      cell_id[dat$age==i1& dat$eth==i2 & dat$edu==i3]<-j
      pop_cell_id[acs_ad$age_dc==i1& acs_ad$race_dc==i2 & acs_ad$educat==i3]<-j
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

N_cell<-as.numeric(table(pop_cell_id))[as.numeric(names(table(cell_id)))]
#prediction
#J_diff<-J_use-J
#N_cell_empty<-acs_N[-as.numeric(names(table(cell_id)))]
#J_empty<-(1:J_true)[-as.numeric(names(table(cell_id)))]
N_cell_true <- as.numeric(table(pop_cell_id))
# #X_n<-as.numeric(table(dat$age,dat$eth,dat$edu))
# plot(N_cell/n_cell/sum(N_cell)*n)
# hist(N_cell/n_cell/sum(N_cell)*n)
# summary(N_cell/n_cell/sum(N_cell)*n)
# sd(N_cell/n_cell/sum(N_cell)*n)
# plot(n_cell,N_cell/n_cell/sum(N_cell)*n)


R<-1

mu_cell<-rep(0,J)
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


y_cell <- aggregate(.~cell_id,data=dat,mean)$Y

ss_cell<-0
for (j in 1:J){
  ss_cell<-ss_cell+sum((dat$Y[cell_id==J_use[j]]-y_cell[j])^2)
}

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


S.compile_cell_2 <- stan(file='stan/mrpweights03062016-3var.stan',
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


print(S,digits=3)
#plot(S)
#pairs(S, pars = c("pri_var", "lambda_inter", "lp__"))

output<-extract(S, permuted=TRUE)



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


dim(output$ps_w)
apply(output$ps_w,2,mean)



plot(apply(output$y_cell_new,2,mean))

pop_data<-data.frame(cbind(pop_cell_id,Y))
mu_cell_true<-aggregate(.~pop_cell_id,data=pop_data,mean)$V2
mu_cell_use<-mu_cell_true[J_use]

plot(apply(output$mu_cell_pred,2,mean)-mu_cell_true)
plot(n_cell,apply(output$mu_cell,2,mean)-mu_cell_use)

summary(output$theta_pred)
quantile(output$theta_pred,c(0.025,0.975))
mean(Y)

apply(output$w_new,2,mean)
w_unit<-rep(0,n)
for (i in 1:n){
  w_unit[i]<-apply(output$w_new,2,mean)[J_use==cell_id[i]]
}
w_unit<-w_unit/mean(w_unit)
summary(w_unit)
sd(w_unit)
###------inverse-prob weighted estimator------###
sum(dat$Y/sel_prob[I])/sum(1/sel_prob[I])
w_ips<-1/sel_prob[I]/mean(1/sel_prob[I])
summary(w_ips)
sd(w_ips)
###------raking estimator------###
dat.design<-svydesign(id=~1,data=dat)

pop.age<-data.frame(1:J_age,Freq=as.numeric(table(acs_ad$age_dc)))
names(pop.age)<-c("age","Freq")
pop.eth<-data.frame(1:J_eth,Freq=as.numeric(table(acs_ad$race_dc)))
names(pop.eth)<-c("eth","Freq")
pop.edu<-data.frame(1:J_edu,Freq=as.numeric(table(acs_ad$educat)))
names(pop.edu)<-c("edu","Freq")
dat_rake<-rake(dat.design,list(~age,~eth,~edu),list(pop.age,pop.eth,pop.edu))
weights(dat_rake)
plot(weights(dat_rake))
summary(weights(dat_rake))
length(unique(weights(dat_rake)))
w_rake<-weights(dat_rake)/mean(weights(dat_rake))

summary(w_rake) 
sd(w_rake)
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
 p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=w_new.1)) + geom_point()
 p + geom_hline(yintercept=N_cell[1]/n_cell[1]/sum(N_cell)*n)+
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
 p <- ggplot(data.frame(output), aes(x=1:dim(data.frame(output))[1],y=ps_w.1)) + geom_point()
 p + geom_hline(yintercept=1)+
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
 plot(n_cell, apply(output$w_new,2,mean))
 plot(n_cell, apply(output$ps_w,2,mean))
# summary(apply(output$w_new,2,mean))
# pdf("plot/model-wght10202015.pdf")
p <- ggplot() + geom_point(aes(x=n_cell,y=apply(output$w_new,2,mean)))
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
