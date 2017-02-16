###---------------MRP to Construct Survey Weights--------------###
###Author: YS
###Latest Edit date: 02/11/2015
setwd("/Users/Shared/GoogleDriveFolder/working/weighting/code")
###clear
remove(list=objects())
###code
#recode arm:rescale
#----------required packages-----------------#
require(arm)
require(car)
require(foreign)
require(rstan)
require(foreach)
require(doMC)
set.seed(20150213)
#----------data simulation-----------------#
data<-read.dta("data/SRBIandAGENCYbaselinew1w2w3datawithwghts072114.dta")

SRBIdata<-data[data$sample=="SRBI",]
###age
SRBIdata$age_dc<-SRBIdata$age
SRBIdata$age_dc[SRBIdata$age<=34]<-1
SRBIdata$age_dc[SRBIdata$age<=64 & SRBIdata$age > 34]<-2
SRBIdata$age_dc[SRBIdata$age > 64]<-3
###sex
SRBIdata$sex<-as.numeric(as.factor(SRBIdata$r_gender))

###race
SRBIdata$race_dc<-SRBIdata$race
SRBIdata$race_dc[SRBIdata$race==4]<-3
SRBIdata$race_dc[SRBIdata$race==5]<-4

###edu
SRBIdata$educat<-SRBIdata$edu

###poverty gap
SRBIdata$opmres_x<-1
SRBIdata$opmres_x[SRBIdata$povgap=="3 100-200%"]<-2
SRBIdata$opmres_x[SRBIdata$povgap=="4 200-300%"]<-3
SRBIdata$opmres_x[SRBIdata$povgap=="5 300%+"]<-3

###eldx
SRBIdata$eldx_ca<-SRBIdata$eldx
SRBIdata$eldx_ca[SRBIdata$eldx>1]<-2


###childx 0 1 2 3
SRBIdata$childx_ca<-SRBIdata$childx
SRBIdata$childx_ca[SRBIdata$childx_ca>2]<-3

###wax 0 1 2
SRBIdata$wax_ca<-SRBIdata$wax
SRBIdata$wax_ca[SRBIdata$wax_ca>1]<-2

###
X<-data.frame(age=SRBIdata$age_dc,sex=SRBIdata$sex,eth=SRBIdata$race_dc,edu=SRBIdata$edu,
              inc=SRBIdata$opmres_x, eldx=SRBIdata$eldx_ca+1,childx=SRBIdata$childx_ca+1,
              wax=SRBIdata$wax_ca+1)
#eldx childx wax +1 
#"imp_incret" "imp_incdis"  "imp_incwelf" "imp_incui"                    
# "imp_incsnap"  "imp_increg"  "imp_incoth"                   
# "imp_earnhd"   "imp_earnsp"  "imp_incothhh"   "imp_health"
Y<-log(SRBIdata$opmres+0.5)
dat <- data.frame(Y, X)
remove(list=objects()[!(objects() %in% "dat")])
gc()

###cell id
n<-dim(dat)[1] #sample size
q<-8
#names(table(dat$age, dat$sex, dat$eth, dat$edu,dat$inc,dat$eldx,dat$childx,dat$wax))
n_age <- length(unique(dat$age))
n_sex <- length(unique(dat$sex))
n_eth <- length(unique(dat$eth))
n_edu <- length(unique(dat$edu))
n_inc <- length(unique(dat$inc))
n_eldx <- length(unique(dat$eldx))
n_childx <- length(unique(dat$childx))
n_wax <- length(unique(dat$wax))

cell_id<-rep(0,n)
cell_str<-matrix(0,n_age*n_sex*n_eth*n_edu*n_inc*n_eldx*n_childx*n_wax,q)
 j<-0
for (i1 in unique(dat$age)){
  for (i2 in unique(dat$sex)){
    for (i3 in unique(dat$eth)){
      for (i4 in unique(dat$edu)){
        for (i5 in unique(dat$inc)){
          for (i6 in unique(dat$eldx)){
            for (i7 in unique(dat$childx)){
              for (i8 in unique(dat$wax)){
                j<-j+1
                cell_id[dat$age==i1 & dat$sex==i2 & dat$eth==i3 & dat$edu==i4 &
                            dat$inc==i5 & dat$eldx==i6 & dat$childx==i7 & dat$wax==i8]<-j
                cell_str[j,]<-c(i1,i2,i3,i4,i5,i6,i7,i8)
              }
            }
          }
        }
      }
    }
  }
}

# interactions

#two-way
dat$age_sex <- (dat$age - 1) * n_sex + dat$sex
dat$age_eth <- (dat$age - 1) * n_eth + dat$eth
dat$age_edu <- (dat$age - 1) * n_edu + dat$edu
dat$age_inc <- (dat$age - 1) * n_inc + dat$inc
dat$age_eldx <- (dat$age - 1) * n_eldx + dat$eldx
dat$age_childx <- (dat$age - 1) * n_childx + dat$childx
dat$age_wax <- (dat$age - 1) * n_wax + dat$wax

dat$sex_eth <- (dat$sex - 1) * n_eth + dat$eth
dat$sex_edu <- (dat$sex - 1) * n_edu + dat$edu
dat$sex_inc <- (dat$sex - 1) * n_inc + dat$inc
dat$sex_eldx <- (dat$sex - 1) * n_eldx + dat$eldx
dat$sex_childx <- (dat$sex - 1) * n_childx + dat$childx
dat$sex_wax <- (dat$sex - 1) * n_wax + dat$wax

dat$eth_edu <- (dat$eth - 1) * n_edu + dat$edu
dat$eth_inc <- (dat$eth - 1) * n_inc + dat$inc
dat$eth_eldx <- (dat$eth - 1) * n_eldx + dat$eldx
dat$eth_childx <- (dat$eth - 1) * n_childx + dat$childx
dat$eth_wax <- (dat$eth - 1) * n_wax + dat$wax

dat$edu_inc <- (dat$edu - 1) * n_inc + dat$inc
dat$edu_eldx <- (dat$edu - 1) * n_eldx + dat$eldx
dat$edu_childx <- (dat$edu - 1) * n_childx + dat$childx
dat$edu_wax <- (dat$edu - 1) * n_wax + dat$wax

dat$inc_eldx <- (dat$inc - 1) * n_eldx + dat$eldx
dat$inc_childx <- (dat$inc - 1) * n_childx + dat$childx
dat$inc_wax <- (dat$inc - 1) * n_wax + dat$wax

dat$eldx_childx <- (dat$eldx - 1) * n_childx + dat$childx
dat$eldx_wax <- (dat$eldx - 1) * n_wax + dat$wax

dat$childx_wax <- (dat$childx - 1) * n_wax + dat$wax
#three-way
dat$age_sex_eth <- (dat$age - 1) * n_sex * n_eth + (dat$sex - 1) * n_eth + dat$eth
dat$age_sex_edu <- (dat$age - 1) * n_sex * n_edu + (dat$sex - 1) * n_edu + dat$edu
dat$age_sex_inc <- (dat$age - 1) * n_sex * n_inc + (dat$sex - 1) * n_inc + dat$inc
dat$age_sex_eldx <- (dat$age - 1) * n_sex * n_eldx + (dat$sex - 1) * n_eldx + dat$eldx
dat$age_sex_childx <- (dat$age - 1) * n_sex * n_childx + (dat$sex - 1) * n_childx + dat$childx
dat$age_sex_wax <- (dat$age - 1) * n_sex * n_wax + (dat$sex - 1) * n_wax + dat$wax

dat$age_eth_edu <- (dat$age - 1) * n_eth * n_edu + (dat$eth - 1) * n_edu + dat$edu
dat$age_eth_inc <- (dat$age - 1) * n_eth * n_inc + (dat$eth - 1) * n_inc + dat$inc
dat$age_eth_eldx <- (dat$age - 1) * n_eth * n_eldx + (dat$eth - 1) * n_eldx + dat$eldx
dat$age_eth_childx <- (dat$age - 1) * n_eth * n_childx + (dat$eth - 1) * n_childx + dat$childx
dat$age_eth_wax <- (dat$age - 1) * n_eth * n_wax + (dat$eth - 1) * n_wax + dat$wax

dat$age_edu_inc <- (dat$age - 1) * n_edu * n_inc + (dat$edu - 1) * n_inc + dat$inc
dat$age_edu_eldx <- (dat$age - 1) * n_edu * n_eldx + (dat$edu - 1) * n_eldx + dat$eldx
dat$age_edu_childx <- (dat$age - 1) * n_edu * n_childx + (dat$edu - 1) * n_childx + dat$childx
dat$age_edu_wax <- (dat$age - 1) * n_edu * n_wax + (dat$edu - 1) * n_wax + dat$wax

dat$age_inc_eldx <- (dat$age - 1) * n_inc * n_eldx + (dat$inc - 1) * n_eldx + dat$eldx
dat$age_inc_childx <- (dat$age - 1) * n_inc * n_childx + (dat$inc - 1) * n_childx + dat$childx
dat$age_inc_wax <- (dat$age - 1) * n_inc * n_wax + (dat$inc - 1) * n_wax + dat$wax

dat$age_eldx_childx <- (dat$age - 1) * n_eldx * n_childx + (dat$eldx - 1) * n_childx + dat$childx
dat$age_eldx_wax <- (dat$age - 1) * n_eldx * n_wax + (dat$eldx - 1) * n_wax + dat$wax
dat$age_childx_wax <- (dat$age - 1) * n_childx * n_wax + (dat$childx - 1) * n_wax + dat$wax

dat$sex_eth_edu <- (dat$sex - 1) * n_eth * n_edu + (dat$eth - 1) * n_edu + dat$edu
dat$sex_eth_inc <- (dat$sex - 1) * n_eth * n_inc + (dat$eth - 1) * n_inc + dat$inc
dat$sex_eth_eldx <- (dat$sex - 1) * n_eth * n_eldx + (dat$eth - 1) * n_eldx + dat$eldx
dat$sex_eth_childx <- (dat$sex - 1) * n_eth * n_childx + (dat$eth - 1) * n_childx + dat$childx
dat$sex_eth_wax <- (dat$sex - 1) * n_eth * n_wax + (dat$eth - 1) * n_wax + dat$wax

dat$sex_edu_inc <- (dat$sex - 1) * n_edu * n_inc + (dat$edu - 1) * n_inc + dat$inc
dat$sex_edu_eldx <- (dat$sex - 1) * n_edu * n_eldx + (dat$edu - 1) * n_eldx + dat$eldx
dat$sex_edu_childx <- (dat$sex - 1) * n_edu * n_childx + (dat$edu - 1) * n_childx + dat$childx
dat$sex_edu_wax <- (dat$sex - 1) * n_edu * n_wax + (dat$edu - 1) * n_wax + dat$wax

dat$sex_inc_eldx <- (dat$sex - 1) * n_inc * n_eldx + (dat$inc - 1) * n_eldx + dat$eldx
dat$sex_inc_childx <- (dat$sex - 1) * n_inc * n_childx + (dat$inc - 1) * n_childx + dat$childx
dat$sex_inc_wax <- (dat$sex - 1) * n_inc * n_wax + (dat$inc - 1) * n_wax + dat$wax

dat$sex_eldx_childx <- (dat$sex - 1) * n_eldx * n_childx + (dat$eldx - 1) * n_childx + dat$childx
dat$sex_eldx_wax <- (dat$sex - 1) * n_eldx * n_wax + (dat$eldx - 1) * n_wax + dat$wax

dat$sex_childx_wax <- (dat$sex - 1) * n_childx * n_wax + (dat$childx - 1) * n_wax + dat$wax

dat$eth_edu_inc <- (dat$eth - 1) * n_edu * n_inc + (dat$edu - 1) * n_inc + dat$inc
dat$eth_edu_eldx <- (dat$eth - 1) * n_edu * n_eldx + (dat$edu - 1) * n_eldx + dat$eldx
dat$eth_edu_childx <- (dat$eth - 1) * n_edu * n_childx + (dat$edu - 1) * n_childx + dat$childx
dat$eth_edu_wax <- (dat$eth - 1) * n_edu * n_wax + (dat$edu - 1) * n_wax + dat$wax

dat$eth_inc_eldx <- (dat$eth - 1) * n_inc * n_eldx + (dat$inc - 1) * n_eldx + dat$eldx
dat$eth_inc_childx <- (dat$eth - 1) * n_inc * n_childx + (dat$inc - 1) * n_childx + dat$childx
dat$eth_inc_wax <- (dat$eth - 1) * n_inc * n_wax + (dat$inc - 1) * n_wax + dat$wax

dat$eth_eldx_childx <- (dat$eth - 1) * n_eldx * n_childx + (dat$eldx - 1) * n_childx + dat$childx
dat$eth_eldx_wax <- (dat$eth - 1) * n_eldx * n_wax + (dat$eldx - 1) * n_wax + dat$wax

dat$eth_childx_wax <- (dat$eth - 1) * n_childx * n_wax + (dat$childx - 1) * n_wax + dat$wax

dat$edu_inc_eldx <- (dat$edu - 1) * n_inc * n_eldx + (dat$inc - 1) * n_eldx + dat$eldx
dat$edu_inc_childx <- (dat$edu - 1) * n_inc * n_childx + (dat$inc - 1) * n_childx + dat$childx
dat$edu_inc_wax <- (dat$edu - 1) * n_inc * n_wax + (dat$inc - 1) * n_wax + dat$wax

dat$edu_eldx_childx <- (dat$edu - 1) * n_eldx * n_childx + (dat$eldx - 1) * n_childx + dat$childx
dat$edu_eldx_wax <- (dat$edu - 1) * n_eldx * n_wax + (dat$eldx - 1) * n_wax + dat$wax

dat$edu_childx_wax <- (dat$edu - 1) * n_childx * n_wax + (dat$childx - 1) * n_wax + dat$wax

dat$inc_eldx_childx <- (dat$inc - 1) * n_eldx * n_childx + (dat$eldx- 1) * n_childx + dat$childx
dat$inc_eldx_wax <- (dat$inc - 1) * n_eldx * n_wax + (dat$eldx - 1) * n_wax + dat$wax
dat$inc_childx_wax <- (dat$inc - 1) * n_childx * n_wax + (dat$childx - 1) * n_wax + dat$wax
dat$eldx_childx_wax <- (dat$eldx - 1) * n_childx * n_wax + (dat$childx - 1) * n_wax + dat$wax

###-----------------STAN--------------------------###
stan.data <- list(n=nrow(dat),
                  q=8,
                  #k_grp_2=28,
                  #k_grp_3=56,
                  #cell_id=cell_id,
                  #J=length(as.numeric(table(cell_id))),
                  y=dat$Y, 
                  age=dat$age,
                  sex=dat$sex,
                  eth=dat$eth,
                  edu=dat$edu,
                  inc=dat$inc,
                  eldx=dat$eldx,
                  childx=dat$childx,
                  wax=dat$wax,
                  age_sex=dat$age_sex,
                  age_eth=dat$age_eth,
                  age_edu=dat$age_edu,
                  age_inc=dat$age_inc,
                  age_eldx=dat$age_eldx,
                  age_childx=dat$age_childx,
                  age_wax=dat$age_wax,  
                  sex_eth=dat$sex_eth,
                  sex_edu=dat$sex_edu,                  
                  sex_inc=dat$sex_inc,
                  sex_eldx=dat$sex_eldx,
                  sex_childx=dat$sex_childx,
                  sex_wax=dat$sex_wax,
                  eth_edu=dat$eth_edu,
                  eth_inc=dat$eth_inc,
                  eth_eldx=dat$eth_eldx,
                  eth_childx=dat$childx,
                  eth_wax=dat$eth_wax,
                  edu_inc=dat$edu_inc,
                  edu_eldx=dat$edu_eldx,
                  edu_childx=dat$edu_childx,
                  edu_wax=dat$edu_wax,
                  inc_eldx=dat$inc_eldx,
                  inc_childx=dat$inc_childx,
                  inc_wax=dat$inc_wax,
                  eldx_childx=dat$eldx_childx,
                  eldx_wax=dat$eldx_wax,
                  childx_wax=dat$childx_wax,
                  age_sex_eth=dat$age_sex_eth,     
                  age_sex_edu=dat$age_sex_edu,
                  age_sex_inc=dat$age_sex_inc,
                  age_sex_eldx=dat$age_sex_eldx,
                  age_sex_childx=dat$age_sex_childx,
                  age_sex_wax=dat$age_sex_wax,
                  age_eth_edu=dat$age_eth_edu,
                  age_eth_inc=dat$age_eth_inc, 
                  age_eth_eldx=dat$age_eth_eldx,
                  age_eth_childx=dat$age_eth_childx,
                  age_eth_wax=dat$age_eth_wax,
                  age_edu_inc=dat$age_edu_inc,
                  age_edu_eldx=dat$age_edu_eldx,
                  age_edu_childx=dat$age_edu_childx,
                  age_edu_wax=dat$age_edu_wax,
                  age_inc_eldx=dat$age_inc_eldx,
                  age_inc_childx=dat$age_inc_childx,
                  age_inc_wax=dat$age_inc_wax,
                  age_eldx_childx=dat$age_eldx_childx,
                  age_eldx_wax=dat$age_eldx_wax,
                  age_childx_wax=dat$age_childx_wax,
                  sex_eth_edu=dat$sex_eth_edu,
                  sex_eth_inc=dat$sex_eth_inc,
                  sex_eth_eldx=dat$sex_eth_eldx,
                  sex_eth_childx=dat$sex_eth_childx,
                  sex_eth_wax=dat$sex_eth_wax,
                  sex_edu_inc=dat$sex_edu_inc,
                  sex_edu_eldx=dat$sex_edu_eldx,
                  sex_edu_childx=dat$sex_edu_childx,
                  sex_edu_wax=dat$sex_edu_wax,
                  sex_inc_eldx=dat$sex_inc_eldx,
                  sex_inc_childx=dat$sex_inc_childx,
                  sex_inc_wax=dat$sex_inc_wax,
                  sex_eldx_childx=dat$sex_eldx_childx,
                  sex_eldx_wax=dat$sex_eldx_wax,
                  sex_childx_wax=dat$sex_childx_wax,
                  eth_edu_inc=dat$eth_edu_inc,
                  eth_edu_eldx=dat$eth_edu_eldx,
                  eth_edu_childx=dat$eth_edu_childx,
                  eth_edu_wax=dat$eth_edu_wax,
                  eth_inc_eldx=dat$eth_inc_eldx,
                  eth_inc_childx=dat$eth_inc_childx,
                  eth_inc_wax=dat$eth_inc_wax,
                  eth_eldx_childx=dat$eth_eldx_childx,
                  eth_eldx_wax=dat$eth_eldx_wax,
                  eth_childx_wax=dat$eth_childx_wax,
                  edu_inc_eldx=dat$edu_inc_eldx,
                  edu_inc_childx=dat$edu_inc_childx,
                  edu_inc_wax=dat$edu_inc_wax,
                  edu_eldx_childx=dat$edu_eldx_childx,
                  edu_eldx_wax=dat$edu_eldx_wax,
                  edu_childx_wax=dat$edu_childx_wax,
                  inc_eldx_childx=dat$inc_eldx_childx,
                  inc_eldx_wax=dat$inc_eldx_wax,
                  inc_childx_wax=dat$inc_childx_wax,
                  eldx_childx_wax=dat$eldx_childx_wax,
                  J_age=max(dat$age),
                  J_sex=max(dat$sex),
                  J_eth=max(dat$eth),
                  J_edu=max(dat$edu),
                  J_inc=max(dat$inc),
                  J_eldx=max(dat$eldx),
                  J_childx=max(dat$childx),
                  J_wax=max(dat$wax),
                  J_age_sex=max(dat$age_sex),
                  J_age_eth=max(dat$age_eth),
                  J_age_edu=max(dat$age_edu),
                  J_age_inc=max(dat$age_inc),
                  J_age_eldx=max(dat$age_eldx),
                  J_age_childx=max(dat$age_childx),
                  J_age_wax=max(dat$age_wax),  
                  J_sex_eth=max(dat$sex_eth),
                  J_sex_edu=max(dat$sex_edu),                  
                  J_sex_inc=max(dat$sex_inc),
                  J_sex_eldx=max(dat$sex_eldx),
                  J_sex_childx=max(dat$sex_childx),
                  J_sex_wax=max(dat$sex_wax),
                  J_eth_edu=max(dat$eth_edu),
                  J_eth_inc=max(dat$eth_inc),
                  J_eth_eldx=max(dat$eth_eldx),
                  J_eth_childx=max(dat$childx),
                  J_eth_wax=max(dat$eth_wax),
                  J_edu_inc=max(dat$edu_inc),
                  J_edu_eldx=max(dat$edu_eldx),
                  J_edu_childx=max(dat$edu_childx),
                  J_edu_wax=max(dat$edu_wax),
                  J_inc_eldx=max(dat$inc_eldx),
                  J_inc_childx=max(dat$inc_childx),
                  J_inc_wax=max(dat$inc_wax),
                  J_eldx_childx=max(dat$eldx_childx),
                  J_eldx_wax=max(dat$eldx_wax),
                  J_childx_wax=max(dat$childx_wax),
                  J_age_sex_eth=max(dat$age_sex_eth),     
                  J_age_sex_edu=max(dat$age_sex_edu),
                  J_age_sex_inc=max(dat$age_sex_inc),
                  J_age_sex_eldx=max(dat$age_sex_eldx),
                  J_age_sex_childx=max(dat$age_sex_childx),
                  J_age_sex_wax=max(dat$age_sex_wax),
                  J_age_eth_edu=max(dat$age_eth_edu),
                  J_age_eth_inc=max(dat$age_eth_inc), 
                  J_age_eth_eldx=max(dat$age_eth_eldx),
                  J_age_eth_childx=max(dat$age_eth_childx),
                  J_age_eth_wax=max(dat$age_eth_wax),
                  J_age_edu_inc=max(dat$age_edu_inc),
                  J_age_edu_eldx=max(dat$age_edu_eldx),
                  J_age_edu_childx=max(dat$age_edu_childx),
                  J_age_edu_wax=max(dat$age_edu_wax),
                  J_age_inc_eldx=max(dat$age_inc_eldx),
                  J_age_inc_childx=max(dat$age_inc_childx),
                  J_age_inc_wax=max(dat$age_inc_wax),
                  J_age_eldx_childx=max(dat$age_eldx_childx),
                  J_age_eldx_wax=max(dat$age_eldx_wax),
                  J_age_childx_wax=max(dat$age_childx_wax),
                  J_sex_eth_edu=max(dat$sex_eth_edu),
                  J_sex_eth_inc=max(dat$sex_eth_inc),
                  J_sex_eth_eldx=max(dat$sex_eth_eldx),
                  J_sex_eth_childx=max(dat$sex_eth_childx),
                  J_sex_eth_wax=max(dat$sex_eth_wax),
                  J_sex_edu_inc=max(dat$sex_edu_inc),
                  J_sex_edu_eldx=max(dat$sex_edu_eldx),
                  J_sex_edu_childx=max(dat$sex_edu_childx),
                  J_sex_edu_wax=max(dat$sex_edu_wax),
                  J_sex_inc_eldx=max(dat$sex_inc_eldx),
                  J_sex_inc_childx=max(dat$sex_inc_childx),
                  J_sex_inc_wax=max(dat$sex_inc_wax),
                  J_sex_eldx_childx=max(dat$sex_eldx_childx),
                  J_sex_eldx_wax=max(dat$sex_eldx_wax),
                  J_sex_childx_wax=max(dat$sex_childx_wax),
                  J_eth_edu_inc=max(dat$eth_edu_inc),
                  J_eth_edu_eldx=max(dat$eth_edu_eldx),
                  J_eth_edu_childx=max(dat$eth_edu_childx),
                  J_eth_edu_wax=max(dat$eth_edu_wax),
                  J_eth_inc_eldx=max(dat$eth_inc_eldx),
                  J_eth_inc_childx=max(dat$eth_inc_childx),
                  J_eth_inc_wax=max(dat$eth_inc_wax),
                  J_eth_eldx_childx=max(dat$eth_eldx_childx),
                  J_eth_eldx_wax=max(dat$eth_eldx_wax),
                  J_eth_childx_wax=max(dat$eth_childx_wax),
                  J_edu_inc_eldx=max(dat$edu_inc_eldx),
                  J_edu_inc_childx=max(dat$edu_inc_childx),
                  J_edu_inc_wax=max(dat$edu_inc_wax),
                  J_edu_eldx_childx=max(dat$edu_eldx_childx),
                  J_edu_eldx_wax=max(dat$edu_eldx_wax),
                  J_edu_childx_wax=max(dat$edu_childx_wax),
                  J_inc_eldx_childx=max(dat$inc_eldx_childx),
                  J_inc_eldx_wax=max(dat$inc_eldx_wax),
                  J_inc_childx_wax=max(dat$inc_childx_wax),
                  J_eldx_childx_wax=max(dat$eldx_childx_wax),
                  lambda_m=1,
                  log_sigma2_m=rep(2,8),
                  lambda_inter=rep(1,2),
                  sigma_y=2
                  )

set_cppo("debug")
S.compile <- stan(file='stan/mrpweights120214.stan',
                  data=stan.data,
                  iter=2, chains=1)
gc()

n.chains <- 6
stan.seed <- round(runif(1,0,9999999))
registerDoMC(n.chains)
st <- system.time(sflist1 <- foreach(i.cores = 1:getDoParWorkers()) %dopar% {
  S <- stan(fit=S.compile,
            data=stan.data,
            iter=1000, chains=1, seed=stan.seed, chain_id=i.cores)
  return(S)
})[3]
S <- sflist2stanfit(sflist1)

save.image("model_2012_20130520.RData")

############################################################################################################
############################################################################################################
############################################################################################################

#load("model_2012_20130520.RData")
require(car)
require(arm)
require(rstan)

set.seed(12345)
ext <- extract(S)

n.iter <- length(ext$alpha)
n.samp <- 100
samp <- sample(1:n.iter, n.samp)
yhat <- array(NA, c(length(stan.data$y), n.samp))
for (i in 1:n.samp) {
  cat(paste(i, "of", n.samp, "\n"))
  ix <- samp[i]
  yhat[,i] <- invlogit(ext$alpha[ix] +
                         ext$alpha_sex[ix, stan.data$sex] * (ext$sigma_alpha[ix,1] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth[ix, stan.data$eth] * (ext$sigma_alpha[ix,2] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc[ix, stan.data$inc] * (ext$sigma_alpha[ix,3] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_mar[ix, stan.data$mar] * (ext$sigma_alpha[ix,4] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_edu[ix, stan.data$edu] * (ext$sigma_alpha[ix,5] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_age[ix, stan.data$age] * (ext$sigma_alpha[ix,6] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_pid[ix, stan.data$pid] * (ext$sigma_alpha[ix,7] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_prmd[ix, stan.data$prmd] * (ext$sigma_alpha[ix,8] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_votes[ix, stan.data$votes] * (ext$sigma_alpha[ix,9] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sttgrp[ix, stan.data$sttgrp] * (ext$sigma_alpha[ix,10] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_reg[ix, stan.data$reg] * (ext$sigma_alpha[ix,11] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_stt[ix, stan.data$stt] * (ext$sigma_alpha[ix,12] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_eth[ix, stan.data$sex_eth] * (ext$sigma_alpha[ix,13] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_inc[ix, stan.data$sex_inc] * (ext$sigma_alpha[ix,14] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_mar[ix, stan.data$sex_mar] * (ext$sigma_alpha[ix,15] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_edu[ix, stan.data$sex_edu] * (ext$sigma_alpha[ix,16] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_age[ix, stan.data$sex_age] * (ext$sigma_alpha[ix,17] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_pid[ix, stan.data$sex_pid] * (ext$sigma_alpha[ix,18] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_prmd[ix, stan.data$sex_prmd] * (ext$sigma_alpha[ix,19] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_votes[ix, stan.data$sex_votes] * (ext$sigma_alpha[ix,20] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_sttgrp[ix, stan.data$sex_sttgrp] * (ext$sigma_alpha[ix,21] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_reg[ix, stan.data$sex_reg] * (ext$sigma_alpha[ix,22] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_sex_stt[ix, stan.data$sex_stt] * (ext$sigma_alpha[ix,23] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_inc[ix, stan.data$eth_inc] * (ext$sigma_alpha[ix,24] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_mar[ix, stan.data$eth_mar] * (ext$sigma_alpha[ix,25] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_edu[ix, stan.data$eth_edu] * (ext$sigma_alpha[ix,26] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_age[ix, stan.data$eth_age] * (ext$sigma_alpha[ix,27] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_pid[ix, stan.data$eth_pid] * (ext$sigma_alpha[ix,28] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_prmd[ix, stan.data$eth_prmd] * (ext$sigma_alpha[ix,29] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_votes[ix, stan.data$eth_votes] * (ext$sigma_alpha[ix,30] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_sttgrp[ix, stan.data$eth_sttgrp] * (ext$sigma_alpha[ix,31] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_reg[ix, stan.data$eth_reg] * (ext$sigma_alpha[ix,32] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_eth_stt[ix, stan.data$eth_stt] * (ext$sigma_alpha[ix,33] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc_mar[ix, stan.data$inc_mar] * (ext$sigma_alpha[ix,34] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc_edu[ix, stan.data$inc_edu] * (ext$sigma_alpha[ix,35] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc_age[ix, stan.data$inc_age] * (ext$sigma_alpha[ix,36] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc_pid[ix, stan.data$inc_pid] * (ext$sigma_alpha[ix,37] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc_prmd[ix, stan.data$inc_prmd] * (ext$sigma_alpha[ix,38] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc_votes[ix, stan.data$inc_votes] * (ext$sigma_alpha[ix,39] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc_sttgrp[ix, stan.data$inc_sttgrp] * (ext$sigma_alpha[ix,40] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc_reg[ix, stan.data$inc_reg] * (ext$sigma_alpha[ix,41] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_inc_stt[ix, stan.data$inc_stt] * (ext$sigma_alpha[ix,42] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_mar_edu[ix, stan.data$mar_edu] * (ext$sigma_alpha[ix,43] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_mar_age[ix, stan.data$mar_age] * (ext$sigma_alpha[ix,44] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_mar_pid[ix, stan.data$mar_pid] * (ext$sigma_alpha[ix,45] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_mar_prmd[ix, stan.data$mar_prmd] * (ext$sigma_alpha[ix,46] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_mar_votes[ix, stan.data$mar_votes] * (ext$sigma_alpha[ix,47] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_mar_sttgrp[ix, stan.data$mar_sttgrp] * (ext$sigma_alpha[ix,48] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_mar_reg[ix, stan.data$mar_reg] * (ext$sigma_alpha[ix,49] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_mar_stt[ix, stan.data$mar_stt] * (ext$sigma_alpha[ix,50] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_edu_age[ix, stan.data$edu_age] * (ext$sigma_alpha[ix,51] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_edu_pid[ix, stan.data$edu_pid] * (ext$sigma_alpha[ix,52] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_edu_prmd[ix, stan.data$edu_prmd] * (ext$sigma_alpha[ix,53] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_edu_votes[ix, stan.data$edu_votes] * (ext$sigma_alpha[ix,54] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_edu_sttgrp[ix, stan.data$edu_sttgrp] * (ext$sigma_alpha[ix,55] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_edu_reg[ix, stan.data$edu_reg] * (ext$sigma_alpha[ix,56] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_edu_stt[ix, stan.data$edu_stt] * (ext$sigma_alpha[ix,57] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_age_pid[ix, stan.data$age_pid] * (ext$sigma_alpha[ix,58] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_age_prmd[ix, stan.data$age_prmd] * (ext$sigma_alpha[ix,59] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_age_votes[ix, stan.data$age_votes] * (ext$sigma_alpha[ix,60] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_age_sttgrp[ix, stan.data$age_sttgrp] * (ext$sigma_alpha[ix,61] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_age_reg[ix, stan.data$age_reg] * (ext$sigma_alpha[ix,62] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_age_stt[ix, stan.data$age_stt] * (ext$sigma_alpha[ix,63] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_pid_prmd[ix, stan.data$pid_prmd] * (ext$sigma_alpha[ix,64] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_pid_votes[ix, stan.data$pid_votes] * (ext$sigma_alpha[ix,65] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_pid_sttgrp[ix, stan.data$pid_sttgrp] * (ext$sigma_alpha[ix,66] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_pid_reg[ix, stan.data$pid_reg] * (ext$sigma_alpha[ix,67] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_pid_stt[ix, stan.data$pid_stt] * (ext$sigma_alpha[ix,68] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_prmd_votes[ix, stan.data$prmd_votes] * (ext$sigma_alpha[ix,69] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_prmd_sttgrp[ix, stan.data$prmd_sttgrp] * (ext$sigma_alpha[ix,70] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_prmd_reg[ix, stan.data$prmd_reg] * (ext$sigma_alpha[ix,71] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_prmd_stt[ix, stan.data$prmd_stt] * (ext$sigma_alpha[ix,72] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_votes_sttgrp[ix, stan.data$votes_sttgrp] * (ext$sigma_alpha[ix,73] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_votes_reg[ix, stan.data$votes_reg] * (ext$sigma_alpha[ix,74] * ext$sigma_sigma_alpha[ix]) + 
                         ext$alpha_votes_stt[ix, stan.data$votes_stt] * (ext$sigma_alpha[ix,75] * ext$sigma_sigma_alpha[ix]) + 
                         (ext$beta[ix] +
                            ext$beta_eth[ix, stan.data$eth] * (ext$sigma_beta[ix,1] * ext$sigma_sigma_beta[ix]) + 
                            ext$beta_pid[ix, stan.data$pid] * (ext$sigma_beta[ix,2] * ext$sigma_sigma_beta[ix]) + 
                            ext$beta_prmd[ix, stan.data$prmd] * (ext$sigma_beta[ix,3] * ext$sigma_sigma_beta[ix])) * stan.data$z_dem2wayprev +
                         as.numeric(as.matrix(stan.data$Z) %*% as.matrix(ext$beta_z[ix,])))
}
print(cor(yhat[,1:3]))
yhat.mu <- apply(yhat, 1, mean)

############################################################################################################

alphas <- as.data.frame(array(NA, c(n_sex*n_eth*n_inc*n_mar*n_edu*n_age*n_pid*n_prmd*n_votes*n_stt, 176)))
alphas[,1:10] <- as.data.frame(expand.grid(1:stan.data$n_sex,
                                           1:stan.data$n_eth,
                                           1:stan.data$n_inc,
                                           1:stan.data$n_mar,
                                           1:stan.data$n_edu,
                                           1:stan.data$n_age,
                                           1:stan.data$n_pid,
                                           1:stan.data$n_prmd,
                                           1:stan.data$n_votes,
                                           1:stan.data$n_stt))
colnames(alphas) <- 
  c("sex", "eth", "inc", "mar", "edu", "age", "pid", "prmd", "votes", "stt", "grp", "reg", "sttgrp", "sex_eth", 
    "sex_inc", "sex_mar", "sex_edu", "sex_age", "sex_pid", "sex_prmd", "sex_votes", "sex_sttgrp", "sex_reg", 
    "sex_stt", "eth_inc", "eth_mar", "eth_edu", "eth_age", "eth_pid", "eth_prmd", "eth_votes", "eth_sttgrp", 
    "eth_reg", "eth_stt", "inc_mar", "inc_edu", "inc_age", "inc_pid", "inc_prmd", "inc_votes", "inc_sttgrp", 
    "inc_reg", "inc_stt", "mar_edu", "mar_age", "mar_pid", "mar_prmd", "mar_votes", "mar_sttgrp", "mar_reg", 
    "mar_stt", "edu_age", "edu_pid", "edu_prmd", "edu_votes", "edu_sttgrp", "edu_reg", "edu_stt", "age_pid", 
    "age_prmd", "age_votes", "age_sttgrp", "age_reg", "age_stt", "pid_prmd", "pid_votes", "pid_sttgrp", 
    "pid_reg", "pid_stt", "prmd_votes", "prmd_sttgrp", "prmd_reg", "prmd_stt", "votes_sttgrp", "votes_reg", "votes_stt",
    paste0("a", 1:100))
alphas$grp <- apply(alphas[,1:10], 1, paste0, collapse="")
gc()

cat("reg\n"); alphas$reg <- car::recode(alphas$stt, "1=4; 2=3; 3=3; 4=4; 5=4; 6=4; 7=1; 8=5; 9=1; 10=3; 11=3; 12=4; 13=2; 14=4; 15=2; 16=2; 17=2; 18=3; 19=3; 20=1; 21=1; 22=1; 23=2; 24=2; 25=2; 26=3; 27=4; 28=3; 29=2; 30=2; 31=1; 32=1; 33=4; 34=4; 35=1; 36=2; 37=3; 38=4; 39=1; 40=1; 41=3; 42=2; 43=3; 44=3; 45=4; 46=3; 47=1; 48=4; 49=2; 50=1; 51=4; else=NA")
cat("sttgrp\n"); alphas$sttgrp <- car::recode(alphas$stt, "1=3; 2=1; 3=2; 4=4; 5=4; 6=4; 7=3; 8=3; 9=3; 10=3; 11=2; 12=1; 13=4; 14=3; 15=2; 16=2; 17=3; 18=3; 19=3; 20=4; 21=4; 22=4; 23=1; 24=1; 25=1; 26=2; 27=1; 28=4; 29=1; 30=4; 31=4; 32=4; 33=4; 34=3; 35=4; 36=4; 37=3; 38=3; 39=4; 40=4; 41=2; 42=3; 43=2; 44=2; 45=3; 46=2; 47=2; 48=2; 49=1; 50=4; 51=4; else=NA")
cat("sex_eth\n"); alphas$sex_eth <- (alphas$sex - 1) * n_eth + alphas$eth
cat("sex_inc\n"); alphas$sex_inc <- (alphas$sex - 1) * n_inc + alphas$inc
cat("sex_mar\n"); alphas$sex_mar <- (alphas$sex - 1) * n_mar + alphas$mar
cat("sex_edu\n"); alphas$sex_edu <- (alphas$sex - 1) * n_edu + alphas$edu
cat("sex_age\n"); alphas$sex_age <- (alphas$sex - 1) * n_age + alphas$age
cat("sex_pid\n"); alphas$sex_pid <- (alphas$sex - 1) * n_pid + alphas$pid
cat("sex_prmd\n"); alphas$sex_prmd <- (alphas$sex - 1) * n_prmd + alphas$prmd
cat("sex_votes\n"); alphas$sex_votes <- (alphas$sex - 1) * n_votes + alphas$votes
cat("sex_sttgrp\n"); alphas$sex_sttgrp <- (alphas$sex - 1) * n_sttgrp + alphas$sttgrp
cat("sex_reg\n"); alphas$sex_reg <- (alphas$sex - 1) * n_reg + alphas$reg
cat("sex_stt\n"); alphas$sex_stt <- (alphas$sex - 1) * n_stt + alphas$stt
cat("eth_inc\n"); alphas$eth_inc <- (alphas$eth - 1) * n_inc + alphas$inc
cat("eth_mar\n"); alphas$eth_mar <- (alphas$eth - 1) * n_mar + alphas$mar
cat("eth_edu\n"); alphas$eth_edu <- (alphas$eth - 1) * n_edu + alphas$edu
cat("eth_age\n"); alphas$eth_age <- (alphas$eth - 1) * n_age + alphas$age
cat("eth_pid\n"); alphas$eth_pid <- (alphas$eth - 1) * n_pid + alphas$pid
cat("eth_prmd\n"); alphas$eth_prmd <- (alphas$eth - 1) * n_prmd + alphas$prmd
cat("eth_votes\n"); alphas$eth_votes <- (alphas$eth - 1) * n_votes + alphas$votes
cat("eth_sttgrp\n"); alphas$eth_sttgrp <- (alphas$eth - 1) * n_sttgrp + alphas$sttgrp
cat("eth_reg\n"); alphas$eth_reg <- (alphas$eth - 1) * n_reg + alphas$reg
cat("eth_stt\n"); alphas$eth_stt <- (alphas$eth - 1) * n_stt + alphas$stt
cat("inc_mar\n"); alphas$inc_mar <- (alphas$inc - 1) * n_mar + alphas$mar
cat("inc_edu\n"); alphas$inc_edu <- (alphas$inc - 1) * n_edu + alphas$edu
cat("inc_age\n"); alphas$inc_age <- (alphas$inc - 1) * n_age + alphas$age
cat("inc_pid\n"); alphas$inc_pid <- (alphas$inc - 1) * n_pid + alphas$pid
cat("inc_prmd\n"); alphas$inc_prmd <- (alphas$inc - 1) * n_prmd + alphas$prmd
cat("inc_votes\n"); alphas$inc_votes <- (alphas$inc - 1) * n_votes + alphas$votes
cat("inc_sttgrp\n"); alphas$inc_sttgrp <- (alphas$inc - 1) * n_sttgrp + alphas$sttgrp
cat("inc_reg\n"); alphas$inc_reg <- (alphas$inc - 1) * n_reg + alphas$reg
cat("inc_stt\n"); alphas$inc_stt <- (alphas$inc - 1) * n_stt + alphas$stt
cat("mar_edu\n"); alphas$mar_edu <- (alphas$mar - 1) * n_edu + alphas$edu
cat("mar_age\n"); alphas$mar_age <- (alphas$mar - 1) * n_age + alphas$age
cat("mar_pid\n"); alphas$mar_pid <- (alphas$mar - 1) * n_pid + alphas$pid
cat("mar_prmd\n"); alphas$mar_prmd <- (alphas$mar - 1) * n_prmd + alphas$prmd
cat("mar_votes\n"); alphas$mar_votes <- (alphas$mar - 1) * n_votes + alphas$votes
cat("mar_sttgrp\n"); alphas$mar_sttgrp <- (alphas$mar - 1) * n_sttgrp + alphas$sttgrp
cat("mar_reg\n"); alphas$mar_reg <- (alphas$mar - 1) * n_reg + alphas$reg
cat("mar_stt\n"); alphas$mar_stt <- (alphas$mar - 1) * n_stt + alphas$stt
cat("edu_age\n"); alphas$edu_age <- (alphas$edu - 1) * n_age + alphas$age
cat("edu_pid\n"); alphas$edu_pid <- (alphas$edu - 1) * n_pid + alphas$pid
cat("edu_prmd\n"); alphas$edu_prmd <- (alphas$edu - 1) * n_prmd + alphas$prmd
cat("edu_votes\n"); alphas$edu_votes <- (alphas$edu - 1) * n_votes + alphas$votes
cat("edu_sttgrp\n"); alphas$edu_sttgrp <- (alphas$edu - 1) * n_sttgrp + alphas$sttgrp
cat("edu_reg\n"); alphas$edu_reg <- (alphas$edu - 1) * n_reg + alphas$reg
cat("edu_stt\n"); alphas$edu_stt <- (alphas$edu - 1) * n_stt + alphas$stt
cat("age_pid\n"); alphas$age_pid <- (alphas$age - 1) * n_pid + alphas$pid
cat("age_prmd\n"); alphas$age_prmd <- (alphas$age - 1) * n_prmd + alphas$prmd
cat("age_votes\n"); alphas$age_votes <- (alphas$age - 1) * n_votes + alphas$votes
cat("age_sttgrp\n"); alphas$age_sttgrp <- (alphas$age - 1) * n_sttgrp + alphas$sttgrp
cat("age_reg\n"); alphas$age_reg <- (alphas$age - 1) * n_reg + alphas$reg
cat("age_stt\n"); alphas$age_stt <- (alphas$age - 1) * n_stt + alphas$stt
cat("pid_prmd\n"); alphas$pid_prmd <- (alphas$pid - 1) * n_prmd + alphas$prmd
cat("pid_votes\n"); alphas$pid_votes <- (alphas$pid - 1) * n_votes + alphas$votes
cat("pid_sttgrp\n"); alphas$pid_sttgrp <- (alphas$pid - 1) * n_sttgrp + alphas$sttgrp
cat("pid_reg\n"); alphas$pid_reg <- (alphas$pid - 1) * n_reg + alphas$reg
cat("pid_stt\n"); alphas$pid_stt <- (alphas$pid - 1) * n_stt + alphas$stt
cat("prmd_votes\n"); alphas$prmd_votes <- (alphas$prmd - 1) * n_votes + alphas$votes
cat("prmd_sttgrp\n"); alphas$prmd_sttgrp <- (alphas$prmd - 1) * n_sttgrp + alphas$sttgrp
cat("prmd_reg\n"); alphas$prmd_reg <- (alphas$prmd - 1) * n_reg + alphas$reg
cat("prmd_stt\n"); alphas$prmd_stt <- (alphas$prmd - 1) * n_stt + alphas$stt
cat("votes_sttgrp\n"); alphas$votes_sttgrp <- (alphas$votes - 1) * n_sttgrp + alphas$sttgrp
cat("votes_reg\n"); alphas$votes_reg <- (alphas$votes - 1) * n_reg + alphas$reg
cat("votes_stt\n"); alphas$votes_stt <- (alphas$votes - 1) * n_stt + alphas$stt

grps <- c("sex", "eth", "inc", "mar", "edu", "age", "pid", "prmd", "votes", "sttgrp", "reg", "stt", "sex_eth", "sex_inc", 
          "sex_mar", "sex_edu", "sex_age", "sex_pid", "sex_prmd", "sex_votes", "sex_sttgrp", "sex_reg", "sex_stt", "eth_inc", 
          "eth_mar", "eth_edu", "eth_age", "eth_pid", "eth_prmd", "eth_votes", "eth_sttgrp", "eth_reg", "eth_stt", "inc_mar", 
          "inc_edu", "inc_age", "inc_pid", "inc_prmd", "inc_votes", "inc_sttgrp", "inc_reg", "inc_stt", "mar_edu", "mar_age", 
          "mar_pid", "mar_prmd", "mar_votes", "mar_sttgrp", "mar_reg", "mar_stt", "edu_age", "edu_pid", "edu_prmd", "edu_votes", 
          "edu_sttgrp", "edu_reg", "edu_stt", "age_pid", "age_prmd", "age_votes", "age_sttgrp", "age_reg", "age_stt", "pid_prmd", 
          "pid_votes", "pid_sttgrp", "pid_reg", "pid_stt", "prmd_votes", "prmd_sttgrp", "prmd_reg", "prmd_stt", "votes_sttgrp", 
          "votes_reg", "votes_stt")
for (i in 1:n.samp) {
  cat(paste(i, "of", n.samp, "\n"))
  #  eval(parse(text=paste0("alphas$a", i, " <- NA")))
  ix <- samp[i]
  alpha_tmp <- rep(ext$alpha[ix], nrow(alphas))
  for (i.grp in 1:length(grps)) {
    ix.grp <- grps[i.grp]
    ok <- alphas[,ix.grp] %in% unique(dat[,ix.grp])
    alpha_tmp[ok] <- alpha_tmp[ok] + 
      ext[[paste0("alpha_", ix.grp)]][cbind(ix, alphas[ok, ix.grp])] * 
      ext$sigma_alpha[ix, i.grp] * ext$sigma_sigma_alpha[ix]
  }
  alphas[, paste0("a", i)] <- round(alpha_tmp, 3)
  gc()
}

system.time(write.table(alphas[,c("grp", paste0("a", 1:n.samp))], 
                        "yg_academic2012_alphas_20130520.txt", sep="\t", quote=F, row.names=F))

###############################################################################################

betas <- as.data.frame(expand.grid(1:stan.data$n_eth,
                                   1:stan.data$n_pid,
                                   1:stan.data$n_prmd))
colnames(betas) <- c("eth", "pid", "prmd")
betas$grp <- apply(betas, 1, paste, collapse="")
grps <- c("eth", "pid", "prmd")
for (i in 1:n.samp) {
  cat(paste(i, "of", n.samp, "\n"))
  eval(parse(text=paste0("betas$b", i, " <- NA")))
  ix <- samp[i]
  beta_tmp <- rep(ext$beta[ix], nrow(betas))
  for (i.grp in 1:length(grps)) {
    ix.grp <- grps[i.grp]
    ok <- betas[,ix.grp] %in% unique(dat[,ix.grp])
    beta_tmp[ok] <- beta_tmp[ok] + 
      ext[[paste0("beta_", ix.grp)]][cbind(ix, betas[ok, ix.grp])] * 
      ext$sigma_beta[ix, i.grp] * ext$sigma_sigma_beta[ix]
  }
  betas[, paste0("b", i)] <- round(beta_tmp, 3)
}
gc()

system.time(write.table(betas[,c("grp", paste0("b", 1:n.samp))], 
                        "yg_academic2012_betas_20130520.txt", sep="\t", quote=F, row.names=F))

# ###############################################################################################
# ###############################################################################################

/opt/vertica/bin/vsql -U yair -w yairg -h qt4 -c "create table analytics.yg_academic2012_alphas_20130520 
(grp varchar(11),
a1 float,
a2 float,
a3 float,
a4 float,
a5 float,
a6 float,
a7 float,
a8 float,
a9 float,
a10 float,
a11 float,
a12 float,
a13 float,
a14 float,
a15 float,
a16 float,
a17 float,
a18 float,
a19 float,
a20 float,
a21 float,
a22 float,
a23 float,
a24 float,
a25 float,
a26 float,
a27 float,
a28 float,
a29 float,
a30 float,
a31 float,
a32 float,
a33 float,
a34 float,
a35 float,
a36 float,
a37 float,
a38 float,
a39 float,
a40 float,
a41 float,
a42 float,
a43 float,
a44 float,
a45 float,
a46 float,
a47 float,
a48 float,
a49 float,
a50 float,
a51 float,
a52 float,
a53 float,
a54 float,
a55 float,
a56 float,
a57 float,
a58 float,
a59 float,
a60 float,
a61 float,
a62 float,
a63 float,
a64 float,
a65 float,
a66 float,
a67 float,
a68 float,
a69 float,
a70 float,
a71 float,
a72 float,
a73 float,
a74 float,
a75 float,
a76 float,
a77 float,
a78 float,
a79 float,
a80 float,
a81 float,
a82 float,
a83 float,
a84 float,
a85 float,
a86 float,
a87 float,
a88 float,
a89 float,
a90 float,
a91 float,
a92 float,
a93 float,
a94 float,
a95 float,
a96 float,
a97 float,
a98 float,
a99 float,
a100 float)
order by grp
segmented by hash(grp) all nodes;
"

time cat yg_academic2012_alphas_20130520.txt | sed '1d' | /opt/vertica/bin/vsql -U yair -w yairg -h qt4 -c "copy analytics.yg_academic2012_alphas_20130520 from STDIN delimiter E'\t' REJECTED DATA './deltas.txt.bad' direct;"

/opt/vertica/bin/vsql -U yair -w yairg -h qt4 -c "create table analytics.yg_academic2012_betas_20130520 
(grp varchar(3),
b1 float,
b2 float,
b3 float,
b4 float,
b5 float,
b6 float,
b7 float,
b8 float,
b9 float,
b10 float,
b11 float,
b12 float,
b13 float,
b14 float,
b15 float,
b16 float,
b17 float,
b18 float,
b19 float,
b20 float,
b21 float,
b22 float,
b23 float,
b24 float,
b25 float,
b26 float,
b27 float,
b28 float,
b29 float,
b30 float,
b31 float,
b32 float,
b33 float,
b34 float,
b35 float,
b36 float,
b37 float,
b38 float,
b39 float,
b40 float,
b41 float,
b42 float,
b43 float,
b44 float,
b45 float,
b46 float,
b47 float,
b48 float,
b49 float,
b50 float,
b51 float,
b52 float,
b53 float,
b54 float,
b55 float,
b56 float,
b57 float,
b58 float,
b59 float,
b60 float,
b61 float,
b62 float,
b63 float,
b64 float,
b65 float,
b66 float,
b67 float,
b68 float,
b69 float,
b70 float,
b71 float,
b72 float,
b73 float,
b74 float,
b75 float,
b76 float,
b77 float,
b78 float,
b79 float,
b80 float,
b81 float,
b82 float,
b83 float,
b84 float,
b85 float,
b86 float,
b87 float,
b88 float,
b89 float,
b90 float,
b91 float,
b92 float,
b93 float,
b94 float,
b95 float,
b96 float,
b97 float,
b98 float,
b99 float,
b100 float)
order by grp
segmented by hash(grp) all nodes;
"

time cat yg_academic2012_betas_20130520.txt | sed '1d' | /opt/vertica/bin/vsql -U yair -w yairg -h qt4 -c "copy analytics.yg_academic2012_betas_20130520 from STDIN delimiter E'\t' REJECTED DATA './deltas.txt.bad' direct;"

###############################################################################################
###############################################################################################
###############################################################################################

sql.txt <- NULL
for (i.samp in 1:n.samp) {
  ix <- samp[i.samp]
  sql.txt <- c(sql.txt, 
               paste0("round(1/(1+exp(-1*(a", i.samp, " + ",
                      round(ext$beta_z[ix, 1], 10), "*z_age + ",
                      round(ext$beta_z[ix, 2], 10), "*z_inc + ",
                      round(ext$beta_z[ix, 3], 10), "*z_edu + ",
                      round(ext$beta_z[ix, 4], 10), "*z_num_dem_votes + ",
                      round(ext$beta_z[ix, 5], 10), "*z_num_rep_votes + ",
                      round(ext$beta_z[ix, 6], 10), "*z_ce_pctwhite + ",
                      round(ext$beta_z[ix, 7], 10), "*z_ce_pctblack + ",
                      round(ext$beta_z[ix, 8], 10), "*z_ce_pcthispaniclatino + ",
                      round(ext$beta_z[ix, 9], 10), "*z_ce_pctmarriedwithchildren + ",
                      round(ext$beta_z[ix, 10], 10), "*z_ce_pctnoncitizenforeignborn + ",
                      round(ext$beta_z[ix, 11], 10), "*z_ce_pctpublictranstowork + ",
                      round(ext$beta_z[ix, 12], 10), "*z_ce_marriedcouplehomeowners + ",
                      round(ext$beta_z[ix, 13], 10), "*z_ce_medianhhincome + ",
                      round(ext$beta_z[ix, 14], 10), "*z_ce_pctpublicassistance + ",
                      "b", i.samp, "*z_dem2way2008))), 3) as yhat", i.samp, ","))
}
write.table(cbind(sql.txt), "sql.txt", row.names=F)

dat_sql <- data.frame(apply(cbind(stan.data$sex,
                                  stan.data$eth,
                                  stan.data$inc,
                                  stan.data$mar,
                                  stan.data$edu,
                                  stan.data$age,
                                  stan.data$pid,
                                  stan.data$prmd,
                                  stan.data$votes,
                                  stan.data$stt), 1, paste0, collapse=""), 
                      apply(cbind(stan.data$eth,
                                  stan.data$pid,
                                  stan.data$prmd), 1, paste0, collapse=""), 
                      stan.data$Z, 
                      z_dem2way2008=stan.data$z_dem2wayprev, 
                      stringsAsFactors=F)
colnames(dat_sql)[1] <- "grp_a"
colnames(dat_sql)[2] <- "grp_b"

require(sqldf)
yhat.sql <- sqldf("select 
round(1/(1+exp(-1*(a1 + 0.2415770774*z_age + -0.1487611933*z_inc + 0.4234464746*z_edu + 0.0014480253*z_num_dem_votes + -0.2904682575*z_num_rep_votes + 0.0448116813*z_ce_pctwhite + 0.1218735352*z_ce_pctblack + -0.0584035533*z_ce_pcthispaniclatino + -0.0535450447*z_ce_pctmarriedwithchildren + 0.0495156975*z_ce_pctnoncitizenforeignborn + -0.0247845137*z_ce_pctpublictranstowork + -0.3001081302*z_ce_marriedcouplehomeowners + 0.1706569259*z_ce_medianhhincome + 0.0179421475*z_ce_pctpublicassistance + b1*z_dem2way2008))), 3) as yhat1,
round(1/(1+exp(-1*(a2 + -0.0597988618*z_age + -0.0948403452*z_inc + 0.4869132632*z_edu + -0.1834287614*z_num_dem_votes + -0.3675975021*z_num_rep_votes + 0.0838154516*z_ce_pctwhite + 0.2003070769*z_ce_pctblack + 0.1642007135*z_ce_pcthispaniclatino + -0.0923610051*z_ce_pctmarriedwithchildren + -0.0554874072*z_ce_pctnoncitizenforeignborn + 0.0164753999*z_ce_pctpublictranstowork + -0.2136950528*z_ce_marriedcouplehomeowners + 0.1840429482*z_ce_medianhhincome + -0.0700921646*z_ce_pctpublicassistance + b2*z_dem2way2008))), 3) as yhat2,
round(1/(1+exp(-1*(a3 + -0.086435666*z_age + -0.2896225104*z_inc + 0.4874564033*z_edu + 0.0377214402*z_num_dem_votes + -0.4404920388*z_num_rep_votes + -0.0804521519*z_ce_pctwhite + -0.003518548*z_ce_pctblack + -0.0806690521*z_ce_pcthispaniclatino + -0.0480128658*z_ce_pctmarriedwithchildren + 0.0642062025*z_ce_pctnoncitizenforeignborn + -0.0958265528*z_ce_pctpublictranstowork + -0.4329121761*z_ce_marriedcouplehomeowners + 0.2159034119*z_ce_medianhhincome + -0.0420882553*z_ce_pctpublicassistance + b3*z_dem2way2008))), 3) as yhat3
from dat_sql d
inner join alphas a on (d.grp_a=a.grp)
inner join betas b on (d.grp_b=b.grp)
")
cor(yhat.sql[,1:3])
table(round(yhat.sql[,1] - yhat[,1], 3))