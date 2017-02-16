require(arm)
require(car)
require(foreign)
require(rstan)
require(foreach)
require(doMC)

remove(list=objects())
setwd("/home/yair/postelection_analysis_2012/academic/obama_mrp/2012/1_modeling")

##########################################################################################
# load data

system.time(dat <- read.table("yg_obama2012mrp_dv_acad_20130514.txt",
                              sep="\t", header=T, comment.char="", quote=""))
colnames(dat) <- tolower(colnames(dat))
dat$date <- as.Date(gsub(".+ ", "", dat$survey_reference_name), "%Y%m%d")
dat <- dat[dat$date >= as.Date("20120301", "%Y%m%d"),]

### replace race with polling
race <- read.table("../../../../mrp_county/yg_mrp_race_responses_20130226.txt", header=T, sep="\t", stringsAsFactors=F)
race$race <- tolower(race$race)
race$race2 <- car::recode(race$race, "'1'='white'; 
                                      '2'='black'; 
                                      '3'='hispanic'; 
                                      '4'='other'; 
                                      '5'='other'; 
                                      '6'='other'; 
                                      '7'='white'; 
                                      'a'='other'; 
                                      'asian'='other'; 
                                      'b'='black'; 
                                      'black'='black'; 
                                      'c'='white'; 
                                      'caucasian'='white'; 
                                      'h'='hispanic'; 
                                      'hispanic'='hispanic'; 
                                      'j'='white'; 
                                      'jewish'='white'; 
                                      'm'='other'; 
                                      'middleeast'='other'; 
                                      'middleeastern'='other'; 
                                      'n'='other'; 
                                      'nativeame'='other'; 
                                      'nativeamer'='other'; 
                                      'nativeamerican'='other'; 
                                      'u'='white'; 
                                      'unknown'='white'; 
                                      else='white'")
dup <- duplicated(race[, c("dwid", "state", "survey_reference_name")])
race <- race[!dup,]
m <- dat[, c("dwid", "state", "survey_reference_name")]
m$ix <- 1:nrow(m)
m <- merge(x=m, y=race, all.x=T, by=c("dwid", "state", "survey_reference_name"))
m <- m[order(m$ix),]
m$race2[is.na(m$race2)] <- "white"
dat$race2 <- m$race2

##########################################################################################

X <- data.frame(sex=car::recode(dat$ca_gender, "'M'=1; 'F'=2; else=3", as.factor.result=F),
                #eth=car::recode(dat$ca_race, "'C'=1; 'J'=1; 'B'=2; 'H'=3; 'A'=4; 'M'=4; 'N'=4; 'U'=1; else=NA", as.factor.result=F),
                eth=car::recode(dat$race2, "'white'=1; 'black'=2; 'hispanic'=3; 'other'=4; else=1", as.factor.result=F),
                inc=car::recode(dat$co_find/1000, "0:50=1; 50:75=2; 75:100=3; 100:150=4; 150:Inf=5; else=6", as.factor.result=F),
                mar=car::recode(dat$co_smarstat, "'M'=1; 'S'=2; else=3", as.factor.result=F),
                edu=car::recode(dat$sy_educscore, "0:0.5=1; 0.5:1=2; else=3", as.factor.result=F),
                age=car::recode(dat$ca_age, "18:29=1; 30:44=2; 45:64=3; 65:Inf=4; else=5", as.factor.result=F),
                pid=car::recode(dat$ca_partyaffiliation, "'DEM'=1; 'REP'=3; else=2", as.factor.result=F),
                prmd=pmin(3, pmax(-3, dat$num_dem_votes - dat$num_rep_votes)) + 4,
                votes=dat$voted2008g + dat$voted2010g + 1,
                sttgrp=car::recode(as.character(dat$state), "'AK'=3;'AL'=1;'AR'=2;'AZ'=4;'CA'=4;'CO'=4;'CT'=3;'DC'=3;'DE'=3;'FL'=3;'GA'=2;'HI'=1;'IA'=4;'ID'=3;'IL'=2;'IN'=2;'KS'=3;'KY'=3;'LA'=3;'MA'=4;'MD'=4;'ME'=4;'MI'=1;'MN'=1;'MO'=1;'MS'=2;'MT'=1;'NC'=4;'ND'=1;'NE'=4;'NH'=4;'NJ'=4;'NM'=4;'NV'=3;'NY'=4;'OH'=4;'OK'=3;'OR'=3;'PA'=4;'RI'=4;'SC'=2;'SD'=3;'TN'=2;'TX'=2;'UT'=3;'VA'=2;'VT'=2;'WA'=2;'WI'=1;'WV'=4;'WY'=4;else=NA", as.factor.result=F),
                reg=car::recode(as.character(dat$state), "'AK'=4;'AL'=3;'AR'=3;'AZ'=4;'CA'=4;'CO'=4;'CT'=1;'DC'=5;'DE'=1;'FL'=3;'GA'=3;'HI'=4;'IA'=2;'ID'=4;'IL'=2;'IN'=2;'KS'=2;'KY'=3;'LA'=3;'MA'=1;'MD'=1;'ME'=1;'MI'=2;'MN'=2;'MO'=2;'MS'=3;'MT'=4;'NC'=3;'ND'=2;'NE'=2;'NH'=1;'NJ'=1;'NM'=4;'NV'=4;'NY'=1;'OH'=2;'OK'=3;'OR'=4;'PA'=1;'RI'=1;'SC'=3;'SD'=2;'TN'=3;'TX'=3;'UT'=4;'VA'=3;'VT'=1;'WA'=4;'WI'=2;'WV'=1;'WY'=4; else=NA", as.factor.result=F),
                stt=car::recode(as.character(dat$state), "'AK'=1;'AL'=2;'AR'=3;'AZ'=4;'CA'=5;'CO'=6;'CT'=7;'DC'=8;'DE'=9;'FL'=10;'GA'=11;'HI'=12;'IA'=13;'ID'=14;'IL'=15;'IN'=16;'KS'=17;'KY'=18;'LA'=19;'MA'=20;'MD'=21;'ME'=22;'MI'=23;'MN'=24;'MO'=25;'MS'=26;'MT'=27;'NC'=28;'ND'=29;'NE'=30;'NH'=31;'NJ'=32;'NM'=33;'NV'=34;'NY'=35;'OH'=36;'OK'=37;'OR'=38;'PA'=39;'RI'=40;'SC'=41;'SD'=42;'TN'=43;'TX'=44;'UT'=45;'VA'=46;'VT'=47;'WA'=48;'WI'=49;'WV'=50;'WY'=51; else=NA", as.factor.result=F),
                z_age=arm::rescale(dat$ca_age),
                z_inc=arm::rescale(dat$co_find),
                z_edu=arm::rescale(dat$sy_educscore),
                z_num_dem_votes=arm::rescale(dat$num_dem_votes),
                z_num_rep_votes=arm::rescale(dat$num_rep_votes),
                z_ce_pctwhite=arm::rescale(dat$ce_pctwhite),
                z_ce_pctblack=arm::rescale(dat$ce_pctblack),
                z_ce_pcthispaniclatino=arm::rescale(dat$ce_pcthispaniclatino),
                z_ce_pctmarriedwithchildren=arm::rescale(dat$ce_pctmarriedwithchildren),
                z_ce_pctnoncitizenforeignborn=arm::rescale(dat$ce_pctnoncitizenforeignborn),
                z_ce_pctpublictranstowork=arm::rescale(dat$ce_pctpublictranstowork),
                z_ce_marriedcouplehomeowners=arm::rescale(dat$ce_marriedcouplehomeowners),
                z_ce_medianhhincome=arm::rescale(dat$ce_medianhhincome),
                z_ce_pctpublicassistance=arm::rescale(dat$ce_pctpublicassistance),
                n_grp=arm::rescale(dat$dem2way2008))
z_mu <- cbind(sapply(grep("z_", colnames(X), value=T), function(i) mean(X[,i], na.rm=T)))
for (i in rownames(z_mu))
  X[is.na(X[,i]),i] <- as.numeric(z_mu[i,1])
y <- dat$y

dat <- data.frame(y=dat$y, X)
remove(list=objects()[!(objects() %in% c("dat", "z_mu"))])
gc()

##########################################################################################
# interactions

n_sex <- length(unique(dat$sex))
n_eth <- length(unique(dat$eth))
n_inc <- length(unique(dat$inc))
n_mar <- length(unique(dat$mar))
n_edu <- length(unique(dat$edu))
n_age <- length(unique(dat$age))
n_pid <- length(unique(dat$pid))
n_prmd <- length(unique(dat$prmd))
n_votes <- length(unique(dat$votes))
n_sttgrp <- length(unique(dat$sttgrp))
n_reg <- length(unique(dat$reg))
n_stt <- length(unique(dat$stt))

dat$sex_eth <- (dat$sex - 1) * n_eth + dat$eth
dat$sex_inc <- (dat$sex - 1) * n_inc + dat$inc
dat$sex_mar <- (dat$sex - 1) * n_mar + dat$mar
dat$sex_edu <- (dat$sex - 1) * n_edu + dat$edu
dat$sex_age <- (dat$sex - 1) * n_age + dat$age
dat$sex_pid <- (dat$sex - 1) * n_pid + dat$pid
dat$sex_prmd <- (dat$sex - 1) * n_prmd + dat$prmd
dat$sex_votes <- (dat$sex - 1) * n_votes + dat$votes
dat$sex_sttgrp <- (dat$sex - 1) * n_sttgrp + dat$sttgrp
dat$sex_reg <- (dat$sex - 1) * n_reg + dat$reg
dat$sex_stt <- (dat$sex - 1) * n_stt + dat$stt
dat$eth_inc <- (dat$eth - 1) * n_inc + dat$inc
dat$eth_mar <- (dat$eth - 1) * n_mar + dat$mar
dat$eth_edu <- (dat$eth - 1) * n_edu + dat$edu
dat$eth_age <- (dat$eth - 1) * n_age + dat$age
dat$eth_pid <- (dat$eth - 1) * n_pid + dat$pid
dat$eth_prmd <- (dat$eth - 1) * n_prmd + dat$prmd
dat$eth_votes <- (dat$eth - 1) * n_votes + dat$votes
dat$eth_sttgrp <- (dat$eth - 1) * n_sttgrp + dat$sttgrp
dat$eth_reg <- (dat$eth - 1) * n_reg + dat$reg
dat$eth_stt <- (dat$eth - 1) * n_stt + dat$stt
dat$inc_mar <- (dat$inc - 1) * n_mar + dat$mar
dat$inc_edu <- (dat$inc - 1) * n_edu + dat$edu
dat$inc_age <- (dat$inc - 1) * n_age + dat$age
dat$inc_pid <- (dat$inc - 1) * n_pid + dat$pid
dat$inc_prmd <- (dat$inc - 1) * n_prmd + dat$prmd
dat$inc_votes <- (dat$inc - 1) * n_votes + dat$votes
dat$inc_sttgrp <- (dat$inc - 1) * n_sttgrp + dat$sttgrp
dat$inc_reg <- (dat$inc - 1) * n_reg + dat$reg
dat$inc_stt <- (dat$inc - 1) * n_stt + dat$stt
dat$mar_edu <- (dat$mar - 1) * n_edu + dat$edu
dat$mar_age <- (dat$mar - 1) * n_age + dat$age
dat$mar_pid <- (dat$mar - 1) * n_pid + dat$pid
dat$mar_prmd <- (dat$mar - 1) * n_prmd + dat$prmd
dat$mar_votes <- (dat$mar - 1) * n_votes + dat$votes
dat$mar_sttgrp <- (dat$mar - 1) * n_sttgrp + dat$sttgrp
dat$mar_reg <- (dat$mar - 1) * n_reg + dat$reg
dat$mar_stt <- (dat$mar - 1) * n_stt + dat$stt
dat$edu_age <- (dat$edu - 1) * n_age + dat$age
dat$edu_pid <- (dat$edu - 1) * n_pid + dat$pid
dat$edu_prmd <- (dat$edu - 1) * n_prmd + dat$prmd
dat$edu_votes <- (dat$edu - 1) * n_votes + dat$votes
dat$edu_sttgrp <- (dat$edu - 1) * n_sttgrp + dat$sttgrp
dat$edu_reg <- (dat$edu - 1) * n_reg + dat$reg
dat$edu_stt <- (dat$edu - 1) * n_stt + dat$stt
dat$age_pid <- (dat$age - 1) * n_pid + dat$pid
dat$age_prmd <- (dat$age - 1) * n_prmd + dat$prmd
dat$age_votes <- (dat$age - 1) * n_votes + dat$votes
dat$age_sttgrp <- (dat$age - 1) * n_sttgrp + dat$sttgrp
dat$age_reg <- (dat$age - 1) * n_reg + dat$reg
dat$age_stt <- (dat$age - 1) * n_stt + dat$stt
dat$pid_prmd <- (dat$pid - 1) * n_prmd + dat$prmd
dat$pid_votes <- (dat$pid - 1) * n_votes + dat$votes
dat$pid_sttgrp <- (dat$pid - 1) * n_sttgrp + dat$sttgrp
dat$pid_reg <- (dat$pid - 1) * n_reg + dat$reg
dat$pid_stt <- (dat$pid - 1) * n_stt + dat$stt
dat$prmd_votes <- (dat$prmd - 1) * n_votes + dat$votes
dat$prmd_sttgrp <- (dat$prmd - 1) * n_sttgrp + dat$sttgrp
dat$prmd_reg <- (dat$prmd - 1) * n_reg + dat$reg
dat$prmd_stt <- (dat$prmd - 1) * n_stt + dat$stt
dat$votes_sttgrp <- (dat$votes - 1) * n_sttgrp + dat$sttgrp
dat$votes_reg <- (dat$votes - 1) * n_reg + dat$reg
dat$votes_stt <- (dat$votes - 1) * n_stt + dat$stt

##########################################################################################

stan.code <- "
  data {
    int<lower=0> n;
    int<lower=0> k;
    int<lower=0> y[n];
    int<lower=0> sex[n];
    int<lower=0> eth[n];
    int<lower=0> inc[n];
    int<lower=0> mar[n];
    int<lower=0> edu[n];
    int<lower=0> age[n];
    int<lower=0> pid[n];
    int<lower=0> prmd[n];
    int<lower=0> votes[n];
    int<lower=0> sttgrp[n];
    int<lower=0> reg[n];
    int<lower=0> stt[n];
    int<lower=0> sex_eth[n];
    int<lower=0> sex_inc[n];
    int<lower=0> sex_mar[n];
    int<lower=0> sex_edu[n];
    int<lower=0> sex_age[n];
    int<lower=0> sex_pid[n];
    int<lower=0> sex_prmd[n];
    int<lower=0> sex_votes[n];
    int<lower=0> sex_sttgrp[n];
    int<lower=0> sex_reg[n];
    int<lower=0> sex_stt[n];
    int<lower=0> eth_inc[n];
    int<lower=0> eth_mar[n];
    int<lower=0> eth_edu[n];
    int<lower=0> eth_age[n];
    int<lower=0> eth_pid[n];
    int<lower=0> eth_prmd[n];
    int<lower=0> eth_votes[n];
    int<lower=0> eth_sttgrp[n];
    int<lower=0> eth_reg[n];
    int<lower=0> eth_stt[n];
    int<lower=0> inc_mar[n];
    int<lower=0> inc_edu[n];
    int<lower=0> inc_age[n];
    int<lower=0> inc_pid[n];
    int<lower=0> inc_prmd[n];
    int<lower=0> inc_votes[n];
    int<lower=0> inc_sttgrp[n];
    int<lower=0> inc_reg[n];
    int<lower=0> inc_stt[n];
    int<lower=0> mar_edu[n];
    int<lower=0> mar_age[n];
    int<lower=0> mar_pid[n];
    int<lower=0> mar_prmd[n];
    int<lower=0> mar_votes[n];
    int<lower=0> mar_sttgrp[n];
    int<lower=0> mar_reg[n];
    int<lower=0> mar_stt[n];
    int<lower=0> edu_age[n];
    int<lower=0> edu_pid[n];
    int<lower=0> edu_prmd[n];
    int<lower=0> edu_votes[n];
    int<lower=0> edu_sttgrp[n];
    int<lower=0> edu_reg[n];
    int<lower=0> edu_stt[n];
    int<lower=0> age_pid[n];
    int<lower=0> age_prmd[n];
    int<lower=0> age_votes[n];
    int<lower=0> age_sttgrp[n];
    int<lower=0> age_reg[n];
    int<lower=0> age_stt[n];
    int<lower=0> pid_prmd[n];
    int<lower=0> pid_votes[n];
    int<lower=0> pid_sttgrp[n];
    int<lower=0> pid_reg[n];
    int<lower=0> pid_stt[n];
    int<lower=0> prmd_votes[n];
    int<lower=0> prmd_sttgrp[n];
    int<lower=0> prmd_reg[n];
    int<lower=0> prmd_stt[n];
    int<lower=0> votes_sttgrp[n];
    int<lower=0> votes_reg[n];
    int<lower=0> votes_stt[n];
    vector[n] z_dem2wayprev;
    matrix[n, k] Z;

    int<lower=0> n_sex;
    int<lower=0> n_eth;
    int<lower=0> n_inc;
    int<lower=0> n_mar;
    int<lower=0> n_edu;
    int<lower=0> n_age;
    int<lower=0> n_pid;
    int<lower=0> n_prmd;
    int<lower=0> n_votes;
    int<lower=0> n_sttgrp;
    int<lower=0> n_reg;
    int<lower=0> n_stt;
    int<lower=0> n_sex_eth;
    int<lower=0> n_sex_inc;
    int<lower=0> n_sex_mar;
    int<lower=0> n_sex_edu;
    int<lower=0> n_sex_age;
    int<lower=0> n_sex_pid;
    int<lower=0> n_sex_prmd;
    int<lower=0> n_sex_votes;
    int<lower=0> n_sex_sttgrp;
    int<lower=0> n_sex_reg;
    int<lower=0> n_sex_stt;
    int<lower=0> n_eth_inc;
    int<lower=0> n_eth_mar;
    int<lower=0> n_eth_edu;
    int<lower=0> n_eth_age;
    int<lower=0> n_eth_pid;
    int<lower=0> n_eth_prmd;
    int<lower=0> n_eth_votes;
    int<lower=0> n_eth_sttgrp;
    int<lower=0> n_eth_reg;
    int<lower=0> n_eth_stt;
    int<lower=0> n_inc_mar;
    int<lower=0> n_inc_edu;
    int<lower=0> n_inc_age;
    int<lower=0> n_inc_pid;
    int<lower=0> n_inc_prmd;
    int<lower=0> n_inc_votes;
    int<lower=0> n_inc_sttgrp;
    int<lower=0> n_inc_reg;
    int<lower=0> n_inc_stt;
    int<lower=0> n_mar_edu;
    int<lower=0> n_mar_age;
    int<lower=0> n_mar_pid;
    int<lower=0> n_mar_prmd;
    int<lower=0> n_mar_votes;
    int<lower=0> n_mar_sttgrp;
    int<lower=0> n_mar_reg;
    int<lower=0> n_mar_stt;
    int<lower=0> n_edu_age;
    int<lower=0> n_edu_pid;
    int<lower=0> n_edu_prmd;
    int<lower=0> n_edu_votes;
    int<lower=0> n_edu_sttgrp;
    int<lower=0> n_edu_reg;
    int<lower=0> n_edu_stt;
    int<lower=0> n_age_pid;
    int<lower=0> n_age_prmd;
    int<lower=0> n_age_votes;
    int<lower=0> n_age_sttgrp;
    int<lower=0> n_age_reg;
    int<lower=0> n_age_stt;
    int<lower=0> n_pid_prmd;
    int<lower=0> n_pid_votes;
    int<lower=0> n_pid_sttgrp;
    int<lower=0> n_pid_reg;
    int<lower=0> n_pid_stt;
    int<lower=0> n_prmd_votes;
    int<lower=0> n_prmd_sttgrp;
    int<lower=0> n_prmd_reg;
    int<lower=0> n_prmd_stt;
    int<lower=0> n_votes_sttgrp;
    int<lower=0> n_votes_reg;
    int<lower=0> n_votes_stt;
    int<lower=0> n_grp;
  }
  parameters {
    real alpha;
    vector[k] beta_z;

    vector[n_sex] alpha_sex;
    vector[n_eth] alpha_eth;
    vector[n_inc] alpha_inc;
    vector[n_mar] alpha_mar;
    vector[n_edu] alpha_edu;
    vector[n_age] alpha_age;
    vector[n_pid] alpha_pid;
    vector[n_prmd] alpha_prmd;
    vector[n_votes] alpha_votes;
    vector[n_sttgrp] alpha_sttgrp;
    vector[n_reg] alpha_reg;
    vector[n_stt] alpha_stt;
    vector[n_sex_eth] alpha_sex_eth;
    vector[n_sex_inc] alpha_sex_inc;
    vector[n_sex_mar] alpha_sex_mar;
    vector[n_sex_edu] alpha_sex_edu;
    vector[n_sex_age] alpha_sex_age;
    vector[n_sex_pid] alpha_sex_pid;
    vector[n_sex_prmd] alpha_sex_prmd;
    vector[n_sex_votes] alpha_sex_votes;
    vector[n_sex_sttgrp] alpha_sex_sttgrp;
    vector[n_sex_reg] alpha_sex_reg;
    vector[n_sex_stt] alpha_sex_stt;
    vector[n_eth_inc] alpha_eth_inc;
    vector[n_eth_mar] alpha_eth_mar;
    vector[n_eth_edu] alpha_eth_edu;
    vector[n_eth_age] alpha_eth_age;
    vector[n_eth_pid] alpha_eth_pid;
    vector[n_eth_prmd] alpha_eth_prmd;
    vector[n_eth_votes] alpha_eth_votes;
    vector[n_eth_sttgrp] alpha_eth_sttgrp;
    vector[n_eth_reg] alpha_eth_reg;
    vector[n_eth_stt] alpha_eth_stt;
    vector[n_inc_mar] alpha_inc_mar;
    vector[n_inc_edu] alpha_inc_edu;
    vector[n_inc_age] alpha_inc_age;
    vector[n_inc_pid] alpha_inc_pid;
    vector[n_inc_prmd] alpha_inc_prmd;
    vector[n_inc_votes] alpha_inc_votes;
    vector[n_inc_sttgrp] alpha_inc_sttgrp;
    vector[n_inc_reg] alpha_inc_reg;
    vector[n_inc_stt] alpha_inc_stt;
    vector[n_mar_edu] alpha_mar_edu;
    vector[n_mar_age] alpha_mar_age;
    vector[n_mar_pid] alpha_mar_pid;
    vector[n_mar_prmd] alpha_mar_prmd;
    vector[n_mar_votes] alpha_mar_votes;
    vector[n_mar_sttgrp] alpha_mar_sttgrp;
    vector[n_mar_reg] alpha_mar_reg;
    vector[n_mar_stt] alpha_mar_stt;
    vector[n_edu_age] alpha_edu_age;
    vector[n_edu_pid] alpha_edu_pid;
    vector[n_edu_prmd] alpha_edu_prmd;
    vector[n_edu_votes] alpha_edu_votes;
    vector[n_edu_sttgrp] alpha_edu_sttgrp;
    vector[n_edu_reg] alpha_edu_reg;
    vector[n_edu_stt] alpha_edu_stt;
    vector[n_age_pid] alpha_age_pid;
    vector[n_age_prmd] alpha_age_prmd;
    vector[n_age_votes] alpha_age_votes;
    vector[n_age_sttgrp] alpha_age_sttgrp;
    vector[n_age_reg] alpha_age_reg;
    vector[n_age_stt] alpha_age_stt;
    vector[n_pid_prmd] alpha_pid_prmd;
    vector[n_pid_votes] alpha_pid_votes;
    vector[n_pid_sttgrp] alpha_pid_sttgrp;
    vector[n_pid_reg] alpha_pid_reg;
    vector[n_pid_stt] alpha_pid_stt;
    vector[n_prmd_votes] alpha_prmd_votes;
    vector[n_prmd_sttgrp] alpha_prmd_sttgrp;
    vector[n_prmd_reg] alpha_prmd_reg;
    vector[n_prmd_stt] alpha_prmd_stt;
    vector[n_votes_sttgrp] alpha_votes_sttgrp;
    vector[n_votes_reg] alpha_votes_reg;
    vector[n_votes_stt] alpha_votes_stt;

    real beta;
    vector[n_eth] beta_eth;
    vector[n_pid] beta_pid;
    vector[n_prmd] beta_prmd;

    real<lower=0> sigma_alpha[n_grp];
    real<lower=0> sigma_sigma_alpha;
    real<lower=0> sigma_beta[3];
    real<lower=0> sigma_sigma_beta;
  }
  model {
    vector[n] alpha_v_sex;
    vector[n] alpha_v_eth;
    vector[n] alpha_v_inc;
    vector[n] alpha_v_mar;
    vector[n] alpha_v_edu;
    vector[n] alpha_v_age;
    vector[n] alpha_v_pid;
    vector[n] alpha_v_prmd;
    vector[n] alpha_v_votes;
    vector[n] alpha_v_sttgrp;
    vector[n] alpha_v_reg;
    vector[n] alpha_v_stt;
    vector[n] alpha_v_sex_eth;
    vector[n] alpha_v_sex_inc;
    vector[n] alpha_v_sex_mar;
    vector[n] alpha_v_sex_edu;
    vector[n] alpha_v_sex_age;
    vector[n] alpha_v_sex_pid;
    vector[n] alpha_v_sex_prmd;
    vector[n] alpha_v_sex_votes;
    vector[n] alpha_v_sex_sttgrp;
    vector[n] alpha_v_sex_reg;
    vector[n] alpha_v_sex_stt;
    vector[n] alpha_v_eth_inc;
    vector[n] alpha_v_eth_mar;
    vector[n] alpha_v_eth_edu;
    vector[n] alpha_v_eth_age;
    vector[n] alpha_v_eth_pid;
    vector[n] alpha_v_eth_prmd;
    vector[n] alpha_v_eth_votes;
    vector[n] alpha_v_eth_sttgrp;
    vector[n] alpha_v_eth_reg;
    vector[n] alpha_v_eth_stt;
    vector[n] alpha_v_inc_mar;
    vector[n] alpha_v_inc_edu;
    vector[n] alpha_v_inc_age;
    vector[n] alpha_v_inc_pid;
    vector[n] alpha_v_inc_prmd;
    vector[n] alpha_v_inc_votes;
    vector[n] alpha_v_inc_sttgrp;
    vector[n] alpha_v_inc_reg;
    vector[n] alpha_v_inc_stt;
    vector[n] alpha_v_mar_edu;
    vector[n] alpha_v_mar_age;
    vector[n] alpha_v_mar_pid;
    vector[n] alpha_v_mar_prmd;
    vector[n] alpha_v_mar_votes;
    vector[n] alpha_v_mar_sttgrp;
    vector[n] alpha_v_mar_reg;
    vector[n] alpha_v_mar_stt;
    vector[n] alpha_v_edu_age;
    vector[n] alpha_v_edu_pid;
    vector[n] alpha_v_edu_prmd;
    vector[n] alpha_v_edu_votes;
    vector[n] alpha_v_edu_sttgrp;
    vector[n] alpha_v_edu_reg;
    vector[n] alpha_v_edu_stt;
    vector[n] alpha_v_age_pid;
    vector[n] alpha_v_age_prmd;
    vector[n] alpha_v_age_votes;
    vector[n] alpha_v_age_sttgrp;
    vector[n] alpha_v_age_reg;
    vector[n] alpha_v_age_stt;
    vector[n] alpha_v_pid_prmd;
    vector[n] alpha_v_pid_votes;
    vector[n] alpha_v_pid_sttgrp;
    vector[n] alpha_v_pid_reg;
    vector[n] alpha_v_pid_stt;
    vector[n] alpha_v_prmd_votes;
    vector[n] alpha_v_prmd_sttgrp;
    vector[n] alpha_v_prmd_reg;
    vector[n] alpha_v_prmd_stt;
    vector[n] alpha_v_votes_sttgrp;
    vector[n] alpha_v_votes_reg;
    vector[n] alpha_v_votes_stt;
    vector[n] beta_v;
 
    alpha_sex ~ normal(0,1);
    alpha_eth ~ normal(0,1);
    alpha_inc ~ normal(0,1);
    alpha_mar ~ normal(0,1);
    alpha_edu ~ normal(0,1);
    alpha_age ~ normal(0,1);
    alpha_pid ~ normal(0,1);
    alpha_prmd ~ normal(0,1);
    alpha_votes ~ normal(0,1);
    alpha_sttgrp ~ normal(0,1);
    alpha_reg ~ normal(0,1);
    alpha_stt ~ normal(0,1);
    alpha_sex_eth ~ normal(0,1);
    alpha_sex_inc ~ normal(0,1);
    alpha_sex_mar ~ normal(0,1);
    alpha_sex_edu ~ normal(0,1);
    alpha_sex_age ~ normal(0,1);
    alpha_sex_pid ~ normal(0,1);
    alpha_sex_prmd ~ normal(0,1);
    alpha_sex_votes ~ normal(0,1);
    alpha_sex_sttgrp ~ normal(0,1);
    alpha_sex_reg ~ normal(0,1);
    alpha_sex_stt ~ normal(0,1);
    alpha_eth_inc ~ normal(0,1);
    alpha_eth_mar ~ normal(0,1);
    alpha_eth_edu ~ normal(0,1);
    alpha_eth_age ~ normal(0,1);
    alpha_eth_pid ~ normal(0,1);
    alpha_eth_prmd ~ normal(0,1);
    alpha_eth_votes ~ normal(0,1);
    alpha_eth_sttgrp ~ normal(0,1);
    alpha_eth_reg ~ normal(0,1);
    alpha_eth_stt ~ normal(0,1);
    alpha_inc_mar ~ normal(0,1);
    alpha_inc_edu ~ normal(0,1);
    alpha_inc_age ~ normal(0,1);
    alpha_inc_pid ~ normal(0,1);
    alpha_inc_prmd ~ normal(0,1);
    alpha_inc_votes ~ normal(0,1);
    alpha_inc_sttgrp ~ normal(0,1);
    alpha_inc_reg ~ normal(0,1);
    alpha_inc_stt ~ normal(0,1);
    alpha_mar_edu ~ normal(0,1);
    alpha_mar_age ~ normal(0,1);
    alpha_mar_pid ~ normal(0,1);
    alpha_mar_prmd ~ normal(0,1);
    alpha_mar_votes ~ normal(0,1);
    alpha_mar_sttgrp ~ normal(0,1);
    alpha_mar_reg ~ normal(0,1);
    alpha_mar_stt ~ normal(0,1);
    alpha_edu_age ~ normal(0,1);
    alpha_edu_pid ~ normal(0,1);
    alpha_edu_prmd ~ normal(0,1);
    alpha_edu_votes ~ normal(0,1);
    alpha_edu_sttgrp ~ normal(0,1);
    alpha_edu_reg ~ normal(0,1);
    alpha_edu_stt ~ normal(0,1);
    alpha_age_pid ~ normal(0,1);
    alpha_age_prmd ~ normal(0,1);
    alpha_age_votes ~ normal(0,1);
    alpha_age_sttgrp ~ normal(0,1);
    alpha_age_reg ~ normal(0,1);
    alpha_age_stt ~ normal(0,1);
    alpha_pid_prmd ~ normal(0,1);
    alpha_pid_votes ~ normal(0,1);
    alpha_pid_sttgrp ~ normal(0,1);
    alpha_pid_reg ~ normal(0,1);
    alpha_pid_stt ~ normal(0,1);
    alpha_prmd_votes ~ normal(0,1);
    alpha_prmd_sttgrp ~ normal(0,1);
    alpha_prmd_reg ~ normal(0,1);
    alpha_prmd_stt ~ normal(0,1);
    alpha_votes_sttgrp ~ normal(0,1);
    alpha_votes_reg ~ normal(0,1);
    alpha_votes_stt ~ normal(0,1);

    beta_eth ~ normal(0,1);
    beta_pid ~ normal(0,1);
    beta_prmd ~ normal(0,1);
    sigma_alpha ~ student_t(8,0,1);
    sigma_beta ~ student_t(8,0,1);

    for (i_n in 1:n) {
        alpha_v_sex[i_n] <- alpha_sex[sex[i_n]];
        alpha_v_eth[i_n] <- alpha_eth[eth[i_n]];
        alpha_v_inc[i_n] <- alpha_inc[inc[i_n]];
        alpha_v_mar[i_n] <- alpha_mar[mar[i_n]];
        alpha_v_edu[i_n] <- alpha_edu[edu[i_n]];
        alpha_v_age[i_n] <- alpha_age[age[i_n]];
        alpha_v_pid[i_n] <- alpha_pid[pid[i_n]];
        alpha_v_prmd[i_n] <- alpha_prmd[prmd[i_n]];
        alpha_v_votes[i_n] <- alpha_votes[votes[i_n]];
        alpha_v_sttgrp[i_n] <- alpha_sttgrp[sttgrp[i_n]];
        alpha_v_reg[i_n] <- alpha_reg[reg[i_n]];
        alpha_v_stt[i_n] <- alpha_stt[stt[i_n]];
        alpha_v_sex_eth[i_n] <- alpha_sex_eth[sex_eth[i_n]];
        alpha_v_sex_inc[i_n] <- alpha_sex_inc[sex_inc[i_n]];
        alpha_v_sex_mar[i_n] <- alpha_sex_mar[sex_mar[i_n]];
        alpha_v_sex_edu[i_n] <- alpha_sex_edu[sex_edu[i_n]];
        alpha_v_sex_age[i_n] <- alpha_sex_age[sex_age[i_n]];
        alpha_v_sex_pid[i_n] <- alpha_sex_pid[sex_pid[i_n]];
        alpha_v_sex_prmd[i_n] <- alpha_sex_prmd[sex_prmd[i_n]];
        alpha_v_sex_votes[i_n] <- alpha_sex_votes[sex_votes[i_n]];
        alpha_v_sex_sttgrp[i_n] <- alpha_sex_sttgrp[sex_sttgrp[i_n]];
        alpha_v_sex_reg[i_n] <- alpha_sex_reg[sex_reg[i_n]];
        alpha_v_sex_stt[i_n] <- alpha_sex_stt[sex_stt[i_n]];
        alpha_v_eth_inc[i_n] <- alpha_eth_inc[eth_inc[i_n]];
        alpha_v_eth_mar[i_n] <- alpha_eth_mar[eth_mar[i_n]];
        alpha_v_eth_edu[i_n] <- alpha_eth_edu[eth_edu[i_n]];
        alpha_v_eth_age[i_n] <- alpha_eth_age[eth_age[i_n]];
        alpha_v_eth_pid[i_n] <- alpha_eth_pid[eth_pid[i_n]];
        alpha_v_eth_prmd[i_n] <- alpha_eth_prmd[eth_prmd[i_n]];
        alpha_v_eth_votes[i_n] <- alpha_eth_votes[eth_votes[i_n]];
        alpha_v_eth_sttgrp[i_n] <- alpha_eth_sttgrp[eth_sttgrp[i_n]];
        alpha_v_eth_reg[i_n] <- alpha_eth_reg[eth_reg[i_n]];
        alpha_v_eth_stt[i_n] <- alpha_eth_stt[eth_stt[i_n]];
        alpha_v_inc_mar[i_n] <- alpha_inc_mar[inc_mar[i_n]];
        alpha_v_inc_edu[i_n] <- alpha_inc_edu[inc_edu[i_n]];
        alpha_v_inc_age[i_n] <- alpha_inc_age[inc_age[i_n]];
        alpha_v_inc_pid[i_n] <- alpha_inc_pid[inc_pid[i_n]];
        alpha_v_inc_prmd[i_n] <- alpha_inc_prmd[inc_prmd[i_n]];
        alpha_v_inc_votes[i_n] <- alpha_inc_votes[inc_votes[i_n]];
        alpha_v_inc_sttgrp[i_n] <- alpha_inc_sttgrp[inc_sttgrp[i_n]];
        alpha_v_inc_reg[i_n] <- alpha_inc_reg[inc_reg[i_n]];
        alpha_v_inc_stt[i_n] <- alpha_inc_stt[inc_stt[i_n]];
        alpha_v_mar_edu[i_n] <- alpha_mar_edu[mar_edu[i_n]];
        alpha_v_mar_age[i_n] <- alpha_mar_age[mar_age[i_n]];
        alpha_v_mar_pid[i_n] <- alpha_mar_pid[mar_pid[i_n]];
        alpha_v_mar_prmd[i_n] <- alpha_mar_prmd[mar_prmd[i_n]];
        alpha_v_mar_votes[i_n] <- alpha_mar_votes[mar_votes[i_n]];
        alpha_v_mar_sttgrp[i_n] <- alpha_mar_sttgrp[mar_sttgrp[i_n]];
        alpha_v_mar_reg[i_n] <- alpha_mar_reg[mar_reg[i_n]];
        alpha_v_mar_stt[i_n] <- alpha_mar_stt[mar_stt[i_n]];
        alpha_v_edu_age[i_n] <- alpha_edu_age[edu_age[i_n]];
        alpha_v_edu_pid[i_n] <- alpha_edu_pid[edu_pid[i_n]];
        alpha_v_edu_prmd[i_n] <- alpha_edu_prmd[edu_prmd[i_n]];
        alpha_v_edu_votes[i_n] <- alpha_edu_votes[edu_votes[i_n]];
        alpha_v_edu_sttgrp[i_n] <- alpha_edu_sttgrp[edu_sttgrp[i_n]];
        alpha_v_edu_reg[i_n] <- alpha_edu_reg[edu_reg[i_n]];
        alpha_v_edu_stt[i_n] <- alpha_edu_stt[edu_stt[i_n]];
        alpha_v_age_pid[i_n] <- alpha_age_pid[age_pid[i_n]];
        alpha_v_age_prmd[i_n] <- alpha_age_prmd[age_prmd[i_n]];
        alpha_v_age_votes[i_n] <- alpha_age_votes[age_votes[i_n]];
        alpha_v_age_sttgrp[i_n] <- alpha_age_sttgrp[age_sttgrp[i_n]];
        alpha_v_age_reg[i_n] <- alpha_age_reg[age_reg[i_n]];
        alpha_v_age_stt[i_n] <- alpha_age_stt[age_stt[i_n]];
        alpha_v_pid_prmd[i_n] <- alpha_pid_prmd[pid_prmd[i_n]];
        alpha_v_pid_votes[i_n] <- alpha_pid_votes[pid_votes[i_n]];
        alpha_v_pid_sttgrp[i_n] <- alpha_pid_sttgrp[pid_sttgrp[i_n]];
        alpha_v_pid_reg[i_n] <- alpha_pid_reg[pid_reg[i_n]];
        alpha_v_pid_stt[i_n] <- alpha_pid_stt[pid_stt[i_n]];
        alpha_v_prmd_votes[i_n] <- alpha_prmd_votes[prmd_votes[i_n]];
        alpha_v_prmd_sttgrp[i_n] <- alpha_prmd_sttgrp[prmd_sttgrp[i_n]];
        alpha_v_prmd_reg[i_n] <- alpha_prmd_reg[prmd_reg[i_n]];
        alpha_v_prmd_stt[i_n] <- alpha_prmd_stt[prmd_stt[i_n]];
        alpha_v_votes_sttgrp[i_n] <- alpha_votes_sttgrp[votes_sttgrp[i_n]];
        alpha_v_votes_reg[i_n] <- alpha_votes_reg[votes_reg[i_n]];
        alpha_v_votes_stt[i_n] <- alpha_votes_stt[votes_stt[i_n]];
        beta_v[i_n] <- beta + 
                       beta_eth[eth[i_n]] * sigma_sigma_beta * sigma_beta[1] + 
                       beta_pid[pid[i_n]] * sigma_sigma_beta * sigma_beta[2] +
                       beta_prmd[prmd[i_n]] * sigma_sigma_beta * sigma_beta[3];
    }

    y ~ bernoulli_logit(alpha +
                        alpha_v_sex * (sigma_alpha[1] * sigma_sigma_alpha) +
                        alpha_v_eth * (sigma_alpha[2] * sigma_sigma_alpha) +
                        alpha_v_inc * (sigma_alpha[3] * sigma_sigma_alpha) +
                        alpha_v_mar * (sigma_alpha[4] * sigma_sigma_alpha) +
                        alpha_v_edu * (sigma_alpha[5] * sigma_sigma_alpha) +
                        alpha_v_age * (sigma_alpha[6] * sigma_sigma_alpha) +
                        alpha_v_pid * (sigma_alpha[7] * sigma_sigma_alpha) +
                        alpha_v_prmd * (sigma_alpha[8] * sigma_sigma_alpha) +
                        alpha_v_votes * (sigma_alpha[9] * sigma_sigma_alpha) +
                        alpha_v_sttgrp * (sigma_alpha[10] * sigma_sigma_alpha) +
                        alpha_v_reg * (sigma_alpha[11] * sigma_sigma_alpha) +
                        alpha_v_stt * (sigma_alpha[12] * sigma_sigma_alpha) +
                        alpha_v_sex_eth * (sigma_alpha[13] * sigma_sigma_alpha) +
                        alpha_v_sex_inc * (sigma_alpha[14] * sigma_sigma_alpha) +
                        alpha_v_sex_mar * (sigma_alpha[15] * sigma_sigma_alpha) +
                        alpha_v_sex_edu * (sigma_alpha[16] * sigma_sigma_alpha) +
                        alpha_v_sex_age * (sigma_alpha[17] * sigma_sigma_alpha) +
                        alpha_v_sex_pid * (sigma_alpha[18] * sigma_sigma_alpha) +
                        alpha_v_sex_prmd * (sigma_alpha[19] * sigma_sigma_alpha) +
                        alpha_v_sex_votes * (sigma_alpha[20] * sigma_sigma_alpha) +
                        alpha_v_sex_sttgrp * (sigma_alpha[21] * sigma_sigma_alpha) +
                        alpha_v_sex_reg * (sigma_alpha[22] * sigma_sigma_alpha) +
                        alpha_v_sex_stt * (sigma_alpha[23] * sigma_sigma_alpha) +
                        alpha_v_eth_inc * (sigma_alpha[24] * sigma_sigma_alpha) +
                        alpha_v_eth_mar * (sigma_alpha[25] * sigma_sigma_alpha) +
                        alpha_v_eth_edu * (sigma_alpha[26] * sigma_sigma_alpha) +
                        alpha_v_eth_age * (sigma_alpha[27] * sigma_sigma_alpha) +
                        alpha_v_eth_pid * (sigma_alpha[28] * sigma_sigma_alpha) +
                        alpha_v_eth_prmd * (sigma_alpha[29] * sigma_sigma_alpha) +
                        alpha_v_eth_votes * (sigma_alpha[30] * sigma_sigma_alpha) +
                        alpha_v_eth_sttgrp * (sigma_alpha[31] * sigma_sigma_alpha) +
                        alpha_v_eth_reg * (sigma_alpha[32] * sigma_sigma_alpha) +
                        alpha_v_eth_stt * (sigma_alpha[33] * sigma_sigma_alpha) +
                        alpha_v_inc_mar * (sigma_alpha[34] * sigma_sigma_alpha) +
                        alpha_v_inc_edu * (sigma_alpha[35] * sigma_sigma_alpha) +
                        alpha_v_inc_age * (sigma_alpha[36] * sigma_sigma_alpha) +
                        alpha_v_inc_pid * (sigma_alpha[37] * sigma_sigma_alpha) +
                        alpha_v_inc_prmd * (sigma_alpha[38] * sigma_sigma_alpha) +
                        alpha_v_inc_votes * (sigma_alpha[39] * sigma_sigma_alpha) +
                        alpha_v_inc_sttgrp * (sigma_alpha[40] * sigma_sigma_alpha) +
                        alpha_v_inc_reg * (sigma_alpha[41] * sigma_sigma_alpha) +
                        alpha_v_inc_stt * (sigma_alpha[42] * sigma_sigma_alpha) +
                        alpha_v_mar_edu * (sigma_alpha[43] * sigma_sigma_alpha) +
                        alpha_v_mar_age * (sigma_alpha[44] * sigma_sigma_alpha) +
                        alpha_v_mar_pid * (sigma_alpha[45] * sigma_sigma_alpha) +
                        alpha_v_mar_prmd * (sigma_alpha[46] * sigma_sigma_alpha) +
                        alpha_v_mar_votes * (sigma_alpha[47] * sigma_sigma_alpha) +
                        alpha_v_mar_sttgrp * (sigma_alpha[48] * sigma_sigma_alpha) +
                        alpha_v_mar_reg * (sigma_alpha[49] * sigma_sigma_alpha) +
                        alpha_v_mar_stt * (sigma_alpha[50] * sigma_sigma_alpha) +
                        alpha_v_edu_age * (sigma_alpha[51] * sigma_sigma_alpha) +
                        alpha_v_edu_pid * (sigma_alpha[52] * sigma_sigma_alpha) +
                        alpha_v_edu_prmd * (sigma_alpha[53] * sigma_sigma_alpha) +
                        alpha_v_edu_votes * (sigma_alpha[54] * sigma_sigma_alpha) +
                        alpha_v_edu_sttgrp * (sigma_alpha[55] * sigma_sigma_alpha) +
                        alpha_v_edu_reg * (sigma_alpha[56] * sigma_sigma_alpha) +
                        alpha_v_edu_stt * (sigma_alpha[57] * sigma_sigma_alpha) +
                        alpha_v_age_pid * (sigma_alpha[58] * sigma_sigma_alpha) +
                        alpha_v_age_prmd * (sigma_alpha[59] * sigma_sigma_alpha) +
                        alpha_v_age_votes * (sigma_alpha[60] * sigma_sigma_alpha) +
                        alpha_v_age_sttgrp * (sigma_alpha[61] * sigma_sigma_alpha) +
                        alpha_v_age_reg * (sigma_alpha[62] * sigma_sigma_alpha) +
                        alpha_v_age_stt * (sigma_alpha[63] * sigma_sigma_alpha) +
                        alpha_v_pid_prmd * (sigma_alpha[64] * sigma_sigma_alpha) +
                        alpha_v_pid_votes * (sigma_alpha[65] * sigma_sigma_alpha) +
                        alpha_v_pid_sttgrp * (sigma_alpha[66] * sigma_sigma_alpha) +
                        alpha_v_pid_reg * (sigma_alpha[67] * sigma_sigma_alpha) +
                        alpha_v_pid_stt * (sigma_alpha[68] * sigma_sigma_alpha) +
                        alpha_v_prmd_votes * (sigma_alpha[69] * sigma_sigma_alpha) +
                        alpha_v_prmd_sttgrp * (sigma_alpha[70] * sigma_sigma_alpha) +
                        alpha_v_prmd_reg * (sigma_alpha[71] * sigma_sigma_alpha) +
                        alpha_v_prmd_stt * (sigma_alpha[72] * sigma_sigma_alpha) +
                        alpha_v_votes_sttgrp * (sigma_alpha[73] * sigma_sigma_alpha) +
                        alpha_v_votes_reg * (sigma_alpha[74] * sigma_sigma_alpha) +
                        alpha_v_votes_stt * (sigma_alpha[75] * sigma_sigma_alpha) +
                        beta_v .* z_dem2wayprev +
                        Z * beta_z);
  }
"

zvars <- grep("z_", colnames(dat), value=T)
zvars <- zvars[zvars != "z_dem2wayprev"]
stan.data <- list(n=nrow(dat), 
                  k=length(zvars),
                  y=dat$y, 
                  sex=dat$sex,
                  eth=dat$eth,
                  inc=dat$inc,
                  mar=dat$mar,
                  edu=dat$edu,
                  age=dat$age,
                  pid=dat$pid,
                  prmd=dat$prmd,
                  votes=dat$votes,
                  sttgrp=dat$sttgrp,
                  reg=dat$reg,
                  stt=dat$stt,
                  sex_eth=dat$sex_eth,
                  sex_inc=dat$sex_inc,
                  sex_mar=dat$sex_mar,
                  sex_edu=dat$sex_edu,
                  sex_age=dat$sex_age,
                  sex_pid=dat$sex_pid,
                  sex_prmd=dat$sex_prmd,
                  sex_votes=dat$sex_votes,
                  sex_sttgrp=dat$sex_sttgrp,
                  sex_reg=dat$sex_reg,
                  sex_stt=dat$sex_stt,
                  eth_inc=dat$eth_inc,
                  eth_mar=dat$eth_mar,
                  eth_edu=dat$eth_edu,
                  eth_age=dat$eth_age,
                  eth_pid=dat$eth_pid,
                  eth_prmd=dat$eth_prmd,
                  eth_votes=dat$eth_votes,
                  eth_sttgrp=dat$eth_sttgrp,
                  eth_reg=dat$eth_reg,
                  eth_stt=dat$eth_stt,
                  inc_mar=dat$inc_mar,
                  inc_edu=dat$inc_edu,
                  inc_age=dat$inc_age,
                  inc_pid=dat$inc_pid,
                  inc_prmd=dat$inc_prmd,
                  inc_votes=dat$inc_votes,
                  inc_sttgrp=dat$inc_sttgrp,
                  inc_reg=dat$inc_reg,
                  inc_stt=dat$inc_stt,
                  mar_edu=dat$mar_edu,
                  mar_age=dat$mar_age,
                  mar_pid=dat$mar_pid,
                  mar_prmd=dat$mar_prmd,
                  mar_votes=dat$mar_votes,
                  mar_sttgrp=dat$mar_sttgrp,
                  mar_reg=dat$mar_reg,
                  mar_stt=dat$mar_stt,
                  edu_age=dat$edu_age,
                  edu_pid=dat$edu_pid,
                  edu_prmd=dat$edu_prmd,
                  edu_votes=dat$edu_votes,
                  edu_sttgrp=dat$edu_sttgrp,
                  edu_reg=dat$edu_reg,
                  edu_stt=dat$edu_stt,
                  age_pid=dat$age_pid,
                  age_prmd=dat$age_prmd,
                  age_votes=dat$age_votes,
                  age_sttgrp=dat$age_sttgrp,
                  age_reg=dat$age_reg,
                  age_stt=dat$age_stt,
                  pid_prmd=dat$pid_prmd,
                  pid_votes=dat$pid_votes,
                  pid_sttgrp=dat$pid_sttgrp,
                  pid_reg=dat$pid_reg,
                  pid_stt=dat$pid_stt,
                  prmd_votes=dat$prmd_votes,
                  prmd_sttgrp=dat$prmd_sttgrp,
                  prmd_reg=dat$prmd_reg,
                  prmd_stt=dat$prmd_stt,
                  votes_sttgrp=dat$votes_sttgrp,
                  votes_reg=dat$votes_reg,
                  votes_stt=dat$votes_stt,
                  z_dem2wayprev=dat$z_dem2wayprev,
                  Z=dat[, zvars],
                  n_sex=max(dat$sex),
                  n_eth=max(dat$eth),
                  n_inc=max(dat$inc),
                  n_mar=max(dat$mar),
                  n_edu=max(dat$edu),
                  n_age=max(dat$age),
                  n_pid=max(dat$pid),
                  n_prmd=max(dat$prmd),
                  n_votes=max(dat$votes),
                  n_sttgrp=max(dat$sttgrp),
                  n_reg=max(dat$reg),
                  n_stt=max(dat$stt),
                  n_sex_eth=max(dat$sex_eth),
                  n_sex_inc=max(dat$sex_inc),
                  n_sex_mar=max(dat$sex_mar),
                  n_sex_edu=max(dat$sex_edu),
                  n_sex_age=max(dat$sex_age),
                  n_sex_pid=max(dat$sex_pid),
                  n_sex_prmd=max(dat$sex_prmd),
                  n_sex_votes=max(dat$sex_votes),
                  n_sex_sttgrp=max(dat$sex_sttgrp),
                  n_sex_reg=max(dat$sex_reg),
                  n_sex_stt=max(dat$sex_stt),
                  n_eth_inc=max(dat$eth_inc),
                  n_eth_mar=max(dat$eth_mar),
                  n_eth_edu=max(dat$eth_edu),
                  n_eth_age=max(dat$eth_age),
                  n_eth_pid=max(dat$eth_pid),
                  n_eth_prmd=max(dat$eth_prmd),
                  n_eth_votes=max(dat$eth_votes),
                  n_eth_sttgrp=max(dat$eth_sttgrp),
                  n_eth_reg=max(dat$eth_reg),
                  n_eth_stt=max(dat$eth_stt),
                  n_inc_mar=max(dat$inc_mar),
                  n_inc_edu=max(dat$inc_edu),
                  n_inc_age=max(dat$inc_age),
                  n_inc_pid=max(dat$inc_pid),
                  n_inc_prmd=max(dat$inc_prmd),
                  n_inc_votes=max(dat$inc_votes),
                  n_inc_sttgrp=max(dat$inc_sttgrp),
                  n_inc_reg=max(dat$inc_reg),
                  n_inc_stt=max(dat$inc_stt),
                  n_mar_edu=max(dat$mar_edu),
                  n_mar_age=max(dat$mar_age),
                  n_mar_pid=max(dat$mar_pid),
                  n_mar_prmd=max(dat$mar_prmd),
                  n_mar_votes=max(dat$mar_votes),
                  n_mar_sttgrp=max(dat$mar_sttgrp),
                  n_mar_reg=max(dat$mar_reg),
                  n_mar_stt=max(dat$mar_stt),
                  n_edu_age=max(dat$edu_age),
                  n_edu_pid=max(dat$edu_pid),
                  n_edu_prmd=max(dat$edu_prmd),
                  n_edu_votes=max(dat$edu_votes),
                  n_edu_sttgrp=max(dat$edu_sttgrp),
                  n_edu_reg=max(dat$edu_reg),
                  n_edu_stt=max(dat$edu_stt),
                  n_age_pid=max(dat$age_pid),
                  n_age_prmd=max(dat$age_prmd),
                  n_age_votes=max(dat$age_votes),
                  n_age_sttgrp=max(dat$age_sttgrp),
                  n_age_reg=max(dat$age_reg),
                  n_age_stt=max(dat$age_stt),
                  n_pid_prmd=max(dat$pid_prmd),
                  n_pid_votes=max(dat$pid_votes),
                  n_pid_sttgrp=max(dat$pid_sttgrp),
                  n_pid_reg=max(dat$pid_reg),
                  n_pid_stt=max(dat$pid_stt),
                  n_prmd_votes=max(dat$prmd_votes),
                  n_prmd_sttgrp=max(dat$prmd_sttgrp),
                  n_prmd_reg=max(dat$prmd_reg),
                  n_prmd_stt=max(dat$prmd_stt),
                  n_votes_sttgrp=max(dat$votes_sttgrp),
                  n_votes_reg=max(dat$votes_reg),
                  n_votes_stt=max(dat$votes_stt),
                  n_grp=75)

set_cppo("fast")
S.compile <- stan(model_code=stan.code,
                  data=stan.data,
                  iter=10, chains=1)
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



