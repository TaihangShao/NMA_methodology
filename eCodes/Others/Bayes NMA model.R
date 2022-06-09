library(dplyr)
library(ggplot2)
library(R2jags)
library(ggmcmc)
library(survival)

ref.study <- "Kim2008"
ref.trt   <- "Docetaxel"
##Set up

km <- data.frame(
  studyn = c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,1,1,1,1,1,1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7),
  trtn = c(3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,2,2,2,2,2,2,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2),
  time = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,2,4,6,8,10,12,14,2,4,6,8,10,12,14,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,2,4,6,8,10,12,14,16,18,20,22,24,26,28,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,2,4,6,8,10,12,14,16,18,2,4,6,8,10,12,14,16,18,20,2,4,6,8,10,12,14,2,4,6,8,10,12,2,4,6,8,10,12,2,4,6,8,10,12,14,16,18),
  timeDelta = c(2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2),
  nevents = c(7,5,7,6,6,3,4,3,3,3,5,4,3,1,3,3,1,9,4,8,5,8,4,6,2,4,3,3,2,1,1,2,2,22,24,11,8,12,6,1,34,35,29,15,11,14,1,98,104,94,79,64,46,35,30,14,14,14,8,5,3,6,73,109,102,66,68,41,32,29,18,16,13,10,6,5,4,3,18,30,25,22,12,18,10,8,6,4,3,1,1,2,9,21,23,15,23,17,9,10,15,4,5,3,2,1,1,27,47,31,33,40,17,21,8,11,33,48,41,21,34,17,13,18,9,10,11,8,5,14,9,4,8,14,11,6,6,7,4,17,71,53,70,12,22,49,67,60,63,49,14,55,41,30),
  natrisk = c(81,74,69,62,55,49,46,42,38,35,29,24,21,18,17,14,12,76,67,64,56,51,38,34,28,26,22,18,15,13,12,10,8,107,85,61,50,42,30,24,235,201,166,137,122,110,97,723,625,518,424,336,272,225,190,131,117,83,69,50,45,31,710,637,503,401,339,271,228,196,139,121,89,76,46,40,24,20,245,226,197,169,148,127,98,77,63,47,35,29,25,18,244,233,214,189,173,140,105,87,69,44,35,25,18,14,10,283,256,209,178,144,78,61,40,33,288,255,207,166,144,78,61,48,30,21,68,57,49,44,31,21,18,73,59,48,42,36,29,222,205,134,160,90,78,441,392,325,340,277,228,221,166,125)
)
studies <- c("Chang2006", "Cufer2006", "Hanna2004", "Kim2008", "Lee2010", "Maruyama2008", "Shepherd2000")
treatments <- c("BSC", "Docetaxel", "Gefitinib", "Pemetrexed")
km$studyf <- factor(km$studyn, labels = studies)
km$trtf   <- factor(km$trtn, labels = treatments)


#Relevel by reference study and referencetreatment 
km$studyf <- relevel(km$studyf, ref=ref.study)
km$trtf <- relevel(km$trtf, ref=ref.trt)
km$trtn=as.numeric(km$trtf)
km$studyn=as.numeric(km$studyf)
#Generate necessary data objects for Bayesian model

# Prepare input data for jags
d_arms <- km %>% 
  group_by(studyn, trtn) %>%
  slice(1) %>%
  group_by(studyn) %>%
  dplyr::mutate(arm = 1:n(), n_arms = max(arm)) %>%
  select(studyf, trtf, studyn, trtn, arm, n_arms)
d_arms


d_std <- d_arms %>%
  group_by(studyn) %>%
  select(studyn, n_arms) %>%
  slice(1)
d_std


dat <- km %>%
  left_join(d_arms, by = c("studyf", "trtf", "studyn", "trtn"))

d_trts <- dat %>%
  mutate(studyn.arm = interaction(studyn, arm)) %>%
  filter(!duplicated(studyn.arm)) %>%
  select(studyn, arm, trtn) %>%
  arrange(studyn, arm) %>%
  tidyr::spread(key = arm, trtn, drop = FALSE) # identify trt in each am for each study
d_trts


Nobs <- nrow(dat)


#Data list for jags fit
dat_jg <- list(
  Nobs = Nobs,
  Ns = nrow(d_std),
  Na = d_std$n_arms,
  r = dat$nevents,
  n = dat$natrisk,
  time = dat$time,
  dt = dat$timeDelta,
  s = dat$studyn,
  a = dat$arm,
  t = as.matrix(select(ungroup(d_trts), -studyn)),
  Ntrt = max(select(ungroup(d_trts), -studyn), na.rm = TRUE)
)

#--------------------------------------------------------------------------
#### PART2: Fit the models
#### ------------


## Model code (jags)


    # Fractional polynomial NMA, 2nd order, fixed effect model
    # --------------------------------------------------------------------
    # Data: grouped survival data, binomial likelihood, linear predictor on log-hazard
    #         Nobs     number of observations
    #         n[i]     patients at risk in interval i
    #         r[i]     events during interval i
    #         time[i]  mid-point of interval i
    #         dt[i]    length of interval i
    #         Ns       number of studies
    #         Na[j]    number of arms in study j
    #         Ntrt     number of treatments
    #         s[i]     study number for obs i
    #         a[i]     arm number (within study) for obs i
    #         t[i,j]   treatment in study i arm j
    #         P1, P2   exponents of the time varying terms in the fractional polynomial
    #         mean[1:2]         prior mean (for contrasts d and baselines mu)
    #         prec2[1:2, 1:2]   prior precision (for d and mu)
    # --------------------------------------------------------------------

cat("
    
    model{
    
    ## Sampling model
    for (i in 1:Nobs){
    time1[i] <- (equals(P1,0) * log(time[i]) + (1-equals(P1,0)) * pow(time[i],P1)   ) 
    time2[i] <- (  (1-equals(P2,P1)) * (   equals(P2,0) * log(time[i]) + (1-equals(P2,0)) * pow(time[i],P2)  ) + 
    equals(P2,P1) * (   equals(P2,0) * log(time[i])*log(time[i])   +   (1-equals(P2,0)) * pow(time[i],P2) * log(time[i]) ) )
    }
    
    for (i in 1:Nobs){
    # likelihood: digitized KM curves, grouped into intervals [t, t+dt]
    r[i] ~ dbin(p[i], n[i])
    p[i] <- 1 - exp(-h[i] * dt[i])  # cumulative hazard over interval [t,t+dt] expressed as deaths per person-month
    
    # fractional polynomial
    log(h[i]) <- Beta[s[i], a[i], 1] + Beta[s[i], a[i], 2] * time1[i] + Beta[s[i], a[i], 3] * time2[i]
    }
    
    
    ## Arm level parameters = study effect + trt effect (consistency eq)
    for (l in 1:Ns){
    for (ll in 1:Na[l]){
    Beta[l, ll, 1] <- mu[l, 1] + d[t[l, ll], 1] - d[t[l, 1], 1]
    Beta[l, ll, 2] <- mu[l, 2] + d[t[l, ll], 2] - d[t[l, 1], 2]
    Beta[l, ll, 3] <- mu[l, 3] + d[t[l, ll], 3] - d[t[l, 1], 3]
    }
    }
    
    ## Priors
    for (j in 1:Ns){
    mu[j, 1:3] ~ dmnorm(prior.mean[1:3], prior.prec[,]) 
    }
    
    d[1, 1] <- 0
    d[1, 2] <- 0
    d[1, 3] <- 0
    for (k in 2:Ntrt){
    d[k, 1:3] ~ dmnorm(prior.mean[1:3], prior.prec[,]) 
    }
    
    
    } # end of model
    ", file = "tte-fracpoly-2o-fe.txt")


model.pars <- list(P1 = -0.5, P2= 0)
#Fit the second order fractional polynomial model
set.seed(9487397)
fit.fp <- jags(model.file = "tte-fracpoly-2o-fe.txt", 
               data = c(dat_jg,
                        list(prior.mean = rep(0, 3)),
                        list(prior.prec = diag(rep(0.0001, 3))),
                        model.pars),
               parameters = c("d", "mu", "Beta"),
               n.chains = 3, n.iter = 100000, n.burnin = 10000, n.thin = 3) 



#--------------------------------------------------------------------------
#### PART3: Report the results
#### ------------

##HD and DIC
DIC_results <- c(fit.fp$BUGSoutput$pD,fit.fp$BUGSoutput$DIC)
names(DIC_results)<-c("pD","DIC")
DIC_results



##After comparing DIC,choose the best P value
##Delta results
res <- fit.fp
res <- as.data.frame(res$BUGSoutput$summary[,c(5,3,7)])
res <- res[grep("d\\[",rownames(res)),]
res <- round(res,3)
names(res) <- c("median","lower","upper")
res$ci <- paste("(",res$lower,",",res$upper,")",sep="")
res$comparison<-paste(levels(km$trtf),ref.trt,sep=" vs ")
res[,c(5,1,4)]




