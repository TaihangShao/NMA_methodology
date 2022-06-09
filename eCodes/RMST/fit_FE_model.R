# Fit NMA of RMST at 18 months in WinBUGS 

###############################################################

# Load library
library(R2WinBUGS)

# Start with empty environment
rm(list=ls())

# Set working directory
path <- "C:/Users/MAC/Desktop/RMST"
setwd(path)

# load RMST at 18 months data
rmst_18 <- read.csv("C:/Users/MAC/Desktop/Exploratory analyses/rmst_18.csv")

# Now create a version which has one row per study
# At the moment the three-arm trial has two rows of data and need to change this to one row
rmst_18_wide <- rmst_18[, 1:8]
rmst_18_wide$t3 <- NA
rmst_18_wide$rmst2 <- NA
rmst_18_wide$rmst2_se <- NA

rmst_18_wide$t3[7] <- 7
rmst_18_wide$rmst2[7] <- rmst_18$rmst1[8]
rmst_18_wide$rmst2_se[7] <- rmst_18$rmst1_se[8]
rmst_18_wide <- rmst_18_wide[1:7,]

data <- rmst_18_wide

# Order by studycode
data <- data[order(data$studyCode),]

# Set the location for WinBUGS
bugs.directory <- "C:/Users/MAC/Desktop/WinBUGS14"

# WinBUGS burn-in & simulation size
num.sims <- 3000
burn.in <- 3000


#-----------------------------------------------------------------------------
# Data
#-----------------------------------------------------------------------------

# No. of studies
ns <- length(unique(data$studyCode))

# No. of treatments
nt <- max(data$t2)

# No. of arms in each trial
data$na <- 2
data$na[data$studyCode==4] <- 3

y <- array(c(data$rmst0, data$rmst1, data$rmst2), dim=c(ns,3))
se <- array(c(data$rmst0_se, data$rmst1_se, data$rmst2_se), dim=c(ns,3))
t <- array(c(data$t1, data$t2, data$t3), dim=c(ns,3))

bugs_data <- list(ns=ns, nt=nt, t=t, y=y, se=se, na=data$na)

#-----------------------------------------------------------------------------
# Initial values
#-----------------------------------------------------------------------------

d1 <- c(NA, rep(0,6))
d2 <- c(NA, rep(0.1, 6))
d3 <- c(NA, rep(-0.1, 6))

mu1 <- c(rep(0.1, ns))
mu2 <- c(rep(0.3, ns))
mu3 <- c(rep(0.5, ns))

fe_inits <- list(list(mu=mu1, d=d1), 
                 list(mu=mu2, d=d2),
                 list(mu=mu3, d=d3))

#-----------------------------------------------------------------------------
# Fit FE model in WinBUGS
#-----------------------------------------------------------------------------

bugs.fe <- bugs(data=bugs_data, inits=fe_inits, 
                parameters.to.save=c("d", "best", "prob", "rk"), 
                model.file="FE_model.txt", clearWD=F, 
                summary.only=FALSE, n.iter=(num.sims+burn.in), 
                n.sims=num.sims, n.burnin=burn.in, n.chains=3, 
                bugs.seed=385916, bugs.directory=bugs.directory, 
                debug=F, working.directory=path)

fe_results <- bugs.fe$summary

# Save results in csv file
write.csv(fe_results,file="fe_results.csv")

# Check results
results2 <- bugs.fe$sims.matrix[,grep("d",rownames(bugs.fe$summary))]
results2 <- cbind(rep(0,dim(results2)[1]),results2)
summary(results2)

results_mcmc<-mcmc(results2)
par(mfrow=c(3,2))

# Check autocorrelation
autocorr.plot(results_mcmc[,2:7])

# Check trace for convergence
traceplot(results_mcmc[,2])
traceplot(results_mcmc[,3])
traceplot(results_mcmc[,4])
traceplot(results_mcmc[,5])
traceplot(results_mcmc[,6])
traceplot(results_mcmc[,7])


# Histograms of posterior distributions
densplot(results_mcmc[,2])
densplot(results_mcmc[,3])
densplot(results_mcmc[,4])
densplot(results_mcmc[,5])
densplot(results_mcmc[,6])
densplot(results_mcmc[,7])


