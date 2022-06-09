# Generalised gamma model

##################################################################################

# Start with a blank environment
rm(list=ls())

# Load packages
library(survival)
library(flexsurv)
library(R2WinBUGS)

# Set the working directory
path <- 'C:/Users/MAC/Desktop/Generalised_Gamma/Fixed'
setwd(path)

# load data
data <- read.csv("C:/Users/MAC/Desktop/Data/RP_OSXIU.csv")
data <- data[, 2:9]

# Set the location for WinBUGS
bugs.directory <- "C:/Users/MAC/Desktop/WinBUGS14"

# WinBUGS burn-in & simulation size (this will be divided by the number of chains)
num.sims <- 30000
burn.in <- 30000

#-----------------------------------------------------------------------------
# Initial data formatting
#-----------------------------------------------------------------------------

# Add treatment as a character variable
data$trt[data$txCode==1] <- "Che"
data$trt[data$txCode==2] <- "Niv+ipi+che"
data$trt[data$txCode==3] <- "Ate+che"
data$trt[data$txCode==4] <- "Bev+che"
data$trt[data$txCode==5] <- "Ate+bev+che"
data$trt[data$txCode==6] <- "Pem+che"
data$trt[data$txCode==7] <- "Niv+bev+che"

#select data set
selected <- data.frame(trt=data$trt, study=data$study, time=data$time, event=data$event, 
                       studyCode=data$studyCode, txCode=data$txCode)

# Convert trt and study to factor variables
selected$trt <- as.factor(selected$trt)
selected$study <- as.factor(selected$study)

# Re-order trt so that the treatments are in the order we want (rather than alphabetical)
selected$trt=factor(selected$trt, levels(selected$trt)[c(4,6,2,3,1,7,5)])

# Vector of study names
Studies <- levels(selected$study)

# Vector of treatment names
Treat <- levels(selected$trt)

# Number of studies
nStudies<-length(Studies)

# Number of treatments
nTreat<-length(Treat)

# Select the greatest f/up time and round it to the nearest 10
maxFU<-round(max(selected$time),-1)


#-----------------------------------------------------------------------------
# Two-stage BUGS Analysis: First stage - Analyse each trial individually and 
# create a data frame containing results from each trial
#-----------------------------------------------------------------------------

# Create data set for winbugs NMA 
study_GG<-rep(NA, nStudies)

# Loop over the number of studies
for (i in 1:nStudies){
  # Select one study
  data_i<-selected[selected$study==Studies[i],]
  # Vector of treatments used in study 
  data_i$trt<-factor(data_i$trt)
  #Fit generalised gamma model
  nmastud<-flexsurvreg(formula=Surv(time,event)~trt,
                       data=data_i, dist="gengamma", method="BFGS")
  # Identify treatments other than the reference
  labels<-paste("trt",levels(data_i$trt)[2:length(levels(data_i$trt))],
                sep="")
  #Number of treatment contrasts in study
  contrasts<-length(labels)
  # Save results of beta coefficients and standard errors
  beta<-nmastud$coefficients[labels]
  se<-sqrt(diag(nmastud$cov))[labels]
  #Set up covariance matrix but leave empty if only two treatments in study
  cov<-NA
  if (contrasts>1) {cov<-sqrt(nmastud$cov[labels,labels][1,2])}
  # Add results for study to a data frame 
  study_i<-data.frame(STUDY=rep(Studies[i],contrasts),
                      COMP=gsub("trt","",labels),
                      REF=rep(levels(data_i$trt)[1],contrasts),
                      MEAN=beta,
                      MEANSE=se,
                      COV=cov)
  # Add results for most recent study to a data frame which contains the results of previous studies
  study_GG<-rbind(study_GG,study_i)
}
study_GG<-study_GG[-1,]

#---------------------------------
# Correction for multi-arm trials
#--------------------------------

study_GG$multi<-0

# Create row vector which contains FALSE if a two-arm trial and TRUE if a multi-arm trial
multi_index<-study_GG$STUDY%in%rownames(table(study_GG$STUDY))[table(study_GG$STUDY)>1]

# Use this vector to replace 0's with 1's in the column multi if multi_index is TRUE
study_GG[multi_index,]$multi<-1
# Adjust the variable MEANSE to take into account multi-arm trials
study_GG[multi_index,"MEANSE"]<-sqrt(study_GG[multi_index,"MEANSE"]^2-study_GG[multi_index,"COV"]^2)

# Identify multi-arm trials
multi_study<-unique(study_GG$STUDY[study_GG$multi==1])
# Identify first row in study_GG which belongs to the multi-arm trial
multi_first<-match(multi_study,study_GG$STUDY)

# Add a row to the data frame for each multi-arm trial
study_GG<-rbind(study_GG,
                data.frame(STUDY=multi_study,
                           COMP=study_GG$REF[multi_first],
                           REF=study_GG$REF[multi_first],
                           MEAN=rep(0,length(multi_study)),
                           MEANSE=study_GG$COV[multi_first],
                           COV=rep(NA,length(multi_study)),
                           multi=rep(1,length(multi_study))
                )   
)

# Remove the row names (turns them back to numbers)
row.names(study_GG)<-NULL

# Make study a factor variable
study_GG$studyCode <- as.factor(study_GG$STUDY)

#-----------------------------------------------------------------------------
# Prepare data for analysis in WinBUGS
#-----------------------------------------------------------------------------

#Use the list command to group together data needed for WinBUGS
# LnObs - number of rows of data
# nTx - number of treatments in network
# nStudies - number of studies in network
# Lstudy - study ID number
# Ltx - comparison treatment
# Lbase - reference treatment
# Lmean - mean treatment effect
# Lse - mean SE
# multi - indicator for whether row belongs to a multi-arm trial
BUGS_data<-list(LnObs=dim(study_GG)[1], nTx=nTreat, nStudies=nStudies, Lstudy=as.numeric(study_GG$studyCode),
                Ltx=match(study_GG$COMP,Treat), Lbase=match(study_GG$REF,Treat),
                Lmean=study_GG[,"MEAN"], Lse=study_GG[,"MEANSE"], multi=study_GG$multi)

# Set up initial values for FE model
initsFE1<-list(alpha = rep(-0.5,nStudies), beta = c(NA,rep(-0.5,nTreat-1)))
initsFE2<-list(alpha = rep(0.5,nStudies), beta = c(NA,rep(0.5,nTreat-1)))
initsFE3<-list(alpha = rep(0.1,nStudies), beta = c(NA,rep(0.1,nTreat-1)))
initsFE<-list(initsFE1, initsFE2, initsFE3)




#-----------------------------------------------------------------------------
# Fixed Effect Model
#-----------------------------------------------------------------------------

# Run WinBUGS model 
nmaAggrFE<-bugs(data=BUGS_data, inits=initsFE, parameters.to.save=c("beta", "aft", "rk"),
                model.file="FE_model.txt", clearWD=F, summary.only=FALSE, n.iter=(num.sims+burn.in), 
                n.sims=num.sims, n.burnin=burn.in, n.chains=3, 
                debug=FALSE, bugs.seed=385916, bugs.directory = bugs.directory, working.directory=path)

# Save results in csv file
FE_results <- nmaAggrFE$summary
write.csv(FE_results,file="FE_OS.csv")

# Check results
nmaAggrFEbetas<-nmaAggrFE$sims.matrix[,grep("beta",rownames(nmaAggrFE$summary))]
nmaAggrFEbetas<-cbind(rep(0,dim(nmaAggrFEbetas)[1]),nmaAggrFEbetas)
colnames(nmaAggrFEbetas)<-Treat
summary(nmaAggrFEbetas)

coda.options(combine.plots=FALSE)
nmaAggrFE_mcmc<-mcmc(nmaAggrFEbetas)

all(nmaAggrFE$summary[,"Rhat"] < 1.1) 
print(nmaAggrFE,digits=3)


# Check autocorrelation, trace and density plots
par(mfrow=c(3,2))

autocorr.plot(nmaAggrFE_mcmc)

traceplot(nmaAggrFE_mcmc[,2])
traceplot(nmaAggrFE_mcmc[,3])
traceplot(nmaAggrFE_mcmc[,4])
traceplot(nmaAggrFE_mcmc[,5])
traceplot(nmaAggrFE_mcmc[,6])
traceplot(nmaAggrFE_mcmc[,7])


densplot(nmaAggrFE_mcmc[,2])
densplot(nmaAggrFE_mcmc[,3])
densplot(nmaAggrFE_mcmc[,4])
densplot(nmaAggrFE_mcmc[,5])
densplot(nmaAggrFE_mcmc[,6])
densplot(nmaAggrFE_mcmc[,7])



#-----------------------------------------------------------------------------
# Survival prediction - reference curve CheckMate 066
#-----------------------------------------------------------------------------

# Fit generalised gamma model just for the Che arm of the Keynote189 trial
df <- data[data$studyCode==5 & data$txCode==1,]

ma <- flexsurvreg(formula=Surv(time,event)~1,
                  data=df, dist="gengamma", method="BFGS")

# Identify coefficicents needed for predicting survival
mu <- ma$coefficients["mu"]
sigma <- exp(ma$coefficients["sigma"])
q <- ma$coefficients["Q"]

# Store treatment effects for each treatment
trt2 <- nmaAggrFE$summary[1,1]
trt3 <- nmaAggrFE$summary[2,1]
trt4 <- nmaAggrFE$summary[3,1]
trt5 <- nmaAggrFE$summary[4,1]
trt6 <- nmaAggrFE$summary[5,1]
trt7 <- nmaAggrFE$summary[6,1]


#Calculate survival across 120 months
x <- seq(0,120,0.1)
S.trt1 <- 1-pgengamma(x, mu = mu, sigma = sigma, Q=q, lower.tail = TRUE, log.p = FALSE)
S.trt2 <- 1-pgengamma(x, mu = mu+trt2, sigma = sigma, Q=q, lower.tail = TRUE, log.p = FALSE)
S.trt3 <- 1-pgengamma(x, mu = mu+trt3, sigma = sigma, Q=q, lower.tail = TRUE, log.p = FALSE)
S.trt4 <- 1-pgengamma(x, mu = mu+trt4, sigma = sigma, Q=q, lower.tail = TRUE, log.p = FALSE)
S.trt5 <- 1-pgengamma(x, mu = mu+trt5, sigma = sigma, Q=q, lower.tail = TRUE, log.p = FALSE)
S.trt6 <- 1-pgengamma(x, mu = mu+trt6, sigma = sigma, Q=q, lower.tail = TRUE, log.p = FALSE)
S.trt7 <- 1-pgengamma(x, mu = mu+trt7, sigma = sigma, Q=q, lower.tail = TRUE, log.p = FALSE)

graph_data <- data.frame(time=x, trt1=S.trt1, trt2=S.trt2, trt3=S.trt3, trt4=S.trt4, trt5=S.trt5, trt6=S.trt6,
                         trt7=S.trt7)
write.csv(graph_data, "graph_OS.csv")

# Calculate KM estimate for DTIC from CheckMate 066
ipd_data <- df[df$txCode==1,]
KM.est<-survfit(Surv(time,event)~1, data=ipd_data, type="kaplan-meier", conf.int=FALSE)


# Using the Paired colour palette from RColorBrewer
color=c("#070d43", "#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00",
        "#cab2d6", "#6a3d9a", "#581845", "#b15928", "#184558")

# Graph region parameters
par(xpd = T, mar = par()$mar + c(7,0,0,0))

# Start by plotting the Kaplan-Meier DTIC curve for the CheckMate 066 trial
plot(KM.est,xlab="Time (months)",ylab="Overall Survival",xaxt="n",yaxt="n",main=" ",xlim=c(0,120),ylim=c(0,1),
     mark.time=FALSE, col=color[14], conf.int=F)

#Add y axis (2 specifies that axis goes on the left of the plot)
axis(2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1))

#Add x axis (1 specified that axis goes at the bottom of the plot)
axis(1, at=c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120))

# Add survival curves for each treatment
lines(x,S.trt1, col=color[1])
lines(x,S.trt2, col=color[2])
lines(x,S.trt3, col=color[3])
lines(x,S.trt4, col=color[4])
lines(x,S.trt5, col=color[5])
lines(x,S.trt6, col=color[6])
lines(x,S.trt7, col=color[7])

# Add legend
legend(0, -0.3,
       c("KM","Che", "Niv+ipi+che", "Ate+che", "Bev+che", "Ate+bev+che","Pem+che", "Niv+bev+che"),
       col=c(color[14],color[1], color[2], color[3], color[4], color[5], color[6],color[7]),
       lty=c(1,1,1,1), ncol=3, text.width=20, box.lty=0)

# save plot
dev.copy(pdf, "survival_OS.pdf")
dev.off()

library(pracma)

# Calculate AUC at 120 months
graph60 <- graph_data[1:1201, ]
auc60 <- data.frame(trt=c(1:7), auc=NA)

auc60$auc[1] <- trapz(graph60$time, graph60$trt1)
auc60$auc[2] <- trapz(graph60$time, graph60$trt2)
auc60$auc[3] <- trapz(graph60$time, graph60$trt3)
auc60$auc[4] <- trapz(graph60$time, graph60$trt4)
auc60$auc[5] <- trapz(graph60$time, graph60$trt5)
auc60$auc[6] <- trapz(graph60$time, graph60$trt6)
auc60$auc[7] <- trapz(graph60$time, graph60$trt7)


write.csv(auc60, file="auc120_OS.csv")
