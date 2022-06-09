library(survHE)
library(ggthemes)
library(survival)
library(survminer)
library(ggplot2)
library(rJava)
library(xlsx)
library(MASS)
library(splines)
getwd()
setwd("C:\\Users\\MAC\\Desktop\\PFS")
##Construct IPD NIVOS
surv_inp <- "survNIVOS.txt"
nrisk_inp <- "nriskNIVOS.txt"
km_out <- "KMNIVOS.txt"
ipd_out <- "IPDNIVOS.txt"
digitise(surv_inp, nrisk_inp, km_output = "KMNIVOS.txt",
         ipd_output = "IPDNIVOS.txt")
ipd_filesp <- list("IPDNIVOS.txt")
NIVOS<-make.ipd(ipd_filesp,var.labs = c("time", "event", "arm"))
##Construct IPD SOCOS
surv_inp <- "survSOCOS.txt"
nrisk_inp <- "nriskSOCOS.txt"
m_out <- "KMSOCOS.txt"
ipd_out <- "IPDSOCOS.txt"
digitise(surv_inp, nrisk_inp, km_output = "KMSOCOS.txt",
         ipd_output = "IPDSOCOS.txt")
ipd_filesp <- list("IPDSOCOS.txt")
SOCOS<-make.ipd(ipd_filesp,var.labs = c("time", "event", "arm"))

###Sort data
ipd_files1 <- list("IPDSOCOS.txt")
ipd_files0 <- list("IPDNIVOS.txt")
ipd_files <- list("IPDSOCOS.txt","IPDNIVOS.txt")
#Individual data of the experimental group
intervation1 <- make.ipd(ipd_files1,var.labs = c("time","event","arm"))
#Individual data of the control group
intervation0 <- make.ipd(ipd_files0,var.labs = c("time","event","arm"))
#Merging the two individual data
data <- make.ipd(ipd_files, ctr=1 ,var.labs = c("time","event","arm"))
