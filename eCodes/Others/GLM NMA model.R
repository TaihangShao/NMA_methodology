library(dplyr)
library(ggplot2)

ref.study <- "Kim2008"
ref.trt   <- "Docetaxel"
#Generate the data
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

#Relevel by reference study and reference treatment 
km$studyf <- relevel(km$studyf, ref=ref.study)
km$trtf <- relevel(km$trtf, ref=ref.trt)
km$trtn=as.numeric(km$trtf)
km$studyn=as.numeric(km$studyf)

#### PART2: Fit the models
#### ------------

#list of models to be fitted
models <- list(
  "First order FP, p1=-2" = list(g1=function(x){x^-2},g2=function(x){0},f1=function(x){x^-2},f2=function(x){0}),
  "First order FP,p1=-1" = list(g1=function(x){x^-1},g2=function(x){0},f1=function(x){x^-1},f2=function(x){0}),
  "First order FP,p1=-0.5" = list(g1=function(x){x^-0.5},g2=function(x){0},f1=function(x){x^-0.5},f2=function(x){0}),
  "First order FP,p1=0" = list(g1=function(x){log(x)},g2=function(x){0},f1=function(x){log(x)},f2=function(x){0}),
  "First order FP,p1=0.5" = list(g1=function(x){x^0.5},g2=function(x){0},f1=function(x){x^0.5},f2=function(x){0}),
  "First order FP,p1=1" = list(g1=function(x){x},g2=function(x){0},f1=function(x){x},f2=function(x){0}),
  "First order FP, p1=2" = list(g1=function(x){x^2},g2=function(x){0},f1=function(x){x^2},f2=function(x){0}),
  "First order FP,p1=3" = list(g1=function(x){x^3},g2=function(x){0},f1=function(x){x^3},f2=function(x){0}),
  "Second order FP,p1=-2, p2=-2" = list(g1=function(x){x^-2},g2=function(x){x^-2},f1=function(x){x^-2},f2=function(x){x^-2}),
  "Second order FP,p1=-2, p2=-1" = list(g1=function(x){x^-2},g2=function(x){x^-1},f1=function(x){x^-2},f2=function(x){x^-1}),
  "Second order FP,p1=-2, p2=-0.5" = list(g1=function(x){x^-2},g2=function(x){x^-0.5},f1=function(x){x^-2},f2=function(x){x^-0.5}),
  "Second order FP,p1=-2, p2=0" = list(g1=function(x){x^-2},g2=function(x){log(x)},f1=function(x){x^-2},f2=function(x){log(x)}),
  "Second order FP,p1=-2, p2=1" = list(g1=function(x){x^-2},g2=function(x){x^1},f1=function(x){x^-2},f2=function(x){x^1}),
  "Second order FP,p1=-2, p2=0.5" = list(g1=function(x){x^-2},g2=function(x){x^0.5},f1=function(x){x^-2},f2=function(x){x^0.5}),
  "Second order FP,p1=-2, p2=2" = list(g1=function(x){x^-2},g2=function(x){x^2},f1=function(x){x^-2},f2=function(x){x^2}),
  "Second order FP,p1=-2, p2=3" = list(g1=function(x){x^-2},g2=function(x){x^3},f1=function(x){x^-2},f2=function(x){x^3}),
  "Second order FP,p1=-1, p2=-1" = list(g1=function(x){x^-1},g2=function(x){x^-1},f1=function(x){x^-1},f2=function(x){x^-1}),
  "Second order FP,p1=-1, p2=-0.5" = list(g1=function(x){x^-1},g2=function(x){x^-0.5},f1=function(x){x^-1},f2=function(x){x^-0.5}),
  "Second order FP,p1=-1, p2=0" = list(g1=function(x){x^-1},g2=function(x){log(x)},f1=function(x){x^-1},f2=function(x){log(x)}),
  "Second order FP,p1=-1, p2=1" = list(g1=function(x){x^-1},g2=function(x){x^1},f1=function(x){x^-1},f2=function(x){x^1}),
  "Second order FP,p1=-1, p2=0.5" = list(g1=function(x){x^-1},g2=function(x){x^0.5},f1=function(x){x^-1},f2=function(x){x^0.5}),
  "Second order FP,p1=-1, p2=2" = list(g1=function(x){x^-1},g2=function(x){x^2},f1=function(x){x^-1},f2=function(x){x^2}),
  "Second order FP,p1=-1, p2=3" = list(g1=function(x){x^-1},g2=function(x){x^3},f1=function(x){x^-1},f2=function(x){x^3}),
  "Second order FP,p1=-1, p2=3" = list(g1=function(x){x^-1},g2=function(x){x^3},f1=function(x){x^-1},f2=function(x){x^3}),
  "Second order FP,p1=-0.5, p2=-0.5" = list(g1=function(x){x^-0.5},g2=function(x){x^-0.5},f1=function(x){x^-0.5},f2=function(x){x^-0.5}),
  "Second order FP,p1=-0.5, p2=0" = list(g1=function(x){x^-0.5},g2=function(x){log(x)},f1=function(x){x^-0.5},f2=function(x){log(x)}),
  "Second order FP,p1=-0.5, p2=1" = list(g1=function(x){x^-0.5},g2=function(x){x^1},f1=function(x){x^-0.5},f2=function(x){x^1}),
  "Second order FP,p1=-0.5, p2=0.5" = list(g1=function(x){x^-0.5},g2=function(x){x^0.5},f1=function(x){x^-0.5},f2=function(x){x^0.5}),
  "Second order FP,p1=-0.5, p2=2" = list(g1=function(x){x^-0.5},g2=function(x){x^2},f1=function(x){x^-0.5},f2=function(x){x^2}),
  "Second order FP,p1=-0.5, p2=3" = list(g1=function(x){x^-0.5},g2=function(x){x^3},f1=function(x){x^-0.5},f2=function(x){x^3}),
  "Second order FP,p1=0, p2=0" = list(g1=function(x){log(x)},g2=function(x){log(x)},f1=function(x){log(x)},f2=function(x){log(x)}),
  "Second order FP,p1=0, p2=1" = list(g1=function(x){log(x)},g2=function(x){x^1},f1=function(x){log(x)},f2=function(x){x^1}),
  "Second order FP,p1=0, p2=0.5" = list(g1=function(x){log(x)},g2=function(x){x^0.5},f1=function(x){log(x)},f2=function(x){x^0.5}),
  "Second order FP,p1=0, p2=2" = list(g1=function(x){log(x)},g2=function(x){x^2},f1=function(x){log(x)},f2=function(x){x^2}),
  "Second order FP,p1=0, p2=3" = list(g1=function(x){log(x)},g2=function(x){x^3},f1=function(x){log(x)},f2=function(x){x^3}),
  "Second order FP,p1=0.5, p2=1" = list(g1=function(x){x^0.5},g2=function(x){x^1},f1=function(x){x^0.5},f2=function(x){x^1}),
  "Second order FP,p1=0.5, p2=0.5" = list(g1=function(x){x^0.5},g2=function(x){x^0.5},f1=function(x){x^0.5},f2=function(x){x^0.5}),
  "Second order FP,p1=0.5, p2=2" = list(g1=function(x){x^0.5},g2=function(x){x^2},f1=function(x){x^0.5},f2=function(x){x^2}),
  "Second order FP,p1=0.5, p2=3" = list(g1=function(x){x^0.5},g2=function(x){x^3},f1=function(x){x^0.5},f2=function(x){x^3}),
  "Second order FP,p1=1, p2=1" = list(g1=function(x){x^1},g2=function(x){x^1},f1=function(x){x^1},f2=function(x){x^1}),
  "Second order FP,p1=1, p2=2" = list(g1=function(x){x^1},g2=function(x){x^2},f1=function(x){x^1},f2=function(x){x^2}),
  "Second order FP,p1=1, p2=3" = list(g1=function(x){x^1},g2=function(x){x^3},f1=function(x){x^1},f2=function(x){x^3}),
  "Second order FP,p1=2, p2=2" = list(g1=function(x){x^2},g2=function(x){x^2},f1=function(x){x^2},f2=function(x){x^2}),
  "Second order FP,p1=2, p2=3" = list(g1=function(x){x^2},g2=function(x){x^3},f1=function(x){x^2},f2=function(x){x^3}),
  "Second order FP,p1=3, p2=3" = list(g1=function(x){x^3},g2=function(x){x^3},f1=function(x){x^3},f2=function(x){x^3})
)

#Fit all models
fit.KM.NMA<-function(bf){
  km.new=km
  km.new$g0=1
  km.new$f0=1
  km.new$g1=bf[[1]](km.new$time)
  km.new$g2=bf[[2]](km.new$time)
  km.new$f1=bf[[3]](km.new$time)
  km.new$f2=bf[[4]](km.new$time)
  #model formula
  f=cbind(nevents,natrisk-nevents)~trtf*f0+studyf*g0+trtf*f1+trtf*f2+studyf*g1+studyf*g2
  glm(f,family=binomial(link=cloglog),data=km.new,offset = log(timeDelta))
}

fits=lapply(models,fit.KM.NMA)


#--------------------------------------------------------------------------
#### PART3: Report the results
#### ------------


#Get AIC from each model
aics=lapply(fits,AIC)
#Print the AICs
data.frame(AIC=round(unlist(aics),2))


#predict all models for all treatments from 1:60 months
pred.KM.NMA<-function(bf){
  trts=data.frame(trtf=unique(km$trtf))
  trts$studyf=sort(unique(km$studyf))[1] # Select reference study as baseline
  timehorizon=data.frame(time=1*(1:120))
  km.pred=merge.data.frame(timehorizon,trts)
  km.pred$g0=1
  km.pred$f0=1
  km.pred$g1=bf[[1]](km.pred$time)
  km.pred$g2=bf[[2]](km.pred$time)
  km.pred$f1=bf[[3]](km.pred$time)
  km.pred$f2=bf[[4]](km.pred$time)
  km.pred$timeDelta<-1
  km.pred
}
pred.KM.data=lapply(models,pred.KM.NMA)


#calculate HR(vs ref.trt)
for(i in 1:length(models)){
  pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
  d=pred.KM.data[[i]]
  d1<-d %>% 
    filter(trtf == ref.trt)%>%
    mutate(pred.doce = pred)%>% 
    select(pred.doce,time) %>% 
    arrange(time) %>% 
    left_join(d) %>%
    mutate(lnhr = pred-pred.doce) %>% 
    mutate(hr=exp(lnhr))#%>%
  d1$modelc=names(models)[[i]]
  if(i==1)dpred<-d1
  if(i!=1)dpred<-rbind(dpred,d1)
}
dpred$Model<-factor(dpred$modelc,levels=names(models))
#Plot the predicted hazard ratios for each model
f1<-ggplot() +
  geom_line(data=dpred,aes(x=time, y=hr, group=trtf,colour=trtf), size=1) +
  geom_hline(yintercept=1, lty=2) + 
  #geom_vline(xintercept=14, lty=2) + 
  facet_wrap(~Model,nrow=3)+
  scale_color_discrete(name="Treatment")+
  scale_y_log10(limits = c(0.1, 10), breaks = c(0.1,0.25, 0.5,  1,  2, 4, 10)) +
  scale_x_continuous(breaks = c(seq(from=0, to=60,by = 12))) +
  ylab("Hazard Ratio") +
  xlab("Time(months)") +
  theme(legend.position = "bottom") + 
  theme_bw() 
f1
ggsave(plot = f1, filename = "paperHRbest.png",  width = 12, height = 9)

#calculate survival over time
for(i in 1:length(models)){
  pred.KM.data[[i]]$pred=predict.glm(fits[[i]],pred.KM.data[[i]])
  d=pred.KM.data[[i]]
  d1<-d %>% 
    dplyr::mutate(haz = exp(pred))%>% 
    dplyr::group_by(trtf) %>% 
    dplyr::arrange(time) %>% 
    dplyr::mutate(cumhaz = cumsum(haz)) %>% 
    dplyr::mutate(survProp = exp(-1*cumhaz))
  zero <- unique(d1[,c("trtf", "studyf")])
  zero$time=0
  zero$survProp=1
  d1=rbind(zero,d1)
  
  d1$modelc=names(models)[[i]]
  if(i==1)dpred<-d1
  if(i!=1)dpred<-rbind(dpred,d1)
}
dpred$Model<-factor(dpred$modelc,levels=names(models))
#Plot the predicted survival functions for each model
f2= ggplot() +
  geom_line(data=dpred, aes(x=time, y=survProp, group=trtf, colour=trtf), size=1) +
  scale_color_discrete(name="Treatment")+
  expand_limits(y=c(0,1),x=c(0,60)) + 
  facet_wrap(~Model,nrow=3)+
  scale_x_continuous(breaks = c(seq(from=0, to=60,by = 12))) +
  ylab("Proportion surviving") +
  xlab("Time(months)") +
  guides(color = guide_legend(ncol = 1))  +
  theme(legend.position = "bottom") + 
  theme_bw() 
f2
ggsave(plot = f2, filename = "paperSurv0404best.png",  width = 12, height = 9)

#predict all models for all treatments from 1:60 months
pred.KM.NMA<-function(bf){
  trts=data.frame(trtf=unique(km$trtf))
  trts$studyf=sort(unique(km$studyf))[1] # select reference study as baseline
  timehorizon=data.frame(time=2*(1:30))
  km.pred=merge.data.frame(timehorizon,trts)
  km.pred$g0=1
  km.pred$f0=1
  km.pred$g1=bf[[1]](km.pred$time)
  km.pred$g2=bf[[2]](km.pred$time)
  km.pred$f1=bf[[3]](km.pred$time)
  km.pred$f2=bf[[4]](km.pred$time)
  km.pred$timeDelta<-2
  km.pred
}
pred.KM.data=pred.KM.NMA(models[["Second order FP,p1=-2, p2=1"]])


timeh<-data.frame(time=1:60,a=1)
fit<-fits[["Second order FP,p1=-2, p2=1" ]]
pred.data<-fit$data
pred.data$est1<-predict.glm(fit)
pred.data$est2=NA
pred.data$se<-predict.glm(fit,se.fit=T)$se.fit
pred.data$model<-"Second order FP,p1=-2, p2=1"
a1<-pred.data

fit<-fits[["PWE, cutpoint 2" ]]

pred.data<-fit$data
pred.data$est1=NA
pred.data$est2<-predict.glm(fit)
pred.data$se<-predict.glm(fit,se.fit=T)$se.fit
pred.data$model<-"PWE, cutpoint 2"

a2<-pred.data
pred.data<-rbind(a1,a2)

pred.data$est<-ifelse(is.na(pred.data$est1),pred.data$est2,pred.data$est1)
pred.data$ci.lower=pred.data$est-qnorm(0.975)*pred.data$se
pred.data$ci.upper=pred.data$est+qnorm(0.975)*pred.data$se
pred.data$obs.h=log(-log(1-pred.data$nevents/pred.data$natrisk))
f3<-ggplot(data=pred.data) +
  facet_wrap(~studyf+model,ncol=2)+  
  geom_step(aes(x=time, y=est2,group=trtf,colour=trtf), size=0.5)+
  geom_line(aes(x=time, y=est1,group=trtf,colour=trtf), size=0.5)+
  geom_point(aes(x=time, y=obs.h,group=trtf,colour=trtf), size=1)+
  scale_color_discrete(name="Treatment")+
  xlab("Time(months)") +
  ylab("log(h)") +
  #geom_ribbon(aes(x = time, ymin=ci.lower, ymax=ci.upper,group=trtf,fill=trtf), alpha = 0.1, colour = "white")+
  theme_bw()
f3
ggsave(plot = f3, filename = "paperFits.png",  width = 8.5, height = 12)


#Data processing to extract the contrast coefficient
coeff.data            <- as.data.frame(coef(summary(fits[["Second order FP,p1=0.5, p2=1"]])))[,1:2]
names(coeff.data)     <- c("est","std.err")
attach(coeff.data)
coeff.data$conf.int.lower   <- est-std.err*qnorm(0.975)
coeff.data$conf.int.upper   <- est+std.err*qnorm(0.975)
coeff.data[,1:4]      <- round(coeff.data[,1:4],3)
coeff.data$pn         <- rownames(coeff.data)
coeff.data            <- coeff.data[grepl("trtf",coeff.data$pn),]
coeff.data$contrast   <- paste(levels(km$trtf)[-1],"vs",ref.trt)
detach(coeff.data)
coeff.data

coeff.data            <- as.data.frame(coef(summary(fits[["PWE, cutpoint 2" ]])))[,1:2]
names(coeff.data)     <- c("est","std.err")
attach(coeff.data)
coeff.data$conf.int.lower   <- est-std.err*qnorm(0.975)
coeff.data$conf.int.upper   <- est+std.err*qnorm(0.975)
coeff.data[,1:4]      <- round(coeff.data[,1:4],3)
coeff.data$pn         <- rownames(coeff.data)
coeff.data            <- coeff.data[grepl("trtf",coeff.data$pn),]
coeff.data$contrast   <- paste(levels(km$trtf)[-1],"vs",ref.trt)
detach(coeff.data)
coeff.data