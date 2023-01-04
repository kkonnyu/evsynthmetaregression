setwd("/Users/Kristin/Dropbox/1. PhD/8-Thesis/Publications/2-Methods/Analysis")

library(meta) 
library(metafor)

#data
data <- read.csv("cleaned_data_Model_1.txt", header = TRUE, sep = ",", quote="\"", dec=".")

#RE meta-analysis using meta package
n.e <- data$EffectiveNGroup2
mean.e <- data$PostHbA1cmeanGroup2
sd.e <- data$PostHbA1cSDGroup2
n.c <- data$EffectiveNGroup1
mean.c <- data$PostHbA1cmeanGroup1
sd.c <- data$PostHbA1cSDGroup1
CM <- data$CM2==1
TC  <- data$TC2==1
EPR  <- data$EPR2==1
CE  <- data$CE2==1
FR  <- data$FR2==1
PE  <- data$PE2==1
PSM  <- data$PSM2==1
PR <- data$PR2==1
Other <- data$QI_other2==1

meta_all<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, comb.random=gs("comb.random"), prediction = TRUE)
print(meta_all)

dat <- escalc(measure ="MD", m1i=mean.e, sd1i=sd.e, n1i=n.e, m2i=mean.c, sd2i=sd.c, n2i=n.c)

res_all <-rma(yi, vi, data=dat) 
res_all

res_CM <-rma(yi, vi, mods=CM,  data=dat) 
res_CM

res_TC <-rma(yi, vi, mods=TC,  data=dat) 
res_TC

res_EPR <-rma(yi, vi, mods=EPR,  data=dat) 
res_EPR

res_CE <-rma(yi, vi, mods=CE,  data=dat) 
res_CE

res_FR <-rma(yi, vi, mods=FR,  data=dat) 
res_FR

res_PE <-rma(yi, vi, mods=PE,  data=dat) 
res_PE

res_PSM <-rma(yi, vi, mods=PSM,  data=dat) 
res_PSM

res_PR <-rma(yi, vi, mods=PR,  data=dat) 
res_PR

res_Other <-rma(yi, vi, mods=Other,  data=dat) 
res_Other

#CM
meta1<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, subset=(CM), comb.random=gs("comb.random"), prediction = TRUE)
print(meta1)
CM_effect<-cbind(meta1$lower.random, meta1$TE.random, meta1$upper.random)

#TC
meta2<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, subset=(TC), comb.random=gs("comb.random"))
print(meta2)
TC_effect<-cbind(meta2$lower.random, meta2$TE.random, meta2$upper.random)

#EPR
meta3<-meta1<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, subset=(EPR), comb.random=gs("comb.random"))
print(meta3)
EPR_effect<-cbind(meta3$lower.random, meta3$TE.random, meta3$upper.random)

#CE
meta4<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, subset=(CE), comb.random=gs("comb.random"))
print(meta4)
CE_effect<-cbind(meta4$lower.random, meta4$TE.random, meta4$upper.random)

#FR
meta5<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, subset=(FR), comb.random=gs("comb.random"))
print(meta5)
FR_effect<-cbind(meta5$lower.random, meta5$TE.random, meta5$upper.random)

#PE
meta6<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, subset=(PE), comb.random=gs("comb.random"))
print(meta6)
PE_effect<-cbind(meta6$lower.random, meta6$TE.random, meta6$upper.random)

#PSM
meta7<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, subset=(PSM), comb.random=gs("comb.random"))
print(meta7)
PSM_effect<-cbind(meta7$lower.random, meta7$TE.random, meta7$upper.random)

#PR
meta8<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, subset=(PR), comb.random=gs("comb.random"))
print(meta8)
PR_effect<-cbind(meta8$lower.random, meta8$TE.random, meta8$upper.random)

#Other
meta9<-metacont(n.e, mean.e, sd.e, n.c, mean.c, sd.c, subset=(Other), comb.random=gs("comb.random"))
print(meta9)
Other_effect<-cbind(meta9$lower.random, meta9$TE.random, meta9$upper.random)

summary.table<-rbind(CM_effect,TC_effect,EPR_effect,CE_effect,FR_effect,PE_effect,PSM_effect,PR_effect,Other_effect)
write.table(summary.table, file = paste("04_model_Model_1",".csv", sep=""), quote = F, row.names = T)
