library(plyr)
library(psych)
library(lme4)
library(lattice)
library(Hmisc)
library(dfoptim)
library(coefplot)
library(effects)
library (car)
library (lsmeans)
library (afex)
library (pbkrtest)
library (parallel)
library(plotrix)
library (ggplot2)
library(influence.ME)
library(optimx)



options(scipen = 1000000)
fcv <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_12_31_3.csv", stringsAsFactors = F)
str(fcv)
names(fcv)
fcv$fecha <- "2018_12"

fcv1 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_11_30_3.csv", stringsAsFactors = F)
fcv1$fecha <- "2018_11"

fcvcomp<- rbind(fcv,fcv1)
rm(fcv)
rm(fcv1)

fcv2 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_10_31_3.csv", stringsAsFactors = F)
fcv2$fecha <- "2018_10"
fcvcomp<- rbind(fcvcomp,fcv2)
rm(fcv2)

fcv3 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_09_30_3.csv", stringsAsFactors = F)
fcv3$fecha <- "2018_09"
fcvcomp<- rbind(fcvcomp,fcv3)
rm(fcv3)

fcv4 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_08_31_3.csv", stringsAsFactors = F)
fcv4$fecha <- "2018_08"
fcvcomp<- rbind(fcvcomp,fcv4)
rm(fcv4)

fcv5 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_07_31_3.csv", stringsAsFactors = F)
fcv5$fecha <- "2018_07"
fcvcomp<- rbind(fcvcomp,fcv5)
rm(fcv5)

fcv6 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_06_30_3.csv", stringsAsFactors = F)
fcv6$fecha <- "2018_06"
fcvcomp<- rbind(fcvcomp,fcv6)
rm(fcv6)

fcv7 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_05_31_3.csv", stringsAsFactors = F)
fcv7$fecha <- "2018_05"
fcvcomp<- rbind(fcvcomp,fcv7)
rm(fcv7)

fcv8 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_04_30_3.csv", stringsAsFactors = F)
fcv8$fecha <- "2018_04"
fcvcomp<- rbind(fcvcomp,fcv8)
rm(fcv8)

fcv9 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_03_31_3.csv", stringsAsFactors = F)
fcv9$fecha <- "2018_03"
fcvcomp<- rbind(fcvcomp,fcv9)
rm(fcv9)

fcv10 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_02_28_3.csv", stringsAsFactors = F)
fcv10$fecha <- "2018_02"
fcvcomp<- rbind(fcvcomp,fcv10)
rm(fcv10)

fcv11 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2018_01_31_3.csv", stringsAsFactors = F)
fcv11$fecha <- "2018_01"
fcvcomp<- rbind(fcvcomp,fcv11)
rm(fcv11)

fcv12 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_01_31_3.csv", stringsAsFactors = F)
fcv12$fecha <- "2017_01"
fcvcomp<- rbind(fcvcomp,fcv12)
rm(fcv12)

fcv13 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_02_28_3.csv", stringsAsFactors = F)
fcv13$fecha <- "2017_02"
fcvcomp<- rbind(fcvcomp,fcv13)
rm(fcv13)

fcv14 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_03_31_3.csv", stringsAsFactors = F)
fcv14$fecha <- "2017_03"
fcvcomp<- rbind(fcvcomp,fcv14)
rm(fcv14)

fcv15 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_04_30_3.csv", stringsAsFactors = F)
fcv15$fecha <- "2017_04"
fcvcomp<- rbind(fcvcomp,fcv15)
rm(fcv15)

fcv16 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_05_31_3.csv", stringsAsFactors = F)
fcv16$fecha <- "2017_05"
fcvcomp<- rbind(fcvcomp,fcv16)
rm(fcv16)

fcv17 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_06_30_3.csv", stringsAsFactors = F)
fcv17$fecha <- "2017_06"
fcvcomp<- rbind(fcvcomp,fcv17)
rm(fcv17)

fcv18 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_07_31_3.csv", stringsAsFactors = F)
fcv18$fecha <- "2017_07"
fcvcomp<- rbind(fcvcomp,fcv18)
rm(fcv18)

fcv19 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_08_31_3.csv", stringsAsFactors = F)
fcv19$fecha <- "2017_08"
fcvcomp<- rbind(fcvcomp,fcv19)
rm(fcv19)

fcv20 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_09_30_3.csv", stringsAsFactors = F)
fcv20$fecha <- "2017_09"
fcvcomp<- rbind(fcvcomp,fcv20)
rm(fcv20)

fcv21 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_10_31_3.csv", stringsAsFactors = F)
fcv21$fecha <- "2017_10"
fcvcomp<- rbind(fcvcomp,fcv21)
rm(fcv21)

fcv22 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_11_30_3.csv", stringsAsFactors = F)
fcv22$fecha <- "2017_11"
fcvcomp<- rbind(fcvcomp,fcv22)
rm(fcv22)

fcv23 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcv_2017_12_30_3.csv", stringsAsFactors = F)
fcv23$fecha <- "2017_12"
fcvcomp<- rbind(fcvcomp,fcv23)
rm(fcv23)



fcvtot <- ddply(fcvcomp,.(Territorio,Tipox,fecha),summarize,CT=sum(Cartera_total), CVen=sum(Cartera_ven), CVig=sum(Cartera_vig))
rm(fcvcomp)

fcvtot<- as.data.frame(fcvtot)
describe(fcvtot)
summary(fcvtot)
str(fcvtot)



write.csv(fcvtot, "C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot.csv")

fcvtot <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot.csv")
fcvtot <- fcvtot[,-1]
#drop tipox ''
fcvtot$Territorio <- as.factor(fcvtot$Territorio)
levels(fcvtot$Territorio)

fcvtot$Tipox <- as.factor(fcvtot$Tipox)
levels(fcvtot$Tipox)

fcvtot$fecha <- as.factor(fcvtot$fecha)
levels(fcvtot$fecha)
describe((fcvtot[which(fcvtot$Tipox ==''),]))

fcvtotN <- fcvtot[which(fcvtot$Tipox =='Nuevo'),]

fcvtotR <- fcvtot[which(fcvtot$Tipox == 'Recompra'),]
fcvtot2 <- rbind(fcvtotN, fcvtotR)
#subseting data
fcvtot <- fcvtot[which(fcvtot$Tipox =='Nuevo') & (fcvtot$Tipox == 'Recompra'),]
fcvtot2$imor <- fcvtot2$CVen/fcvtot2$CT

write.csv(fcvtot2, "C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot2.csv")
fcvtot2 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot2.csv")
str(fcvtot2)

xyplot(fcvtot2$imor ~ fcvtot2$Var_CT + fcvtot2$Var_CVen, type=c("p","smooth"))
plot(fcvtot2$Var_CVen,fcvtot2$imor)
plot(fcvtot2$Var_CT,fcvtot2$imor)
xyplot(fcvtot2$imor ~ fcvtot2$Var_CT, type=c("p","smooth"))
xyplot(fcvtot2$imor ~ fcvtot2$Var_CVen, type=c("p","smooth"))
fcvtot2$Var_CT_z <- scale(fcvtot2$Var_CT, scale = T, center = T) 
fcvtot2[which(fcvtot2$Var_CT_z >= abs(3)),]
hist(fcvtot2$Var_CT_z)

fcvtot2 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot2.csv")


fcvtot2$Var_CT_2<- poly(fcvtot2$Var_CT, 2)[,2]
fcvtot2$Var_CT_1<- poly(fcvtot2$Var_CT, 2)[,1]
str(fcvtot2)
rcorr(as.matrix(fcvtot2[,-1:-3]))

xyplot(fcvtot2$imor ~ fcvtot2$Var_CT_1, type=c("p","smooth"))
xyplot(fcvtot2$imor ~ fcvtot2$Var_CT_2, type=c("p","smooth"))
xyplot(imor ~ Var_CT_z | Territorio,
       data = fcvtot2, type = c("p","r"))


fcvtot3 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot3.csv")

fcvtot3$Var_CT_2<- poly(fcvtot3$Var_CT, 2)[,2]
fcvtot3$Var_CT_1<- poly(fcvtot3$Var_CT, 2)[,1]
write.csv(fcvtot3, "C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot3.csv")

#plots
xyplot(fcvtot3$imor ~ fcvtot3$Var_CT + fcvtot3$Var_CVen, type=c("p","smooth"))
plot(fcvtot3$Var_CVen,fcvtot3$imor)
plot(fcvtot3$Var_CT,fcvtot3$imor)
xyplot(fcvtot3$imor ~ fcvtot3$Var_CT, type=c("p","smooth"))
xyplot(fcvtot3$imor ~ fcvtot3$Var_CVen, type=c("p","smooth"))
####por territorio
xyplot(imor ~ Var_CT_z | Territorio,
       data = fcvtot3, type = c("p","r"))

xyplot(imor ~ Var_CT_1 | Territorio,
       data = fcvtot3, type = c("p","r"))

xyplot(imor ~ Var_CT_2 | Territorio,
       data = fcvtot3, type = c("p","r"))

xyplot(imor ~ Var_CVen | Territorio,
       data = fcvtot3, type = c("p","r"))



#####por tipo
xyplot(imor ~ Var_CT_1 | Tipox,
       data = fcvtot3, type = c("p","r"))

xyplot(imor ~ Var_CT_2 | Tipox,
       data = fcvtot3, type = c("p","r"))

xyplot(imor ~ Var_CVen | Tipox,
       data = fcvtot3, type = c("p","r"))

###por fecha

xyplot(imor ~ Var_CT_1 | fecha,
       data = fcvtot3, type = c("p","r"))
xyplot(imor ~ Var_CT_2 | fecha,
       data = fcvtot3, type = c("p","r"))
xyplot(imor ~ Var_CVen | fecha,
       data = fcvtot3, type = c("p","r"))


####modelo

maxmodel <- lmer(imor ~ 1 + Var_CT_1 + Var_CT_2 + Var_CVen + 
  Var_CT_1:Var_CVen + Var_CT_2:Var_CVen + 
  Var_CT_1:Territorio + Var_CT_2:Territorio + Var_CVen:Territorio + 
  Var_CT_1:Tipox + Var_CT_2:Tipox + Var_CVen:Tipox + 
  Var_CT_1:fecha + Var_CT_2:fecha + Var_CVen:fecha + 
  Territorio + Tipox + fecha + (1 + Var_CT_1 + Var_CT_2 + Var_CVen | Territorio) + 
  (1 + Var_CT_1 + Var_CT_2 + Var_CVen | Tipox) + (1 + Var_CT_1 + Var_CT_2 + Var_CVen | fecha),
  data = fcvtot3,
  control = lmerControl(optCtrl = list(maxfun = 1e+9, calc.derivs = FALSE)))


fcvtot3$Var_CVen_z <- scale(fcvtot3$Var_CVen, scale =T, center = T)

maxmodel2 <- lmer(imor ~ 0 + Var_CT_1 + Var_CT_2 + Var_CVen_z + 
                   Var_CT_1:Var_CVen_z + Var_CT_2:Var_CVen_z + 
                   Var_CT_1:Territorio + Var_CT_2:Territorio + Var_CVen_z:Territorio + 
                   Var_CT_1:Tipox + Var_CT_2:Tipox + Var_CVen_z:Tipox + 
                   Var_CT_1:fecha + Var_CT_2:fecha + Var_CVen_z:fecha + 
                   Territorio + Tipox + fecha + (0 + Var_CT_1 + Var_CT_2 + Var_CVen_z || Territorio) + 
                   (0 + Var_CT_1 + Var_CT_2 + Var_CVen_z | Tipox) +
                    (0 + Var_CT_1 + Var_CT_2 + Var_CVen_z || fecha),
                 data = fcvtot3,
                 control = lmerControl(optCtrl = list(maxfun = 1e+9, calc.derivs = FALSE)))


summary(maxmodel)    


## 2. check singularity
isSingular(maxmodel2)

## 3. recompute gradient and Hessian with Richardson extrapolation
devfun <- update(maxmodel2, devFunOnly=TRUE)
if (isLMM(maxmodel2)) {
  pars <- getME(maxmodel2,"theta")
} else {
  ## GLMM: requires both random and fixed parameters
  pars <- getME(maxmodel2, c("theta","fixef"))
}
if (require("numDeriv")) {
  cat("hess:\n"); print(hess <- hessian(devfun, unlist(pars)))
  cat("grad:\n"); print(grad <- grad(devfun, unlist(pars)))
  cat("scaled gradient:\n")
  print(scgrad <- solve(chol(hess), grad))
}
## compare with internal calculations:
maxmodel2@optinfo$derivs

summary(maxmodel2)

#trying other optimizers in case of convergence problems
maxmodel.Nltrp <- update(maxmodel2, control=lmerControl(optimizer="nloptwrap", optCtrl = list(maxfun = 10e+9)))
maxmodel.NM <- update(maxmodel2, control=lmerControl(optimizer="Nelder_Mead"))
maxmodel.nlimb <- update(maxmodel2, control=lmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
maxmodel.LBFGSB <- update(maxmodel2,control=lmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))


#Model diagnostics:
#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(maxmodel2, scaled = TRUE))
dplot
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(maxmodel2, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(maxmodel2, scaled = TRUE)) > 3)/ length(resid(maxmodel2))*100
sum(abs(resid(maxmodel2, scaled = TRUE)) > 2.5)/ length(resid(maxmodel2))*100
sum(abs(resid(maxmodel2, scaled = TRUE)) > 2)/ length(resid(maxmodel2))*100
#A boxplot showing for each participant the distribution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(maxmodel2, Territorio ~ resid(.))
plot(maxmodel2, fecha ~ resid(.))
plot(maxmodel2, Tipox ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(maxmodel2, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values
fitobs <- car::scatterplot(fitted(maxmodel2) ~ fcvtot3$imor, boxplots =FALSE, smoother = FALSE, xlim = c(0, .16), ylim = c(0, .16))
summary(maxmodel2)
#DFBETAS n COOKS not necessary... takes long time
influ <- influence(maxmodel2, "Territorio")
influ <- influence(maxmodel2, "fecha")
influ <- influence(maxmodel2, "Tipox")
cooks.distance(influ)
dfbetas(influ)
plot(influ, which = 'cook')
plot(influ, which = 'dfbetas')


###############nuevos###############
#generar modelo para nuevos 
fcvtotN$imor <- fcvtotN$CVen/fcvtotN$CT
write.csv(fcvtotN, "C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtotN.csv")
fcvtotN<-read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtotN.csv")
str(fcvtotN)
fcvtotN$Var_CT_1 <- poly(fcvtotN$Var_CT, 2)[,1]
fcvtotN$Var_CT_2 <- poly(fcvtotN$Var_CT, 2)[,2]
fcvtotN$Var_CVen_z <- scale(fcvtotN$Var_Cven, scale = T, center = T)

maxmodel3 <- lmer(imor ~ 1 + Var_CT_1*Var_CVen_z+ 
                     (1 + Var_CT_1*Var_CVen_z | Territorio) 
                    ,
                  data = fcvtotN,
                  control = lmerControl(optCtrl = list(maxfun = 1e+9, calc.derivs = FALSE)))


summary(maxmodel3)


#Model diagnostics:
#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(maxmodel3, scaled = TRUE))
dplot
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(maxmodel3, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(maxmodel3, scaled = TRUE)) > 3)/ length(resid(maxmodel3))*100
sum(abs(resid(maxmodel3, scaled = TRUE)) > 2.5)/ length(resid(maxmodel3))*100
sum(abs(resid(maxmodel3, scaled = TRUE)) > 2)/ length(resid(maxmodel3))*100
#A boxplot showing for each participant the distribution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(maxmodel3, Territorio ~ resid(.))
plot(maxmodel3, Var_CT_2 ~ resid(.))
plot(maxmodel3, Var_CT_1 ~ resid(.))
plot(maxmodel3, Var_CVen_z ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(maxmodel3, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values
fitobs <- car::scatterplot(fitted(maxmodel3) ~ fcvtotN$imor, boxplots =FALSE, smoother = FALSE, xlim = c(0, .16), ylim = c(0, .16))
summary(maxmodel3)





##################recompra######################
#generar modelo para RECOMPRA 
fcvtotR$imor <- fcvtotR$CVen/fcvtotR$CT
write.csv(fcvtotR, "C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtotR.csv")
fcvtotR<-read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtotR.csv")
str(fcvtotR)
fcvtotR$Var_CT_1 <- poly(fcvtotR$Var_CT, 2)[,1]
fcvtotR$Var_CT_2 <- poly(fcvtotR$Var_CT, 2)[,2]
fcvtotR$Var_CVen_z <- scale(fcvtotR$Var_Cven, scale = T, center = T)

maxmodel4 <- lmer(imor ~ 1 + Var_CT_1*Var_CVen_z+ 
                    (1 + Var_CT_1*Var_CVen_z | Territorio) 
                  ,
                  data = fcvtotR,
                  control = lmerControl(optCtrl = list(maxfun = 1e+9, calc.derivs = FALSE)))


summary(maxmodel4)
summary(maxmodel3)

#Model diagnostics:
#A densityplot of the scaled model residuals
dplot <- lattice::densityplot(resid(maxmodel4, scaled = TRUE))
dplot
#A q-q plot of the scaled residuals; use qqPlot() from the package car
qqPlot(resid(maxmodel4, scaled = TRUE))
#Compute the proportion of residuals larger than +/- 2, 2.5, 3. Are any of these numbers problematic?
#larger than |± 2| (should be around 5%)larger than |± 2.5| (should be around 1%) Every case with a residual > |± 3| could be anutlier
sum(abs(resid(maxmodel4, scaled = TRUE)) > 3)/ length(resid(maxmodel4))*100
sum(abs(resid(maxmodel4, scaled = TRUE)) > 2.5)/ length(resid(maxmodel4))*100
sum(abs(resid(maxmodel4, scaled = TRUE)) > 2)/ length(resid(maxmodel4))*100
#A boxplot showing for each participant the distribution of the scaled residuals. Are there any participants with problematic data points? If so, what are these participants' participant IDs?
plot(maxmodel4, Territorio ~ resid(.))
plot(maxmodel4, Var_CT_2 ~ resid(.))
plot(maxmodel4, Var_CT_1 ~ resid(.))
plot(maxmodel4, Var_CVen_z ~ resid(.))
#A scatterplot showing the fitted vs. the residuals to check for homo/heteroskedasticity; add a smoothed line.
plot(maxmodel4, type = c('p', 'smooth'))	
#A scatter plot showing the observed vs. the fitted values
fitobs <- car::scatterplot(fitted(maxmodel4) ~ fcvtotR$imor, boxplots =FALSE, smoother = FALSE, xlim = c(0, .05), ylim = c(0, .05))
summary(maxmodel4)



#plotsNuevos
xyplot(fcvtotN$imor ~ fcvtotN$Var_CT + fcvtotN$Var_Cven, type=c("p","smooth"))
xyplot(fcvtotN$imor ~ fcvtotN$Var_CT + fcvtotN$Var_Cven, type=c("p","r"))
xyplot(fcvtotN$imor ~ fcvtotN$CT + fcvtotN$CVen, type=c("p","r"))
plot(fcvtotN$Var_CVen,fcvtotN$imor)
plot(fcvtotN$Var_CT,fcvtotN$imor)
xyplot(fcvtotN$imor ~ fcvtotN$Var_CT_1, type=c("p","r"))
xyplot(fcvtotN$imor ~ fcvtotN$CT, type=c("p","r"))
xyplot(fcvtotN$imor ~ fcvtotN$Var_Cven, type=c("p","smooth"))
xyplot(fcvtotN$imor ~ fcvtotN$Var_Cven, type=c("p","r"))
####por territorio

xyplot(imor ~ Var_CT_1 | Territorio,
       data = fcvtotN, type = c("p","r"))

xyplot(imor ~ Var_Cven | Territorio,
       data = fcvtotN, type = c("p","r"))




#plotsRECOMPRA
xyplot(fcvtotR$imor ~ fcvtotR$Var_CT + fcvtotR$Var_Cven, type=c("p","smooth"))
xyplot(fcvtotR$imor ~ fcvtotR$Var_CT + fcvtotR$Var_Cven, type=c("p","r"))
xyplot(fcvtotR$imor ~ fcvtotR$CT + fcvtotR$CVen, type=c("p","r"))
plot(fcvtotR$Var_Cven,fcvtotR$imor)
plot(fcvtotR$Var_CT,fcvtotR$imor)
xyplot(fcvtotR$imor ~ fcvtotR$Var_CT_1, type=c("p","r"))
xyplot(fcvtotR$imor ~ fcvtotR$Var_CVen, type=c("p","smooth"))
xyplot(fcvtotR$imor ~ fcvtotR$Var_CVen, type=c("p","r"))
####por territorio

xyplot(imor ~ Var_CT_1 | Territorio,
       data = fcvtotR, type = c("p","r"))

xyplot(imor ~ Var_Cven | Territorio,
       data = fcvtotR, type = c("p","r"))


##########por territorios############

#########Hidalgo###########
#nuevos
fcvtotN_Hidalgo <- fcvtotN[which(fcvtotN$Territorio =='HIDALGO'),]

hidalgo<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotN_Hidalgo)
summary(hidalgo)
#recompra
fcvtotR_Hidalgo <- fcvtotR[which(fcvtotR$Territorio =='HIDALGO'),]

hidalgoR<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotR_Hidalgo)
summary(hidalgoR)



#########Norte###########
#nuevos
fcvtotN_Norte <- fcvtotN[which(fcvtotN$Territorio =='NORTE'),]

norte<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotN_Norte)
summary(norte)
#recompra
fcvtotR_Norte <- fcvtotR[which(fcvtotR$Territorio =='NORTE'),]

norteR<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotR_Norte)
summary(norteR)



#########Puebla###########
#nuevos
fcvtotN_puebla <- fcvtotN[which(fcvtotN$Territorio =='PUEBLA'),]

puebla<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotN_puebla)
summary(puebla)
#recompra
fcvtotR_puebla <- fcvtotR[which(fcvtotR$Territorio =='PUEBLA'),]

pueblaR<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotR_puebla)
summary(pueblaR)



#########Sur###########
#nuevos
fcvtotN_SUR <- fcvtotN[which(fcvtotN$Territorio =='SUR'),]

SUR<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotN_SUR)
summary(SUR)
#recompra
fcvtotR_SUR <- fcvtotR[which(fcvtotR$Territorio =='SUR'),]

SURR<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotR_SUR)
summary(SURR)



#########Poniente###########
#nuevos
fcvtotN_PONIENTE <- fcvtotN[which(fcvtotN$Territorio =='PONIENTE'),]

PONIENTE<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotN_PONIENTE)
summary(PONIENTE)
#recompra
fcvtotR_PONIENTE <- fcvtotR[which(fcvtotR$Territorio =='PONIENTE'),]

PONIENTER<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotR_PONIENTE)
summary(PONIENTER)



#########Oriente###########
#nuevos
fcvtotN_ORIENTE <- fcvtotN[which(fcvtotN$Territorio =='ORIENTE'),]

ORIENTE<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotN_ORIENTE)
summary(ORIENTE)
#recompra
fcvtotR_ORIENTE <- fcvtotR[which(fcvtotR$Territorio =='ORIENTE'),]

ORIENTER<- lm(imor ~ 1 + Var_CT_1*Var_CVen_z, data = fcvtotR_ORIENTE)
summary(ORIENTER)
