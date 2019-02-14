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
#####generar base#####
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



fcvtot <- ddply(fcvcomp,.(Oficina_Asesor,Tipox,fecha, Territorio),summarize,CT=sum(Cartera_total), CVen=sum(Cartera_ven), CVig=sum(Cartera_vig))
rm(fcvcomp)

fcvtot<- as.data.frame(fcvtot)
describe(fcvtot)
summary(fcvtot)
str(fcvtot)



write.csv(fcvtot, "C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot_suc.csv")
########cargar la base####
fcvtot <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot_suc.csv")
str(fcvtot)
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
#fcvtot <- fcvtot[which(fcvtot$Tipox =='Nuevo') & (fcvtot$Tipox == 'Recompra'),]
fcvtot2$imor <- fcvtot2$CVen/fcvtot2$CT

#write.csv(fcvtot2, "C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot2_suc.csv")
fcvtot2 <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtot2.csv")
str(fcvtot2)

#####el problema esta en NORTE Y Poniente de recompraspor lo que:

str(fcvtotR)
summary(fcvtotR)
table(fcvtotR$Territorio,fcvtotR$Oficina_Asesor)
fcvtotR_Norte <- fcvtotR[which(fcvtotR$Territorio =='NORTE'),]
fcvtotR_PONIENTE <- fcvtotR[which(fcvtotR$Territorio =='PONIENTE'),]
summary(fcvtotR_Norte)
summary(fcvtotR_PONIENTE)
table(fcvtotR_Norte$Territorio,fcvtotR_Norte$Oficina_Asesor)
#calcular variaciones, normalizar e IMOR 

fcvtotR_Norte$imor <- fcvtotR_Norte$CVen/fcvtotR_Norte$CT
fcvtotR_PONIENTE$imor <- fcvtotR_PONIENTE$CVen/fcvtotR_PONIENTE$CT
write.csv(fcvtotR_Norte, "C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtotR_Norte_suc.csv")
write.csv(fcvtotR_PONIENTE, "C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtotR_Poniente_suc.csv")


#######INICIAR DESDE AQUI#####
#variaciones a mano en excel
fcvtotR_Norte <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtotR_Norte_suc.csv")

str(fcvtotR_Norte$Oficina_Asesor)
str(fcvtotR_Norte)
fcvtotR_Norte$Var_CT_z <- scale(fcvtotR_Norte$Var_CT, center = T, scale=T)
fcvtotR_Norte$Var_CVen_z <- scale(fcvtotR_Norte$Var_Cven, center = T, scale=T)
fcvtotR_Norte$sucursalf <- as.factor(fcvtotR_Norte$Oficina_Asesor)
levels(fcvtotR_Norte$sucursalf)

#con este modelo verificamos que sucursales tienen peligro
maxmodelsuc <- lmer(imor ~ 1 + Var_CT_z*Var_CVen_z*sucursalf +
                    (0 + Var_CT_z +Var_CVen_z | sucursalf) 
                  ,
                  data = fcvtotR_Norte,
                  control = lmerControl(optCtrl = list(maxfun = 1e+9, calc.derivs = FALSE)))


summary(maxmodelsuc)


#sucursales
#81, 83, 84, 85, 87, 121, 131
fcvsuc81 <- fcvtotR_Norte[which(fcvtotR_Norte$sucursalf =='81'),]
fcvsuc81_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc81)
summary(fcvsuc81_reg)

fcvsuc83 <- fcvtotR_Norte[which(fcvtotR_Norte$sucursalf =='83'),]
fcvsuc83_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc83)
summary(fcvsuc83_reg)

fcvsuc84 <- fcvtotR_Norte[which(fcvtotR_Norte$sucursalf =='84'),]
fcvsuc84_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc84)
summary(fcvsuc84_reg)

fcvsuc85 <- fcvtotR_Norte[which(fcvtotR_Norte$sucursalf =='85'),]
fcvsuc85_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc85)
summary(fcvsuc85_reg)

fcvsuc87 <- fcvtotR_Norte[which(fcvtotR_Norte$sucursalf =='87'),]
fcvsuc87_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc87)
summary(fcvsuc87_reg)

fcvsuc121 <- fcvtotR_Norte[which(fcvtotR_Norte$sucursalf =='121'),]
fcvsuc121_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc121)
summary(fcvsuc121_reg)

fcvsuc131 <- fcvtotR_Norte[which(fcvtotR_Norte$sucursalf =='131'),]
fcvsuc131_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc131)
summary(fcvsuc131_reg)



#####NOTAS: CREAR MODELOS PARA ESTAS SUCS. CREAR MODELOS PONIENTE

#######PONIENTE
#recompra

fcvtotR_PONIENTE <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\BasesTerr\\fcvtotR_Poniente_suc.csv")

str(fcvtotR_PONIENTE$Oficina_Asesor)
str(fcvtotR_PONIENTE)
fcvtotR_PONIENTE$Var_CT_z <- scale(fcvtotR_PONIENTE$Var_CT, center = T, scale=T)
fcvtotR_PONIENTE$Var_CVen_z <- scale(fcvtotR_PONIENTE$Var_Cven, center = T, scale=T)
fcvtotR_PONIENTE$sucursalf <- as.factor(fcvtotR_PONIENTE$Oficina_Asesor)
levels(fcvtotR_PONIENTE$sucursalf)

maxmodelsuc <- lmer(imor ~ 1 + Var_CT_z*Var_CVen_z*sucursalf +
                      (0 + Var_CT_z + Var_CVen_z | sucursalf) 
                    ,
                    data = fcvtotR_PONIENTE,
                    control = lmerControl(optCtrl = list(maxfun = 1e+9, calc.derivs = FALSE)))


summary(maxmodelsuc)

#sucursales 
# 312, 313, 321, 331, 332, 336, 341, 342, 343, 344, 345, 350, 352
#313, 321

fcvsuc312 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='312'),]
fcvsuc312_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc312)
summary(fcvsuc312_reg)

fcvsuc313 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='313'),]
fcvsuc313_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc313)
summary(fcvsuc313_reg)


fcvsuc321 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='321'),]
fcvsuc321_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc321)
summary(fcvsuc321_reg)


fcvsuc331 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='331'),]
fcvsuc331_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc331)
summary(fcvsuc331_reg)

fcvsuc332 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='332'),]
fcvsuc332_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc332)
summary(fcvsuc332_reg)

fcvsuc336 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='336'),]
fcvsuc336_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc336)
summary(fcvsuc336_reg)

fcvsuc341 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='341'),]
fcvsuc341_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc341)
summary(fcvsuc341_reg)

fcvsuc342 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='342'),]
fcvsuc342_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc342)
summary(fcvsuc342_reg)

fcvsuc343 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='343'),]
fcvsuc343_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc343)
summary(fcvsuc343_reg)

fcvsuc344 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='344'),]
fcvsuc344_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc344)
summary(fcvsuc344_reg)

fcvsuc345 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='345'),]
fcvsuc345_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc345)
summary(fcvsuc345_reg)

fcvsuc350 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='350'),]
fcvsuc350_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc350)
summary(fcvsuc350_reg)

fcvsuc352 <- fcvtotR_PONIENTE[which(fcvtotR_PONIENTE$sucursalf =='352'),]
fcvsuc352_reg <- lm(imor ~ 1 + Var_CT_z*Var_CVen_z, data = fcvsuc352)
summary(fcvsuc352_reg)
