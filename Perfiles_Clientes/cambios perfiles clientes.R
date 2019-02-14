library(psych)
library(dplyr)
library(randomForest)
library(caTools)

fcv <- read.csv("D:/Documentos_Andre/genero/fcv3.csv", stringsAsFactors = F)
summary(fcv)
str(fcv)
options(scipen = 10)
fcv$montosol <- gsub(",","",fcv$montosol)
fcv$DeudTot <- gsub(",","",fcv$DeudTot)
fcv$MtoVen <- gsub(",","",fcv$MtoVen)
fcv$ValIni <- gsub(",","",fcv$ValIni)
fcv$CapVig <- gsub(",","",fcv$CapVig)
fcv$IntVig <- gsub(",","",fcv$IntVig)



fcv$Giro <- as.factor(fcv$Giro)
fcv$Edo_Civil <- as.factor(fcv$Edo_Civil)
fcv$Genero <- as.factor(fcv$Genero)
fcv$Prod <- as.factor(fcv$Prod)
fcv$Oficina_Asesor <- as.factor(fcv$Oficina_Asesor)
levels(fcv$Oficina_Asesor)
fcv$Territorio <- as.factor(fcv$Territorio)
levels(fcv$Territorio)
fcv$Calific_Act_Socio <- as.factor(fcv$Calific_Act_Socio)
levels(fcv$Giro)
fcv$MtoVen <- as.numeric(fcv$MtoVen)
fcv$montosol <- as.numeric(fcv$montosol)
fcv$DeudTot <- as.numeric(fcv$DeudTot)
fcv$ValIni <- as.numeric(fcv$ValIni)
fcv$CapVig <- as.numeric(fcv$CapVig)
fcv$IntVig <- as.numeric(fcv$IntVig)
# 78 92 177 264 558 626
levels(fcv$Giro)[1:77] <- "Otros"
levels(fcv$Giro)[3:15] <- "Otros"
levels(fcv$Giro)[4:87] <- "Otros"
levels(fcv$Giro)[5:90] <- "Otros"
levels(fcv$Giro)[6:298] <- "Otros"
levels(fcv$Giro)[7:73] <- "Otros"
levels(fcv$Giro)[8:31] <- "Otros"


levels(fcv$Giro)
levels(fcv$Prod)
str(fcv)
describe(fcv$Edad)
describeBy(fcv$Edad, group= fcv$Giro)
describeBy(fcv$MtoVen, group= fcv$Giro)

describeBy(fcv$Edad, group= fcv$Genero)
describeBy(fcv$MtoVen, group= fcv$Genero)




toBeRemoved<-which(fcv$Calific_Act_Socio=="SIN CALIFICACION")
fcv<-fcv[-toBeRemoved,]

levels(fcv$Calific_Act_Socio)
fcv$Calific_Act_Socio = factor(fcv$Calific_Act_Socio,levels(fcv$Calific_Act_Socio)[c(2,1,4,3,5,6)])

fcv %>%
  group_by(Genero, Calific_Act_Socio) %>%
  summarise(rom_MtoVen=mean(MtoVen),
            Prom_Edad=(mean(Edad)))

sample = sample.split(fcv$MtoVen, SplitRatio = .70)
fcvP <- subset(fcv, sample == TRUE)
fcvT <- subset(fcv, sample == FALSE)

rf_gen=randomForest(MtoVen ~ Edad*Genero*Edo_Civil*Giro*Frec*Prod*
                      ValIni*montosol*Tasa_Final 
                    *No_Cuotas*Score_Consolidado*Territorio
                    
                    , data = fcvT, 
                    na.action = na.omit, 
                    ntree = 500,
                    mtry = 3,
                    nodesize = 7,
                    importance = T
)
rf_gen

plot(rf_gen)

# Predicting on train set
predTrain <- predict(rf_gen, fcvT, type = "response")
# Checking classification accuracy
table(predTrain, fcvT$MtoVen)
x = mean(predTrain)
y = mean(fcvT $MtoVen)
y/x

# Predicting on Validation set
predValid <- predict(rf_gen, fcvP, type = "response")
# Checking classification accuracy
#table(predValid, fcvP$MtoVen)
x1 = mean(predValid)
y1 = mean(fcvP$MtoVen)
y1/x1

# To check important variables
importance(rf_gen)        
varImpPlot(rf_gen)
summary(fcv$Prod)
levels(fcv$Prod)
levels(fcv$Giro)
levels(fcv$Territorio)
fcv$Edad1 <- poly(fcv$Edad, 2)[,1]
fcv$Edad2 <- poly(fcv$Edad, 2)[,2]
plot(fcv$Edad,fcv$MtoVen)
plot(fcv$Edad1,fcv$MtoVen)
plot(fcv$Edad2,fcv$MtoVen)

regx<- lm(MtoVen ~ Tasa_Final + ValIni + montosol + Prod +
            Genero*Territorio + Edad1 + Edad2 + No_Cuotas + Edo_Civil + Frec + Giro , data = fcv)
summary(regx)

describeBy(fcv$MtoVen, group = fcv$Prod)
describeBy(fcv$ValIni, group = fcv$Prod)
#comprado con 3015
#4011
#4012
#4021,4032, 4221, 4222, 4321, 5010
#GeneroM
#Edad1
#Edad2
#nocutoas
#Edo civil
#Frec
#comparado con otros Otros
# todo COMPRA VENTA DE ARTICULOS NO CLASIFICADOS EN OTRA PARTE

