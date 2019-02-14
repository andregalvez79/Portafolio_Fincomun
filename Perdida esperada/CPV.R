
library(dplyr)
library(randomForest)
library(eeptools)
library(caTools)
library(randomForestExplainer)
#Score de Pérdida Esperada TOTAL y sin gobierno##
#Insumos#
########## limpiar bases y cruzarlas ############
MAXATR <- read.csv("D:/Documentos_Andre/Perdida_esperada/PE_Ivan/MAXATR_dic.csv")
MAXATR2 <- MAXATR %>% select("Nro_Prest","Oct.Dic_18")  #cambiar los meses
MAXATR2 <- unique(MAXATR2, Nro_Prest = TRUE )
MAXATR2$Nro_Prest <- as.character(MAXATR2$Nro_Prest)

cnbv <- read.csv("D:/Documentos_Andre/Perdida_esperada/PE_Ivan/12CNBV_dic.csv",
                 stringsAsFactors = F)

fcv <- read.delim("D:/Documentos_Andre/Perdida_esperada/PE_Ivan/FCV_1812_dic.txt",
                  stringsAsFactors = F)
fcv$NoPBKS <- as.numeric(fcv$NoPBKS)
fcv <- fcv %>% mutate(Credito2 = paste0("#",NoPBKS))
fcv2 <- fcv  %>%  select("Credito2","Cuota","F_Nacim")
Ciclos <- read.csv("D:/Documentos_Andre/Perdida_esperada/PE_Ivan/1812_ciclos.csv")
Ciclos <- Ciclos %>% select("Credito2","Reg")
cnbv <- cnbv  %>%  left_join(fcv2, by = c("nro_prest"="Credito2")) 
cnbv <- cnbv  %>%  left_join(MAXATR2, by = c("nro_prest"="Nro_Prest"))
cnbv <- cnbv  %>%  left_join(Ciclos, by = c("socio"="Credito2"))

cnbv$Cuota <- sub(",","",cnbv$Cuota)
cnbv$Cuota <- as.numeric(cnbv$Cuota)
cnbv$Oct.Dic_18 <- ifelse(cnbv$Oct.Dic_18 =="NA",0,cnbv$Oct.Dic_18) #cambiar los meses

#cruzar con archivo maestro (jorge) para sacar frecuencias para nómina y gorbierno
mast <- read.csv("D:/Documentos_Andre/Perdida_esperada/PE_Ivan/Arch_mast_2019_01_31.csv")
mast <- mast %>% mutate(CREDITO_FLEX = paste0("#", CUENTA))
#cnbv2 <- merge(cnbv, mast, by="CREDITO_FLEX")
#cnbv2 <-unique(left_join(cnbv, mast, by = "CREDITO_FLEX"))

#CUENTA del mast
#CREDITO_FLEX cnbv

write.csv(cnbv, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvadjunto.csv")
write.csv(mast, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/mast.csv")

#para frecuencias en gob y nomina pedirle a jorge esa base
cnbv<- read.csv("D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvadjunto.csv")
str(cnbv$Frec)
cnbv$Frec <- as.numeric(cnbv$Frec)
cnbv$Frec2 <- ifelse(cnbv$Frec == "NA", cnbv$Frec <-  cnbv$Frecuencia, cnbv$Frec )
names (cnbv)[22] = "Provision1"
cnbv <- cnbv %>% 
  mutate(cartera = if_else(Provision1 <1, "cartera", "no cartera"),
         Exigible = round(((43465 - fecha_apertu)/(Frec2)), digits = 0),#cambiar la fecha
         Pagos = Exigible - cuotas_venc,
         Exigible2 = Exigible*Cuota,
         Pagos2 = Pagos*Cuota,
         P_Pago = Pagos2/Exigible2,
         Tprod = if_else(origen =="Nuevo", 1,0))

#se imputa a mano que el P_Pago es = 1
#esto es para nómina y gobierno
cnbv$P_Pago <- ifelse(cnbv$P_Pago =="NaN",1,cnbv$P_Pago)

cnbvcartera <- cnbv %>% filter(cartera =="cartera")


creditos <- cnbvcartera %>%
  group_by(socio) %>%
  summarise(cred = length(nro_prest))

cnbvcartera <- cnbvcartera %>% left_join(creditos, by ="socio")
cnbvcartera$F_Nacim <- as.Date(cnbvcartera$F_Nacim, format = "%d/%m/%Y") 

cnbvcartera <- cnbvcartera %>% 
  mutate(ciclo = Reg-cred)

cnbvcartera <- cnbvcartera %>%          
  mutate( cicloe = case_when(.$ciclo < 1  ~ 0,
                             .$ciclo < 3  ~ 1,
                             .$ciclo < 5  ~ 2,
                             .$ciclo < 10  ~ 3,
                             .$ciclo < 15  ~ 4,
                             TRUE ~ 5),
          Corte = "2018-12-31")#cambiar fecha

cnbvcartera$Corte <- as.Date(cnbvcartera$Corte)

cnbvcartera <- cnbvcartera %>%          
  mutate( Edad = round((Corte - F_Nacim)/365),
          Severidad = case_when(.$Dias_de_atraso < 90   ~ 0.72,
                                .$Dias_de_atraso < 120  ~ 0.76,
                                .$Dias_de_atraso < 150  ~ 0.82,
                                .$Dias_de_atraso < 180  ~ 0.89,
                                .$Dias_de_atraso < 210  ~ 0.93,
                                .$Dias_de_atraso < 240  ~ 0.97,
                                TRUE ~ 1))

cnbvcartera$Edad <- as.numeric(cnbvcartera$Edad)
cnbvcartera$Oct.Dic_18[is.na(cnbvcartera$Oct.Dic_18)] <- 0 #caMBIAR meses


summary(cnbvcartera)
cnbvcartera$sexof <- as.factor(cnbvcartera$sexo)
levels(cnbvcartera$sexof)


#######separar cartera########

#SIN GOB
cnbvnogob <- cnbvcartera %>% filter(Etiqueta_1_3 !="Consumo gobierno")
#1SOLO GOB
cnbvgob <- cnbvcartera[cnbvcartera$Etiqueta_1_3 == "Consumo gobierno",]
write.csv(cnbvgob, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvgob.csv")
#SIN GOB NI NOMINA
cnbvnogobnonom <- cnbvcartera%>% filter(Convenios !="OTROS" & Convenios !="BIMBO")
#2SOLO NOMINA
cnbvnom1 <- cnbvnogob%>% filter(Convenios =="OTROS")
cnbvnom2 <- cnbvnogob%>% filter(Convenios =="BIMBO")
cnbvnom<- rbind(cnbvnom2, cnbvnom1)
write.csv(cnbvnom, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvnom.csv")
#3REDES
cnbvredes <- cnbvnogobnonom%>% filter(sub_aplic != 2 & sub_aplic != 4350)
#sexo cambiar a mano
write.csv(cnbvredes, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvredes.csv")
cnbvredes <- read.csv("D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvredes.csv")
#4NUEVOS
cnbvredesN <- cnbvredes%>% filter(origen == "Nuevo")
#5RECOMPRAS
cnbvredesR <- cnbvredes%>% filter(origen != "Nuevo")

#write.csv(cnbvcartera, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvcartera.csv" )
#cnbvcartera2 <- read.csv("D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvcartera_2.csv")
sum(cnbvredes$MtoVig2)
##########(POPULAR) REDES SUCURSALES TODO##########
#3REDES 
cnbvredes$sexof <- as.factor(cnbvredes$sexo)
levels(cnbvredes$sexof)
#quitar na's de fechas de nacimiento.
cnbvredes<- cnbvredes[complete.cases(cnbvredes$F_Nacim), ]
cnbvredes<- cnbvredes[which(cnbvredes$F_Nacim != "01/01/1900"), ]
cnbvredes<- cnbvredes[which(cnbvredes$F_Nacim != ""), ]

cnbvredes$Default <- ifelse(cnbvredes$Dias_de_atraso >=90, 0, 1) #mayor a 45?
cnbvredes$Defaultf <- as.factor(cnbvredes$Default)
levels(cnbvredes$Defaultf)
cnbvredes$F_Nacim2 <- as.Date(cnbvredes$F_Nacim, "%d/%m/%Y")
cnbvredes$Edad <- floor(age_calc(cnbvredes$F_Nacim2, units = "years"))
cnbvredes$Edad <- as.numeric(cnbvredes$Edad)
cnbvredes$Oct.Dic_18 <- as.numeric(cnbvredes$Oct.Dic_18)
cnbvredes$cuotas_venc <- as.numeric(cnbvredes$cuotas_venc)
cnbvredes$No_de_Cuotas <- as.numeric(cnbvredes$No_de_Cuotas)
str(cnbvredes)

sum(cnbvredes$MtoVig2)
describe(cnbvredes$MtoVig2)

sample = sample.split(cnbvredes$Defaultf, SplitRatio = .70)
cnbvredesP <- subset(cnbvredes, sample == TRUE)
cnbvredesT <- subset(cnbvredes, sample == FALSE)

rf_redes=randomForest(Defaultf ~ Edad + P_Pago + cicloe + Territorio + monto_inicial +
                        sexof + No_de_Cuotas + Oct.Dic_18 + Unificacion + cuotas_venc +
                        tasa + Frec + Pago_sostenido
                      
                      , data = cnbvredesT, 
                      na.action = na.omit, 
                      ntree = 500,
                      mtry = 2,
                      nodesize = 2,
                      importance = T,
                      keep.forest = T
)
rf_redes

plot(rf_redes)

#distribution of minimal depht
min_depth_frame <- min_depth_distribution(rf_redes)
save(min_depth_frame, file = "min_depth_frame.rda")
load("min_depth_frame.rda")
head(min_depth_frame, n = 10)

# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
#plot of the distribution of minimal depth for 
#top ten variables according to mean minimal depth calculated using top trees 
plot_min_depth_distribution(min_depth_frame)

plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)
options(scipen = 1000)
importance_frame <- measure_importance(rf_redes)
save(importance_frame, file = "importance_frame.rda")
load("importance_frame.rda")
importance_frame

#This test tells us whether the observed number of successes (number of nodes in which Xj 
#was used for splitting)
#exceeds the theoretical number of successes if they were random 

# plot_multi_way_importance(forest, size_measure = "no_of_nodes") 
# gives the same result as below but takes longer
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

#esta no tiene mse increase por que es binomial
#plot_multi_way_importance(importance_frame, x_measure = "mse_increase",
#                          y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)

plot_importance_ggpairs(importance_frame)
plot_importance_rankings(importance_frame)

#investigate interactions among best predictors
(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))

#interactions_frame <- min_depth_interactions(rf_redes, vars)
#save(interactions_frame, file = "interactions_frame.rda")
#load("interactions_frame.rda")
#head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

#interactions_frame <- min_depth_interactions(forest, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
#save(interactions_frame, file = "interactions_frame_relevant.rda")
#load("interactions_frame_relevant.rda")
#plot_min_depth_interactions(interactions_frame)

#todo el reporte (tarda)
explain_forest(rf_redes, interactions = TRUE, data = cnbvredes)

# Predicting on train set
predTrain <- predict(rf_redes, cnbvredesT, type = "response")
# Checking classification accuracy
table(predTrain, cnbvredesT$Defaultf)
x = mean(predTrain)
y = mean(cnbvredesT$Defaultf)
y/x

# Predicting on Validation set
predValid <- predict(rf_redes, cnbvredesP, type = "response")
# Checking classification accuracy
#table(predValid, fcvP$MtoVen)
table(predValid, cnbvredesP$Defaultf)
x1 = mean(predValid)
y1 = mean(cnbvredesP$Defaultf)
y1/x1

# To check important variables
importance(rf_redes)        
varImpPlot(rf_redes)


x=cnbvredes[c(68, 62, 66, 71, 7, 70, 29, 55, 45, 8, 56, 15, 17,12)]

#y=out.data[, response.col]
#randomForest(x,y,xtest=x,ytest=y)
#model=randomForest(x,y,xtest=x,ytest=y,keep.forest=TRUE). 
prob=predict(rf_redes,x,type="prob")
summary(prob)
prob <-  data.frame(prob)
prob2 = round(prob*100, digits=0)
prob2 <-data.frame(prob2)

cnbvredes$ProbDef <-  prob$X0
cnbvredes$ProbPago <-  prob$X1
cnbvredes$ScoreDef <-  prob2$X0
cnbvredes$ScorePago <-  prob2$X1

socrex <- cnbvredes %>%          
  mutate(Banda = case_when(.$ProbDef < 0.10  ~ "A",
                           .$ProbDef < 0.20  ~ "B",
                           .$ProbDef < 0.35  ~ "C",
                           .$ProbDef < 0.50  ~ "D",
                           .$ProbDef < 0.70  ~ "E",
                           .$ProbDef < 0.90  ~ "F",
                           TRUE ~ "G"))

socrex$Banda <- as.character(socrex$Banda)
socrex <- socrex %>%          
  mutate(Perdida = (MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2)*ProbDef*Severidad)


socrex2 <- socrex %>%  
  group_by(Banda)  %>%  
  summarise(Creditos = length(nro_prest),
            capital = sum(MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2),
            PE = sum(Perdida),
            Pe2 = mean(ProbDef),
            sdPE = sd(ProbDef),
            Comp = sum(Total_Provision))

write.csv(socrex2, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/RF_Predict_RedesTodos1.csv" )
write.csv(socrex, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/Base_RedesT1.csv" )

########### SOLO NUEVOS ############
#4NUEVOS

cnbvredesN$sexof <- as.factor(cnbvredesN$sexo)
levels(cnbvredesN$sexof)
#quitar na's de fechas de nacimiento.
cnbvredesN<- cnbvredesN[complete.cases(cnbvredesN$F_Nacim), ]
cnbvredesN<- cnbvredesN[which(cnbvredesN$F_Nacim != "01/01/1900"), ]
cnbvredesN<- cnbvredesN[which(cnbvredesN$F_Nacim != ""), ]

cnbvredesN$Default <- ifelse(cnbvredesN$Dias_de_atraso >=90, 0, 1)
cnbvredesN$Defaultf <- as.factor(cnbvredesN$Default)
levels(cnbvredesN$Defaultf)
cnbvredesN$F_Nacim2 <- as.Date(cnbvredesN$F_Nacim, "%d/%m/%Y")
cnbvredesN$Edad <- floor(age_calc(cnbvredesN$F_Nacim2, units = "years"))
cnbvredesN$Edad <- as.numeric(cnbvredesN$Edad)
cnbvredesN$Oct.Dic_18 <- as.numeric(cnbvredesN$Oct.Dic_18)
cnbvredesN$cuotas_venc <- as.numeric(cnbvredesN$cuotas_venc)
cnbvredesN$No_de_Cuotas <- as.numeric(cnbvredesN$No_de_Cuotas)
str(cnbvredesN)



sample = sample.split(cnbvredesN$Defaultf, SplitRatio = .70)
cnbvredesNP <- subset(cnbvredesN, sample == TRUE)
cnbvredesNT <- subset(cnbvredesN, sample == FALSE)

rf_redesN=randomForest(Defaultf ~ Edad + P_Pago + cicloe + Territorio + monto_inicial +
                        sexof + No_de_Cuotas + Oct.Dic_18 + Unificacion + cuotas_venc +
                        tasa + Frec + Pago_sostenido
                      
                      , data = cnbvredesNT, 
                      na.action = na.omit, 
                      ntree = 500,
                      mtry = 3,
                      nodesize = 2,
                      importance = T,
                      keep.forest = T
)
rf_redesN

plot(rf_redesN)

#distribution of minimal depht
#min_depth_frame <- min_depth_distribution(rf_redesN)
#save(min_depth_frame, file = "min_depth_frame.rda")
#load("min_depth_frame.rda")
#head(min_depth_frame, n = 10)

# plot_min_depth_distribution(forest) # gives the same result as below but takes longer
#plot of the distribution of minimal depth for 
#top ten variables according to mean minimal depth calculated using top trees 
#plot_min_depth_distribution(min_depth_frame)

#plot_min_depth_distribution(min_depth_frame, mean_sample = "relevant_trees", k = 15)

#importance_frame <- measure_importance(rf_redesN)
#save(importance_frame, file = "importance_frame.rda")
#load("importance_frame.rda")
#importance_frame

#This test tells us whether the observed number of successes (number of nodes in which Xj 
#was used for splitting)
#exceeds the theoretical number of successes if they were random 

# plot_multi_way_importance(forest, size_measure = "no_of_nodes") 
# gives the same result as below but takes longer
#plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes")

#esta no tiene mse increase por que es binomial
#plot_multi_way_importance(importance_frame, x_measure = "mse_increase",
#                          y_measure = "node_purity_increase", size_measure = "p_value", no_of_labels = 5)

#plot_importance_ggpairs(importance_frame)
#plot_importance_rankings(importance_frame)

#investigate interactions among best predictors
#(vars <- important_variables(importance_frame, k = 5, measures = c("mean_min_depth", "no_of_trees")))

#interactions_frame <- min_depth_interactions(rf_redesN, vars)
#save(interactions_frame, file = "interactions_frame.rda")
#load("interactions_frame.rda")
#head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

#interactions_frame <- min_depth_interactions(forest, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
#save(interactions_frame, file = "interactions_frame_relevant.rda")
#load("interactions_frame_relevant.rda")
#plot_min_depth_interactions(interactions_frame)

#todo el reporte (tarda)
explain_forest(rf_redesN, interactions = TRUE, data = cnbvredesN)

# Predicting on train set
predTrain <- predict(rf_redesN, cnbvredesNT, type = "response")
# Checking classification accuracy
table(predTrain, cnbvredesNT$Defaultf)
x = mean(predTrain)
y = mean(cnbvredesNT$Defaultf)
y/x

# Predicting on Validation set
predValid <- predict(rf_redesN, cnbvredesNP, type = "response")
# Checking classification accuracy
#table(predValid, fcvP$MtoVen)
table(predValid, cnbvredesNP$Defaultf)
x1 = mean(predValid)
y1 = mean(cnbvredesNP$Defaultf)
y1/x1

# To check important variables
importance(rf_redesN)        
varImpPlot(rf_redesN)

x=cnbvredesN[c(68, 62, 66, 71, 7, 70, 29, 55, 45, 8, 56, 15, 17,12)]
#y=out.data[, response.col]
#randomForest(x,y,xtest=x,ytest=y)
#model=randomForest(x,y,xtest=x,ytest=y,keep.forest=TRUE). 
prob=predict(rf_redesN,x,type="prob")
summary(prob)
prob <-  data.frame(prob)
prob2 = round(prob*100, digits=0)
prob2 <-data.frame(prob2)

cnbvredesN$ProbDef <-  prob$X0
cnbvredesN$ProbPago <-  prob$X1
cnbvredesN$ScoreDef <-  prob2$X0
cnbvredesN$ScorePago <-  prob2$X1

socrex <- cnbvredesN %>%          
  mutate(Banda = case_when(.$ProbDef < 0.10  ~ "A",
                           .$ProbDef < 0.20  ~ "B",
                           .$ProbDef < 0.35  ~ "C",
                           .$ProbDef < 0.50  ~ "D",
                           .$ProbDef < 0.70  ~ "E",
                           .$ProbDef < 0.90  ~ "F",
                           TRUE ~ "G"))

socrex <- socrex %>%          
  mutate(Perdida = (MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2)*ProbDef*Severidad)

socrex2 <- socrex %>%  
  group_by(Banda)  %>%  
  summarise(Creditos = length(nro_prest),
            capital = sum(MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2),
            PE = sum(Perdida),
            Pe2 = mean(ProbDef),
            sdPE = sd(ProbDef),
            Comp = sum(Total_Provision))

write.csv(socrex2, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/RF_Predict_RedesNuevos1.csv" )
write.csv(socrex, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/Base_RedesN1.csv" )

########### SOLO RECOMPRAS ###########

#5RECOMPRAS
cnbvredesR$sexof <- as.factor(cnbvredesR$sexo)
levels(cnbvredesR$sexof)
#quitar na's de fechas de nacimiento.
cnbvredesR<- cnbvredesR[complete.cases(cnbvredesR$F_Nacim), ]
cnbvredesR<- cnbvredesR[which(cnbvredesR$F_Nacim != "01/01/1900"), ]
cnbvredesR<- cnbvredesR[which(cnbvredesR$F_Nacim != ""), ]

cnbvredesR$Default <- ifelse(cnbvredesR$Dias_de_atraso >=90, 0, 1)
cnbvredesR$Defaultf <- as.factor(cnbvredesR$Default)
levels(cnbvredesR$Defaultf)
cnbvredesR$F_Nacim2 <- as.Date(cnbvredesR$F_Nacim, "%d/%m/%Y")
cnbvredesR$Edad <- floor(age_calc(cnbvredesR$F_Nacim2, units = "years"))
cnbvredesR$Edad <- as.numeric(cnbvredesR$Edad)
cnbvredesR$Oct.Dic_18 <- as.numeric(cnbvredesR$Oct.Dic_18)
cnbvredesR$cuotas_venc <- as.numeric(cnbvredesR$cuotas_venc)
cnbvredesR$No_de_Cuotas <- as.numeric(cnbvredesR$No_de_Cuotas)
str(cnbvredesR)



sample = sample.split(cnbvredesR$Defaultf, SplitRatio = .70)
cnbvredesRP <- subset(cnbvredesR, sample == TRUE)
cnbvredesRT <- subset(cnbvredesR, sample == FALSE)

rf_redesR=randomForest(Defaultf ~ Edad + P_Pago + cicloe + Territorio + monto_inicial +
                         sexof + No_de_Cuotas + Oct.Dic_18 + Unificacion + cuotas_venc +
                         tasa + Frec + Pago_sostenido
                       
                       , data = cnbvredesRT, 
                       na.action = na.omit, 
                       ntree = 500,
                       mtry = 3,
                       nodesize = 2,
                       importance = T,
                       keep.forest = T
)
rf_redesR

plot(rf_redesR)


#todo el reporte (tarda)
explain_forest(rf_redesR, interactions = TRUE, data = cnbvredesR)

# Predicting on train set
predTrain <- predict(rf_redesR, cnbvredesRT, type = "response")
# Checking classification accuracy
table(predTrain, cnbvredesRT$Defaultf)
x = mean(predTrain)
y = mean(cnbvredesRT$Defaultf)
y/x

# Predicting on Validation set
predValid <- predict(rf_redesN, cnbvredesRP, type = "response")
# Checking classification accuracy
#table(predValid, fcvP$MtoVen)
table(predValid, cnbvredesRP$Defaultf)
x1 = mean(predValid)
y1 = mean(cnbvredesRP$Defaultf)
y1/x1

# To check important variables
importance(rf_redesN)        
varImpPlot(rf_redesN)

x=cnbvredesR[c(68, 62, 66, 71, 7, 70, 29, 55, 45, 8, 56, 15, 17,12)]
#y=out.data[, response.col]
#randomForest(x,y,xtest=x,ytest=y)
#model=randomForest(x,y,xtest=x,ytest=y,keep.forest=TRUE). 
prob=predict(rf_redesN,x,type="prob")
summary(prob)
prob <-  data.frame(prob)
prob2 = round(prob*100, digits=0)
prob2 <-data.frame(prob2)

cnbvredesR$ProbDef <-  prob$X0
cnbvredesR$ProbPago <-  prob$X1
cnbvredesR$ScoreDef <-  prob2$X0
cnbvredesR$ScorePago <-  prob2$X1

socrex <- cnbvredesR %>%          
  mutate(Banda = case_when(.$ProbDef < 0.10  ~ "A",
                           .$ProbDef < 0.20  ~ "B",
                           .$ProbDef < 0.35  ~ "C",
                           .$ProbDef < 0.50  ~ "D",
                           .$ProbDef < 0.70  ~ "E",
                           .$ProbDef < 0.90  ~ "F",
                           TRUE ~ "G"))

socrex <- socrex %>%          
  mutate(Perdida = (MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2)*ProbDef*Severidad)

socrex2 <- socrex %>%  
  group_by(Banda)  %>%  
  summarise(Creditos = length(nro_prest),
            capital = sum(MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2),
            PE = sum(Perdida),
            Pe2 = mean(ProbDef),
            sdPE = sd(ProbDef),
            Comp = sum(Total_Provision))

write.csv(socrex2, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/RF_Predict_RedesRecompra.csv" )
write.csv(socrex, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/Base_RedesR.csv" )

############# SOLO NOMINA ##########
#2SOLO NOMINA
names(cnbvnom)
str(cnbvnom)
cnbvnom$Frec3 <- as.character(cnbvnom$X.1)
cnbvnom$Frec3 <- as.numeric(cnbvnom$Frec3)
cnbvnom <- cnbvnom %>% 
  mutate(
         Exigible = round(((43465 - fecha_apertu)/(Frec3)), digits = 0),#cambiar la fecha
         Pagos = Exigible - cuotas_venc,
         Exigible2 = Exigible*Cuota,
         Pagos2 = Pagos*Cuota,
         P_Pago = Pagos2/Exigible2)

cnbvnom<- cnbvnom%>%
  mutate(P_Pago = ifelse(cuotas_venc==0,1,0.7))


cnbvnom$sexof <- as.factor(cnbvnom$sexo)
levels(cnbvnom$sexof)
cnbvnom$Default <- ifelse(cnbvnom$Dias_de_atraso >=90, 0, 1)
cnbvnom$Defaultf <- as.factor(cnbvnom$Default)
levels(cnbvnom$Defaultf)
cnbvnom$F_Nacim2 <- as.Date(cnbvnom$F_Nacim, "%d/%m/%Y")
cnbvnom$F_Nacim2[is.na(cnbvnom$F_Nacim2)] <- "01/01/1983" # se imputa a mano que la edad promedio es 36
cnbvnom$Edad <- floor(age_calc(cnbvnom$F_Nacim2, units = "years"))
cnbvnom$Edad <- as.numeric(cnbvnom$Edad)
cnbvnom$Oct.Dic_18 <- as.numeric(cnbvnom$Oct.Dic_18)
cnbvnom$cuotas_venc <- as.numeric(cnbvnom$cuotas_venc)
cnbvnom$No_de_Cuotas <- as.numeric(cnbvnom$No_de_Cuotas)
str(cnbvnom)

write.csv(cnbvnom, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvnom2.csv")

table(factor(cnbvnomT$Defaultf))

sample = sample.split(cnbvnom$Defaultf, SplitRatio = .70)
cnbvnomP <- subset(cnbvnom, sample == TRUE)
cnbvnomT <- subset(cnbvnom, sample == FALSE)


describe(cnbvnomT$Edad)
summary(cnbvnomT$Defaultf)
describe(cnbvnomT$P_Pago)
summary(cnbvnomT$Pago_sostenido)
describe(cnbvnomT$Frec3)
summary(cnbvnomT$sexof)
describe(cnbvnomT$cicloe)
describe(cnbvnomT$monto_inicial)
summary(cnbvnomT$No_de_Cuotas)
describe(cnbvnomT$Oct.Dic_18)
describe(cnbvnomT$cuotas_venc)
describe(cnbvnomT$tasa)


rf_nom=randomForest(Defaultf ~ Edad + P_Pago + cicloe  + monto_inicial +
                         sexof + Oct.Dic_18 + Unificacion + cuotas_venc +
                         tasa + Frec3
                       
                       , data = cnbvnomT, 
                       na.action = na.omit, 
                       ntree = 500,
                       mtry = 3,
                       nodesize = 2,
                       importance = T,
                       keep.forest = T
)
rf_nom

plot(rf_nom)


#todo el reporte (tarda)
explain_forest(rf_nom, interactions = TRUE, data = cnbvnom)

# Predicting on train set
predTrain <- predict(rf_nom, cnbvnomT, type = "response")
# Checking classification accuracy
table(predTrain, cnbvnomT$Defaultf)
x = mean(predTrain)
#y = mean(cnbvnomT$Defaultf)
#y/x

# Predicting on Validation set
predValid <- predict(rf_nom, cnbvnomP, type = "response")
# Checking classification accuracy
#table(predValid, fcvP$MtoVen)
table(predValid, cnbvnomP$Defaultf)
#x1 = mean(predValid)
#y1 = mean(cnbvnomP$Defaultf)
#y1/x1

# To check important variables
importance(rf_nom)        
varImpPlot(rf_nom)
names(cnbvnomP)
x=cnbvnomP[c("Edad" , "P_Pago" , "cicloe" , "monto_inicial" ,
               "sexof" , "Oct.Dic_18" , "Unificacion" , "cuotas_venc" ,
               "tasa" , "Frec3")]
#y=out.data[, response.col]
#randomForest(x,y,xtest=x,ytest=y)
#model=randomForest(x,y,xtest=x,ytest=y,keep.forest=TRUE). 
prob=predict(rf_nom,x,type="prob")
summary(prob)
prob <-  data.frame(prob)
prob2 = round(prob*100, digits=0)
prob2 <-data.frame(prob2)

cnbvnomP$ProbDef <-  prob$X0
cnbvnomP$ProbPago <-  prob$X1
cnbvnomP$ScoreDef <-  prob2$X0
cnbvnomP$ScorePago <-  prob2$X1

socrex <- cnbvnomP %>%          
  mutate(Banda = case_when(.$ProbDef < 0.10  ~ "A",
                           .$ProbDef < 0.20  ~ "B",
                           .$ProbDef < 0.35  ~ "C",
                           .$ProbDef < 0.50  ~ "D",
                           .$ProbDef < 0.70  ~ "E",
                           .$ProbDef < 0.90  ~ "F",
                           TRUE ~ "G"))

socrex <- socrex %>%          
  mutate(Perdida = (MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2)*ProbDef*Severidad)

socrex2 <- socrex %>%  
  group_by(Banda)  %>%  
  summarise(Creditos = length(nro_prest),
            capital = sum(MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2),
            PE = sum(Perdida),
            Pe2 = mean(ProbDef),
            sdPE = sd(ProbDef),
            Comp = sum(Total_Provision))

write.csv(socrex2, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/RF_Predict_Nom.csv" )
write.csv(socrex, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/Base_Nom.csv" )


############# SOLO GOBIERNO ##########
#1 SOLO GOBIERNO

cnbvgob$sexof <- as.factor(cnbvgob$sexo)
levels(cnbvgob$sexof)
#para meter genero a mano ver nombre
write.csv(cnbvgob, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvgob.csv")
cnbvgob <- read.csv("D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvgob.csv")

#quitar na's de fechas de nacimiento.
cnbvgob<- cnbvgob[complete.cases(cnbvgob$F_Nacim), ]
#nota archivo maestrop no pagos etc... esperar jorge
cnbvgob$sexof <- as.factor(cnbvgob$sexo)
levels(cnbvgob$sexof)
cnbvgob$Default <- ifelse(cnbvgob$Dias_de_atraso >=90, 0, 1)
cnbvgob$Defaultf <- as.factor(cnbvgob$Default)
levels(cnbvgob$Defaultf)
cnbvgob$F_Nacim2 <- as.Date(cnbvgob$F_Nacim, "%d/%m/%Y")
cnbvgob$Edad <- floor(age_calc(cnbvgob$F_Nacim2, units = "years"))
cnbvgob$Edad <- as.numeric(cnbvgob$Edad)
cnbvgob$Oct.Dic_18 <- as.numeric(cnbvgob$Oct.Dic_18)
cnbvgob$cuotas_venc <- as.numeric(cnbvgob$cuotas_venc)
cnbvgob$No_de_Cuotas <- as.numeric(cnbvgob$No_de_Cuotas)
str(cnbvgob)


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




cnbvcartera2$sexof <- as.factor(cnbvcartera2$sexo)
levels(cnbvcartera2$sexof)
cnbvcartera2$Defaultf <- as.factor(cnbvcartera2$Default)
levels(cnbvcartera2$Defaultf)
cnbvcartera2$Edad <- as.numeric(cnbvcartera2$Edad)
cnbvcartera2$Oct.Dic_18 <- as.numeric(cnbvcartera2$Oct.Dic_18)
cnbvcartera2$cuotas_venc <- as.numeric(cnbvcartera2$cuotas_venc)
cnbvcartera2$No_de_Cuotas <- as.numeric(cnbvcartera2$No_de_Cuotas)
str(cnbvcartera2)

options(scipen = 100000)

logit1 <- glm(Defaultf ~ Edad + P_Pago + cicloe + Territorio + monto_inicial +
      sexof + No_de_Cuotas + Oct.Dic_18 + Unificacion + cuotas_venc, data = cnbvcartera2, family = binomial(link = "logit"))
summary(logit1)

summary(cnbvcartera2)
logitP <- glm(Default ~ Edad + cicloe + Territorio + monto_inicial + P_Pago+
                sexof + No_de_Cuotas + Oct.Dic_18 + Unificacion + cuotas_venc, data = cnbvcartera2, family = poisson(link = "log"))
summary(logitP)

cnbvcartera2$Unificacion2 <- as.numeric(cnbvcartera2$Unificacion)
cnbvcartera2$sexo2 <- as.numeric(cnbvcartera2$sexo)

cnbvcartera2 <- cnbvcartera2 %>%          
  mutate( PScore = -3.9248 + 0.01180*Edad + -3.08760*P_Pago + -0.17626*cicloe +
            -0.27123*sexo2 + -0.01893*No_de_Cuotas + 0.61765*Oct.Dic_18 +
            1.6392*Unificacion2 + -0.46932*cuotas_venc,#cambiar meses
          expScore = exp(PScore),
          Score = expScore/(1+expScore),
          Score2 = round(Score*1000, digits=0))

cnbvcartera2 <- cnbvcartera2 %>%          
  mutate( PScoreP = -3.4293 + 0.01033*Edad + -2.7898*P_Pago + -0.18090*cicloe +
            -0.3249*sexo2 + -0.01682*No_de_Cuotas + 0.51048*Oct.Dic_18 +
            1.5625*Unificacion2 + -0.3708*cuotas_venc,#cambiar meses
          expScoreP = exp(PScoreP),
          ScoreP = expScoreP/(1+expScoreP),
          Score2P = round(ScoreP*1000, digits=0))

cnbvcartera2$ScoreP

###score logit
cnbvcartera2 <- cnbvcartera2 %>%          
  mutate(Perdida = (MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2)*Score*Severidad)


cnbvcartera22 <- cnbvcartera2 %>%          
  mutate(Perdida = (MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2)*Score*Severidad)


cnbvcartera2 <- cnbvcartera2 %>%          
  mutate(Banda = case_when(.$Score < 0.10  ~ "A",
                           .$Score < 0.20  ~ "B",
                           .$Score < 0.30  ~ "C",
                           .$Score < 0.50  ~ "D",
                           .$Score < 0.70  ~ "E",
                           .$Score < 0.90  ~ "F",
                           TRUE ~ "G"))

cnbvcartera3 <- cnbvcartera2 %>%  
  group_by(Banda)  %>%  
  summarise(Creditos = length(nro_prest),
            capital = sum(MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2),
            PE = sum(Perdida),
            Pe2 = mean(Score),
            sdPE = sd(Score),
            Comp = sum(Total_Provision))


###score poisson

cnbvcartera2 <- cnbvcartera2 %>%          
  mutate(PerdidaP = (MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2)*ScoreP*Severidad)


cnbvcartera22 <- cnbvcartera2 %>%          
  mutate(PerdidaP = (MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2)*ScoreP*Severidad)


cnbvcartera2 <- cnbvcartera2 %>%          
  mutate(BandaP = case_when(.$ScoreP < 0.10  ~ "A",
                           .$ScoreP < 0.20  ~ "B",
                           .$ScoreP < 0.30  ~ "C",
                           .$ScoreP < 0.50  ~ "D",
                           .$ScoreP < 0.70  ~ "E",
                           .$ScoreP < 0.90  ~ "F",
                           TRUE ~ "G"))

cnbvcartera4 <- cnbvcartera2 %>%  
  group_by(BandaP)  %>%  
  summarise(Creditos = length(nro_prest),
            capital = sum(MtoVig2+MtoVenc2+Int_Vig_2+Int_Ven_2),
            PE = sum(PerdidaP),
            Pe2 = mean(ScoreP),
            sdPE = sd(ScoreP),
            Comp = sum(Total_Provision))


sum(cnbvcartera4$PE)
sum(cnbvcartera3$PE)
mean(cnbvcartera4$sdPE[1:6])

write.csv(cnbvcartera2, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvcartera2_2.csv" )
write.csv(cnbvcartera3, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvcartera3.csv" )
write.csv(cnbvcartera4, "D:/Documentos_Andre/Perdida_esperada/PE_Ivan/cnbvcartera4.csv" )
