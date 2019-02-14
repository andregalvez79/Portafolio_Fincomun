library(corrplot)
library(Hmisc)

Meses <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelo_todos\\Meses_recompra.csv")

summary(Meses)
str(Meses[1:72])
str(Meses[73:148])

MesesTrain<- Meses[1:51,]
MesesPrue<- Meses[52:56,]

##Mes 2
library(randomForest)
reg.rf_m2=randomForest(porcentajeM2 ~ porcentajeM1+ Especiales +
                         Modelo +
                         Recomp_Aut_crediv + Recomp_Aut_MiTienda + Recomp_Aut_prod +
                         Redoc_no_Aut_Efec + Redoc_no_Aut_MiTienda + 
                         Redoc_no_Aut_Otros +  Redoc_no_Aut_prod  +
                         Mto_Desembolso + 
                         Asesor + Fza_Movil + Intermediario + Promotor +
                         Unificacion + SinRes + Reestructuracion + OtroTer+
                         Hidalgo+ Norte+ Oriente+ Poniente+ Puebla+ Sur+
                         suc2+suc3 +suc5+ suc11+ suc14+ suc21+ suc22+ suc24+
                         suc25+ suc26+ suc27+ suc28+ suc31+ suc32+ suc41+
                         suc42+ suc43+ suc44+ suc45+ suc46+ suc47+ suc48+
                         suc49+ suc51+ suc52+ suc53+ suc54+ suc55+ suc56+
                         suc57 +suc58+ suc61+  suc72+ suc73+ suc75+ suc77+
                         suc81+ suc83+ suc84+ suc85+ suc87+ suc91+ suc92+
                         suc93+ suc95+ suc101+ suc102+ suc104+ suc111+ suc112+
                         suc113+ suc114+ suc115+ suc121+ suc122+ suc123+
                         suc131+ suc133+ suc134+ suc151+ suc160+ suc211+
                         suc215+ suc221+ suc311+ suc312+ suc313+ suc321+
                         suc331+ suc332+ suc336+ suc341+ suc342+ suc343+
                         suc344+ suc345+ suc346+ suc347+ suc350+ suc352+
                         suc550+ suc561+ suc562+ suc563+ suc564+ suc565+
                         suc566+ suc601+ suc602+ suc603+ suc604+ suc621+
                         suc622+ suc623+ suc624+ suc625+ suc626+ suc627+
                         suc628+ suc629+ suc641+ suc642+ suc801+ suc802+ suc981+
                         bueno+ malo+ NoCamp+ ICC+REC+ CVMes
                    
                    , data = MesesTrain, 
                    na.action = na.omit, 
                    ntree = 10000,
                    mtry = 36,
                    nodesize = 10,
                    importance = T
                    )
reg.rf


plot(reg.rf)

# Predicting on train set
predTrain <- predict(reg.rf_m2, MesesTrain, type = "response")
mean(predTrain[1:45])
predTrain[is.na(predTrain)] <-mean(predTrain[1:45])
# Checking classification accuracy
table(predTrain, MesesTrain$porcentajeM2)
x = mean(predTrain)
y = mean(MesesTrain$porcentajeM2)
y/x

# Predicting on Validation set
predValid <- predict(reg.rf_m2, MesesPrue, type = "response")
#mean(predValid[1:36])
#predValid[is.na(predValid)] <-mean(predValid[1:36])
# Checking classification accuracy
table(predValid, MesesPrue$porcentajeM2)
x1 = mean(predValid)
y1 = mean(MesesPrue$porcentajeM2)
y1/x1

# To check important variables
importance(reg.rf_m2)        
varImpPlot(reg.rf_m2)        
###################################################
#PREDICCIONES
#para todas las predicciones
pred.rf_m2 <- predict(reg.rf_m2, MesesPrue, predict.all=TRUE)

#predict rf a limites superiores
pred.rf.int_m2 <- apply(pred.rf_m2$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(0.025, 0.975)))
})

pred_rf_m2_rec <-t(pred.rf.int_m2)
write.csv(pred_rf_m2_rec,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_rf1_m2_rec.csv")

#predict rf a "Alerta amarilla"

pred.rf.int_m2_ama <- apply(pred.rf_m2$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(.25, .75)))
})

pred_rf_m2_ama_rec <-t(pred.rf.int_m2_ama)
write.csv(pred_rf_m2_ama_rec,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_rf_m2_ama_rec.csv")

#############################################


# Using For loop to identify the right mtry for model
a=c()
i=45
for (i in 3:8) {
  reg.rf=randomForest(porcentajeM2 ~ porcentajeM1 + OtrosProductos + Credinomina + Especiales 
                      + NuevoProd + Nvo_Alcancia +
                        Nvo_Credinom + Nvo_Efec + Nvo_MiTienda + Nvo_Otros +
                        Recomp_Aut_crediv + Recomp_Aut_MiTienda + Recomp_Aut_prod +
                        Redoc_no_Aut_Efec + Redoc_no_Aut_MiTienda + 
                        Redoc_no_Aut_Otros +  Redoc_no_Aut_prod + Reestructura + 
                        Mto_Desembolso +
                        Asesor + Fza_Movil + Intermediario + Promotor +
                        Unificacion + SinRes + Reestructuracion + OtroTer+
                        Hidalgo+ Norte+ Oriente+ Poniente+ Puebla+ Sur+
                        suc2+suc3 +suc5+ suc11+ suc14+ suc21+ suc22+ suc24+
                        suc25+ suc26+ suc27+ suc28+ suc31+ suc32+ suc41+
                        suc42+ suc43+ suc44+ suc45+ suc46+ suc47+ suc48+
                        suc49+ suc51+ suc52+ suc53+ suc54+ suc55+ suc56+
                        suc57 +suc58+ suc61+  suc72+ suc73+ suc75+ suc77+
                        suc81+ suc83+ suc84+ suc85+ suc87+ suc91+ suc92+
                        suc93+ suc95+ suc101+ suc102+ suc104+ suc111+ suc112+
                        suc113+ suc114+ suc115+ suc121+ suc122+ suc123+
                        suc131+ suc133+ suc134+ suc151+ suc160+ suc211+
                        suc215+ suc221+ suc311+ suc312+ suc313+ suc321+
                        suc331+ suc332+ suc336+ suc341+ suc342+ suc343+
                        suc344+ suc345+ suc346+ suc347+ suc350+ suc352+
                        suc550+ suc561+ suc562+ suc563+ suc564+ suc565+
                        suc566+ suc601+ suc602+ suc603+ suc604+ suc621+
                        suc622+ suc623+ suc624+ suc625+ suc626+ suc627+
                        suc628+ suc629+ suc641+ suc642+ suc801+ suc802+ suc981+
                        bueno+ malo+ NoCamp+ ICC+REC+ CVMes
                      
                      , data = MesesPrue, 
                      na.action = na.omit, 
                      ntree = 10000,
                      mtry = i,
                      nodesize = 10,
                      importance = T
  )
  predValid <- predict(reg.rf, MesesPrue, type = "class")
  predValid[is.na(predValid)] <-mean(predValid[1:36])
  a[i-2] = mean(predValid == MesesPrue$porcentajeM2)
}

a

plot(3:15,a)

#####Mes 3

reg.rf_m3=randomForest(porcentajeM3 ~ porcentajeM1+ porcentajeM2 + Especiales +
                         Modelo +
                         Recomp_Aut_crediv + Recomp_Aut_MiTienda + Recomp_Aut_prod +
                         Redoc_no_Aut_Efec + Redoc_no_Aut_MiTienda + 
                         Redoc_no_Aut_Otros +  Redoc_no_Aut_prod  +
                         Mto_Desembolso + 
                         Asesor + Fza_Movil + Intermediario + Promotor +
                         Unificacion + SinRes + Reestructuracion + OtroTer+
                         Hidalgo+ Norte+ Oriente+ Poniente+ Puebla+ Sur+
                         suc2+suc3 +suc5+ suc11+ suc14+ suc21+ suc22+ suc24+
                         suc25+ suc26+ suc27+ suc28+ suc31+ suc32+ suc41+
                         suc42+ suc43+ suc44+ suc45+ suc46+ suc47+ suc48+
                         suc49+ suc51+ suc52+ suc53+ suc54+ suc55+ suc56+
                         suc57 +suc58+ suc61+  suc72+ suc73+ suc75+ suc77+
                         suc81+ suc83+ suc84+ suc85+ suc87+ suc91+ suc92+
                         suc93+ suc95+ suc101+ suc102+ suc104+ suc111+ suc112+
                         suc113+ suc114+ suc115+ suc121+ suc122+ suc123+
                         suc131+ suc133+ suc134+ suc151+ suc160+ suc211+
                         suc215+ suc221+ suc311+ suc312+ suc313+ suc321+
                         suc331+ suc332+ suc336+ suc341+ suc342+ suc343+
                         suc344+ suc345+ suc346+ suc347+ suc350+ suc352+
                         suc550+ suc561+ suc562+ suc563+ suc564+ suc565+
                         suc566+ suc601+ suc602+ suc603+ suc604+ suc621+
                         suc622+ suc623+ suc624+ suc625+ suc626+ suc627+
                         suc628+ suc629+ suc641+ suc642+ suc801+ suc802+ suc981+
                         bueno+ malo+ NoCamp+ ICC+REC+ CVMes
                    
                    , data = MesesTrain, 
                    na.action = na.omit, 
                    ntree = 10000,
                    mtry = 36,
                    nodesize = 10,
                    importance = T
)
reg.rf


plot(reg.rf)

# Predicting on train set
predTrain <- predict(reg.rf_m3, MesesTrain, type = "response")
mean(predTrain[1:45])
predTrain[is.na(predTrain)] <-mean(predTrain[1:45])
# Checking classification accuracy
table(predTrain, MesesTrain$porcentajeM3)
x = mean(predTrain)
y = mean(MesesTrain$porcentajeM3)
y/x

# Predicting on Validation set
predValid <- predict(reg.rf_m3, MesesPrue, type = "response")
#mean(predValid[1:36])
#predValid[is.na(predValid)] <-mean(predValid[1:36])
# Checking classification accuracy
table(predValid, MesesPrue$porcentajeM3)
x1 = mean(predValid)
y1 = mean(MesesPrue$porcentajeM3)
y1/x1

# To check important variables
importance(reg.rf_m3)        
varImpPlot(reg.rf_m3)        

###################################################
#PREDICCIONES
#para todas las predicciones
pred.rf_m3 <- predict(reg.rf_m3, MesesPrue, predict.all=TRUE)

#predict rf a limites superiores
pred.rf.int_m3 <- apply(pred.rf_m3$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(0.025, 0.975)))
})

pred_rf_m3_rec <-t(pred.rf.int_m3)
write.csv(pred_rf_m3_rec,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_rf1_m3_rec.csv")

#predict rf a "Alerta amarilla"

pred.rf.int_m3_ama <- apply(pred.rf_m3$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(.25, .75)))
})

pred_rf_m3_ama_rec <-t(pred.rf.int_m3_ama)
write.csv(pred_rf_m3_ama_rec,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_rf_m3_ama_rec.csv")

#############################################
# Using For loop to identify the right mtry for model
a=c()
i=45
for (i in 3:8) {
  reg.rf=randomForest(porcentajeM3 ~ porcentajeM1+ porcentajeM2 + OtrosProductos + Credinomina + Especiales 
                      + NuevoProd + Nvo_Alcancia +
                        Nvo_Credinom + Nvo_Efec + Nvo_MiTienda + Nvo_Otros +
                        Recomp_Aut_crediv + Recomp_Aut_MiTienda + Recomp_Aut_prod +
                        Redoc_no_Aut_Efec + Redoc_no_Aut_MiTienda + 
                        Redoc_no_Aut_Otros +  Redoc_no_Aut_prod + Reestructura + 
                        Mto_Desembolso +
                        Asesor + Fza_Movil + Intermediario + Promotor +
                        Unificacion + SinRes + Reestructuracion + OtroTer+
                        Hidalgo+ Norte+ Oriente+ Poniente+ Puebla+ Sur+
                        suc2+suc3 +suc5+ suc11+ suc14+ suc21+ suc22+ suc24+
                        suc25+ suc26+ suc27+ suc28+ suc31+ suc32+ suc41+
                        suc42+ suc43+ suc44+ suc45+ suc46+ suc47+ suc48+
                        suc49+ suc51+ suc52+ suc53+ suc54+ suc55+ suc56+
                        suc57 +suc58+ suc61+  suc72+ suc73+ suc75+ suc77+
                        suc81+ suc83+ suc84+ suc85+ suc87+ suc91+ suc92+
                        suc93+ suc95+ suc101+ suc102+ suc104+ suc111+ suc112+
                        suc113+ suc114+ suc115+ suc121+ suc122+ suc123+
                        suc131+ suc133+ suc134+ suc151+ suc160+ suc211+
                        suc215+ suc221+ suc311+ suc312+ suc313+ suc321+
                        suc331+ suc332+ suc336+ suc341+ suc342+ suc343+
                        suc344+ suc345+ suc346+ suc347+ suc350+ suc352+
                        suc550+ suc561+ suc562+ suc563+ suc564+ suc565+
                        suc566+ suc601+ suc602+ suc603+ suc604+ suc621+
                        suc622+ suc623+ suc624+ suc625+ suc626+ suc627+
                        suc628+ suc629+ suc641+ suc642+ suc801+ suc802+ suc981+
                        bueno+ malo+ NoCamp+ ICC+REC+ CVMes
                      
                      , data = MesesPrue, 
                      na.action = na.omit, 
                      ntree = 10000,
                      mtry = i,
                      nodesize = 10,
                      importance = T
  )
  predValid <- predict(reg.rf, MesesPrue, type = "class")
  predValid[is.na(predValid)] <-mean(predValid[1:36])
  a[i-2] = mean(predValid == MesesPrue$porcentajeM3)
}

a

plot(3:15,a)
