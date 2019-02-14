library(corrplot)
library(Hmisc)

Meses <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelo_Nuevos\\Meses_Nuevos.csv")
summary(Meses)
str(Meses[1:72])
str(Meses[73:139])

Meses<- select(Meses, -Periodo)

#mylist <- list(names(Meses2))
#Meses2[mylist[[1]]] <- sapply(Meses2[mylist[[1]]],scale)
##summary(Meses2)
#Meses2$Otros
#outliers
#otros, credinomina, nvo ALcancia, nvo_Credinom, Nvo_Otros, Recomp_Aut_crediv, Recomp_Aut_MiTienda,
#Redoc_no_Aut_MiTienda, Reestructura, Fza_Movil, suc3, suc801, porcentajeM1, CVMes
#Meses2$Otros
#Meses2$Credinomina
#Meses2$Nvo_Alcancia
#Meses2$suc801


########modelos
names(Meses)

#subset
Mesest <- Meses[1:51,]
Mesesp <- Meses[52:56,]



##RF
library(randomForest)
reg.rf=randomForest(porcentajeM1 ~ OtrosProductos  
                    + NuevoProd + Nvo_Alcancia + Modelo +
                      Nvo_Credinom + Nvo_Efec + Nvo_MiTienda + Nvo_Otros +
                      Mto_Desembolso + 
                      Asesor + Fza_Movil + Intermediario + Promotor +
                       OtroTer+
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
                      bueno + malo + NoCamp + ICC
                    
                    , data = Mesest, 
                    na.action = na.omit, 
                    ntree = 10000,
                    mtry = 35,
                    nodesize = 10,
                    importance = T
                    )
reg.rf


plot(reg.rf)

# Predicting on train set
predTrain <- predict(reg.rf, Mesest, type = "response")
mean(predTrain[1:45])
predTrain[is.na(predTrain)] <-mean(predTrain[1:45])
# Checking classification accuracy
table(predTrain, Mesest$porcentajeM1)
x = mean(predTrain)
y = mean(Mesest$porcentajeM1)
y/x



# Predicting on Validation set
predValid <- predict(reg.rf, Mesesp, type = "response")
# Checking classification accuracy
table(predValid, Mesesp$porcentajeM1)
x1 = mean(predValid)
y1 = mean(Mesesp$porcentajeM1)
y1/x1



# To check important variables
importance(reg.rf)        
varImpPlot(reg.rf)        



#PREDICCIONES
#para todas las predicciones
pred.rf <- predict(reg.rf, Mesesp, predict.all=TRUE)

#predict rf a limites superiores
pred.rf.int <- apply(pred.rf$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(0.025, 0.975)))
})

pred_rf <-t(pred.rf.int)
write.csv(pred_rf,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_rf1.csv")

#predict rf a "Alerta amarilla"

pred.rf.int_ama <- apply(pred.rf$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(.25, .75)))
})

pred_rf_ama <-t(pred.rf.int_ama)
write.csv(pred_rf_ama,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_rf_ama.csv")

#############################################
###prediction intervalos 2 limites superiores
pred.rf.int2 <- sapply(1:4, function(i) {
  tmp <- pred.rf$individual[i, ] + rnorm(1001, 0, sqrt(reg.rf$mse))
  quantile(tmp, c(0.025, 0.975))
})

pred_rf2<-t(pred.rf.int2)
write.csv(pred_rf2_t,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_rf2.csv")

#amarilla
pred.rf.int2_ama <- sapply(1:4, function(i) {
  tmp <- pred.rf$individual[i, ] + rnorm(1001, 0, sqrt(reg.rf$mse))
  quantile(tmp, c(.75, .25))
})

pred_rf2_ama <-t(pred.rf.int2_ama)
write.csv(pred_rf2_t,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_rf2_ama.csv")

# Using For loop to identify the right mtry for model
a=c()
i=40
for (i in 3:8) {
  reg.rf=randomForest(porcentajeM1 ~ OtrosProductos  
                      + NuevoProd + Nvo_Alcancia + Modelo +
                        Nvo_Credinom + Nvo_Efec + Nvo_MiTienda + Nvo_Otros +
                        Mto_Desembolso + 
                        Asesor + Fza_Movil + Intermediario + Promotor +
                        OtroTer+
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
                        bueno + malo + NoCamp + ICC
                      
                      , data = Mesest, 
                      na.action = na.omit, 
                      ntree = 10000,
                      mtry = i,
                      nodesize = 10,
                      importance = T
  )
  predValid <- predict(reg.rf, Mesest, type = "class")
  predValid[is.na(predValid)] <-mean(predValid[1:36])
  a[i-2] = mean(predValid == Mesest$porcentajeM1)
}

a

plot(3:8,a)



######PLOT resultados

library(plotly)

Pl<-read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelo_Nuevos\\plot.csv", stringsAsFactors = F)
str(Pl)
Pl$Real<-(Pl$Real*100)
Pl$Límite_Inferior<-(Pl$Límite_Inferior*100)
Pl$Límite_Superior<-(Pl$Límite_Superior*100)
Pl$Amarillo<-(Pl$Amarillo*100)
#Pl <- Pl[order(as.Date(Pl$Meses, format= "%_%m_%Y")),]
#Pl$Meses<-as.factor(Pl$Meses)
p_mes1 <- plot_ly(Pl, x = ~Meses, y = ~Real, name = '% Casos >30 días Mes 1', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Límite_Inferior, name = 'Límite Inferior', type="scatter" ,line = list(color = 'red', width = 1.5, dashed = "dash")) %>%
  add_trace(y = ~Límite_Superior, name = 'Límite Superior', type="scatter" , line = list(color = 'red', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Amarillo, name = 'Alerta Amarilla', type="scatter" , line = list(color = 'black', width = 1.5, dashed = 'dash')) %>%
  add_annotations(x = Pl$Meses,
                  y = Pl$Real,
                  text = paste(round(Pl$Real,2),"%"),
                  showarrow = F)%>%
  layout(title = "% Casos >30 días Mes 1 con Límites del Modelo",
         xaxis = list(title = "Meses", showgrid = FALSE),
         yaxis = list (title = "% Casos >30 días Mes 1", ticksuffix = "%", showgrid = FALSE))
p_mes1


#plot mes2

Pl2<-read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelo_Nuevos\\plot2_1.csv", stringsAsFactors = F)
str(Pl2)
#Pl2 <-Pl2[-58,]
Pl2$Real<-(Pl2$Real*100)
Pl2$Límite_Inferior<-(Pl2$Límite_Inferior*100)
Pl2$Límite_Superior<-(Pl2$Límite_Superior*100)
Pl2$Amarilla<-(Pl2$Amarilla*100)

p_mes2 <- plot_ly(Pl2, x = ~Meses, y = ~Real, name = '% Casos >30 días Mes 2', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Límite_Inferior, name = 'Límite Inferior', type="scatter" ,line = list(color = 'red', width = 1.5, dashed = "dash")) %>%
  add_trace(y = ~Límite_Superior, name = 'Límite Superior', type="scatter" , line = list(color = 'red', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Amarilla, name = 'Alerta Amarilla', type="scatter" , line = list(color = 'black', width = 1.5, dashed = 'dash')) %>%
  add_annotations(x = Pl2$Meses,
                  y = Pl2$Real,
                  text = paste(round(Pl2$Real,2),"%"),
                  showarrow = F)%>%
  layout(title = "% Casos >30 días Mes 2 con Límites del Modelo",
         xaxis = list(title = "Meses", showgrid = FALSE),
         yaxis = list (title = "% Casos >30 días Mes 2", ticksuffix = "%", showgrid = FALSE))
p_mes2


#plot mes3
Pl3<-read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelo_recompra\\plot3.csv", stringsAsFactors = F)
str(Pl3)
Pl3

Pl3$Real<-(Pl3$Real*100)
Pl3$Límite_Inferior<-(Pl3$Límite_Inferior*100)
Pl3$Límite_Superior<-(Pl3$Límite_Superior*100)
Pl3$Amarilla<-(Pl3$Amarilla*100)
#Pl3 <-Pl3[-58,]
#Pl3 <-Pl3[-57,]
#Pl3

p_mes3<- plot_ly(Pl3, x = ~Meses, y = ~Real, name = '% Casos >30 días Mes 3', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Límite_Inferior, name = 'Límite Inferior', type="scatter" ,line = list(color = 'red', width = 1.5, dashed = "dash")) %>%
  add_trace(y = ~Límite_Superior, name = 'Límite Superior', type="scatter" , line = list(color = 'red', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Amarilla, name = 'Alerta Amarilla', type="scatter" , line = list(color = 'black', width = 1.5, dashed = 'dash')) %>%
  add_annotations(x = Pl3$Meses,
                  y = Pl3$Real,
                  text = paste(round(Pl3$Real,2),"%"),
                  showarrow = F)%>%
  layout(title = "% Casos >30 días Mes 3 con Límites del Modelo",
         xaxis = list(title = "Meses", showgrid = FALSE),
         yaxis = list (title = "% Casos >30 días Mes 3", ticksuffix = "%", showgrid = FALSE
                       ))
p_mes3


