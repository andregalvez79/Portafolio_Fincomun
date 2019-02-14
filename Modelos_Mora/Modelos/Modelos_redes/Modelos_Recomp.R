library(corrplot)
library(Hmisc)
library(car)
library(rcompanion)
library(randomForest)
library(plotly)
library(caTools)

Meses <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Crear_Bases\\Redes_Suc\\BaseCosechas\\Base.CM.Recomp.csv")
names(Meses)

#summary(Meses)
########### train

sample = sample.split(Meses$porcentajeM1, SplitRatio = .80)
MesesPrueba <- subset(Meses, sample == TRUE) 
MesesTrain <- subset(Meses, sample == FALSE)
##RF
reg.rf=randomForest(porcentajeM1 ~ Especiales +
                      Recomp_Aut_crediv + Recomp_Aut_MiTienda + Recomp_Aut_prod +
                      Redoc_no_Aut_Efec + Redoc_no_Aut_MiTienda + 
                      Redoc_no_Aut_Otros +  Redoc_no_Aut_prod +
                      Mto_Desembolso +
                      Asesor + Intermediario + Promotor +
                      Unificacion + SinRes + Reestructuracion + OtroTer+
                      Hidalgo+ Norte+ Oriente+ Poniente+ Puebla+ Sur+
                      suc11+ suc14+ suc21+ suc22+ suc24+
                      suc25+ suc26+ suc27+ suc28+ suc31+ suc32+ suc41+
                      suc42+ suc43+ suc44+ suc45+ suc46+ suc47+ suc48+
                      suc49+ suc51+ suc52+ suc53+ suc54+ suc55+ suc56+
                      suc57 +suc58+ suc61+  suc72+ suc73+ suc75+
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
                      suc628+ suc629+ suc641+ suc642+ suc981+
                      NoCamp+ ICC+REC
                    
                    , data = MesesTrain, 
                    na.action = na.omit, 
                    ntree = 10000,
                    mtry = 30,
                    nodesize = 10,
                    importance = T
                    )
reg.rf


plot(reg.rf)

# Predicting on train set
predTrain <- predict(reg.rf, MesesTrain, type = "response")
# Checking classification accuracy
table(predTrain, MesesTrain$porcentajeM1)
x = mean(predTrain)
y = mean(MesesTrain$porcentajeM1)
y/x



# Predicting on Validation set
predValid <- predict(reg.rf, MesesPrueba, type = "response")
# Checking classification accuracy
table(predValid, MesesPrueba$porcentajeM1)
x1 = mean(predValid)
y1 = mean(MesesPrueba$porcentajeM1)
y1/x1



# To check important variables
importance(reg.rf)        
varImpPlot(reg.rf)        



#PREDICCIONES
#para todas las predicciones
pred.rf <- predict(reg.rf, MesesPrueba, predict.all=TRUE)

#predict rf a limites superiores
pred.rf.int <- apply(pred.rf$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(0.025, 0.975)))
})

pred_rf <-t(pred.rf.int)
write.csv(pred_rf,"C:/Users/andrevargas/Documents/Modelo_mora/Modelos_redes/pred_recomp_limite_superior.csv")

#predict rf a "Alerta amarilla"

pred.rf.int_ama <- apply(pred.rf$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(.25, .75)))
})

pred_rf_ama <-t(pred.rf.int_ama)
write.csv(pred_rf_ama,"C:/Users/andrevargas/Documents/Modelo_mora/Modelos_redes/pred_recomp_alerta_amarilla.csv")


#predict rf a "Alerta amarilla"

pred.rf.int_ama2 <- apply(pred.rf$individual, 1, function(x) {
  c(mean(x) + c(-1, 1) * sd(x), 
    quantile(x, c(.50, .50)))
})

pred_rf_ama2 <-t(pred.rf.int_ama2)
write.csv(pred_rf_ama2,"C:/Users/andrevargas/Documents/Modelo_mora/Modelos_redes/pred_recomp_alerta_amarilla2.csv")



######PLOT resultados

Pl<-read.csv("C:/Users/andrevargas/Documents/Modelo_mora/Modelos_redes/plot_recomp.csv", stringsAsFactors = F)
str(Pl)

p_mes1 <- plot_ly(Pl, x = ~Meses, y = ~Real, name = '% Casos >30 días Mes 1', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Límite_Inferior, name = 'Límite Inferior', type="scatter" ,line = list(color = 'rgba(152, 0, 0, .8)', width = 1.5, dashed = "dash")) %>%
  add_trace(y = ~Límite_Superior, name = 'Límite Superior', type="scatter" , line = list(color = 'rgba(152, 0, 0, .8)', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Amarillo, name = 'Alerta Amarilla', type="scatter" , line = list(color = 'orange', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Por_Crecimeinto, name = 'Ajustado por Crecimiento', type="scatter" , line = list(color = 'red', width = 1, dashed = 'dot')) %>%
  add_trace(y = ~Normal, name = 'Límite Promedio', type="scatter" , line = list(color = 'green', width = 1.5, dashed = 'dot')) %>%
  add_annotations(x = Pl$Meses,
                  y = Pl$Real,
                  text = round(Pl$Real*100,1),
                  showarrow = F)%>%
  layout(title = "% Casos >30 días Mes 1 con Límites del Modelo",
         xaxis = list(title = "Meses", showgrid = FALSE),
         yaxis = list (title = "% Casos >30 días Mes 1", tickformat = "%", showgrid = FALSE))
p_mes1

#plot mes2
Pl2<-read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelos_redes\\plot_recomp_m2.csv", stringsAsFactors = F)
str(Pl2)
#Pl2 <-Pl2[-58,]

p_mes2 <- plot_ly(Pl2, x = ~Meses, y = ~Real, name = '% Casos >30 días Mes 2', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Límite_Inferior, name = 'Límite Inferior', type="scatter" ,line = list(color = 'rgba(152, 0, 0, .8)', width = 1.5, dashed = "dash")) %>%
  add_trace(y = ~Límite_Superior, name = 'Límite Superior', type="scatter" , line = list(color = 'rgba(152, 0, 0, .8)', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Amarillo, name = 'Alerta Amarilla', type="scatter" , line = list(color = 'orange', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Por_Crecimeinto, name = 'Ajustado por Crecimiento', type="scatter" , line = list(color = 'red', width = 1, dashed = 'dot')) %>%
  add_trace(y = ~Normal, name = 'Límite Promedio', type="scatter" , line = list(color = 'green', width = 1.5, dashed = 'dot')) %>%
  add_annotations(x = Pl2$Meses,
                  y = Pl2$Real,
                  text = round(Pl2$Real*100,1),
                  showarrow = F)%>%
  layout(title = "% Casos >30 días Mes 2 con Límites del Modelo",
         xaxis = list(title = "Meses", showgrid = FALSE),
         yaxis = list (title = "% Casos >30 días Mes 2", tickformat = "%", showgrid = FALSE))
p_mes2


#plot mes3
#plot mes2
Pl3<-read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelos_redes\\plot_nuevos_m3.csv", stringsAsFactors = F)
str(Pl3)
#Pl3 <-Pl3[-58,]

p_mes3 <- plot_ly(Pl3, x = ~Meses, y = ~Real, name = '% Casos >30 días Mes 3', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Límite_Inferior, name = 'Límite Inferior', type="scatter" ,line = list(color = 'rgba(153, 0, 0, .8)', width = 1.5, dashed = "dash")) %>%
  add_trace(y = ~Límite_Superior, name = 'Límite Superior', type="scatter" , line = list(color = 'rgba(153, 0, 0, .8)', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Amarillo, name = 'Alerta Amarilla', type="scatter" , line = list(color = 'orange', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Por_Crecimeinto, name = 'Ajustado por Crecimiento', type="scatter" , line = list(color = 'red', width = 1, dashed = 'dot')) %>%
  add_trace(y = ~Normal, name = 'Límite Promedio', type="scatter" , line = list(color = 'green', width = 1.5, dashed = 'dot')) %>%
  add_annotations(x = Pl3$Meses,
                  y = Pl3$Real,
                  text = round(Pl3$Real*100,1),
                  showarrow = F)%>%
  layout(title = "% Casos >30 días Mes 3 con Límites del Modelo",
         xaxis = list(title = "Meses", showgrid = FALSE),
         yaxis = list (title = "% Casos >30 días Mes 3", tickformat = "%", showgrid = FALSE))
p_mes3

