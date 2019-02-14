library(corrplot)
library(Hmisc)

Meses <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelo_todos\\Meses.csv")
summary(Meses)
str(Meses[1:72])
str(Meses[73:148])

Meses2<- select(Meses, -Periodo)
mylist <- list(names(Meses2))

Meses2[mylist[[1]]] <- sapply(Meses2[mylist[[1]]],scale)
summary(Meses2)
Meses2$Otros
#outliers
#otros, credinomina, nvo ALcancia, nvo_Credinom, Nvo_Otros, Recomp_Aut_crediv, Recomp_Aut_MiTienda,
#Redoc_no_Aut_MiTienda, Reestructura, Fza_Movil, suc3, suc801, porcentajeM1, CVMes
Meses2$Otros
Meses2$Credinomina
Meses2$Nvo_Alcancia
Meses2$suc801


########modelos
names(Meses)

#subset
Mesest <- Meses[1:51,]
Mesesp <- Meses[52:56,]
#modelo mes 1

reg2 <- lm(porcentajeM1 ~  0 + OtrosProductos +  
             
           Nvo_Efec + Nvo_MiTienda + Nvo_Otros +
          Redoc_no_Aut_Efec + Redoc_no_Aut_MiTienda + 
          Redoc_no_Aut_Otros +   
          
          Asesor +  Intermediario +
           
           suc26+   suc41+
              
            suc49+   suc55+
           suc101+ suc102+  suc111+
            +suc58+  suc75+
           suc85 +
           suc562+
              suc602+
             suc623+
            suc628
              
            
           ,
          data = Mesest)

summary(reg2)

plot(reg2)
  
car::vif(reg2)  
  



## predictions
#get a matrix with the prediction and a 95 percent 
#confidence interval around the mean prediction. according to your model
#Con informacion de los ultimos 5 meses
#el modelo calcula que  la cartera tendra en promedio un 
#porcentje de mora en el mes 1 de >=30 dias
#entre x,y con 95% de confianza segun el modelo

pred_conf<-predict(reg2, newdata=Mesesp, interval='confidence')
pred_conf
write.csv(pred_conf,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_conf2.csv")


#la cartera se encontrara en un porcentaje de mora en el mes 1 de >=30 dias
#entre x,y con 95% de confianza segun el modelo

pred_pred<-predict(reg2, newdata=Mesesp, interval='prediction')
pred_pred
write.csv(pred_conf,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_pred2.csv")

reg <- lm(porcentajeM1 ~ 0   +  Nvo_Otros +
            Redoc_no_Aut_Otros +  Redoc_no_Aut_prod +
            Intermediario   +
               Poniente+ 
                suc28+  suc41
             +suc46+ 
             suc51+    
            suc57 +suc58+   suc72+  
                 
             suc95 +
            suc113+   suc121+  
            suc131+ suc133+ suc134+ suc151+  suc211+
            suc215+  suc311+ suc313+ suc321+
               suc342+ suc343+
             suc345+ suc346+ suc347+  suc352+
            suc550+ suc561+ suc562+ suc563 +suc565
              
            
            , 
          data = Mesest)

reg4 <- lm(porcentajeM1 ~ 0   +  suc26 + suc42 + suc628+suc332+suc92+suc5+
             Puebla + suc53+suc112+ suc601 +suc134+  suc602+ Nvo_Alcancia,
          
          
          
          data = Mesest)

summary(reg4)
#por colinearidad OtroTer+
# suc350+ suc27+ suc122+ suc21+ suc91+ suc114+
car::vif(reg4)
plot(reg)

#VIF, that quantifies the extent of multicollinearity
install.packages("car")
library(car)

car::vif(reg)


library(rcompanion)

nagelkerke(reg)



## predictions
#get a matrix with the prediction and a 95 percent 
#confidence interval around the mean prediction. according to your model
#Con informacion de los ultimos 5 meses
#el modelo calcula que  la cartera tendra en promedio un 
#porcentje de mora en el mes 1 de >=30 dias
#entre x,y con 95% de confianza segun el modelo

pred_conf<-predict(reg, newdata=Mesesp, interval='confidence')
pred_conf
write.csv(pred_conf,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_conf.csv")


#la cartera se encontrara en un porcentaje de mora en el mes 1 de >=30 dias
#entre x,y con 95% de confianza segun el modelo

pred_pred<-predict(reg, newdata=Mesesp, interval='prediction')
pred_pred
write.csv(pred_conf,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\pred_pred.csv")


####Bayes

install.packages("MASS")
library(MASS)
bayesfit<-function(lmfit,N){
  QR<-lmfit$qr
  df.residual<-lmfit$df.residual
  R<-qr.R(QR) ## R component
  coef<-lmfit$coef
  Vb<-chol2inv(R) ## variance(unscaled)
  s2<-(t(lmfit$residuals)%*%lmfit$residuals)
  s2<-s2[1,1]/df.residual
  
  ## now to sample residual variance
  sigma<-df.residual*s2/rchisq(N,df.residual)
  coef.sim<-sapply(sigma,function(x) mvrnorm(1,coef,Vb*x))
  ret<-data.frame(t(coef.sim))
  names(ret)<-names(lmfit$coef)
  ret$sigma<-sqrt(sigma)
  ret
}

bf<-bayesfit(reg, 1000)

Bayes.sum<-function(x)
{
  c("mean"=mean(x),
    "se"=sd(x),
    "t"=mean(x)/sd(x),
    "median"=median(x),
    "CrI"=quantile(x,prob=0.025),
    "CrI"=quantile(x,prob=0.975)
  )
}

Bayes_reg<-t(apply(bf,2,Bayes.sum))
  
write.csv(Bayes_reg,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Bayes_reg.csv")
#+ Redoc_no_Aut_MiTienda + Recomp_Aut_MiTienda + Nvo_MiTienda Nvo_Credinom + + NuevoProd
#+ Especiales + OtrosProductos + Reestructura + Mto_Desembolso +Recomp_Aut_crediv +suc42
#+suc24 + suc14 suc11+ suc2+ + suc43 Sur+ Nvo_Alcancia +  Puebla+
#Nvo_Efec  + + Hidalgo + Promotor + Unificacion + SinRes suc3 +suc5+ + suc45 +suc31
#suc104+ suc102+  suc87+ suc77+ Redoc_no_Aut_Efec  + suc93+ Norte+ + Credinomina Asesor +
# + Reestructuracion suc26+ + suc44 suc54+ suc61+ suc83+ suc85+ suc101+
# suc56+ suc84+ suc73+ suc75+ + suc111  suc47+ suc53+ Fza_Movil + suc341
#+ suc312 suc221+ suc81+ suc92+ +suc112+ suc32+ Oriente+ Recomp_Aut_prod + + suc564
# suc332+ suc160+ suc123+ suc52+  suc22+ suc25+ suc55+ suc115+ suc604+ suc601+
# suc603+ +  suc621  suc566+ suc331+ + suc626 suc623+ + suc629 + suc627+ suc628 + suc625
# +  suc624  suc629+  suc641+ suc642 suc802+ +suc628 + suc801+ bueno+ malo+
# +  NoCamp + CVMes +ICC+ +REC suc602 +suc622 + suc981 suc49+suc336+ suc344+ suc48+

anova(reg, 
      reg2)
library(lmtest)
lrtest(reg, 
       reg2)

library(rcompanion) 




#nonparamteric
library("np")


bw.all <- npregbw(formula = porcentajeM1 ~ 0   +  Nvo_Otros +
                    Redoc_no_Aut_Otros +  Redoc_no_Aut_prod +
                    Intermediario   +
                    Poniente+ 
                    suc28+  suc41
                  +suc46+ 
                    suc51+    
                    suc57 +suc58+   suc72+  
                    
                    suc95 +
                    suc113+   suc121+  
                    suc131+ suc133+ suc134+ suc151+  suc211+
                    suc215+  suc311+ suc313+ suc321+
                    suc342+ suc343+
                    suc345+ suc346+ suc347+  suc352+
                    suc550+ suc561+ suc562+ suc563 +suc565,
regtype = "ll",
bwmethod = "cv.aic",
  data = Mesest)
  model.np <- npreg(bws = bw.all)

  summary(model.np)
  
  #https://cran.r-project.org/web/packages/np/vignettes/np.pdf  #pg16

fit.np <- predict(model.np,
                       data = wage1.train,
                       newdata = wage1.eval)
pse.np <- mean((wage1.eval$lwage - fit.np)^2)
bw.freq <- bw.subset
bw.freq$bw[1] <- 0
bw.freq$bw[2] <- 0
model.np.freq <- npreg(bws = bw.freq)
fit.np.freq <- predict(model.np.freq,
                            + data = wage1.train,
                            + newdata = wage1.eval)
pse.np.freq <- mean((wage1.eval$lwage - fit.np.freq)^2)


plot(model.np, plot.errors.method = "bootstrap",
     plot.errors.boot.num = 25)




##RF
library(randomForest)
reg.rf=randomForest(porcentajeM1 ~ OtrosProductos + Credinomina + Especiales 
                    + NuevoProd + Nvo_Alcancia + Modelo+
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
                    
                    , data = Mesest, 
                    na.action = na.omit, 
                    ntree = 10000,
                    mtry = 36,
                    nodesize = 10,
                    importance = T
                    )
reg.rf


plot(reg.rf)

# Predicting on train set
predTrain <- predict(reg.rf, Mesest, type = "response")
mean(predTrain[1:36])
predTrain[is.na(predTrain)] <-mean(predTrain[1:36])
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
i=45
for (i in 3:8) {
  reg.rf=randomForest(porcentajeM1 ~ OtrosProductos + Credinomina + Especiales 
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

Pl<-read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelo_todos\\plot.csv", stringsAsFactors = F)
str(Pl)

#Pl <- Pl[order(as.Date(Pl$Meses, format= "%_%m_%Y")),]
#Pl$Meses<-as.factor(Pl$Meses)
p_mes1 <- plot_ly(Pl, x = ~Meses, y = ~Real, name = '% Casos >30 días Mes 1', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Límite_Inferior, name = 'Límite Inferior', type="scatter" ,line = list(color = 'red', width = 1.5, dashed = "dash")) %>%
  add_trace(y = ~Límite_Superior, name = 'Límite Superior', type="scatter" , line = list(color = 'red', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Amarillo, name = 'Alerta Amarilla', type="scatter" , line = list(color = 'black', width = 1.5, dashed = 'dash')) %>%
  add_annotations(x = Pl$Meses,
                  y = Pl$Real,
                  text = round(Pl$Real*100,2),
                  showarrow = F)%>%
  layout(title = "% Casos >30 días Mes 1 con Límites del Modelo",
         xaxis = list(title = "Meses", showgrid = FALSE),
         yaxis = list (title = "% Casos >30 días Mes 1", tickformat = "%", showgrid = FALSE))
p_mes1


#plot mes2
Pl2<-read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelo_todos\\plot2_1.csv", stringsAsFactors = F)
str(Pl2)
Pl2 <-Pl2[-58,]

p_mes2 <- plot_ly(Pl2, x = ~Meses, y = ~Real, name = '% Casos >30 días Mes 2', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Límite_Inferior, name = 'Límite Inferior', type="scatter" ,line = list(color = 'red', width = 1.5, dashed = "dash")) %>%
  add_trace(y = ~Límite_Superior, name = 'Límite Superior', type="scatter" , line = list(color = 'red', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Amarilla, name = 'Alerta Amarilla', type="scatter" , line = list(color = 'black', width = 1.5, dashed = 'dash')) %>%
  add_annotations(x = Pl2$Meses,
                  y = Pl2$Real,
                  text = round(Pl2$Real*100,1),
                  showarrow = F)%>%
  layout(title = "% Casos >30 días Mes 2 con Límites del Modelo",
         xaxis = list(title = "Meses", showgrid = FALSE),
         yaxis = list (title = "% Casos >30 días Mes 2", tickformat = "%", showgrid = FALSE))
p_mes2


#plot mes3
Pl3<-read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Modelo_todos\\plot3.csv", stringsAsFactors = F)
str(Pl3)
Pl3
#Pl3 <-Pl3[-58,]
#Pl3 <-Pl3[-57,]
#Pl3

p_mes3<- plot_ly(Pl3, x = ~Meses, y = ~Real, name = '% Casos >30 días Mes 3', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Límite_Inferior, name = 'Límite Inferior', type="scatter" ,line = list(color = 'red', width = 1.5, dashed = "dash")) %>%
  add_trace(y = ~Límite_Superior, name = 'Límite Superior', type="scatter" , line = list(color = 'red', width = 1.5, dashed = 'dash')) %>%
  add_trace(y = ~Amarilla, name = 'Alerta Amarilla', type="scatter" , line = list(color = 'black', width = 1.5, dashed = 'dash')) %>%
  add_annotations(x = Pl3$Meses,
                  y = Pl3$Real,
                  text = round(Pl3$Real*100,1),
                  showarrow = F)%>%
  layout(title = "% Casos >30 días Mes 3 con Límites del Modelo",
         xaxis = list(title = "Meses", showgrid = FALSE),
         yaxis = list (title = "% Casos >30 días Mes 3", tickformat = "%", showgrid = FALSE
                       ))
p_mes3


