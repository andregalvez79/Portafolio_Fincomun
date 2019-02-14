Meses_17<- rbind(Mes_01_17,Mes_02_17)
Meses_17<- rbind(Meses_17,Mes_03_17)
Meses_17<- rbind(Meses_17,Mes_04_17)
Meses_17<- rbind(Meses_17,Mes_05_17)
Meses_17<- rbind(Meses_17,Mes_06_17)
Meses_17<- rbind(Meses_17,Mes_07_17)
Meses_17<- rbind(Meses_17,Mes_08_17)
Meses_17<- rbind(Meses_17,Mes_09_17)
Meses_17<- rbind(Meses_17,Mes_10_17)
Meses_17<- rbind(Meses_17,Mes_11_17)
Meses_17<- rbind(Meses_17,Mes_12_17)



write.csv(Meses_17,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Crear_Bases\\Nuevos\\2017\\Meses_17.csv")

####merge de totales 

Meses_14 <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Crear_Bases\\Nuevos\\2014\\Meses_14.csv")

#Meses_14$Reestructuracion[is.na(Meses_14$Reestructuracion)] <-0
#Meses_14$Unificacion[is.na(Meses_14$Unificacion)] <-0
#Meses_14$suc642[is.na(Meses_14$suc642)] <-0
#Meses_14$suc981[is.na(Meses_14$suc981)] <-0
#Meses_18$suc981[is.na(Meses_18$suc981)] <-0
#Meses_18$suc802[is.na(Meses_18$suc802)] <-0
#write.csv(Meses_14,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Meses_14.csv")

Meses_15 <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Crear_Bases\\Nuevos\\2015\\Meses_15.csv")
Meses_16 <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Crear_Bases\\Nuevos\\2016\\Meses_16.csv")
Meses_17 <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Crear_Bases\\Nuevos\\2017\\Meses_17.csv")
Meses_18 <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Crear_Bases\\Nuevos\\2018\\Meses_18.csv")

Meses <- rbind(Meses_14, Meses_15)
Meses <- rbind(Meses, Meses_16)
Meses <- rbind(Meses, Meses_17)
Meses <- rbind(Meses, Meses_18)
Meses <- select(Meses,-X)
Meses <- cbind(Meses, moraNommes1)
Meses <- cbind(Meses, moraNommes2)
Meses <- cbind(Meses, moraNommes3)

summary(Meses)

write.csv(Meses,"C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Meses_Nuevos.csv")
