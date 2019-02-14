##### para mes 1#####
mora <- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Crear_Bases\\Redes_Suc\\BaseCosechas\\cosechas_redes_nvos.csv")
mora$NoCred <- as.numeric(mora$NoCred)
mora$NoMMes01 <- as.numeric(mora$NoMMes01)
mora$CVMes01 <- as.numeric(mora$CVMes01)
mora$NoCred[is.na(mora$NoCred)] <- 0
mora$NoMMes01[is.na(mora$NoMMes01)] <- 0
mora$CVMes01[is.na(mora$CVMes01)] <- 0

str(mora$Periodo)
moraNommes1<-aggregate(mora$NoMMes01, by=list(Periodo=mora$Periodo), FUN=sum)
moraqcred<-aggregate(mora$NoCred, by=list(Periodo=mora$Periodo), FUN=sum)
moraNommes1$porcentajeM1 <- moraNommes1$x /moraqcred$x
moraNommes1<-moraNommes1[-c(51),]
moraNommes1<-moraNommes1[-c(46),]
moraNommes1<-moraNommes1[-c(1),]
moraNommes1 <- select(moraNommes1,-x)
moraNommes1



######
###para mes 2####

mora$NoCred <- as.numeric(mora$NoCred)
mora$NoMMes02 <- as.numeric(mora$NoMMes02)
mora$CVMes02 <- as.numeric(mora$CVMes02)
mora$NoCred[is.na(mora$NoCred)] <- 0
mora$NoMMes02[is.na(mora$NoMMes02)] <- 0
mora$CVMes02[is.na(mora$CVMes02)] <- 0


str(mora$Periodo)
moraNommes2<-aggregate(mora$NoMMes02, by=list(Periodo=mora$Periodo), FUN=sum)
moraqcred<-aggregate(mora$NoCred, by=list(Periodo=mora$Periodo), FUN=sum)
moraNommes2$porcentajeM2 <- moraNommes2$x /moraqcred$x
moraNommes2<-moraNommes2[-c(51),]
moraNommes2<-moraNommes2[-c(46),]
moraNommes2<-moraNommes2[-c(1),]
moraNommes2 <- select(moraNommes2,-x)
moraNommes2 <- select(moraNommes2,-Periodo)
moraNommes2




######
###para mes 3####

mora$NoCred <- as.numeric(mora$NoCred)
mora$NoMMes03 <- as.numeric(mora$NoMMes03)
mora$CVMes03 <- as.numeric(mora$CVMes03)
mora$NoCred[is.na(mora$NoCred)] <- 0
mora$NoMMes03[is.na(mora$NoMMes03)] <- 0
mora$CVMes03[is.na(mora$CVMes03)] <- 0


str(mora$Periodo)
moraNommes3<-aggregate(mora$NoMMes03, by=list(Periodo=mora$Periodo), FUN=sum)
moraqcred<-aggregate(mora$NoCred, by=list(Periodo=mora$Periodo), FUN=sum)
moraNommes3$porcentajeM3 <- moraNommes3$x /moraqcred$x
moraNommes3<-moraNommes3[-c(51),]
moraNommes3<-moraNommes3[-c(46),]
moraNommes3<-moraNommes3[-c(1),]
moraNommes3 <- select(moraNommes3,-x)
moraNommes3 <- select(moraNommes3,-Periodo)
moraNommes3

Meses<- read.csv("C:\\Users\\andrevargas\\Documents\\Modelo_mora\\BaseCosechas\\Base.CM.Nuevos.csv")

#describe(Meses)
names(Meses)
colnames(Meses)[17] <- "OtroTer"
#colnames(Meses)[14] <- "SinRes"
colnames(Meses)[122] <- "NoCamp"
write.csv(Meses, "C:\\Users\\andrevargas\\Documents\\Modelo_mora\\BaseCosechas\\Base.CM.Nuevos.csv")
dep <- cbind(moraNommes2, moraNommes1)
dep <- cbind(dep, moraNommes3)
write.csv(dep, "C:\\Users\\andrevargas\\Documents\\Modelo_mora\\Crear_Bases\\Redes_Suc\\BaseCosechas\\dep.nue.csv")



