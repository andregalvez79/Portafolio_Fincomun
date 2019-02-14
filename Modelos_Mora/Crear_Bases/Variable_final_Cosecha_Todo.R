library(psych)
library(dplyr)
library(ggplot2)
library(zoo)


mora <- read.csv("C:/Users/xiaoping/Desktop/Nueva carpeta/cosechas_redes_todo.csv", 
                 stringsAsFactors = F)

unique(mora$Periodo)

### Quitar las filas falta informacion en periodos por manera de leer el archivo
mora <- mora[-which(mora$Periodo==""),]   


######## Por tipo 2########
unique(mora$Tipo2)
mora$Tipo2[which(mora$Tipo2=="")] <- "Otros"

tipo <- unique(mora$Tipo2)
tipo

mora <- mora %>%
  group_by(Periodo) %>%
  mutate(Especiales  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Especiales" )])/length(Tipo2), 0)) %>%
  mutate( NuevoProd = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="NuevoProd" )])/length(Tipo2), 0)) %>%
  mutate( Nvo_Alcancia = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Nvo_Alcancia" )])/length(Tipo2), 0)) %>%
  mutate(Nvo_Efec = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Nvo_Efec" )])/length(Tipo2), 0)) %>%
  mutate(Recomp_Aut_prod  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Recomp_Aut_prod" )])/length(Tipo2), 0)) %>%
  mutate(Redoc_no_Aut_Efec  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Redoc_no_Aut_Efec" )])/length(Tipo2), 0)) %>%
  mutate(Redoc_no_Aut_Otros  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Redoc_no_Aut_Otros" )])/length(Tipo2), 0))  %>%
  mutate(Redoc_no_Aut_prod  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Redoc_no_Aut_prod" )])/length(Tipo2), 0)) %>%
  mutate(Recomp_Aut_crediv  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Recomp_Aut_crediv" )])/length(Tipo2), 0)) %>%
  mutate(Redoc_no_Aut_MiTienda  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Redoc_no_Aut_MiTienda" )])/length(Tipo2), 0)) %>%
  mutate(Recomp_Aut_MiTienda  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Recomp_Aut_MiTienda" )])/length(Tipo2), 0)) %>%
  mutate(Reestructura  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Reestructura" )])/length(Tipo2), 0)) %>%
  mutate(Nvo_MiTienda  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Nvo_MiTienda" )])/length(Tipo2), 0)) %>%
  mutate(Otros  = ifelse(
    length(Tipo2) != 0,
    length(Tipo2[which(Tipo2=="Otros" )])/length(Tipo2), 0)) 


####### Monto desembolsado total #######
nombres <- c(tipo, "Mto_Desembolso")

mora <- mora %>%
  group_by(Periodo) %>%
  mutate( Mto_Desembolso = sum(MtoDesemb))


####### Tipo 4 #########
tipo4 <- unique(mora$Tipo4)
nombres <- c(nombres, tipo4)

mora <- mora %>%
  group_by(Periodo) %>%
  mutate(Asesor  = ifelse(
    length(Tipo4) != 0,
    length(Tipo4[which(Tipo4=="Asesor" )])/length(Tipo4), 0)) %>%
  mutate(Intermediario  = ifelse(
    length(Tipo4) != 0,
    length(Tipo4[which(Tipo4=="Intermediario" )])/length(Tipo4), 0)) %>%
  mutate(Promotor  = ifelse(
    length(Tipo4) != 0,
    length(Tipo4[which(Tipo4=="Promotor" )])/length(Tipo4), 0)) %>%
  mutate(FzaMovil  = ifelse(
    length(Tipo4) != 0,
    length(Tipo4[which(Tipo4=="FzaMovil" )])/length(Tipo4), 0))


############ Unificaciones, Reestructuras o null ###########
unique(mora$UNIFICA_RIESGOS)
mora$UNIFICA_RIESGOS[which(mora$UNIFICA_RIESGOS=="")] <- "Ninguno"

unificacion <- c("Ninguno", "Reestructuracion", "Unificacion")
unificacion
nombres <- c(nombres, unificacion)

mora <- mora %>%
  group_by(Periodo) %>%
  mutate(Ninguno  = ifelse(
    length(UNIFICA_RIESGOS) != 0,
    length(UNIFICA_RIESGOS[which(UNIFICA_RIESGOS=="Ninguno")])/length(UNIFICA_RIESGOS), 0)) %>%
  mutate(Reestructuracion = ifelse(
    length(UNIFICA_RIESGOS) != 0,
    length(UNIFICA_RIESGOS[which(UNIFICA_RIESGOS=="R" )])/length(UNIFICA_RIESGOS), 0)) %>%
  mutate(Unificacion  = ifelse(
    length(UNIFICA_RIESGOS) != 0,
    length(UNIFICA_RIESGOS[which(UNIFICA_RIESGOS=="U" )])/length(UNIFICA_RIESGOS), 0))


######### Territorios ###########
unique(mora$Nuevo_Territorio)
mora$Nuevo_Territorio[which(mora$Nuevo_Territorio=="")] <- "Otro"

territorio <- c("Otro", "Hidalgo", "Norte", "Oriente",
                "Poniente", "Puebla", "Sur")
nombres <- c(nombres, territorio)

mora <- mora %>%
  group_by(Periodo) %>%
  mutate(Otro  = ifelse(
    length(Nuevo_Territorio) != 0,
    length(Nuevo_Territorio[which(Nuevo_Territorio=="Otro")])/length(Nuevo_Territorio), 0)) %>%
  mutate(Hidalgo = ifelse(
    length(Nuevo_Territorio) != 0,
    length(Nuevo_Territorio[which(Nuevo_Territorio=="HIDALGO" )])/length(Nuevo_Territorio), 0)) %>%
  mutate(Norte  = ifelse(
    length(Nuevo_Territorio) != 0,
    length(Nuevo_Territorio[which(Nuevo_Territorio=="NORTE" )])/length(Nuevo_Territorio), 0)) %>%
  mutate(Oriente  = ifelse(
    length(Nuevo_Territorio) != 0,
    length(Nuevo_Territorio[which(Nuevo_Territorio=="ORIENTE" )])/length(Nuevo_Territorio), 0)) %>%
  mutate(Poniente  = ifelse(
    length(Nuevo_Territorio) != 0,
    length(Nuevo_Territorio[which(Nuevo_Territorio=="PONIENTE" )])/length(Nuevo_Territorio), 0)) %>%
  mutate(Puebla  = ifelse(
    length(Nuevo_Territorio) != 0,
    length(Nuevo_Territorio[which(Nuevo_Territorio=="PUEBLA" )])/length(Nuevo_Territorio), 0)) %>%
  mutate(Sur  = ifelse(
    length(Nuevo_Territorio) != 0,
    length(Nuevo_Territorio[which(Nuevo_Territorio=="SUR" )])/length(Nuevo_Territorio), 0))


########### Oficina Asesor/Sucursales ###############
unique(mora$Oficina_Asesor)
sucursal <- paste("suc", unique(mora$Oficina_Asesor), sep = "")
nombres <- c(nombres, sucursal)


mora <- mora %>%
  group_by(Periodo) %>%
  mutate(suc11  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==11)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc14  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==14)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc21  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==21)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc22  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==22)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc24  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==24)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc25  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==25)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc26  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==26)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc27  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==27)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc28  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==28)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc31  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==31)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc32  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==32)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc41  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==41)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc42  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==42)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc43  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==43)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc44  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==44)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc45  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==45)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc46  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==46)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc47  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==47)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc48  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==48)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc49  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==49)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc51  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==51)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc52  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==52)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc53  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==53)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc54  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==54)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc55  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==55)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc56  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==56)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc57  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==57)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc58  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==58)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc61  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==61)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc72  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==72)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc73  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==73)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc75  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==75)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc81  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==81)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc83  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==83)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc84  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==84)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc85  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==85)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc87  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==87)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc91  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==91)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc92  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==92)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc93  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==93)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc95  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==95)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc101  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==101)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc102  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==102)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc104  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==104)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc111  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==111)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc112  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==112)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc113  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==113)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc114  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==114)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc115  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==115)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc121  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==121)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc122  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==122)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc123  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==123)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc131  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==131)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc133  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==133)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc134  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==134)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc151  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==151)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc160  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==160)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc211  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==211)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc215  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==215)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc221  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==221)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc311  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==311)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc312  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==312)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc313  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==313)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc321  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==321)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc331  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==331)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc332  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==332)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc336  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==336)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc341  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==341)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc342  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==342)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc343  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==343)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc344  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==344)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc345  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==345)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc346  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==346)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc347  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==347)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc350  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==350)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc352  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==352)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc550  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==550)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc561  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==561)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc562  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==562)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc563  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==563)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc564  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==564)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc565  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==565)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc566  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==566)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc601  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==601)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc602  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==602)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc603  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==603)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc604  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==604)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc621  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==621)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc622  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==622)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc623  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==623)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc624  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==624)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc625  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==625)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc626  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==626)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc627  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==627)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc628  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==628)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc629  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==629)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc641  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==641)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc642  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==642)])/length(Oficina_Asesor), 0)) %>%
  mutate(suc981  = ifelse(
    length(Oficina_Asesor) != 0,
    length(Oficina_Asesor[which(Oficina_Asesor==981)])/length(Oficina_Asesor), 0))

################

######### Etiqueta Bueno y Malo ##########
unique(mora$EtiquetaBM)

nombres <- c(nombres, "bueno", "malo")


mora <- mora %>%
  group_by(Periodo) %>%
  mutate(bueno  = ifelse(
    length(EtiquetaBM) != 0,
    length(EtiquetaBM[which(EtiquetaBM=="BUENO")])/length(EtiquetaBM), 0)) %>%
  mutate(malo = ifelse(
    length(EtiquetaBM) != 0,
    length(EtiquetaBM[which(EtiquetaBM=="MALO")])/length(EtiquetaBM), 0))


######### Campana ##########
unique(mora$Campana)
mora$Campana[which(mora$Campana=="")] <- "Nada"
mora$Campana[grep("icc", mora$Campana)] <- "ICC"
mora$Campana[grep("Rec", mora$Campana)] <- "REC"

nombres <- c(nombres, unique(mora$Campana))

mora <- mora %>%
  group_by(Periodo) %>%
  mutate(Nada  = ifelse(
    length(Campana) != 0,
    length(Campana[which(Campana=="Nada")])/length(Campana), 0)) %>%
  mutate(ICC = ifelse(
    length(Campana) != 0,
    length(Campana[which(Campana=="ICC")])/length(Campana), 0)) %>%
  mutate(REC = ifelse(
    length(Campana) != 0,
    length(Campana[which(Campana=="REC")])/length(Campana), 0))


######### Ordenar ############
C <- mora[, c("Periodo", nombres)]
C <- C[!duplicated(C$Periodo),]

C$Periodo <- gsub("Mes_", "", C$Periodo)
C$Periodo <- gsub("_", "-", C$Periodo)

C$Periodo <- format(as.Date(as.yearmon(C$Periodo, "%m-%y")), "%y-%m")

C <- C[order(C$Periodo),]


# write.csv(C, "C:/Users/xiaoping/Desktop/Nueva carpeta/Base.CM.Todo.csv")


