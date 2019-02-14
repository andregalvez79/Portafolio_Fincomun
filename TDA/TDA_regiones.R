### TDA por territorios

library(rlang)
library(readxl)
library(dplyr)

setwd("C:\\Users\\andrevargas\\Documents\\TDA\\Proyecciones\\")
##### Archivos base ###
cnbv <- read.csv("CNBV_Noviembre_18.csv",
                   stringsAsFactors = F) ### Mes de calculo

fcv <- read.delim("FCV_dic_2018.txt",
                  stringsAsFactors = F)
fcv <- dplyr::select(fcv, "SocioRea","Giro")
fcv <- unique(fcv, SocioRea = FALSE,  MARGIN = 2, fromLast = FALSE, nmax = NA)
fcv <- distinct(fcv, SocioRea, .keep_all= TRUE)

Catalogogiros <- read_excel("Catalogo Giros.xlsx")
Catalogogiros <- distinct(Catalogogiros, Actividad, .keep_all= TRUE)
fcv <- fcv %>% left_join(Catalogogiros, by = c("Giro"="Actividad"))

CatalogoNomina <- read_excel("CONVENIOS_CLASIFICACION.xlsx")
Sucursal <- read_excel("sucursal.xlsx")

CodigoP <-  read_excel("Codigos postales2.xlsx")
CodigoP <- unique(CodigoP, Codigo = FALSE)
CodigoP <- distinct(CodigoP, codigo, .keep_all= TRUE)

Domicilios <- read.csv("Domicilios2.csv")
names(Domicilios)
Domicilios <- dplyr::select(Domicilios, "CLIENTE", "COD_POS")
Domicilios <- unique(Domicilios, CLIENTE = TRUE )
Domicilios <- Domicilios  %>%  left_join(CodigoP, by = c("COD_POS" = "codigo"))


names(cnbv)[34] = "Provision1"

#hacer cruces para calificar
cnbv <- cnbv %>% left_join(CatalogoNomina, by = c("Nro_Prest"="N??mero de Cr??dito")) #problema con acentos
#favor de no poner!
cnbv <- cnbv %>% left_join(Domicilios, by = c("Socio"="CLIENTE"))
cnbv <- cnbv %>% left_join(fcv, by = c("Socio"="SocioRea"))
cnbv <- cnbv %>% left_join(Sucursal, by = c("Sucursal"="sucursal"))

#liberamos memoria removiendo a las bases que ya no utilizaremos
rm(fcv,Catalogogiros,CatalogoNomina,Domicilios,CodigoP,Sucursal)

names(cnbv)
##### Calificacisn cartera Vigente y no vigente
cnbv <- cnbv %>% 
  mutate(cartera = if_else(Provision1 < 1, "cartera", "no cartera"),
         Cartera_Total = MtoVig2 + MtoVenc2 + IntVig2 + IntVen2,
         Cartera_Ven = MtoVenc2 + IntVen2,
         Cartera_Vig = MtoVig2 + IntVig2,
         Par1 = if_else(cnbv$No_DiasAtraso>=1, "Par1", "No ap"),#revisar aqui
         
         Par30 = if_else(cnbv$No_DiasAtraso>=30, "Par30", "No ap"),
         Par60 = if_else(cnbv$No_DiasAtraso>=60, "Par60", "No ap"),
         Par90 = if_else(cnbv$No_DiasAtraso>=90, "Par90", "No ap"))

#Total FinComzn#
cnbvCartera <- cnbv  %>% filter(cartera == "cartera")

Par1FC <- cnbvCartera %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
  Par1FC<- Par1FC %>% filter(Par1 == "Par1")
  Par1FC<- Par1FC %>% select("sum")

Par30FC <- cnbvCartera %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30FC<- Par30FC %>% filter(Par30 == "Par30")
Par30FC<- Par30FC %>% select("sum")

Par60FC <- cnbvCartera %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60FC<- Par60FC %>% filter(Par60 == "Par60")
Par60FC<- Par60FC %>% select("sum")

Par90FC <- cnbvCartera %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90FC<- Par90FC %>% filter(Par90 == "Par90")
Par90FC<- Par90FC %>% select("sum")

IndicadorFC <- rbind(sum(cnbvCartera$Cartera_Total),sum(cnbvCartera$Cartera_Ven),
                     sum(cnbvCartera$Cartera_Vig), Par1FC, Par30FC, Par60FC, Par90FC, sum(cnbvCartera$Total_Provision),
                     sum(cnbvCartera$Total_Provision_mas_IVA),length(cnbvCartera$Total_Provision_mas_IVA))


# Nuevos cartera FC
NuevosFC<-cnbvCartera %>%
  mutate(Status= if_else(origen=="Nuevo", "Nuevo","Recompra"))

nuevosfc <- NuevosFC %>% filter(Status == "Nuevo")

Par1nfc <- nuevosfc %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1nfc<- Par1nfc %>% filter(Par1 == "Par1")
Par1nfc<- Par1nfc %>% select("sum")

Par30nfc <- nuevosfc %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30nfc<- Par30nfc %>% filter(Par30 == "Par30")
Par30nfc<- Par30nfc %>% select("sum")

Par60nfc <- nuevosfc %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60nfc<- Par60nfc %>% filter(Par60 == "Par60")
Par60nfc<- Par60nfc %>% select("sum")

Par90nfc <- nuevosfc %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90nfc<- Par90nfc %>% filter(Par90 == "Par90")
Par90nfc<- Par90nfc %>% select("sum")

Indicadornfc <- rbind(sum(nuevosfc$Cartera_Total),sum(nuevosfc$Cartera_Ven),
                     sum(nuevosfc$Cartera_Vig), Par1nfc, Par30nfc, Par60nfc, Par90nfc, sum(nuevosfc$Total_Provision),
                     sum(nuevosfc$Total_Provision_mas_IVA),length(nuevosfc$Total_Provision_mas_IVA))

#Recompra
recomprafc <- NuevosFC %>% filter(Status != "Nuevo")

Par1rfc <- recomprafc %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1rfc<- Par1rfc %>% filter(Par1 == "Par1")
Par1rfc<- Par1rfc %>% select("sum")

Par30rfc <- recomprafc %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30rfc<- Par30rfc %>% filter(Par30 == "Par30")
Par30rfc<- Par30rfc %>% select("sum")

Par60rfc <- recomprafc %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60rfc<- Par60rfc %>% filter(Par60 == "Par60")
Par60rfc<- Par60rfc %>% select("sum")

Par90rfc <- recomprafc %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90rfc<- Par90rfc %>% filter(Par90 == "Par90")
Par90rfc<- Par90rfc %>% select("sum")

Indicadorrfc <- rbind(sum(recomprafc$Cartera_Total),sum(recomprafc$Cartera_Ven),
                      sum(recomprafc$Cartera_Vig), Par1rfc, Par30rfc, Par60rfc, Par90rfc, sum(recomprafc$Total_Provision),
                      sum(recomprafc$Total_Provision_mas_IVA),length(recomprafc$Total_Provision_mas_IVA))


FC <- data.frame(Cartera= c("Cartera Total","Cartera Vencida", "Cartera Vigente", "Par1", "Par30", "Par 60","Par90","EPRC", "EPRC mas IVA", "Total de Criditos"),FinComun= IndicadorFC, Nuevos_fc =Indicadornfc, Recompras_FC=Indicadorrfc)
colnames(FC) <-(c("Categorma","FinComzn","Nuevos_fc","Recompras_FC"))

resumen<- write.csv(FC,"C:/Users/ivazquez/Desktop/TDAs/octubre/experimento.csv")

# No me aparecen bien los PAR
cnbvCartera2<- select(cnbvCartera, "Sucursal","sub_aplic","nro_prest","Cartera_Total","Cartera_Ven","Cartera_Vig","Par1","Par30","Par60","Par90","Oficina","Total_Provision","Total_Provision_mas_IVA","Etiqueta_1_3","origen")

write.csv(cnbvCartera2,"C:/Users/ivazquez/Desktop/TDAs/octubre/cnbvCondensado.csv")
## Por territorios
Territorio<-cnbvCartera %>%
  group_by(Oficina) %>%
  summarise(sum=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1FC),
            par30=sum(Par30FC),
            par60=sum(Par60FC),
            par90=sum(Par90FC),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(Territorio)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(Territorio,"C:/Users/ivazquez/Desktop/TDAs/octubre/exp_terr.csv")

# por territorios y Nuevos cartera FC
territorionew<-nuevosfc%>%
  group_by(Oficina) %>%
  summarise(Cartot=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1nfc),
            par30=sum(Par30nfc),
            par60=sum(Par60nfc),
            par90=sum(Par90nfc),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(territorionew)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(territorionew,"C:/Users/ivazquez/Desktop/TDAs/octubre/exp_terr_Nuevos2.csv")

# por territorios y recompra cartera FC
territoriore<-recomprafc%>%
  group_by(Oficina) %>%
  summarise(Cartot=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1FC),
            par30=sum(Par30FC),
            par60=sum(Par60FC),
            par90=sum(Par90FC),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(territoriore)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(territoriore,"C:/Users/ivazquez/Desktop/TDAs/octubre/exp_terr_Recom.csv")


### Llamar a los castigados
castigos<- read_excel("C:/Users/ivazquez/Desktop/Provisionados/1810 Castigos Oct 18.xlsx")
#seleccion de las variables para castigo
cnbv2 <- cnbv %>% select("nro_prest","sub_aplic","Total_Provision","Oficina","origen","Etiqueta_1_3","Convenio.x")

CastigosE <- castigos %>% left_join(cnbv2, by = c("nro-prest"="nro_prest"))

# por nuevos y recompra
castigos<-CastigosE%>%
  group_by(Oficina) %>%
  summarise(cas_tot=sum(Total_Provision))

castigosRe <- CastigosE %>% filter(origen != "Nuevo")
castigosRe<-castigosRe%>%
  group_by(Oficina) %>%
  summarise(cas_re=sum(Total_Provision))

castigosNew <- CastigosE %>% filter(origen == "Nuevo")
castigosNew<-castigosNew%>%
  group_by(Oficina) %>%
  summarise(cas_New=sum(Total_Provision))

castigados<-castigos %>% left_join(castigosNew, by = "Oficina")
castigados<-castigados %>% left_join(castigosRe, by = "Oficina")
write.csv(castigados,"C:/Users/ivazquez/Desktop/TDAs/octubre/exp_castigados.csv")






#
##
### TDA C2 & C3
cnbv_C2_C3 <- cnbvCartera %>% filter(Etiqueta_1_3=="Consumo gobierno")

#desgloce
Par1Cs <- cnbv_C2_C3 %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1Cs<- Par1Cs %>% filter(Par1 == "Par1")
Par1Cs<- Par1Cs %>% select("sum")

Par30Cs <- cnbv_C2_C3 %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30Cs<- Par30Cs %>% filter(Par30 == "Par30")
Par30Cs<- Par30Cs %>% select("sum")

Par60Cs <- cnbv_C2_C3 %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60Cs<- Par60Cs %>% filter(Par60 == "Par60")
Par60Cs<- Par60Cs %>% select("sum")

Par90Cs <- cnbv_C2_C3 %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90Cs<- Par90Cs %>% filter(Par90 == "Par90")
Par90Cs<- Par90Cs %>% select("sum")

IndicadorCs <- rbind(sum(cnbv_C2_C3$Cartera_Total),sum(cnbv_C2_C3$Cartera_Ven),
                     sum(cnbv_C2_C3$Cartera_Vig), Par1Cs, Par30Cs, Par60Cs, Par90Cs, sum(cnbv_C2_C3$Total_Provision),
                     sum(cnbv_C2_C3$Total_Provision_mas_IVA),length(cnbv_C2_C3$Total_Provision_mas_IVA))


# Nuevos cartera C2 & C3
NuevosCs<-cnbv_C2_C3 %>%
  mutate(Status= if_else(origen=="Nuevo", "Nuevo","Recompra"))

nuevosCs <- NuevosCs %>% filter(Status == "Nuevo")

Par1ncs <- nuevosCs %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1ncs<- Par1ncs %>% filter(Par1 == "Par1")
Par1ncs<- Par1ncs %>% select("sum")

Par30ncs <- nuevosCs %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30ncs<- Par30ncs %>% filter(Par30 == "Par30")
Par30ncs<- Par30ncs %>% select("sum")

Par60ncs <- nuevosCs %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60ncs<- Par60ncs %>% filter(Par60 == "Par60")
Par60ncs<- Par60ncs %>% select("sum")

Par90ncs <- nuevosCs %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90ncs<- Par90ncs %>% filter(Par90 == "Par90")
Par90ncs<- Par90ncs %>% select("sum")

Indicadorncs <- rbind(sum(nuevosCs$Cartera_Total),sum(nuevosCs$Cartera_Ven),
                      sum(nuevosCs$Cartera_Vig), Par1ncs, Par30ncs, Par60ncs, 0, sum(nuevosCs$Total_Provision),
                      sum(nuevosCs$Total_Provision_mas_IVA),length(nuevosCs$Total_Provision_mas_IVA))

#Recompra
recompracs <- NuevosCs %>% filter(Status != "Nuevo")

Par1rcs <- recompracs %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1rcs<- Par1rcs %>% filter(Par1 == "Par1")
Par1rcs<- Par1rcs %>% select("sum")

Par30rcs <- recompracs %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30rcs<- Par30rcs %>% filter(Par30 == "Par30")
Par30rcs<- Par30rcs %>% select("sum")

Par60rcs <- recompracs %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60rcs<- Par60rcs %>% filter(Par60 == "Par60")
Par60rcs<- Par60rcs %>% select("sum")

Par90rcs <- recompracs %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90rcs<- Par90rcs %>% filter(Par90 == "Par90")
Par90rcs<- Par90rcs %>% select("sum")

Indicadorrcs <- rbind(sum(recompracs$Cartera_Total),sum(recompracs$Cartera_Ven),
                      sum(recompracs$Cartera_Vig), Par1rcs, Par30rcs, Par60rcs, Par90rcs, sum(recompracs$Total_Provision),
                      sum(recompracs$Total_Provision_mas_IVA),length(recompracs$Total_Provision_mas_IVA))


Cs <- data.frame(Cartera= c("Cartera Total","Cartera Vencida", "Cartera Vigente", "Par1", "Par30", "Par 60","Par90","EPRC", "EPRC mas IVA", "Total de Criditos"),C2_C3= IndicadorCs, Nuevos_fc =Indicadorncs, Recompras_FC=Indicadorrcs)
colnames(Cs) <-(c("Categorma","C2_C3","Nuevos_cs","Recompras_Cs"))

resumen<- write.csv(Cs,"C:/Users/ivazquez/Desktop/TDAs/octubre/C2_C3.csv")


#
## Por territorios
TerritorioCs<- cnbv_C2_C3%>%
  group_by(Oficina) %>%
  summarise(sum=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1Cs),
            par30=sum(Par30Cs),
            par60=sum(Par60Cs),
            par90=sum(Par90Cs),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(TerritorioCs)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1cs","Par30cs","Par60cs","Par90cs","EPRC","EPRC_iva","Creditos"))

write.csv(TerritorioCs,"C:/Users/ivazquez/Desktop/TDAs/octubre/territorioCs.csv")

# por territorios y Nuevos cartera C2 & C3
territoriocn<-nuevosCs%>%
  group_by(Oficina) %>%
  summarise(Cartot=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1ncs),
            par30=sum(Par30ncs),
            par60=sum(Par60ncs),
            par90=0,
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(territoriocn)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(territoriocn,"C:/Users/ivazquez/Desktop/TDAs/octubre/terrcs_Nuevos.csv")

# por territorios y recompra cartera C2 & C3
territoriocre<-recompracs%>%
  group_by(Oficina) %>%
  summarise(Cartot=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1rcs),
            par30=sum(Par30rcs),
            par60=sum(Par60rcs),
            par90=sum(Par90rcs),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(territoriocre)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(territoriocre,"C:/Users/ivazquez/Desktop/TDAs/octubre/terrc_Recom.csv")


### Llamar a los castigados
#hacer el filtro de para los que estan castigados por C2 & C3 
CastigosC<-CastigosE%>% filter(Etiqueta_1_3 == "Consumo gobierno")
#castigos por territorio
castigoscs<-CastigosC%>%
  group_by(Oficina) %>%
  summarise(cas_tot=sum(Total_Provision))

castigosRec <- CastigosC %>% filter(origen != "Nuevo")
castigosRec<-castigosRec%>%
  group_by(Oficina) %>%
  summarise(cas_re=sum(Total_Provision))

castigosNewc <- CastigosC %>% filter(origen == "Nuevo")
castigosNewc<-castigosNewc%>%
  group_by(Oficina) %>%
  summarise(cas_New=sum(Total_Provision))

castigadosc<-castigoscs %>% left_join(castigosNewc, by = "Oficina")
castigadosc<-castigadosc %>% left_join(castigosRec, by = "Oficina")
write.csv(castigadosc,"C:/Users/ivazquez/Desktop/TDAs/octubre/castigadosCs.csv")


#
##
### TDA Comercial, hacer filtros por Sucursal != 2 & sub_aplic != 4350
cnbvCarteraPop <- cnbvCartera %>% filter(Sucursal != 2 & sub_aplic != 4350)

#desgloce
Par1P <- cnbvCarteraPop %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1P<- Par1P %>% filter(Par1 == "Par1")
Par1P<- Par1P %>% select("sum")

Par30P <- cnbvCarteraPop %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30P<- Par30P %>% filter(Par30 == "Par30")
Par30P<- Par30P %>% select("sum")

Par60P <- cnbvCarteraPop %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60P<- Par60P %>% filter(Par60 == "Par60")
Par60P<- Par60P %>% select("sum")

Par90P <- cnbvCarteraPop %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90P<- Par90P %>% filter(Par90 == "Par90")
Par90P<- Par90P %>% select("sum")

IndicadorPop <- rbind(sum(cnbvCarteraPop$Cartera_Total),sum(cnbvCarteraPop$Cartera_Ven),
                      sum(cnbvCarteraPop$Cartera_Vig), Par1P, Par30P, Par60P, Par90P, sum(cnbvCarteraPop$Total_Provision),
                      sum(cnbvCarteraPop$Total_Provision_mas_IVA), length(cnbvCarteraPop$Total_Provision_mas_IVA))

# Nuevos cartera Popular
NuevosPop<-cnbvCarteraPop %>%
  mutate(Status= if_else(origen=="Nuevo", "Nuevo","Recompra"))

nuevospop <- NuevosPop %>% filter(Status == "Nuevo")

Par1np <- nuevospop %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1np<- Par1np %>% filter(Par1 == "Par1")
Par1np<- Par1np %>% select("sum")

Par30np <- nuevospop %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30np<- Par30np %>% filter(Par30 == "Par30")
Par30np<- Par30np %>% select("sum")

Par60np <- nuevospop %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60np<- Par60np %>% filter(Par60 == "Par60")
Par60np<- Par60np %>% select("sum")

Par90np <- nuevospop %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90np<- Par90np %>% filter(Par90 == "Par90")
Par90np<- Par90np %>% select("sum")

Indicadornp <- rbind(sum(nuevospop$Cartera_Total),sum(nuevospop$Cartera_Ven),
                      sum(nuevospop$Cartera_Vig), Par1np, Par30np, Par60np, Par90np, sum(nuevospop$Total_Provision),
                      sum(nuevospop$Total_Provision_mas_IVA),length(nuevospop$Total_Provision_mas_IVA))

#Recompra
recomprap <- NuevosPop %>% filter(Status != "Nuevo")

Par1rp <- recomprap %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1rp<- Par1rp %>% filter(Par1 == "Par1")
Par1rp<- Par1rp %>% select("sum")

Par30rp <- recomprap %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30rp<- Par30rp %>% filter(Par30 == "Par30")
Par30rp<- Par30rp %>% select("sum")

Par60rp <- recomprap %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60rp<- Par60rp %>% filter(Par60 == "Par60")
Par60rp<- Par60rp %>% select("sum")

Par90rp <- recomprap %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90rp<- Par90rp %>% filter(Par90 == "Par90")
Par90rp<- Par90rp %>% select("sum")

Indicadorrp <- rbind(sum(recomprap$Cartera_Total),sum(recomprap$Cartera_Ven),
                      sum(recomprap$Cartera_Vig), Par1rp, Par30rp, Par60rp, Par90rp, sum(recomprap$Total_Provision),
                      sum(recomprap$Total_Provision_mas_IVA),length(recomprap$Total_Provision_mas_IVA))


Popular <- data.frame(Cartera= c("Cartera Total","Cartera Vencida", "Cartera Vigente", "Par1", "Par30", "Par 60","Par90","EPRC", "EPRC mas IVA", "Total de Criditos"),Popular= IndicadorPop, Nuevos_pop =Indicadornp, Recompras_FC=Indicadorrp)
colnames(Popular) <-(c("Categorma","Popular","Nuevos_pop","Recompras_pop"))

resumen<- write.csv(Popular,"C:/Users/ivazquez/Desktop/TDAs/octubre/Popular.csv")

#por territorio Popular
territorioPop<-cnbvCarteraPop%>%
  group_by(Oficina) %>%
  summarise(Cartot=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1P),
            par30=sum(Par30P),
            par60=sum(Par60P),
            par90=sum(Par90P),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(territorioPop)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(territorioPop,"C:/Users/ivazquez/Desktop/TDAs/octubre/TerritoriosPop.csv")

# por territorios y Nuevos cartera FC
territorioPnew<-nuevospop%>%
  group_by(Oficina) %>%
  summarise(Cartot=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1np),
            par30=sum(Par30np),
            par60=sum(Par60np),
            par90=sum(Par90np),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(territorioPnew)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(territorioPnew,"C:/Users/ivazquez/Desktop/TDAs/octubre/terr_Pop_N.csv")

# por territorios y recompra cartera FC
territoriopre<-recomprap%>%
  group_by(Oficina) %>%
  summarise(Cartot=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1rp),
            par30=sum(Par30rp),
            par60=sum(Par60rp),
            par90=sum(Par90rp),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(territoriopre)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(territoriopre,"C:/Users/ivazquez/Desktop/TDAs/octubre/terr_Pop_Recom.csv")

#no salieron las cifras de los PARs por lo que se hizo manualmente
resumen<- write.csv(cnbvCarteraPop,"C:/Users/ivazquez/Desktop/TDAs/octubre/cnbvPop.csv")


### Sacar los Castigos contables
#hacer el filtro de para los que estan castigados en Popular
CastigosP<-CastigosE%>% filter(Sucursal != 2 & sub_aplic != 4350)

#castigos totales
castigostp<- CastigosP %>% summarise(total=sum(Total_Provision))

#castigo de recompra
castigosreP <- CastigosP %>% filter(origen != "Nuevo")
castigosrep<- castigosreP %>% summarise(total=sum(Total_Provision))

#castigos de nuevos
castigosnP <- CastigosP %>% filter(origen == "Nuevo")
castigosnp<- castigosnP %>% summarise(total=sum(Total_Provision))

#agrupando
CastigosPop <- rbind(castigostp,castigosrep,castigosnp)

resumen<- write.csv(CastigosPop,"C:/Users/ivazquez/Desktop/TDAs/octubre/Castigados_Popular.csv")

#castigos por territorio
castigosp<-CastigosP%>%
  group_by(Oficina) %>%
  summarise(cas_tot=sum(Total_Provision))

# castigos por territorio recompra
CastigosRep<-castigosreP%>%
  group_by(Oficina) %>%
  summarise(cas_re=sum(Total_Provision))

# por territorio nuevos
castigosnewP<-castigosnP%>%
  group_by(Oficina) %>%
  summarise(cas_New=sum(Total_Provision))

castigadosP<-castigosp %>% left_join(castigosnewP, by = "Oficina")
castigadosP<-castigadosP %>% left_join(CastigosRep, by = "Oficina")

write.csv(castigadosP,"C:/Users/ivazquez/Desktop/TDAs/octubre/castigadosPopT.csv")


#
##
### TDA de Bimbo y Privados
cnbvCarteraNom1 <- cnbvCartera %>% filter(Convenio.x == "BIMBO")
cnbvCarteraNom2 <- cnbvCartera %>% filter(Convenio.x == "OTROS")

cnbvCarteraNom <- bind_rows(cnbvCarteraNom1,cnbvCarteraNom2)

rm("cnbvCarteraNom1","cnbvCarteraNom2")

Par1Nom <- cnbvCarteraNom %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1Nom<- Par1Nom %>% filter(Par1 == "Par1")
Par1Nom<- Par1Nom %>% select("sum")

Par30Nom <- cnbvCarteraNom %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30Nom<- Par30Nom %>% filter(Par30 == "Par30")
Par30Nom<- Par30Nom %>% select("sum")

Par60Nom <- cnbvCarteraNom %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60Nom<- Par60Nom %>% filter(Par60 == "Par60")
Par60Nom<- Par60Nom %>% select("sum")


Par90Nom <- cnbvCarteraNom %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90Nom<- Par90Nom %>% filter(Par90 == "Par90")
Par90Nom<- Par90Nom %>% select("sum")

IndicadorNom <- rbind(sum(cnbvCarteraNom$Cartera_Total),sum(cnbvCarteraNom$Cartera_Ven),
                     sum(cnbvCarteraNom$Cartera_Vig), Par1Nom,Par30Nom, Par60Nom, Par90Nom, sum(cnbvCarteraNom$Total_Provision),
                     sum(cnbvCarteraNom$Total_Provision_mas_IVA),length(cnbvCarteraNom$Total_Provision_mas_IVA))

# Nuevos cartera Nomina
Nuevosnom<-cnbvCarteraNom %>%
  mutate(Status= if_else(origen=="Nuevo", "Nuevo","Recompra"))

nuevosnom <- Nuevosnom %>% filter(Status == "Nuevo")

Par1nnom <- nuevosnom %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1nnom<- Par1nnom %>% filter(Par1 == "Par1")
Par1nnom<- Par1nnom %>% select("sum")

Par30nnom <- nuevosnom %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30nnom<- Par30nnom %>% filter(Par30 == "Par30")
Par30nnom<- Par30nnom %>% select("sum")

Par60nnom <- nuevosnom %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60nnom<- Par60nnom %>% filter(Par60 == "Par60")
Par60nnom<- Par60nnom %>% select("sum")

Par90nnom <- nuevosnom %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90nnom<- Par90nnom %>% filter(Par90 == "Par90")
Par90nnom<- Par90nnom %>% select("sum")

Indicadornnom <- rbind(sum(nuevosnom$Cartera_Total),sum(nuevosnom$Cartera_Ven),
                      sum(nuevosnom$Cartera_Vig), Par1nnom, Par30nnom, Par60nnom, Par90nnom, sum(nuevosnom$Total_Provision),
                      sum(nuevosnom$Total_Provision_mas_IVA),length(nuevosnom$Total_Provision_mas_IVA))

#Recompra
recompranom <- Nuevosnom %>% filter(Status != "Nuevo")

Par1rnom <- recompranom %>%
  group_by(Par1) %>%
  summarise(sum = sum(Cartera_Total))
Par1rnom<- Par1rnom %>% filter(Par1 == "Par1")
Par1rnom<- Par1rnom %>% select("sum")

Par30rnom <- recompranom %>%
  group_by(Par30) %>%
  summarise(sum = sum(Cartera_Total))
Par30rnom<- Par30rnom %>% filter(Par30 == "Par30")
Par30rnom<- Par30rnom %>% select("sum")

Par60rnom <- recompranom %>%
  group_by(Par60) %>%
  summarise(sum = sum(Cartera_Total))
Par60rnom<- Par60rnom %>% filter(Par60 == "Par60")
Par60rnom<- Par60rnom %>% select("sum")

Par90rnom <- recompranom %>%
  group_by(Par90) %>%
  summarise(sum = sum(Cartera_Total))
Par90rnom<- Par90rnom %>% filter(Par90 == "Par90")
Par90rnom<- Par90rnom %>% select("sum")

Indicadorrnom <- rbind(sum(recompranom$Cartera_Total),sum(recompranom$Cartera_Ven),
                      sum(recompranom$Cartera_Vig), Par1rnom, Par30rnom, Par60rnom, Par90rnom, sum(recompranom$Total_Provision),
                      sum(recompranom$Total_Provision_mas_IVA),length(recompranom$Total_Provision_mas_IVA))


Cnom <- data.frame(Cartera= c("Cartera Total","Cartera Vencida", "Cartera Vigente", "Par1", "Par30", "Par 60","Par90","EPRC", "EPRC mas IVA", "Total de Criditos"),nomina= IndicadorNom, Nuevos_nomina =Indicadornnom, Recompras_Nom=Indicadorrnom)
colnames(Cnom) <-(c("Categorma","nomina","Nuevos_cs","Recompras_Cs"))

resumen<- write.csv(Cnom,"C:/Users/ivazquez/Desktop/TDAs/octubre/nominaT.csv")

## Por territorios
Territoriosnom<- cnbvCarteraNom%>%
  group_by(Oficina) %>%
  summarise(sum=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1Nom),
            par30=sum(Par30Nom),
            par60=sum(Par60Nom),
            par90=sum(Par90Nom),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(Territoriosnom)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1cs","Par30cs","Par60cs","Par90cs","EPRC","EPRC_iva","Creditos"))

write.csv(Territoriosnom,"C:/Users/ivazquez/Desktop/TDAs/octubre/territorionom.csv")

# por territorios y Nuevos cartera Nomina
territorio_newnom<-nuevosnom%>%
  group_by(Oficina) %>%
  summarise(Cartot=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1Nom),
            par30=sum(Par30Nom),
            par60=sum(Par60Nom),
            par90=sum(Par90Nom),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(territorio_newnom)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(territorio_newnom,"C:/Users/ivazquez/Desktop/TDAs/octubre/terr_Nuevosnom.csv")

# por territorios y recompra cartera FC
territorioreNom<-recompranom%>%
  group_by(Oficina) %>%
  summarise(Cartot=sum(Cartera_Total),
            ven=sum(Cartera_Ven),
            vig=sum(Cartera_Vig),
            par1=sum(Par1Nom),
            par30=sum(Par30Nom),
            par60=sum(Par60Nom),
            par90=sum(Par90Nom),
            EPRC=sum(Total_Provision),
            EPRC_iva=sum(Total_Provision_mas_IVA),
            creditos=length(Total_Provision_mas_IVA))
colnames(territorioreNom)<-(c("Territorio","Cartera Total","Cartera Vencida","Cartera Vigente","Par1","Par30","Par60","Par90","EPRC","EPRC_iva","Creditos"))

write.csv(territorioreNom,"C:/Users/ivazquez/Desktop/TDAs/octubre/terr_RecomNom.csv")


### Llamar a los castigados: CastigosE

CastigosNom1<-CastigosE%>% filter(Convenio.x == "BIMBO")
CastigosNom2<-CastigosE%>% filter(Convenio.x == "OTROS")
CastigosNom<- bind_rows(CastigosNom1,CastigosNom2)
rm("CastigosNom1",CastigosNom2)

# Castigos por territorios: nuevos y recompra
castigosNom<-CastigosNom%>%
  group_by(Oficina) %>%
  summarise(cas_tot=sum(Total_Provision))

castigosRenom <- CastigosNom %>% filter(origen != "Nuevo")
castigosRenom<-castigosRenom%>%
  group_by(Oficina) %>%
  summarise(cas_re=sum(Total_Provision))

castigosNewnom <- CastigosNom %>% filter(origen == "Nuevo")
castigosNewnom<-castigosNewnom%>%
  group_by(Oficina) %>%
  summarise(cas_New=sum(Total_Provision))

castigados<-castigosNom %>% left_join(castigosNewnom, by = "Oficina")
castigados<-castigados %>% left_join(castigosRenom, by = "Oficina")
write.csv(castigados,"C:/Users/ivazquez/Desktop/TDAs/octubre/castigadosterrNom.csv")


## FIN

