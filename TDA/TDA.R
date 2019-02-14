### Veremos ###
write.csv(FC,"09TDAs2.csv")
setwd("C:/Users/agaona/Desktop/TDA")
TDA <- function(CNBV, FCV, CG, CN, SUC, CP, DOM, WR, TDA_O) {
  ##### Archivos base ###
  if((file.exists(CNBV)) &
     (file.exists(FCV)) &
     (file.exists(CG)) &
     (file.exists(CN)) &
     (file.exists(SUC)) &
     (file.exists(CP)) &
     (file.exists(DOM)) == FALSE) {
    print("Revisa el nombre de los documentos o Folder del Working Directory")}
  else{
    cnbv <- read.csv(CNBV,
                     stringsAsFactors = F)    ### Mes de cálculo
    fcv <- read.csv(FCV,
                    stringsAsFactors = F)
    fcv <- select(fcv, "SocioRea","Giro")
    fcv <- unique(fcv, SocioRea = FALSE,  MARGIN = 2, fromLast = FALSE, nmax = NA)
    fcv <- distinct(fcv, SocioRea, .keep_all= TRUE)
    Catalogogiros <- read.csv(CG,
                              stringsAsFactors = F)
    Catalogogiros <- distinct(Catalogogiros, Actividad, .keep_all= TRUE)
    fcv <- fcv %>% left_join(Catalogogiros, by = c("Giro"="Actividad"))
    CatalogoNomina <- read.csv(CN,
                               stringsAsFactors = F)
    Sucursal <- read.csv(SUC,
                         stringsAsFactors = F)
    CodigoP <-  read.csv(CP,
                         stringsAsFactors = F)
    CodigoP <- unique(CodigoP, Codigo = FALSE)
    CodigoP <- distinct(CodigoP, codigo, .keep_all= TRUE)
    Domicilios <- read.csv(DOM)
    Domicilios <- select(Domicilios, "CLIENTE","COD_POS")
    Domicilios <- unique(Domicilios, CLIENTE = TRUE )
    Domicilios$COD_POS <- as.numeric(Domicilios$COD_POS)
    CodigoP$codigo <- as.numeric(CodigoP$codigo)
    Domicilios <- Domicilios  %>%  left_join(CodigoP, by = c("COD_POS" = "codigo"))
    names (cnbv)[34] = "Provision1"
    names (cnbv)[52] = "en_base_menor100"
    ##### Exportar la base de calificación
    cnbv <- cnbv %>% left_join(CatalogoNomina, by = c("nro_prest"="Numero.Credito"))
    cnbv <- cnbv %>% left_join(Domicilios, by = c("socio"="CLIENTE"))
    cnbv <- cnbv %>% left_join(fcv, by = c("socio"="SocioRea"))
    cnbv <- cnbv %>% left_join(Sucursal, by = c("Sucursal"="sucursal"))
    cnbv <- cnbv %>%
      mutate(cartera = if_else(Provision1 < 1, "cartera", "no cartera"),
             Cartera_Total = MtoVig2 + MtoVenc2 + Int_Vig_2 + Int_Ven_2,
             Cartera_Ven = MtoVenc2 + Int_Ven_2,
             Cartera_Vig = MtoVig2 + Int_Vig_2,
             Par1 = if_else(Dias_de_atraso>=1, "Par1", "No ap"),
             Par30 = if_else(Dias_de_atraso>=30, "Par30", "No ap"),
             Par60 = if_else(Dias_de_atraso>=60, "Par60", "No ap"),
             Par90 = if_else(Dias_de_atraso>=90, "Par90", "No ap"),
             Estratificacion = case_when(.$monto_inicial < 3001  ~ "a.) Menor a $,3000",
                                         .$monto_inicial < 7001  ~ "b.) $3,000 a $7,000",
                                         .$monto_inicial < 10001  ~ "c.) $7,001 a $10,000",
                                         .$monto_inicial < 13001  ~ "d.) $10,001 a $13,000",
                                         .$monto_inicial < 16001  ~ "e.) $13,001 a $16,000",
                                         .$monto_inicial < 20001  ~ "f.) $16,001 a $20,000",
                                         .$monto_inicial < 25001  ~ "g.) $20,001 a $25,000",
                                         .$monto_inicial < 30001  ~ "h.) $25,001 a $30,000",
                                         .$monto_inicial < 40001  ~ "i.) $30,001 a $40,000",
                                         .$monto_inicial < 100001  ~ "j.) $40,001 a $100,000",
                                         TRUE ~ "k.) Mayor a $100,000"),
             CVencidos = if_else(Cartera_Ven> 0, 1,0))
    ## Inicia el codigo ##
    #Seleccion de la cartera#
    cnbvCartera <- cnbv  %>% filter(cartera == "cartera")
    #
    ##
    ### Comercial ###
    cnbvCarteraPop <- cnbvCartera %>% filter(Sucursal != 2 & sub_aplic != 4350)
    cnbvCarteraPop1<-cnbvCarteraPop%>% select("sub_aplic","Cartera_Total","Cartera_Ven","Cartera_Vig","Par1","Par30","Par60","Par90","Total_Provision","Total_Provision_mas_IVA")
    write.csv(cnbvCarteraPop1,WR)
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
    #
    ##
    ### Para Efectivo ###
    ## Filtrar por 5010 y 3015
    cnbvCarEfec <- cnbvCarteraPop %>% filter(sub_aplic == 3015 | sub_aplic == 5010)
    Par1E <- cnbvCarEfec %>%
      group_by(Par1) %>%
      summarise(sum = sum(Cartera_Total))
    Par1E<- Par1E %>% filter(Par1 == "Par1")
    Par1E<- Par1E %>% select("sum")
    Par30E <- cnbvCarEfec %>%
      group_by(Par30) %>%
      summarise(sum = sum(Cartera_Total))
    Par30E<- Par30E %>% filter(Par30 == "Par30")
    Par30E<- Par30E %>% select("sum")
    Par60E <- cnbvCarEfec %>%
      group_by(Par60) %>%
      summarise(sum = sum(Cartera_Total))
    Par60E<- Par60E %>% filter(Par60 == "Par60")
    Par60E<- Par60E %>% select("sum")
    Par90E <- cnbvCarEfec %>%
      group_by(Par90) %>%
      summarise(sum = sum(Cartera_Total))
    Par90E<- Par90E %>% filter(Par90 == "Par90")
    Par90E<- Par90E %>% select("sum")
    IndicadorEfec <- rbind(sum(cnbvCarEfec$Cartera_Total),sum(cnbvCarEfec$Cartera_Ven),
                           sum(cnbvCarEfec$Cartera_Vig), Par1E, Par30E, Par60E, Par90E, sum(cnbvCarEfec$Total_Provision),
                           sum(cnbvCarEfec$Total_Provision_mas_IVA), length(cnbvCarEfec$Total_Provision_mas_IVA))
    #
    ##
    ### Productivo  ###
    ### Filtrar por 4011, 4014, 4021, 4024 & 4050 y 4051
    cnbvCarProduc <- cnbvCarteraPop %>% filter(sub_aplic == 4011 | sub_aplic == 4014 | sub_aplic ==4021 | sub_aplic ==4024 | sub_aplic ==4050 | sub_aplic ==4051)
    Par1Prod <- cnbvCarProduc %>%
      group_by(Par1) %>%
      summarise(sum = sum(Cartera_Total))
    Par1Prod<- Par1Prod %>% filter(Par1 == "Par1")
    Par1Prod<- Par1Prod %>% select("sum")
    Par30Prod <- cnbvCarProduc %>%
      group_by(Par30) %>%
      summarise(sum = sum(Cartera_Total))
    Par30Prod<- Par30Prod %>% filter(Par30 == "Par30")
    Par30Prod<- Par30Prod %>% select("sum")
    Par60Prod <- cnbvCarProduc %>%
      group_by(Par60) %>%
      summarise(sum = sum(Cartera_Total))
    Par60Prod<- Par60Prod %>% filter(Par60 == "Par60")
    Par60Prod<- Par60Prod %>% select("sum")
    Par90Prod <- cnbvCarEfec %>%
      group_by(Par90) %>%
      summarise(sum = sum(Cartera_Total))
    Par90Prod<- Par90Prod %>% filter(Par90 == "Par90")
    Par90Prod<- Par90Prod %>% select("sum")
    IndicadorProduc <- rbind(sum(cnbvCarProduc$Cartera_Total),sum(cnbvCarProduc$Cartera_Ven),
                             sum(cnbvCarProduc$Cartera_Vig), Par1Prod, Par30Prod, Par60Prod, Par90Prod, sum(cnbvCarProduc$Total_Provision),
                             sum(cnbvCarProduc$Total_Provision_mas_IVA), length(cnbvCarProduc$Total_Provision_mas_IVA))
    #
    ##
    ### Para Colchoncito ###
    ## Filtrar por 4030
    cnbvCarCol <- cnbvCarteraPop %>% filter(sub_aplic == 4030)
    Par1Col <- cnbvCarCol %>%
      group_by(Par1) %>%
      summarise(sum = sum(Cartera_Total))
    Par1Col<- Par1Col %>% filter(Par1 == "Par1")
    Par1Col<- Par1Col %>% select("sum")
    Par30Col <- cnbvCarCol %>%
      group_by(Par30) %>%
      summarise(sum = sum(Cartera_Total))
    Par30Col<- Par30Col %>% filter(Par30 == "Par30")
    Par30Col<- Par30Col %>% select("sum")
    Par60Col <- cnbvCarCol %>%
      group_by(Par60) %>%
      summarise(sum = sum(Cartera_Total))
    Par60Col<- Par60Col %>% filter(Par60 == "Par60")
    Par60Col<- Par60Col %>% select("sum")
    Par90Col <- cnbvCarCol %>%
      group_by(Par90) %>%
      summarise(sum = sum(Cartera_Total))
    Par90Col<- Par90Col %>% filter(Par90 == "Par90")
    Par90Col<- Par90Col %>% select("sum")
    IndicadorCol <- rbind(sum(cnbvCarCol$Cartera_Total),sum(cnbvCarCol$Cartera_Ven),
                          sum(cnbvCarCol$Cartera_Vig), Par1Col, Par30Col, Par60Col, Par90Col, sum(cnbvCarCol$Total_Provision),
                          sum(cnbvCarCol$Total_Provision_mas_IVA), length(cnbvCarCol$Total_Provision_mas_IVA))
    #
    ##
    ###Credivivienda
    ### Filtrar por 4231
    cnbvCarCrv <- cnbvCarteraPop %>% filter(sub_aplic == 4231)
    Par1Crv <- cnbvCarCrv %>%
      group_by(Par1) %>%
      summarise(sum = sum(Cartera_Total))
    Par1Crv<- Par1Crv %>% filter(Par1 == "Par1")
    Par1Crv<- Par1Crv %>% select("sum")
    Par30Crv <- cnbvCarCrv %>%
      group_by(Par30) %>%
      summarise(sum = sum(Cartera_Total))
    Par30Crv<- Par30Crv %>% filter(Par30 == "Par30")
    Par30Crv<- Par30Crv %>% select("sum")
    Par60Crv <- cnbvCarCrv %>%
      group_by(Par60) %>%
      summarise(sum = sum(Cartera_Total))
    Par60Crv<- Par60Crv %>% filter(Par60 == "Par60")
    Par60Crv<- Par60Crv %>% select("sum")
    Par90Crv <- cnbvCarCrv %>%
      group_by(Par90) %>%
      summarise(sum = sum(Cartera_Total))
    Par90Crv<- Par90Crv %>% filter(Par90 == "Par90")
    Par90Crv<- Par90Crv %>% select("sum")
    IndicadorCrv <- rbind(sum(cnbvCarCrv$Cartera_Total),sum(cnbvCarCrv$Cartera_Ven),
                          sum(cnbvCarCrv$Cartera_Vig), Par1Crv, Par30Crv, Par60Crv, Par90Crv, sum(cnbvCarCrv$Total_Provision),
                          sum(cnbvCarCrv$Total_Provision_mas_IVA), length(cnbvCarCrv$Total_Provision_mas_IVA))
    #
    ##
    ### Mi Tienda
    ### Filtrar por 4321
    cnbvCarTienda <- cnbvCarteraPop %>% filter(sub_aplic == 4321)
    Par1Ti <- cnbvCarTienda %>%
      group_by(Par1) %>%
      summarise(sum = sum(Cartera_Total))
    Par1Ti<- Par1Ti %>% filter(Par1 == "Par1")
    Par1Ti<- Par1Ti %>% select("sum")
    Par30Ti <- cnbvCarTienda %>%
      group_by(Par30) %>%
      summarise(sum = sum(Cartera_Total))
    Par30Ti<- Par30Ti %>% filter(Par30 == "Par30")
    Par30Ti<- Par30Ti %>% select("sum")
    Par60Ti <- cnbvCarTienda %>%
      group_by(Par60) %>%
      summarise(sum = sum(Cartera_Total))
    Par60Ti<- Par60Ti %>% filter(Par60 == "Par60")
    Par60Ti<- Par60Ti %>% select("sum")
    Par90Ti <- cnbvCarTienda %>%
      group_by(Par90) %>%
      summarise(sum = sum(Cartera_Total))
    Par90Ti<- Par90Ti %>% filter(Par90 == "Par90")
    Par90Ti<- Par90Ti %>% select("sum")
    IndicadorTienda <- rbind(sum(cnbvCarTienda$Cartera_Total),sum(cnbvCarTienda$Cartera_Ven),
                             sum(cnbvCarTienda$Cartera_Vig), Par1Ti, Par30Ti, Par60Ti, Par90Ti, sum(cnbvCarTienda$Total_Provision),
                             sum(cnbvCarTienda$Total_Provision_mas_IVA), length(cnbvCarTienda$Total_Provision_mas_IVA))
    #
    ##
    ###Alcancia
    ### Filtrar por  4032
    cnbvCarAlc <- cnbvCarteraPop %>% filter(sub_aplic == 4032)
    Par1Alc <- cnbvCarAlc %>%
      group_by(Par1) %>%
      summarise(sum = sum(Cartera_Total))
    Par1Alc<- Par1Alc %>% filter(Par1 == "Par1")
    Par1Alc<- Par1Alc %>% select("sum")
    Par30Alc <- cnbvCarAlc %>%
      group_by(Par30) %>%
      summarise(sum = sum(Cartera_Total))
    Par30Alc<- Par30Alc %>% filter(Par30 == "Par30")
    Par30Alc<- Par30Alc %>% select("sum")
    Par60Alc <- cnbvCarAlc %>%
      group_by(Par60) %>%
      summarise(sum = sum(Cartera_Total))
    Par60Alc<- Par60Alc %>% filter(Par60 == "Par60")
    Par60Alc<- Par60Alc %>% select("sum")
    Par90Alc <- cnbvCarAlc %>%
      group_by(Par90) %>%
      summarise(sum = sum(Cartera_Total))
    Par90Alc<- Par90Alc %>% filter(Par90 == "Par90")
    Par90Alc<- Par90Alc %>% select("sum")
    IndicadorAlc <- rbind(sum(cnbvCarAlc$Cartera_Total),sum(cnbvCarAlc$Cartera_Ven),
                          sum(cnbvCarAlc$Cartera_Vig), Par1Alc, Par30Alc, Par60Alc, Par90Alc, sum(cnbvCarAlc$Total_Provision),
                          sum(cnbvCarAlc$Total_Provision_mas_IVA), length(cnbvCarAlc$Total_Provision_mas_IVA))
    #Pat <- data.frame(Cartera3= c("Cartera Total","Cartera Vencida", "Cartera Vigente", "Par1", "Par30","EPRC", "EPRC mas IVA", "Total de Creditos"),Patr= IndicadorPat)
    #colnames(Pat) <- (c("Categoria","Patrimonial"))
    #
    ##
    ###Hipotecario
    ### Filtrar por  4801
    cnbvCarHip <- cnbvCarteraPop %>% filter(sub_aplic == 4801)
    Par1Hip <- cnbvCarHip %>%
      group_by(Par1) %>%
      summarise(sum = sum(Cartera_Total))
    Par1Hip<- Par1Hip %>% filter(Par1 == "Par1")
    Par1Hip<- Par1Hip %>% select("sum")
    Par30Hip <- cnbvCarHip %>%
      group_by(Par30) %>%
      summarise(sum = sum(Cartera_Total))
    Par30Hip<- Par30Hip %>% filter(Par30 == "Par30")
    Par30Hip<- Par30Hip %>% select("sum")
    Par60Hip <- cnbvCarHip %>%
      group_by(Par60) %>%
      summarise(sum = sum(Cartera_Total))
    Par60Hip<- Par60Hip %>% filter(Par60 == "Par60")
    Par60Hip<- Par60Hip %>% select("sum")
    Par90Hip <- cnbvCarHip %>%
      group_by(Par90) %>%
      summarise(sum = sum(Cartera_Total))
    Par90Hip<- Par90Hip %>% filter(Par90 == "Par90")
    Par90Hip<- Par90Hip %>% select("sum")
    IndicadorHip <- rbind(sum(cnbvCarHip$Cartera_Total),sum(cnbvCarHip$Cartera_Ven),
                          sum(cnbvCarHip$Cartera_Vig), Par1Hip, Par30Hip, Par60Hip, Par90Hip, sum(cnbvCarHip$Total_Provision),
                          sum(cnbvCarHip$Total_Provision_mas_IVA), length(cnbvCarHip$Total_Provision_mas_IVA))
    IndicadorAlc<-rbind(IndicadorAlc, c("ignorar"))
    for (i in 1:4){
      IndicadorHip<-rbind(IndicadorHip, c("ignorar"))}
    FC <- data.frame(Cartera= c("Cartera Total","Cartera Vencida", "Cartera Vigente", "Par1", "Par30", "Par 60","Par90","EPRC", "EPRC mas IVA", "Total de Creditos"), Popular = IndicadorPop, Efectivo = IndicadorEfec,
                     Productivo = IndicadorProduc, Colchoncito = IndicadorCol, Credivivienda = IndicadorCrv, MiTienda = IndicadorTienda, Alcncia = IndicadorAlc, Hipotecario = IndicadorHip)
    colnames(FC) <-(c("Categoria","Popular", "Efectivo", "Productivo","Colchoncito","Credivivienda","MiTienda","Alcancia","Hipotecario"))
    ### Veremos ###
    output = write.csv(FC,TDA_O)
    return (output)
  }}
#CNBV, FCV, CG, CN, SUC, CP, DOM, WR ,TDA_O
TDA("Cnbv2018_09sep_30_cont - Riesgos.csv",
    "fcv.csv",
    "Catalogo Giros.csv",
    "Entidad federativa.csv",
    "Sucursal.csv",
    "Codigos postales2.csv",
    "Domicilios2.csv",
    "TDAs2.csv",
    "TDA_output.csv")
