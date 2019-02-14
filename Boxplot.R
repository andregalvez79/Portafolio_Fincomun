# library
install.packages("ggplot2")
library(ggplot2)
install.packages("rmarkdown")
library(rmarkdown)
install.packages("psych")
library(psych)

giros <- read.csv("C:\\Users\\andrevargas\\Documents\\plot_yahir\\giros-completo.csv")

cbind(Clave = 0, giros)
as.factor(giros$Giro)
levels(giros$Giro)



girossplit1 <- split(giros,giros$Giro)[[1]]
girossplit1$Clave <- paste0("Acabado Telas")
girossplit2 <- split(giros,giros$Giro)[[2]]
girossplit2$Clave <- paste0("Arrendamiento residenciales")
girossplit3 <- split(giros,giros$Giro)[[3]]
girossplit3$Clave <- paste0("C/V Tlapaleria")
girossplit4 <- split(giros,giros$Giro)[[4]]
girossplit4$Clave <- paste0("No Clasificados")
girossplit5 <- split(giros,giros$Giro)[[5]]
girossplit5$Clave <- paste0("C/V Decoracion")
girossplit6 <- split(giros,giros$Giro)[[6]]
girossplit6$Clave <- paste0("C/V Regalo")
girossplit7 <- split(giros,giros$Giro)[[7]]
girossplit7$Clave <- paste0("C/V Aretsanias/Art. Regionales")
girossplit8 <- split(giros,giros$Giro)[[8]]
girossplit8$Clave <- paste0("C/V Calzado")
girossplit9 <- split(giros,giros$Giro)[[9]]
girossplit9$Clave <- paste0("C/V Carne de Aves")
girossplit10 <- split(giros,giros$Giro)[[10]]
girossplit10$Clave <- paste0("C/V CarneRes y Ganado")
girossplit11 <- split(giros,giros$Giro)[[11]]
girossplit11$Clave <- paste0("C/V Desperdicio Ind.")
girossplit12 <- split(giros,giros$Giro)[[12]]
girossplit12$Clave <- paste0("C/V Dulces")
girossplit13 <- split(giros,giros$Giro)[[13]]
girossplit13$Clave <- paste0("C/V Flores")
girossplit14 <- split(giros,giros$Giro)[[14]]
girossplit14$Clave <- paste0("C/V Frutas")
girossplit15 <- split(giros,giros$Giro)[[15]]
girossplit15$Clave <- paste0("C/V Computo")
girossplit16 <- split(giros,giros$Giro)[[16]]
girossplit16$Clave <- paste0("C/V Juguetes")
girossplit17 <- split(giros,giros$Giro)[[17]]
girossplit17$Clave <- paste0("C/V Legumbres/Hortalizas")
girossplit18 <- split(giros,giros$Giro)[[18]]
girossplit18$Clave <- paste0("C/V Plastico")
girossplit19 <- split(giros,giros$Giro)[[19]]
girossplit19$Clave <- paste0("Venta Art. Hogar")
girossplit20 <- split(giros,giros$Giro)[[20]]
girossplit20$Clave <- paste0("Alimenticios Agricolas")
girossplit21 <- split(giros,giros$Giro)[[21]]
girossplit21$Clave <- paste0("C/V Pan")
girossplit22 <- split(giros,giros$Giro)[[22]]
girossplit22$Clave <- paste0("Papeleria")
girossplit23 <- split(giros,giros$Giro)[[23]]
girossplit23$Clave <- paste0("C/V Perfumes")
girossplit24 <- split(giros,giros$Giro)[[24]]
girossplit24$Clave <- paste0("C/V Ropa")
girossplit25 <- split(giros,giros$Giro)[[25]]
girossplit25$Clave <- paste0("Tejidos Fibra")
girossplit26 <- split(giros,giros$Giro)[[26]]
girossplit26$Clave <- paste0("Cria Ovino")
girossplit27 <- split(giros,giros$Giro)[[27]]
girossplit27$Clave <- paste0("Plantas Ornato")
girossplit28 <- split(giros,giros$Giro)[[28]]
girossplit28$Clave <- paste0("Fabricas Frituras")
girossplit29 <- split(giros,giros$Giro)[[29]]
girossplit29$Clave <- paste0("Nieves y helados")
girossplit30 <- split(giros,giros$Giro)[[30]]
girossplit30$Clave <- paste0("Fabricacion Pan")
girossplit31 <- split(giros,giros$Giro)[[31]]
girossplit31$Clave <- paste0("Fabrica Perfumes")
girossplit32 <- split(giros,giros$Giro)[[32]]
girossplit32$Clave <- paste0("Prof. Independiente")
girossplit33 <- split(giros,giros$Giro)[[33]]
girossplit33$Clave <- paste0("Restaurante")
girossplit34 <- split(giros,giros$Giro)[[34]]
girossplit34$Clave <- paste0("Salon Belleza")
girossplit35 <- split(giros,giros$Giro)[[35]]
girossplit35$Clave <- paste0("Loncheria, Taqueria, Torerias")
girossplit36 <- split(giros,giros$Giro)[[36]]
girossplit36$Clave <- paste0("Merenderos/antojitos/platillos")
girossplit37 <- split(giros,giros$Giro)[[37]]
girossplit37$Clave <- paste0("Confeccion Vestido")
girossplit38 <- split(giros,giros$Giro)[[38]]
girossplit38$Clave <- paste0("Taller Autos")
girossplit39 <- split(giros,giros$Giro)[[39]]
girossplit39$Clave <- paste0("Sastreria")
girossplit40 <- split(giros,giros$Giro)[[40]]
girossplit40$Clave <- paste0("Abarrotes/Miscelania")
girossplit41 <- split(giros,giros$Giro)[[41]]
girossplit41$Clave <- paste0("Tortilleria")
girossplit42 <- split(giros,giros$Giro)[[42]]
girossplit42$Clave <- paste0("Ruletero")


giros_all<-Reduce(function(x, y) merge(x, y, all=TRUE),
                 list(girossplit1, girossplit2, girossplit3,
                      girossplit4, girossplit5,girossplit6,
                      girossplit7, girossplit8, girossplit9,
                      girossplit10,girossplit11,girossplit12,
                      girossplit13,girossplit14,girossplit15,
                      girossplit16,girossplit17,girossplit18,
                      girossplit19,girossplit20,girossplit21,
                      girossplit22, girossplit23, girossplit24,
                      girossplit25,girossplit26,girossplit27,
                      girossplit28,girossplit29,girossplit30,
                      girossplit31,girossplit32,girossplit33,
                      girossplit34,girossplit35,girossplit36,
                      girossplit37,girossplit38,girossplit39,
                      girossplit40,girossplit41,girossplit42))

giros_all <- giros_all[order(giros_all$creditos),]
summary(giros_all)
giros_all[order(giros_all$Periodo, decreasing = T),]
giro_lonch_misc<-Reduce(function(x, y) merge(x, y, all=TRUE), 
                  list(girossplit40, girossplit35))

#23per
# grouped boxplot

g_dos_altas<- ggplot(giro_lonch_misc, aes(x=Clave, y=creditos, fill=Periodo)) + 
  geom_boxplot() +
  xlab(label = "Giro") +
  ylab(label = "Créditos") +
  labs(title = "Cantidad de créditos colocados por Giro en cada Trimestre del 2017 y 2018")
g_dos_altas<- g_dos_altas + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.text.x = element_text(angle = 0, hjust = 1))
g_dos_altas


ggsave(
  "masaltas.png",
  plot = g_dos_altas,
  path = "C:\\Users\\andrevargas\\Documents\\plot_yahir\\",
  dpi = 300,
  scale = 1,
  width = 30, height = 20, units = "cm"
)

giro_2mas<-Reduce(function(x, y) merge(x, y, all=TRUE), 
                        list(girossplit24, girossplit8))
g_2mas<- ggplot(giro_2mas, aes(x=Clave, y=creditos, fill=Periodo)) + 
  geom_boxplot() +
  xlab(label = "Giro") +
  ylab(label = "Créditos") +
  labs(title = "Cantidad de créditos colocados por Giro en cada Trimestre del 2017 y 2018")
g_2mas<- g_2mas + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.text.x = element_text(angle = 0, hjust = 1))
g_2mas

ggsave(
  "first2.png",
  plot = g_2mas,
  path = "C:\\Users\\andrevargas\\Documents\\plot_yahir\\",
  dpi = 300,
  scale = 1,
  width = 30, height = 20, units = "cm"
)

giro_5mas<-Reduce(function(x, y) merge(x, y, all=TRUE), 
                  list(girossplit23,girossplit22,girossplit34,girossplit12))
giro_5mas<- subset(giro_5mas, Periodo != "4_16 Trim")
g_5mas<- ggplot(giro_5mas, aes(x=Clave, y=creditos, fill=Periodo)) + 
  geom_boxplot() +
  xlab(label = "Giro") +
  ylab(label = "Créditos") +
  labs(title = "Cantidad de créditos colocados por Giro en cada Trimestre del 2017 y 2018")
g_5mas<- g_5mas + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.text.x = element_text(angle = 0, hjust = 1))
g_5mas



ggsave(
  "plot final.png",
  plot = g_5mas,
  path = "C:\\Users\\andrevargas\\Documents\\plot_yahir\\",
  dpi = 300,
  scale = 1,
  width = 30, height = 20, units = "cm"
)

giro_10mas<-Reduce(function(x, y) merge(x, y, all=TRUE), 
                  list(girossplit14, girossplit41,girossplit37,girossplit17,girossplit6))
g_10mas<- ggplot(giro_10mas, aes(x=Clave, y=creditos, fill=Periodo)) + 
  geom_boxplot() +
  xlab(label = "Giro") +
  ylab(label = "Créditos") +
  labs(title = "Cantidad de créditos colocados por Giro en cada Trimestre del 2017 y 2018")
g_10mas<- g_10mas + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                        panel.background = element_blank(), axis.text.x = element_text(angle = 0, hjust = 1))
g_10mas

ggsave(
  "cinco_mas.png",
  plot = g_10mas,
  path = "C:\\Users\\andrevargas\\Documents\\plot_yahir\\",
  dpi = 300,
  scale = 1,
  width = 30, height = 20, units = "cm"
)

giro_last<-Reduce(function(x, y) merge(x, y, all=TRUE), 
                   list(girossplit21, girossplit10,girossplit42))
giro_last<- ggplot(giro_last, aes(x=Clave, y=creditos, fill=Periodo)) + 
  geom_boxplot() +
  xlab(label = "Giro") +
  ylab(label = "Créditos") +
  labs(title = "Cantidad de créditos colocados por Giro en cada Trimestre del 2017 y 2018")
giro_last<- giro_last + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                          panel.background = element_blank(), axis.text.x = element_text(angle = 0, hjust = 1))
giro_last

ggsave(
  "ultimos.png",
  plot = giro_last,
  path = "C:\\Users\\andrevargas\\Documents\\plot_yahir\\",
  dpi = 300,
  scale = 1,
  width = 30, height = 20, units = "cm"
)
##################
g2<-ggplot(data=giros_all, aes(x=Clave, y=creditos, fill=Periodo)) +
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Paired")
g2<- g2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))
g2
