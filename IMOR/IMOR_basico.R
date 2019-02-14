library(psych)
library(lattice)
library(Hmisc)
library(e1071)
library(tidyverse)
library(broom)

options(scipen=10)
imor <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\Sucs_Nuevos_sinterr.csv", stringsAsFactors = F)

summary(imor)
describe(imor)
str(imor)
imor[,-8]
xyplot(imor$Var_IMOR ~ imor$Var_CT + imor$Var_CV, type=c("p","smooth"))
plot(imor$Var_CV,imor$Var_IMOR)
plot(imor$Var_CT,imor$Var_IMOR)
xyplot(imor$Var_IMOR ~ imor$Var_CT, type=c("p","smooth"))
xyplot(imor$Var_IMOR ~ imor$Var_CV, type=c("p","smooth"))
rcorr(as.matrix(imor[,-8]))
#haciendo polinomios ortogonales
imor$Var_CT_2<- poly(imor$Var_CT, 2)[,2]
imor$Var_CT_1<- poly(imor$Var_CT, 2)[,1]
xyplot(imor$Var_IMOR ~ imor$Var_CT_1, type=c("p","smooth"))
xyplot(imor$Var_IMOR ~ imor$Var_CT_2, type=c("p","smooth"))
#xyplot(Var_IMOR ~ Var_CT | Territorio,
#       data = imor, type = c("p","r"))

boxplot(imor$Var_CT_1, main="CT poly 1", sub=paste("Outlier rows: ", boxplot.stats(imor$Var_CT_1)$out))  # box plot for 'speed'
boxplot(imor$Var_CT_2, main="CT poly 2", sub=paste("Outlier rows: ", boxplot.stats(imor$Var_CT_2)$out))  # box plot for 'distance'
boxplot(imor$Var_CV, main="CV", sub=paste("Outlier rows: ", boxplot.stats(imor$Var_CV)$out))  # box plot for 'distance'
boxplot(imor$Var_IMOR, main="IMOR", sub=paste("Outlier rows: ", boxplot.stats(imor$Var_IMOR)$out))  # box plot for 'distance'



plot(density(imor$Var_CT_1), main="Density Plot: CT1", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(imor$Var_CT_1), 2)))  # density plot for 'speed'
polygon(density(imor$Var_CT_1), col="blue")
plot(density(imor$Var_CT_2), main="Density Plot: CT2", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(imor$Var_CT_2), 2)))  # density plot for 'speed'
polygon(density(imor$Var_CT_2), col="blue")
plot(density(imor$Var_CV), main="Density Plot: CV", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(imor$Var_CV2), 2)))  # density plot for 'speed'
polygon(density(imor$Var_CV), col="blue")
plot(density(imor$Var_IMOR), main="Density Plot: IMOR", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(imor$Var_IMOR), 2)))  # density plot for 'speed'
polygon(density(imor$Var_IMOR), col="blue")

cor(imor$Var_IMOR, imor$Var_CT_1)
cor(imor$Var_IMOR, imor$Var_CT_2)
cor(imor$Var_IMOR, imor$Var_CV)

model1<- lm(Var_IMOR~ 1+ Var_CT_1 + Var_CT_2 + Var_CV, data = imor)
summary(model1)
model.diag.metrics <- augment(model1)


ggplot(model.diag.metrics, aes(Var_CT_1, Var_IMOR)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Var_CT_1, yend = .fitted), color = "red", size = 0.3)

ggplot(model.diag.metrics, aes(Var_CT_2, Var_IMOR)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Var_CT_2, yend = .fitted), color = "red", size = 0.3)

ggplot(model.diag.metrics, aes(Var_CV, Var_IMOR)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Var_CV, yend = .fitted), color = "red", size = 0.3)

plot(model1)
summary(model1)
#model<- lm(Var_IMOR~ 1+ Var_CT+ Var_CV, data = imor)
#summary(model)
limit= -0.0084220155032 + 2*0.0010314004819*mean(imor$Var_CT)
limit
mean(imor$Var_CT) + limit
cor(imor$Var_CT, imor$Var_CV)

#######################
###############################################recompra##########################################
#########################
imor <- read.csv("C:\\Users\\andrevargas\\Documents\\IMOR\\Sucs_Rec_sinterr.csv", stringsAsFactors = F)

summary(imor)
describe(imor)
str(imor)
imor[,-8]
xyplot(imor$Var_IMOR ~ imor$Var_CT + imor$Var_CV, type=c("p","smooth"))
plot(imor$Var_CV,imor$Var_IMOR)
plot(imor$Var_CT,imor$Var_IMOR)
xyplot(imor$Var_IMOR ~ imor$Var_CT, type=c("p","smooth"))
xyplot(imor$Var_IMOR ~ imor$Var_CV, type=c("p","smooth"))
rcorr(as.matrix(imor[,-8]))
#haciendo polinomios ortogonales
imor$Var_CT_2<- poly(imor$Var_CT, 2)[,2]
imor$Var_CT_1<- poly(imor$Var_CT, 2)[,1]
xyplot(imor$Var_IMOR ~ imor$Var_CT_1, type=c("p","smooth"))
xyplot(imor$Var_IMOR ~ imor$Var_CT_2, type=c("p","smooth"))
xyplot(Var_IMOR ~ Var_CT | Territorio,
       data = imor, type = c("p","r"))

boxplot(imor$Var_CT_1, main="CT poly 1", sub=paste("Outlier rows: ", boxplot.stats(imor$Var_CT_1)$out))  # box plot for 'speed'
boxplot(imor$Var_CT_2, main="CT poly 2", sub=paste("Outlier rows: ", boxplot.stats(imor$Var_CT_2)$out))  # box plot for 'distance'
boxplot(imor$Var_CV, main="CV", sub=paste("Outlier rows: ", boxplot.stats(imor$Var_CV)$out))  # box plot for 'distance'
boxplot(imor$Var_IMOR, main="IMOR", sub=paste("Outlier rows: ", boxplot.stats(imor$Var_IMOR)$out))  # box plot for 'distance'



plot(density(imor$Var_CT_1), main="Density Plot: CT1", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(imor$Var_CT_1), 2)))  # density plot for 'speed'
polygon(density(imor$Var_CT_1), col="blue")
plot(density(imor$Var_CT_2), main="Density Plot: CT2", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(imor$Var_CT_2), 2)))  # density plot for 'speed'
polygon(density(imor$Var_CT_2), col="blue")
plot(density(imor$Var_CV), main="Density Plot: CV", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(imor$Var_CV2), 2)))  # density plot for 'speed'
polygon(density(imor$Var_CV), col="blue")
plot(density(imor$Var_IMOR), main="Density Plot: IMOR", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(imor$Var_IMOR), 2)))  # density plot for 'speed'
polygon(density(imor$Var_IMOR), col="blue")

cor(imor$Var_IMOR, imor$Var_CT_1)
cor(imor$Var_IMOR, imor$Var_CT_2)
cor(imor$Var_IMOR, imor$Var_CV)

model1<- lm(Var_IMOR~ 1+ Var_CT_1 + Var_CT_2 + Var_CV , data = imor)
summary(model1)
model.diag.metrics <- augment(model1)


ggplot(model.diag.metrics, aes(Var_CT_1, Var_IMOR)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Var_CT_1, yend = .fitted), color = "red", size = 0.3)

ggplot(model.diag.metrics, aes(Var_CT_2, Var_IMOR)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Var_CT_2, yend = .fitted), color = "red", size = 0.3)

ggplot(model.diag.metrics, aes(Var_CV, Var_IMOR)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Var_CV, yend = .fitted), color = "red", size = 0.3)

plot(model1)
summary(model1)
#model<- lm(Var_IMOR~ 1+ Var_CT+ Var_CV, data = imor)
#summary(model)
limit= -0.0084220155032 + 2*0.0010314004819*mean(imor$Var_CT)
limit
mean(imor$Var_CT) + limit
cor(imor$Var_CT, imor$Var_CV)



