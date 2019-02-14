######DATA#####
#Equity Volatility
base$equity_vol
SigmaE = sd(base$equity_vol)*250^0.5
#Equity Value
Ve = mean(base$equity)
#Debt or Liabilities
D = mean(base$Debt_l)
#interest rate "free-risk"
r = 1.21/100
#T-t mensual
Th = 1/12

####unknowns###
#Asset Value
Va = Ve + D
#Asset volatility
SigmaA = (Ve*SigmaE)/Va

#####Black-Scholes#####
d1 = (log(Va/D)+(r + SigmaA^2/2)*Th)/(SigmaA*sqrt(Th))
d2 = d1 - Va*sqrt(Th)
#Equity Value
EVal = Va*pnorm(d1) - D*exp(-r*Th)*pnorm(d2)
EVar = (Ve/EVal)*SigmaA*pnorm(d1)

Cost = (EVal/Ve-1)^2 - (EVar/SigmaE-1)^2

V0 = 889176.73 #asset value
Z = 344990.22 # Debts
sigmaV= 4.2702 #Asset volatility
Th= 1/12 #time
r = 1.21/100 #tasa
equityvol = 5.698267142 # variacion a 250 dias 
equityval =  544186.51  #Valor promedio 2017 y 2018 mensual


D1 <- function(V0,Z,r,sigmaV,Th) 
{((log(V0/Z) + (r + sigmaV^2/2)*Th))/(sigmaV*sqrt(Th))} 

d1 <- D1(V0,Z,r,sigmaV,Th)


D2<-function(d1,sigmaV ,Th)
  {d1-sigmaV*sqrt(Th)}
d2 <- D2(d1,sigmaV,Th)

#Equity Value
f1<-function(Va,d1,d2,Z,r,Th) {Va*pnorm(d1)- Z*exp(-r*Th)*pnorm(d2)}
#Equity Var
f2<-function(Va,VE,d1) {Va/VE*pnorm(d1)}
Va=V0
VE <- f1(Va, d1, d2,Z ,r,Th)
SE <- f2(Va,VE,d1)

IT1<-VE #valor equity
IT2<-SE #volatilidad equity
counter<-0 


f1_1<-function(Va) {Va*pnorm(D1(V0,Z,r,sigmaV,Th))-exp(-r)*Z*pnorm(D2(D1( V0,Z,r,sigmaV,Th), SE,1))-VE} 
f2_1<-function(sigmaV) {Va/VE*pnorm(D1(V0,Z,r,sigmaV,Th)*sigmaV-SE)} 
f1_1(Va)
f2_1(sigmaV)

# no me sale uniroot
while (sqrt((equityval-IT1)^2+(equityvol-IT2)^2)>0.1*(1+sqrt( IT1^2+IT2^2))
        & counter < 1000) 
  {sigmaV<-IT2
  IT1=uniroot(f1_1,c(0,VE*100))$root
  VA<-IT1
  h<-Vectorize(f2_1)
  IT2=uniroot(h,c(0,SE*100))$root
  counter<-counter+1}

IT1=uniroot(f1_1,c(0,VE*100))$root
IT2<-curve(h,c(0,SE*100))
IT2$y


####Newton's, Broyden####
fnewton <- function(V0) {
  y <- numeric(2) 
  d1 = ((log(V0/Z) + (r + sigmaV^2/2)*Th))/(sigmaV*sqrt(Th))
  d2 = d1-sigmaV*sqrt(Th)
  y[1] <- Va*pnorm(d1)- Z*exp(-r*Th)*pnorm(d2)
  y[2] <- Va/VE*pnorm(d1)
  y} 
options(scipen = 1000)

fnewton(V0)
PD = pnorm(-d2)
PD

#minimización e itreaciones con metodos Newton y Broyden
install.packages("nleqslv")
library(nleqslv)

x = c(V0, sigmaV)
iterf <- nleqslv(x, fnewton, control=list(btol= 10^-10))
iterf <- nleqslv(x, fnewton, jacobian=TRUE, method="Newton" ,control=list(trace=1,allowSingular=TRUE))
summary(iterf)
iterf$fvec

iterf2 <- nleqslv(c(VE,SE), fnewton , control=list(btol=.01),
        method ="Broyden")$x
iterf2
iterf3 <- nleqslv(c(VE,SE), fnewton , control=list(btol=.01),
        method ="Newton")$x
iterf3




######one dimensional case######
b=c(Va, sigmaV)
f <- function(b) {
  VA = b[1]
  SA = b[2]
  d1 = (log(VE/Z) + (r + SA^2/2)*T)/SA/sqrt(Th)
  d2 = d1-SA*sqrt(Th)
  e1 = VE - (VA*pnorm(d1)- Z*exp(-r*Th)*pnorm(d2))
  e2 = SE * VE - pnorm(d1)*SA*VA
  return(e1^2 + e2^2)}

nlminb(c(Va,sigmaV), f, lower=c(0, 100000000), upper=c(0, 10000000))$par

optimize(c(Va,sigmaV), f, maximum = F)$par


###########otro metodo##########
#Alexander J. McNeil.pdf
install.packages("sde")
library(sde) ## Parameters for Merton model
V0 <- 1
muV <- 0.03
sigmaV <- 0.25
r <- 0.02
B <- 0.85
T <- 1
N <- 364
## Simulated asset value trajectories for Merton model 
npaths <- 50
paths <- matrix(NA,nrow=N+1,ncol=npaths)
for (i in 1:npaths) {paths[,i] <- GBM(x=V0,r=muV,sigma=sigmaV,T=T,N=N)}

#Draw a path of Merton GBM leading to default and superimpose value of default-free and defaultable debt
set.seed(63)
Vt <- GBM(x=V0,r=muV,sigma=sigmaV,T=T,N=N)
times <- seq(from=0,to=1,length=N+1)
par(mar=c(3,3,2,1),mgp=c(2,1,0))
plot(times,Vt,type="l",ylim=range(0.6,max(Vt)), xlab="t",ylab=expression(V[t]))
abline(v=1)
# Add default free debt value as green line
lines(times,B*exp(-r*(T-times)),col=3)
# Add defaultable debt as red line
Bt <- B*exp(-r*(T-times)) - BlackScholes(times,Vt,r,sigmaV,B,T,type="put")
lines(times,Bt,col=2)

###radioshack example

require(timeSeries) load("RadioShack.RData") source("Black_Scholes.R")
# Use 2010 to March 2012 (as in Moody's example)
RadioShack = window(RadioShack, start="2010-01-01", end="2012-03-31") 
# Number of shares in millions (approximately) 
N.shares = 100 
# Value of equity in millions of dollars (approx)
Svalues = RadioShack*N.shares
# Equity vol 
sigmaS <- as.numeric(sd(returns(Svalues)))*sqrt(250)
# Value of one-year debt in millions approximately 
B = 1042
Vvalues = timeSeries(rep(NA,length(Svalues)),time(Svalues))
par(mar=c(3,3,2,1),mgp=c(2,1,0))
plot(Svalues,ylab="Equity")

#iterative solution
rooteqn <- function(V,S,t,r,sigmaV,B,T) { S - BlackScholes(t,V,r,sigmaV,B,T,"call") }
# Initial estimate of volatility
sigmaV <- sigmaS 
sigmaV.old <- 0
it = 1
# iterative solution for asset values
while (abs(sigmaV-sigmaV.old)/sigmaV.old > 0.000001) 
{ it = it + 1
  for (i in 1:length(Svalues)){
    tmp = uniroot(rooteqn, interval =c(Svalues[i],10*Svalues[i]),
                  S=Svalues[i],t=0,r=0.03,sigmaV=sigmaV,B=B,T=1)
    Vvalues[i] = tmp$root }
sigmaV.old = sigmaV
sigmaV = as.numeric(sd(returns(Vvalues)))*sqrt(250) }

# results 
DD <- (log(Vvalues[length(Vvalues)])-log(B))/sigmaV
results<-c(it=it,sigmaS=sigmaS,V0=Vvalues[length(Vvalues)],sigmaV=sigmaV,DD)

# graphs
par(mar=c(3,3,2,1),mgp=c(2,1,0))
plot(Vvalues,ylim=range(Vvalues,Svalues),ylab="Values")
lines(Svalues,col=2)
legend(x=1265200000,y=1100,legend=c("Asset value","Equity value"), lty=c(1,1), col=c(1,2))



####loss distribution####

laplace.transform <- function(t,pd,exposure,lgd=rep(1,length(exposure))) {
  output <- rep(NA,length(t))
  for (i in 1:length(t))
    output[i] <- exp(sum(log(1-pd*(1- exp(-exposure*lgd*t[i]))))) 
    output } 
# no common factor for simplicity 
m <- 20
exposure <- c(5,5,5,5,10,10,10,10,20,20,20,20,30,30,30,30,40,40,40,40)
pd <- c(rep(0.1,10),rep(0.05,10))
N <- sum(exposure)+1
t <- 2*pi*(0:(N-1))/N
cfunction <- laplace.transform(-t*(1i),pd,exposure)
par(mar=c(3,3,2,1),mgp=c(2,1,0)) plot(cfunction)
fft.out <- round(Re(fft(cfunction)),digits=20)
probs <- fft.out/N
barplot(probs[(0:20)*5+1],names.arg=paste((0:20)*5))




####Estimation from Default Data####



