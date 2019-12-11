library(tseries)
library(TSA)

izvoz.vhod <- read.table("Izvoz_Avstrija_letni.txt")
izvoz.t <- izvoz.vhod[,1]
izvoz <- izvoz.vhod[,2]
ts.plot(izvoz)

# smo ugotovili, da ko odstranimo linearni trend, dobimo neprepricljiv ar
# bolje - logaritmiramo in diferenciramo
izvoz.log <- log(izvoz)
izvoz.log.d <- diff(izvoz.log)
ts.plot(izvoz.log.d)
# smo ugotovili, da je to verjetno beli šum
acf(izvoz.log.d)
#špica je pri zamiku 0

# kako napovemo beli šum
izvoz.log.d.m <- mean(izvoz.log.d)
izvoz.log.d.m # = 0.0910855
# avstrijski izvoz se je v povprecju povecal za 0.0910855 torej 9% na leto
# lahko rečemo da bo tako tudi v prihodnje
# ampak  morda smo se zmotili, fino postaviti napovedni interval za to koliko smo se zmotili
# fino je vedeti kako je zadeva porazdeljena, da naredimo napovedni interval
# splošen adut je gaussova porazdelitev
# pogledamo koliko je zadeva blizu gaussa
# kako to vizualno pogledamo
# primerjalni kvantilni grafikon, qq grafikon

qqnorm(izvoz.log.d)
# malo skrivljeno, ampak imamo malo podatkov
# to nam pove koliko qq grafikon odstopa od premice
shapiro.test(izvoz.log.d)
# ne pokaže nič posebnega glede odstopanja
# torej je podobna normalni
# lahko uporabimo napovedni interval za normalno porazdelitev

izvoz.log.d.s <- sd(izvoz.log.d)
q <- qnorm(0.975);q # 1.96

# zgornja in spodnja meja
izvoz.log.d.zg <- izvoz.log.d.m + q*izvoz.log.d.s
izvoz.log.d.sp <- izvoz.log.d.m - q*izvoz.log.d.s

c(izvoz.log.d.zg, izvoz.log.d.sp)
# ker imamo malo podatkov, pride široko

exp(c(izvoz.log.d.sp, izvoz.log.d.zg)) - 1
# torej od -24% dol in do 58% gor - zelo velik razpon

# želimo napovedati
# oddiferenciramo
n <- 5
# vzamemo zadnjo opaženo vrednost
ivoz.log.nap <- tail(izvoz.log,1) + n*izvoz.log.d.m

izvoz.log.nap.s <- sqrt(n) * izvoz.log.d.s
izvoz.log.nap.zg <- izvoz.log.nap + q*izvoz.log.nap.s
izvoz.log.nap.sp <- izvoz.log.nap - q*izvoz.log.nap.s

izvoz.nap <- exp(izvoz.log.nap)
izvoz.nap.sp <- exp(izvoz.log.nap.sp)
izvoz.nap.zg <- exp(izvoz.log.nap.zg)

c(izvoz.nap.sp,izvoz.nap,izvoz.nap.zg)

# TEMPERATURE

temp.vhod <- read.table("Temp_LJ_NASA.txt", header = TRUE)
temp.matrika <- as.matrix(temp.vhod[57:90, 2:13])
temp.m <- c(t(temp.matrika))
temp.t <- seq(1,length(temp.m))

temp.cos <- cos(pi/6*temp.t)
temp.sin <- sin(pi/6*temp.t)
# še dvojna perioda
temp.cos2 <- cos(pi/3*temp.t)
temp.sin2 <- sin(pi/3*temp.t)

# linearni model
temp.ltrig2 <- lm(temp.m ~ temp.t + temp.cos + temp.sin + temp.cos2 + temp.sin2)

temp.ltrig2$coefficients[1]
temp.ltrig2$coefficients["(Intercept)"]
# pogledamo residuale
temp.ltrig2.rez <- temp.ltrig2$residuals

ts.plot(temp.ltrig2.rez)
# iygleda kot staciornani proces

temp.ltrig2.rez.arma <- arima(temp.ltrig2.rez., c(5,0,0))

qqnorm(temp.ltrig2.rez.arma$residuals)
#izgleda kot premica

shapiro.test(temp.ltrig2.rez.arma$residuals)
# ne zavrne

predict(temp.ltrig2.rez.arma, n.ahead=12)
predict(temp.ltrig2.rez.arma, n.ahead=12)$pred -> temp.ltrig2.rez.nap
# predict vrne napovedi, torej časovno vrsto #pred
# in standardne napake, te napake skonvergirajo
# ker to je stacionaren proces

# arma procesu moramo dodati ustrezen trend, in sin in 
temp.nap.t <- tail(temp.t,1) + seq(1,12)

#zgeneriramo še sin in cos
temp.nap.cos <- cos(pi/6*temp.nap.t)
temp.nap.sin <- sin(pi/6*temp.nap.t)
temp.nap.cos2 <- cos(pi/3*temp.nap.t)
temp.nap.sin2 <- sin(pi/3*temp.nap.t)

# koeficienti modela
koef <- temp.ltrig2$coefficients
koef <- as.vector(koef)

#koef[1 je intercept
temp.ltrig2.rez.nap + koef[1] + koef[2]*temp.nap.t + koef[3]*temp.nap.cos + koef[4]*temp.nap.sin + koef[5]*temp.nap.cos2 + koef[6]*temp.nap.sin2



