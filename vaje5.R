AIC.prazna <- matrix(nrow=4, ncol=4)
rownames(AIC.prazna) <- sprintf("AR=%d", 0:3)
colnames(AIC.prazna) <- sprintf("MA=%d", 0:3)
AIC  <- function(x) {
  tabela <- AIC.prazna
  for (p in 0:3) {
    for (q in 0:3) {
      tabela[p+1, q+1] <- arima(x, c(p,0,q))$aic
    }
  }
  return(tabela)
}


# 1. IZVIR
faithful
er <- faithful$eruptions
ts.plot(er)
# zgleda stacionarno, ni da bi imeloa kakšen trend


library(tseries)
library(TSA)
kpss.test(er)
# p-value = 0.1 -> ne zavrne stacionarnosti


acf(er)
# zgleda stacionarna, a ni vidt beli šum
# red 0 je pokazal, reda 1, 2 signifikantna

pacf(er)
# zgleda kot AR(1), malo je na robu

ar(er) 
# zbere en 
#Coefficients:
# 1  
#-0.5492  
# zmodelira kot X_t = -0.5492* X_(t-1) + Z_t

# Kaj pravi akaike? :)
tabela <- AIC(er)
which.min(tabela)
# akaike predlaga MA(3)
# to je sodeč po acf funkciji bolj AR(1)
# tu pride v poštev lastna presoja, saj tudi vrednosti v tabeli niso tako različne
# AKAIKE upošteva tudi kompleksnost modela, kompleksnim modelom nabije kazenske točke :)

# Videli bomo, da so AR in MA isti šmorn. Vsak pameten ARMA modle pri katerih so koeficienti razumno omejini, 
# torej, da je analitična funckija definiriana še malo naprej od 1. V tem primeru arma ustreza za ma(q) za q je neskončno, 
# torej imamo konvergentno neskončno vrsto
# Tudi obratno se da narediti, da iz MA v AR. Zato se lahko zgodi, da en klic vrne modle ar drugi pa ma. 

arima(er, c(1,0,0))
# ocveni naa -0.5487, to ni bistveno drugače kot ar zgoraj

arima(er, c(0,0,3))
# ma3 je -0.1276 , približno dvakrat standarna napaka, torje na meji značilnosti

####################################################################################################################
# 2. DONOSI DIVIDEND NEKE DELNICE
# mesečni donosi
SDYR.vhod <- read.table("SDYR.txt")
SDYR <-ts(SDYR.vhod$V2, start=c(1947,1),
          frequency =12)
plot(SDYR)

acf(SDYR)
# treba pogledat ali je zamik 1 pokazan -> ni prvi je 1, ozrioma zamik 1/12
# zgleda kot ma(1)

pacf(SDYR)

Box.test(SDYR)
ar(SDYR)
tabela <- AIC(SDYR); tabela
which.min(tabela)
# AIC predlaga MA(1), AR(2) je predlagala funckija AR
# raič bi se oprl na akaike in vzel MA(1) :)

arima(SDYR, c(0,0,1))


####################################################################################################################
# 3. LETNI AVSTRIJSKI IZVOZ 
izvoz.vhod <- read.table("Izvoz_Avstrija_letni.txt")
izvoz.t <- izvoz.vhod[,1]
izvoz <- ts(izvoz.vhod[,2], start=1960)
plot(izvoz)
# ta izvoz smo že logaritmirali, saj se vidi da zadeva eksponetno raste
#logaritmiramo:
izvoz.log <- log(izvoz)
plot(izvoz.log)
# sedaj zgleda kot linearen tredn, zadnjič smo ga tudi odstranili

# odstranimo linerani trend:
izvoz.log.lm <- lm(izvoz.log ~ izvoz.t)
izvoz.log.lm.rez <- izvoz.log.lm$residuals
ts.plot(izvoz.log.lm.rez)
# zelo niha a zgleda stacionarna, človek bi se vprašal ali je tu kakšna perioda?
periodogram(izvoz.log.lm.rez)
# če bi neka špica štrlela, potem bi bile kakšne periode, tako pa nekako ni vidt

kpss.test(izvoz.log.lm.rez)
acf(izvoz.log.lm.rez)
# zgleda sumljivo nekje pri 5; če bi zbrali MA model, bi moral biti z zamikom 6
pacf(izvoz.log.lm.rez)
# AR je videti boljš, a še vedno imamo pri 5 sumljivo, mogoče zaradi male špice pri periodičnosti
# človek bi rekel, da je to AR(1)

ar(izvoz.log.lm.rez)
# red 5

tabela <- AIC(izvoz.log.lm.rez); tabela
which.min(tabela)
# ARMA(2,2)
# vsota 4 je kar malo velika
arima(izvoz.log.lm.rez, c(5,0,0))
# ta funckija vrne tudi aic informacijo
# BASRAK PREDLAGA ali AR z VELIKIM redom ali pa ARMA in vsota redov največ 3!
# Basrak bi se verjetno za AR odločil
ts.plot(izvoz.log)

# KAj je bil še drug način da smo naredili stacionarno ČV? -> DIFERENCIRANJE
izvoz.log.d <- diff(izvoz.log)
kpss.test(izvoz.log.d)
acf(izvoz.log.d)
pacf(izvoz.log.d)

ar(izvoz.log.d)
# izbere red 0 -> AR pravi, da je to beli šum
tabela <- AIC(izvoz.log.d); tabela
which.min(tabela)
# 15 -> ARMA(2,3)
Box.test(izvoz.log.d)
# ne zavrne da je to beli šum
# glede na to da je ar izbral 0, akaike visok red, mogoče ne bi kompliciral in rekli da diference tvorijo beli šum
# diferenciranje LAHKO DA SLABŠO NAPOVED, kot da odstranimo linerani trend

####################################################################################################################
# 4. LJUBLJANSKE PADAVINE
temp.vhod <- read.table("Temp_LJ_NASA.txt", header = TRUE)
#mesečne temperature
temp.matrika <- as.matrix(
  temp.vhod[57:90, 2:13])
temp.m <- c(t(temp.matrika))
ts.plot(temp.m)
# ni videti segrevanja
# po analizi periodograma smo opazili nihanja tudi na pol leta
# odstranili bom trend pa nihanja na eno leot in pol leta

temp.t <- seq(1, length(temp.m))
temp.cos <- cos(pi/6*temp.t) # ko se temp.t poveča za 12 se to za 2pi in to je prav
temp.sin <- sin(pi/6*temp.t)

temp.cos2 <- cos(pi/3*temp.t) # dvakrat hitreje niha dodamo krat 2 * (pi/6)
temp.sin2 <- sin(pi/3*temp.t) 

temp.ltrig2 <- lm(temp.m ~ 
                    temp.t + temp.cos + temp.sin
                  + temp.cos2 + temp.sin2)
temp.ltrig2.rez <- temp.ltrig2$residuals
ts.plot(temp.ltrig2.rez)
# zgleda bolj stacionarno 
acf(temp.ltrig2.rez)
# zgleda ma(1)
pacf(temp.ltrig2.rez)
# lahko bi bil ar(1)

ar(temp.ltrig2.rez)
# zbere red 5
tabela <- AIC(temp.ltrig2.rez); tabela
which.min(tabela)
# 8 -> ARMA(3,1)
# Basrak bi izbral red 5
# v tabeli vsi zelo blizu, testiramo za beli šum
Box.test(temp.ltrig2.rez)
# ne kaže na beli šum

arima(temp.ltrig2.rez, c(1,0,0))
#0.1519
# poglejmo če je prvi kaj dost drugačen, če zberemo red 5
arima(temp.ltrig2.rez, c(5,0,0))


####################################################################################################################
# 4. DOW JONES
DJ.vhod <- read.table("Dow_Jones.txt", header=TRUE)
DJ <- rev(DJ.vhod[,2])
ts.plot(DJ)
DJ.log.d <- diff(log(DJ))
ts.plot(DJ.log.d)
# pade v oči da pri 60 ful zaniha
acf(DJ.log.d)
# zgleda kot beli šum
Box.test(DJ.log.d)
# ne zavrne da je beli šum
acf(DJ.log.d^2)
# TRIK: če bi imeli neodvisne slučajne spremenljivke potem tudi acf na KVADRATNIH PRIDE 0 
# -> torej te zadeve so samo nekolerirane, volatilnost pa se spreminja
acf(izvoz.log.d^2) # tu ni tako


#DJ.log.d KANDIDAT ZA MODEL GARCH 

# SIMULACIJA MODELA GARCH
#install.packages("fGarch")
library(fGarch)
garchsim <- garch.sim(c(1,0.7), 0.2, n = 1000)
ts.plot(garchsim)
acf(garchsim)
pacf(garchsim)
# beli šum, če damo pa kvadraten pa zgleda: precej daljše črte
acf(garchsim^2)

# BASRAK PREDLAGA GARCH(1,1), če modeliramo z garch
garchFit(formula = ~garch(1,1), data = garchsim)

garchFit(formula = ~garch(1,1), data = DJ.log.d)
# sigma_t^2 = 0.7 sigma_(t-1)^2 + 0.175X_(t-1)
# X ne smemo zanemarit (alph) -> drugače bi sigme eksponentno padale

