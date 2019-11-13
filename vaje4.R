library(tseries)

# Dow_Jones.txt -> z DIFF naredili stacionarno
DJ.vhod <- read.table("Dow_Jones.txt", header = TRUE)
DJ.vhod
DJ <- rev(DJ.vhod[,2]);DJ
ts.plot(DJ)
DJ.log <- log(DJ)
ts.plot(DJ.log) #samo malo se premakne
acf(DJ.log) # ni nič stacionarno!!
#ali vidimo kakšen trend? bolj slabo vidt...lahko si mislimo, da je ta tečaj..pomembne so sprembe tečaja v procnetih
# včasih gre dol včasih dol
# katera transformacija časovne vrste bo tista ki bo stacionarna
# v tem primeru so diference tiste
DJ.log.d <- diff(DJ.log)
ts.plot(DJ.log.d)
# ALI JE VIDETI STACIONARNO? vidne so oscilacije
acf(DJ.log.d)
# ni nič posebnega, kakšna že izstopa
kpss.test(DJ.log.d)
# ne zavrne stacionarnmosti
adf.test(DJ.log.d) 
# potrdi stacionarnost


# Nemška marka
dm<- scan("DM.txt")
ts.plot(dm)
dm.d <- diff(dm)
ts.plot(dm.d) #zgleda zelo stacionarno
acf(dm.d)
# ne ovrže stacionarnosti ni pa videt kot beli šum, saj imamo korelcaijo pri prvih
kpss.test(dm.d)
# p vrednost majhna, zavrže stacionarnost
# dolga čv 1867, če imamo veliko podatkov in bodo malenkostna odstopanja do domneve jih bo test prepoznal in zavrnil domnevo, pa mogoče čv sploh ni tako ne stacionarna.
adf.test(dm.d)

#LAHKO ŠE ENKRAT DIFERENCIRAMO
dm.dd <- diff(dm.d)
acf(dm.dd)
# diferenca iz belega procesa Z_t naredi proces Z_t - Z_(t-1) in to je kolerirano 
kpss.test(dm.dd) #ne zavrne stacionarnosti je pa vprašanje ali je vredno dvakrat diferencirat
# glej ZVEZEK
ts.plot(dm)


# SIMULIRALI BOMO AUTOREGRESIJSKI MODEL(zvezek)
sim_ar <- arima.sim(list(ar=0.995),2000)
plot(sim_ar) # to je zdaj časovna vrsta ni trebna ts.plot
ts.plot(cumsum(sim_ar)) # kumulativna
kpss.test(sim_ar) # zavrne stacionarnost
# to bomo zabrisal z belim šumom
sim_ar_wn <- sim_ar + ts(rnorm(2000, sd=70))
plot(sim_ar_wn) 
# zelo podobno kot so bile diference od marke
acf(sim_ar_wn)
kpss.test(sim_ar_wn) #zavrnil
ts.plot(cumsum(sim_ar_wn))

ts.plot(dm)
#tečaj marke: trend padanja potem pa trend naraščanja, prav tako tudi pri sim_ar

#če časovna vrsta res tako nastane kot vsota dveh različnih modelov, potem naše metode ki jih bomo obravnavali ne bodo dobre na tem tečaju
# diference tečajev so s pridržkom arma ali beli šum


# SIMULIRALI BOMO AUTOREGRESIJSKI MODEL #2 (zvezek)
arsim <- arima.sim(list(ar=c(0.7,-0.3)),500)
plot(arsim)
acf(arsim)
# ne zgleda kot beli šum, saj značilno za prva dva
pacf(arsim) #vidimo za prva dva značilno
kpss.test(arsim) #testira stacionarnost in ne bo je zavrnil
# pomembno ali je beli šum
Box.test(arsim) # zavrnil

# recimo, da smo servirali to časovno vrsto nekomu inga vprašamo kaj je to?
# treba ocenit koeficiente

# FUNKCIJA AR 
ar(arsim)
# funckija ar sama rede zbere, enim 2 enim 3

# FUNCKIJA ARIMA
arima(arsim, c(2,0,0))
# funkcija odšteje povprečje
arima(arsim + 5, c(2,0,0)) #dobimo popolnoma iste ocene, poveča pa intercept
kpss.test(arsim)
Box.test(arsim)

# AIC tabela
arima(arsim, c(2,0,0))$aic
# [1] 1428.931
# pogledamo za različne rede
arima(arsim, c(3,0,0))$aic
#[1] 1441.042
# to je več
arima(arsim, c(0,0,2))$aic
# naredimo tabelo 
AIC.empty <- matrix(nrow=4,ncol=4)
rownames(AIC.empty) <- sprintf("AR=%d", 0:3)
rownames(AIC.empty)
colnames(AIC.empty) <- sprintf("MA=%d", 0:3)
AIC <- AIC.empty
for(p in 0:3) {
  for(q in 0:3){
    AIC[p+1, q+1] <- arima(arsim, c(p,0,q))$aic
  }
}
# katera vrednost je minimalna?
which.min(AIC) # ARMA(0,2)
# najprej za fixen stolpec po vrsti šteje 
# Basrak: VSOTA REDOV NAJ NE BI PRESEGALA 3, če se gremo ARMO! (če imamo samo AR pa ja)

arsim.ar2 <- arima(arsim, c(2,0,0))
arsim.ar2.rez <- arsim.ar2$residuals # te reisudali bi morali bit beli šum
Box.test(arsim.ar2.rez)
# metoda je uspešno odpravila korelacije
acf(arsim.ar2.rez)
armasim <- arima.sim(list(ar=c(0.7,-0.3),
                          ma =c(0.5,0.4)), 500)
arima(armasim, c(2,0,2)) # pogledamo kako zadane-> manj natančno, ker je bolj kompliciran model


# AIC tabela
# naredimo tabelo 
AIC2.empty <- matrix(nrow=4,ncol=4)
rownames(AIC2.empty) <- sprintf("AR=%d", 0:3)
rownames(AIC2.empty)
colnames(AIC2.empty) <- sprintf("MA=%d", 0:3)
AIC2 <- AIC2.empty
for(p in 0:3) {
  for(q in 0:3){
    AIC2[p+1, q+1] <- arima(armasim, c(p,0,q))$aic
  }
}
which.min(AIC2) #arma(2,2)
# bolj komplicirani modeli so manj zanesljivi, ar(2) je v večini primerov prepoznal arma(2,2) pa ni prepoznal
# preveč komplicirani primeri niso najboljši 

