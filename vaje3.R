## DEŽ V LA

dezLA.vhod <- read.table("Padavine_LA.txt")
dezLA <- dezLA.vhod[,2]
ts.plot(dezLA) #zgleda stacionarno
acf(dezLA)
#kakšni procesi imajo tako autokorelacijsko funkcijo? SLučajni, beli šum
# beli šum ima vse korelacije enake 0 navzkrižno
# s temi testi probavamo stacionarnost pokazat

# TEST STACIONARNOSTI
library(tseries)

kpss.test(dezLA) 
#pri testu kpss je ničelna domneva: časovna vrsta stacionarna -> p vrednost mora biti velika, da dokažeš stacionarnost
# p-value = 0.1 ne moremo zavrnit stacionarnosti

adf.test(dezLA)
#pri testu kpss je ničelna domneva: časovna vrsta NI stacionarna -> p vrednost mora biti majhna, da dokažeš stacionarnost
# p-value = 0.05218 ni čist zavrnil stacionarnosti, ampak autokorelacijska funckija je tako lepa..? :D 

Box.test(dezLA)
# #pri testu box-pierceov je ničelna domneva: časovna vrsta je BELI ŠUM 
# p-value = 0.7257 ne moremo zavrnit da je beli šum, ne mormeo pa niti potrdit


## PRETOK REKE NIL (vgrajena)
Nile
ts.plot(Nile)
#vidimo trend upadanja zato sumimo, da ni stacionarna

kpss.test(Nile) # pravi, da ni stacionarna

Nile.t <- time(Nile) # časovna vrsta iz časov

#ODSTRANIMO TREND
Nile.lm <- lm(Nile ~Nile.t)
abline(Nile.lm, col="red")
# vidimo padanje 
# zdi se da je prelomna točka -> vzamemo samo rep časovne vrste

#poglejmo ali je trend značilen:
summary(Nile.lm)
# p vrednost  1.07e-06 *** -> padanje zelo zelo značilno

Nile.lm.rez <- Nile.lm$residuals
# funckija residuals zbriše strukturo časovne vrste dodamo ts.plot
ts.plot(Nile.lm.rez)

acf(Nile)
# zgleda sumljivo
acf(Nile.lm.rez)
# boljši graf

#parcialni acf (PARTIAL ACF)
pacf(Nile.lm.rez)

kpss.test(Nile.lm.rez)
adf.test(Nile.lm.rez)

Box.test(Nile.lm.rez) 
# test pravi časovna vrsta je beli šum, autokorelacije so nič
# v tem primeru če pogledamo acf sklepamo da bo najbrž zavrnil da je beli šum
# p-value = 0.0001773 

# Nil zgleda, da imamo lineraen trend + še eno stcaionarno časovno vrsto, ki pa ni beli šum. 
# Ima pa določene lokalne autokorelacije 

plot(Nile)
# poglejmo od 1900 naprej
Nil_k <- window(Nile, start=1900)
Nil_k

Nil_k.t <- time(Nil_k)
Nil_k.lm <- lm(Nil_k ~Nil_k.t)
summary(Nil_k.lm) # ni statistično značilno

Nil_k.lm.rez <- Nil_k.lm$residuals

#če ni značilno ne bi na silo odstranjevali trenda, poglejmo acf
acf(Nil_k)
# v povprečju ena od 20 statistično značilna

kpss.test(Nil_k)
# ne potrdi stacionarnosti
adf.test(Nil_k)
# potrdil stacionarnost
Box.test(Nil_k)
# ne zavrne

# zgleda je imel prelomno točko okoli 1900, zdaj je pa že dobrih 100 let stacionaren. 
# nauk: potrebno je podrobno gledati graf in pogledamo kaj se nam zdi
# pogledamo od kje naprej, če so slučajno kakšne prelomne točke
# časovne vrste še niso eksaktna veda :)

## AVSTRIJSKI IZVOZ
izvoz.vhod <- read.table("izvoz_Avstrija_letni.txt")
izvoz.t <- izvoz.vhod[,1]
izvoz <- ts(izvoz.vhod[,2], start = 1960) # naredimo časovno vrsto
plot(izvoz)
# eksponentni trend
# take eksponentne po navadi LOGARITMIRAMO, saj je lažje delat
# ni dobro narediti: linerani modle v katerem aproksimiramo z eksponentno funckijo
# zakaj ne? -> ZVEZEK

# volatilnost lahko pokažemo 
izvoz.d <- diff(izvoz)
plot(izvoz.d)
# vidimo da so diference čedalje večje
# ZGLADIMO JIH s funckijo filter (glej zvezek kaj dela)
izvoz.d.t <- filter(izvoz.d, rep(1/7,7))
# zgladimo na zamik 7 
# povprečja sedmih, ampak teh sedem gre okol in okol, torej so prve tri NA

izvoz.d.f <- sqrt(filter(izvoz.d ^2, rep(1/7,7)))
plot(izvoz.d.f)
# difrence zglaejen in podobne eksponentni funkciji
# pokazali bomo volatilnost ampak ne v odvisnosti od časa ampak od vrednosti
plot(tail(izvoz, -1), izvoz.d.f)
# skrjšamo izvoz za 1 ker smo vzeli diference

# glej zvezek-> f linearne funckija
# če se graf zdi kot linearna funckija je za logaritmirat
izvoz.log <- log(izvoz)
plot(izvoz.log)
# zgleda zelo kot linearni tredn 
izvoz.log.lm <- lm(izvoz.log ~ izvoz.t)
abline(izvoz.log.lm, col = "green")
izvoz.log.lm.rez <- izvoz.log.lm$residuals
ts.plot(izvoz.log.lm.rez)
acf(izvoz.log.lm.rez)
# korelacije imajo sinusno obliko
# pogledamo parc
pacf(izvoz.log.lm.rez)
# izvoz v danem letu s eizraža kot konstanta plus izvoz v prejnšnjem letu + napak a+ lineraen trend
kpss.test(izvoz.log.lm.rez)
adf.test(izvoz.log.lm.rez)
Box.test(izvoz.log.lm.rez)
# zavrne

## TEMPERATURE LJUBLJANA
# SEZONSKI TRENDI
tempLJ.vhod <- read.table("Temp_LJ_NASA.txt", header = TRUE)
# ima prelomno točko
tempLJ.matrika <- as.matrix(tempLJ.vhod[57:90, 2:13])
# narediti moramo dolg vektor
# najlažje s c, ampak r gre defaultno po stolpcu, najprej jan potem feb itd..zato jo še transponiramo
tempLJ_m <- c(t(tempLJ.matrika))
ts.plot(tempLJ_m)
# ni videti globalnega segrevanja 
# radi bi odstranili sezonska nihanja
acf(tempLJ_m)
# sinusne oblike 
kpss.test(tempLJ_m)
# zgleda stacionarna, ima trend a trend se ne vidi zaradi nihanj in ta nihanja zmedejo kpss test
Box.test(tempLJ_m)
# zavrne zaradi korelacij

# KAKO ODSTRANIT SEZONSKA NIHANJA?
# če ne vemo kakšna so nihanja imamo dve funckiji:
# 1
spectrum(tempLJ_m)
# pokaže za določeno frekvenco koliko je močna komponenta nihanja z določeno frekvenco. 
#vidimo, da imamo špico pri 1/12
# frekvenca 1/12 pomeni periodo 12

# 2
library(TSA)
periodogram(tempLJ_m)
# čisto pokaže 
# pri 1/12 nihanje

# ena možnost je TRIGONOMETRISJKI MODEL, temperatura je odvisna od časa, od cos časa in sin časa
tempLJ_m.t <- seq(1,length(tempLJ_m))
tempLJ_m.cos <- cos(pi/6*tempLJ_m.t)
# cos in sin se merita v radiani torej ima periodo 2pi mi želimo perido 12
tempLJ_m.sin <- sin(pi/6*tempLJ_m.t)
tempLJ_m.ltrig <- lm(tempLJ_m ~ tempLJ_m.t + tempLJ_m.cos + tempLJ_m.sin)
tempLJ_m.ltrig.rez <- tempLJ_m.ltrig$residuals
ts.plot(tempLJ_m.ltrig.rez)
# zgleda stacionarno
periodogram(tempLJ_m.ltrig.rez)
# zadeva kaže perido 6, nimamo samo letnih nihanj ampak tudi polletna nihanja bi človek reku :)
# taka polletna nihanja so izrazita oziroma prevladujejo na ekvatorja(tam imamo simetrično)
tempLJ_m.cos2 <-cos(pi/3*tempLJ_m.t)
tempLJ_m.sin2 <-sin(pi/3*tempLJ_m.t)
tempLJ_m.ltrig2 <-lm(tempLJ_m ~ tempLJ_m.t + tempLJ_m.cos2 + tempLJ_m.sin2)
tempLJ_m.ltrig2.rez <- tempLJ_m.ltrig2$residuals
periodogram(tempLJ_m.ltrig2.rez)

acf(tempLJ_m.ltrig.rez)
# sinusna zadeva 
pacf(tempLJ_m.ltrig.rez)
kpss.test(tempLJ_m.ltrig.rez)
# ni zavrnil
# box test bi pa moral zavrnit
Box.test(tempLJ_m.ltrig.rez)
#ni beli šum, to se vidi iz acf

acf(tempLJ_m.ltrig2.rez)
Box.test(tempLJ_m.ltrig2.rez)
# zavrne, samo malo manj prepričljivo
anova(tempLJ_m.ltrig, tempLJ_m.ltrig2)
# zančilno

meseci <- sprintf("%02d", 1:12)
# "%02d" izpiše se na dve mesti z vodilno 0

tempLJ_meseci <- rep(meseci, length.out = length(tempLJ_m))
tempLJ_m.lmes <- lm(tempLJ_m ~ tempLJ_m.t + tempLJ_meseci - 1)
# ni potreben svoboden člen, zbrišemo ga z minus 1
tempLJ_m.lmes.rez <- tempLJ_m.lmes$residuals
periodogram(tempLJ_m.lmes.rez)
acf(tempLJ_m.lmes.rez)
anova(tempLJ_m.ltrig2, tempLJ_m.lmes)
# ni značilno dodane vrednosti, ta modle za vsakl mesec posebaj ne pojasni bistveno bolj kot če imamo enojno pa še dvojno periodično nihanje

