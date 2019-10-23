read.table("Temp_LJ_NASA.txt", header=TRUE)[c("YEAR", "metANN")] -> tempLJ.vhod
tempLJ.leto <- tempLJ.vhod[,"YEAR"]
#vejico rabimo, da vrne čisti vektor, drugače vrne stolpec v tabeli
tempLJ <- tempLJ.vhod[,"metANN"] 
tempLJ <- as.vector(tempLJ)
tempLJ.leto <- as.vector(tempLJ.leto)
plot(tempLJ.leto, tempLJ, type ="l", xlab = "leto", ylab= "temperatura")

# Komentar: Iz grafa se vidi, da je ogrevanje od sredine 80tih v porastu.
# Primerjali bomo kako je bilo pred letom 80 in po letu.

tempLJ.vhod 
#leto 2019 je ekstrapolirano, index pri 1985 je 57

tempLj_kasn <- tempLJ.vhod[57:90, "metANN"]
#ali
tempLJ[57:90]

tempLJ_kasn.t <- 1985:2018

#namesto tetga lahko uporabimo tudi časovne vrste:
tempLJ.ts <- ts(tempLJ, start= 1929)

#če želimo časovno vrsto trenutkov, uporabimo funckijo time
time(tempLJ.ts) #vrne ČASOVNO VRSTO ne vektorja
window(tempLJ.ts,1929,1985)
rbind(tempLJ_kasn.t, tempLj_kasn)


#naredimo linerani model
tempLj_kasn.lm <- lm(tempLj_kasn ~ tempLJ_kasn.t)
summary(tempLj_kasn.lm)
# Komentar: Vsako leto se v povprečju segreje za 0.052793
abline(tempLj_kasn.lm, col = "red")


#poglejmo za bolj zgodnje temperature
tempLJ_zg <- tempLJ[1:56]
tempLJ_zg.t <- 1929:1984
tempLJ_zg.lm <- lm(tempLJ_zg ~ tempLJ_zg.t)
abline(tempLJ_zg.lm, col = "green")
# Komentra: Je viden trend naraščanja a bistveno manjši. 
summary(tempLJ_zg.lm)
# Komentar: 0.008373 stopinje na leto. P vrednost je 0.0896, torej segrevanje niti ni bilo statistično značilno do leta 85. Od leta 85 naprej pa je statistično zelo zelo značilno.


# Lahko gledamo bolj komplicirane trende kot linerane, npr. kvadraten trend. Narišemo oba in primerjamo.
tempLJ.t <- 1929:2019
plot(tempLJ.t, tempLJ, type = "l", xlab = "leto", ylab = "temperatura")
#linearen model
tempLJ.lm <- lm(tempLJ ~ tempLJ.t)
abline(tempLJ.lm)
#kvadraten model
lm(tempLJ ~ tempLJ.t + tempLJ.t^2) 
#ne moremo tako: zgubi funckijo kvadriranja. Če želimo ohranit kvadrat imamo dve varianti:
#1. vektor kvadratov
#2. damo v I
tempLJ.quad <- lm(tempLJ ~ tempLJ.t + I(tempLJ.t^2))

# PRIMERJAMO
anova(tempLJ.lm, tempLJ.quad)
#naredi f test in primerja, koliko ima kvadratni model značilen prispevek. Koliko bolj značilno pojasni zadevo kot lineraen.
#Komentar: zgleda, da značilno pojasni -> 0.000119 ***
#kratkoročno bo bolj pesimitičen, dolgoročno pa celo preveč
# višje stopnje modelov po navadi ne gledamo. Lahko bi narisali polinom na n-1 točkah in bi imeli popolno prileganje, a to ni v redu, saj imamo šum zraven in ne moremo reči, d aje ta model pravi.

tempLJ.quad.coef <- c(tempLJ.quad$coefficients)
tempLJ.quad.f <- function(t) {sum(tempLJ.quad.coef*c(1,t, t^2))}
tempLJ.quad.f(2019)
tempLJ.quad.f(2029)
tempLJ.quad.f(2100)

plot(tempLJ.t, tempLJ, type = "l", xlab="leto", ylab="temperatura",xlim=c(1985,2100), ylim=c(10, 20))

#plot(tempLJ.quad.f, 1929, 2019)
#izvede funckijo f na vektorju, če ne bo javil napako. Vse vgrajene funckije so definirane tudi na vektorjih:
sin(1)
sin(c(1,2,3,4))
# sapply iz funckije k sprejem skalar4 naredi funckijo, ki sprjeme vektor in izvrednoti v komponentah vektorja
sapply(c(1,2,3,4), sin)
tempLJ.quad.fv <- function(v) {sapply(v, tempLJ.quad.f)}
plot(tempLJ.quad.fv, 1985, 2100, add=TRUE) 
#vidimo, kakšen je kvadraten trend


## STACIONARNOST
# če hočemo določit ali je vrsta stacionarna imamo 3 metode:
# 1. gledamo vanjo :) 
# 2. autokorelacijska funkcija
# 3. testi: KPSS, ADF

# SIMULIRANJE VREDNOSTI
# Gauss beli šum
beli <- rnorm(100)
ts.plot(beli)
acf(beli) #nariše autokorelacijsko funckijo
# modra črta je interval zaupanja, če pogleda čez je statistično značilna pri stopnji tveganja 5 procentov
# Komentar: 3 na meji statistične zančilnosti pir njemu, a vska ima drugačno simulacijo.
# Testa:
library(tseries)

kpss.test(beli)
# p-value = 0.1
# odstopanja od stacuionarnosti niso statistoično značilna, ne moremo reči, da je čv nestacionarna


adf.test
# p-value = 0.01 
# lahko se zgodi da je p npr p= 0.2 in ne potrdimo stacionarnosti


#belemu šumu dodamo trend:
beli_trend <- beli + 0.05 *(1:100)
ts.plot(beli_trend)
acf(beli_trend)

# nemška marka do zadnjič
dm <- scan("DM.txt")
ts.plot(dm)
acf(dm)

#kpss test bi moral vrnit nizko vrednost, saj beli_trend ni stacionarna časovna vrsta
kpss.test(beli_trend)
# p-value = 0.01 

kpss.test(dm)
#podobno kot pri beli_trend, vrže nizko p vrednost

#pri adf bi morali dobiti visoko p vrednost
adf.test(beli_trend)
# dobimo nizko p vrednost....?
# adf test ODSTRANI LINERANI TREND, zato da nizko p vrednost in potrdi stacionarnost(po odstranjenem trendu)

adf.test(dm) # ne bi smel potrdit stacionarnosti
# p-value greater than printed p-value  V REDU

beli.t <- 1:100
beli_trend.lm <- lm(beli_trend ~ beli.t)
# premica ki s eprilega podatkom ni čisto enaka tisti kot smo generiral trend ( z 0.2 in 0.05), saj imamo šum zraven

kpss.test(beli_trend.lm$residuals) #residuali so izravnana časovna vrsta, p vrednost bi morala biti visoka, saj so residuali podobni originalni časvoni vrsti beli


# ni vsaka časovna vrsta, ki je stacionarna beli šum, recimo po arma modelu:
# funckija arma.sim  simulira arma modele
arma <- arima.sim(n=500, model=list(ar=c(0.7,-0.3), ma=c(0.5,0.4)))
plot(arma)
# Komentar: ni vidne rzalike med njo in belim šumom, a če pogledmao autokorelacijsko funckijo dobimo drugače
acf(arma)
# dobimo ČASOVNO VRSTO
# vidimo sinusni trend, dušen sinus so te autokorelacije, ampak časovna vrsta je še vedno stacionarna

kpss.test(arma)
# ne zavrne stacionarnosti
adf.test(arma) # zavrnit bi moral nestacionarnost in potrdit stacionarnost-> majhan p vrednost

# dodajmo kvadratni trend
arma.t <- seq(-249.5, 249.5) #zato da bo centirano imn na ta način jih bo točno 500
arma_quad <- arma + 1e-4*arma.t^2
ts.plot(arma_quad)

kpss.test(arma_quad)#ta časovna vrsta ni stacionarna, moral bo vrnit nizko p vrednost
adf.test(arma_quad) #moral bi vrnit visoko p vrednost


# Primer: dež v LA
dezLA.vhod <- read.table("Padavine_LA.txt")
dezLA.t <- dezLA.vhod[,1]
dezLA<- dezLA.vhod[,2]
dezLA.ts <- ts(dezLA, start= head(dezLA.t,1))
dezLA.ts
plot(dezLA.ts)
dezLA.lm <- lm(dezLA~dezLA.t)
abline(dezLA.lm)
summary(dezLA.lm)
#ni zančilno

acf(dezLA)
# zelo zgleda kot beli šum 

kpss.test(dezLA) # pričakujemo visoko p vrednost
adf.test(dezLA) # ni tako nizka, nekako na meji je
# nizki beli šum, ali pa ni krepki, ni gaus beli šum
# mogoče pa slučajno pride na 0.05