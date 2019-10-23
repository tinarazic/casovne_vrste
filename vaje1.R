# PREPISANA 1. ura od Eve:
v <- c(100, 200, 300)
cumsum(v)
diff(v)
diff(v, lag = 2)
diff(v, differences = 2)
diff(v, differences = 1)

M <- matrix(1:15, 3, 5)
M <- matrix(1:15, 3, 5, byrow = TRUE)
M[2,3]
M[2,]
M[,3]
c(M)
t(M)
2 * M
M + 2
A <- rbind(c(2, -2), c(2, 2))
B <- rbind(c(2, 2), c(2, -2))
A * B
A %*% B
A %*% c(3, 4)
solve(A, c(-2, 14))
solve(A)

c(2, "bla")
v[1] + 2

list(2, "bla") -> l
l[1]
l[2]

nas_clovek <- list(ime = "Janez", starost = 35, zena = "Marija", starost_otrok = c(15, 13,2))
# $ime
nas_clovek[["ime"]]

nasi_ljudje <- data.frame()
nasi_ljudje <- data.frame(
  ime = c("Janez", "Franc", "Tone"),
  starost = c(32, 43,55),
  st_otrok = c(3, 2,4),
  zena = c("Marija", "Štefka", "Lojzka")
)
nasi_ljudje[1]
nasi_ljudje[c(1,3)]
as.vector(nasi_ljudje[,1])

############################################################################################################

# http://valjhun.fmf.uni-lj.si/~raicm/Poucevanje/CV/Datoteke/

#iz katere mape r bere
getwd()

setwd("U:/casovne_vrste")
scan("DM.txt") -> dm

# 1) gibanje te?aja nem?ke marke proti ameri?kem dolarju
plot(dm)
plot(dm, type ="l")
ts.plot(dm) #samo x os spremeni v time

# 2) padavine LJ
dezLJ <- read.csv("Padavine_LJ_ARSO.csv", header = TRUE)
dezLJ

#kronolo?ko uredimo
as.matrix(dezLJ[,2:13]) -> dezLJ.matrika
dezLJ.chron <- c(t(dezLJ.matrika))
ts.plot(dezLJ.chron)

# tip ?asovne vrste
dezLJ.chron.ts <- ts(dezLJ.chron)
dezLJ.chron.ts
#frequency koliko je podatkov na eno ?asovno enoto
dezLJ.matrika.ts <- ts(dezLJ.matrika)
plot(ts(dezLJ.matrika[,1:5], start=1991))

dezLJ.chron #navaden vektor
#naredimo ?asovno vrsto
ts(dezLJ.chron, start = 191, frequency = 12)


# 3) CO2
# ugrajena casovna vrsta
co2
plot(co2)

# TREND:
# RO?NO
# y = a + bx + e
# c_xy = sum_i^n (x_i - )

co2.v <- c(co2)
co2.t <- seq(1, length(co2))
co2.v.m <- mean(co2.v)
co2.t.m <- mean(co2.t)

b <-sum((co2.t - co2.t.m)*(co2.v -co2.v.m))/sum((co2.t -co2.t.m)^2)
a <- co2.v.m -b*co2.t.m

#AVTOMATICNO
co2.lm <- lm(co2.v ~ co2.t)
co2.lm

co2.lm <- lm(co2.v ~ co2.t - 1) #ZAČETNI koeficient odstrani
co2.lm

co2.lm <- lm(co2.v ~ co2.t)
co2.lm

plot(co2.v, type = "l")
abline(co2.lm)
co2.lm$coefficients["co2.t"]
summary(co2.lm)
#p test 

#korelacijski test
cor.test(co2.t, co2.v)

#residuali
#ročno:
co2.v - a -b*co2.t
#avtomatično
co2.lm$residuals
co2.lm.res <- c(co2.lm$residuals)
plot(co2.lm.res, type = "l")


# 4) TEMPERATURE V LJUBLJANI

tempLJ <- read.table("Temp_LJ_NASA.txt", header = TRUE)
tempLJ.matrika <- as.matrix(tempLJ[,2:13])
tempLJ.chron <- c(t(tempLJ.matrika))
ts.plot(tempLJ.chron)
tempLJ.chron.t <- seq(1, length(tempLJ.chron))
#testiramo ali imajo tempretaure test:
cor.test(tempLJ.chron.t, tempLJ.chron)
#p vrednost 0.003 -> zelo značilno, tempreature rastejo

lm(tempLJ.chron ~ tempLJ.chron.t)

#funkcija APPLY
apply(tempLJ.matrika, 1, mean) #povprečja po letih
apply(tempLJ.matrika, 2, mean) #povprečja po 
head(apply(tempLJ.matrika, 1, mean), -1) -> tempLJ.letne
plot(ts(tempLJ.letne, start =1929))

tempLJ.leta <- seq(1, length(tempLJ.letne))
cor.test(tempLJ.leta, tempLJ.letne)
#letna nihanja zmotijo test, zato imamo p vrednost še bolj značilno, če gledamo samo po letih

