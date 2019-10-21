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


#datoteka iz spletne strani valjhun
scan("dm.txt")
plot(dm)
plot(dm, type = "l")

dezLJ <- read.csv("Padavine_LJ_ARSO.csv", header = TRUE)
dezLJ
as.matrix(dezLJ[, 2:13]) -> dezLJ.matrika
dezLJ.chron <- c(t(dezLJ.matrika)) ; dezLJ.chron
ts.plot(dezLJ.chron)
dezLJ.chron.ts <- ts(dezLJ.chron)
dezLJ.chron.ts
dezLJ.matrika.ts <- ts(dezLJ.matrika, start = 1991, frequency = 12)

plot(ts(dezLJ.matrika[, 1:5]))
plot(ts(dezLJ.matrika[, 1:5], start = 1991))
dezLJ.chron
ts(dezLJ.chron, start = 1991, frequency = 12)

co2 # vgrajena časovna vrsta v R-ju
plot(co2)
co2.v <- c(co2)
co2.t <- seq(1, length(co2))
co2.v.m <- mean(co2)
co2.v.m
co2.v.m <- mean(co2.v)
co2.v.m
co2.t.m <- mean(co2.t)
b <- sum(co2.t - co2.t.m)*(co2.v - co2.v.m)/(sum(co2.t - co2.t.m))
b
a <- co2.v.m - b * co2.t.m
a
# trend za koliko se količina CO2 poveča na mesec

# z vgrajeno funkcijo v R-u
co2.lm <- lm(co2.v ~ co2.t)
co2.lm

plot(co2, type = "l")
abline(co2.lm) # dodamo regresijsko premico

co2.lm$coefficients
co2.lm$coefficients["co2.t"]

summary(co2.lm)

cor.test(co2.t, co2.v)

co2.v - a - b * co2.t

co2.lm..res <- c(co2.lm$residuals)
plot(co2.lm.res, type = "l")

# datoteka Temp_LJ-NASA.txt na spletu
tempLJ <- read.table("Temp_LJ_NASA.txt", header = TRUE)
tempLJ
tempLJ.matrika <- as_matrix(tempLJ[, 1:13])
tempLJ.chron <- c(t(tempLJ.matrika))
ts.plot(tempLJ.chron)
tempLJ.chron.t <- seq(1, length(tempLJ))
cor.test(tempLJ.chron.t, tempLJ.chron)

lm(tempLJ.chron ~ tempLJ.chron.t)
apply(tempLJ.matrika, 1, mean) # povprečja po letih
apply(tempLJ.matrika, 2, mean) # povprečja po mesecih


