### PNA zajecia 10 ###

# UMM -> statystyka z = ((beta - beta0)/std.err(beta)) *sqrt(N)
# oraz test Walda

#zad 1

#setwd("")
Nobs = 1000

library("maxLik")
mi = 0
dane = read.csv(file="t1000.csv", header=TRUE, sep=",")

View(dane)

y = dane$x
N=length(y)

Qmin = function(v) {
  m = rbind((y-0)^2 - v/(v-2), (y-0)^4 - 3*v^2/((v-2)*(v-4)))
  M=rowMeans(m)
  W= m%*%t(m)/N
  val = -1/2*t(M)%*%solve(W)%*%M
  return(val)
}

wynik = maxNR(fn=Qmin, start=5)
summary(wynik)

#H0 : v=6

vcov=-solve(wynik$hessian)
std.err = sqrt(vcov)
(z.test = (wynik$estimate- 6)/std.err*sqrt(N))

#z.test ~ N(0,1)
#wartosc krytyczna +-1.96, inf
#p.value 
(p.value=2*(1-pnorm(abs(z.test), mean=0, sd=1)))

#z.test = 0.69 nie wpada do obszaru krytycznego
#odpowiadajace jej p.value=0.48 > 0.05
#brak podstaw do odrzucenia H0

#zad 2

#setwd("")

dane = read.csv(file="GammaSamp.csv", header=TRUE, sep=",")
View(dane)

y = dane$x
N=length(y)

Qmin = function(alpha) {
  m = rbind(y - alpha, y^2 - alpha*(alpha+1))
  M=rowMeans(m)
  W= m%*%t(m)/N
  val = -1/2*t(M)%*%solve(W)%*%M
  return(val)
}

wynik = maxNR(fn=Qmin, start=5)
summary(wynik)

#H0 : alpha = 5

vcov=-solve(wynik$hessian)
std.err = sqrt(vcov)
(z.test = (wynik$estimate- 5)/std.err*sqrt(N))

#z.test ~ N(0,1)
#wartosc krytyczna +-1.96, inf
#p.value 
(p.value=2*(1-pnorm(abs(z.test), mean=0, sd=1)))

#z.test = -0.98 nie wpada do obszaru krytycznego
#odpowiadajace jej p.value=0.32 > 0.05
#brak podstaw do odrzucenia H0

#zad 3

#setwd()

dane = read.csv(file="Norm1000.csv", header=TRUE, sep=",")
View(dane)

y = dane$x
N=length(y)

Qmin = function(parametry) {
  mi = parametry[1]
  sigma=parametry[2]
  m = rbind(y - mi, (y-mi)^2-sigma^2, (y-mi)^4-3*sigma^4)
  M=rowMeans(m)
  W= m%*%t(m)/N
  val = -1/2*t(M)%*%solve(W)%*%M
  return(val)
}

wynik = maxNR(fn=Qmin, start=c(mean(y), sd(y)))
summary(wynik)

#pierwsza hipoteza prosta
#H0 : mi = 3

vcov=-solve(wynik$hessian)
std.err.mi = sqrt(vcov[1,1])
(z.test = (wynik$estimate[1]-3)/std.err.mi*sqrt(N))

#z.test ~ N(0,1)
#wartosc krytyczna +-1.96, inf
#p.value 
(p.value=2*(1-pnorm(abs(z.test), mean=0, sd=1)))

#z.test = 1.069 nie wpada do obszaru krytycznego
#odpowiadajace jej p.value=0.28 > 0.05
#brak podstaw do odrzucenia H0

#testujemy hipotezę łączną za pomocą statystyki Walda
#H0: mi = 3 oraz sigma = 1

R = diag(2)
theta = wynik$estimate
q=rbind(3,1)
s = R%*%theta - q

#statystyka testowa Walda

W = N*t(s)%*%solve(R%*%vcov%*%t(R))%*%s
print(W)

#W = 1.18
#W ~ chi2(g)
g=2
alpha = 0.05
w.krytyczna = qchisq(p=1-alpha, df=g)
w.krytyczna
#obszar krytyczny chi2 to 5.99, +inf
#statystyka W nie wpada do obszaru krytycznego
#brak podstaw do odrzucenia hipotezy zerowej

#testujemy prosta hipoteze statystyka Walda
#H0: mi=3

R = cbind(1,0)
theta = wynik$estimate
q=3
s = R%*%theta - q
#statystyka testowa Walda

W = N*t(s)%*%solve(R%*%vcov%*%t(R))%*%s
print(W)
#W = 1.14
#W ~ chi2(g)
(z.test^2)
#z.test^2 = statystyka W   !!!!!
g=2
alpha = 0.05
w.krytyczna = qchisq(p=1-alpha, df=g)
w.krytyczna
#obszar krytyczny chi2 to 5.99, +inf
#statystyka W nie wpada do obszaru krytycznego
#brak podstaw do odrzucenia hipotezy zerowej