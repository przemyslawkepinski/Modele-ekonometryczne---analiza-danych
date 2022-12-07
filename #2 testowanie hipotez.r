#Zadanie 1.1

set.seed(1234567)
dane = rnorm(N=1000. mean = 0. sd=1)
#obciazony estymator wariancji

N= length(dane)
1/N*sum((dane-mean(dane))^2)

#nieobciazony estymator wariancji
1/(N-1)*sum((dane-mean(dane))^2)

#Var

var(dane)

# zad1.2
konie = c(2.0, 2.3, 2.7, 2.8, 2.9,2.7, 2.6, 2.6, 2.5, 2.6, 2.7, 
        2.8, 2.8, 2.7, 2.7, 2.6, 2.6, 2.6, 2.6, 2.6, 2.7, 2.6,
        2.6, 2.5, 2.4, 2.4, 2.3)

#srednia liczba koni w tym okresie
mean(konie)
#mediane
median(konie)
#odchylenie standardowe
sd(konie)
#wsp zmiennosci
sd(konie)/mean(konie)*100

#zad.1.3

oceny = c(3.5, 4, 3,3.5, 4.5, 3, 3, 3, 2.5, 4, 2, 
          3.5, 2, 2.5, 3.5, 4, 5, 2.5, 3, 2, 5, 4, 2, 3, 3)
#srednia
mean(oceny)
#mediana
median(oceny)
#odch probkowa
sqrt ((N-1)/N*var(oceny))
#nieobciozony estymator
sd(oceny)
#pozycyjne miary symetrii
Q3 = quantile(oceny, 0.75)
Q1 = quantile(oceny, 0.25)
Med = median(oceny)
A2 = (Q3 - 2*Med + Q1)/(Q3-Q1)
A2

#Zadanie 3.1 model II
N = 20
srednia = 14.5
odch.std = 5.6
alpha = 1-0.95

# dolny kranie przedzialu ufnosci 
(lb = srednia - qt(p=1-alpha/2, df = N-1)*odch.std/sqrt(N))
#gorny
(ub = srednia + qt(p=1-alpha/2, df = N-1)*odch.std/sqrt(N))

#zadanie 3.2 model II
N = 10
srednia = 24.4
odch.std = 3.5
alpha = 1-0.9

# dolny kranie przedzialu ufnosci 
(lb = srednia - qt(p=1-alpha/2, df = N-1)*odch.std/sqrt(N))
#gorny
(ub = srednia + qt(p=1-alpha/2, df = N-1)*odch.std/sqrt(N))

#zadamie 3.3
p.hat = 144/400
alpha = 1-0.95
N = 400

#dolny
(lb = p.hat - qnorm(1-alpha/2, 
      mean = 0, sd=1)*sqrt(p.hat*(1-p.hat))/sqrt(N))
#gorny
(ub = p.hat + qnorm(1-alpha/2, 
                    mean = 0, sd=1)*sqrt(p.hat*(1-p.hat))/sqrt(N))



