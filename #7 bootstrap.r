# zajecia 11

#setwd()

# bootstrap - wnioskowanie w malych probkach

#przykład

dane = c(-0.15,-0.22,0.09,-2.47,-1.02,-0.69,0.52,-0.04,-0.24,-2.74,-0.18,0.39,
-1.14, 0.69,-0.99,-0.20,-0.40,0.33,1.02,-0.22,0.14,0.33,-1.26,0.23,
-1.12,-1.65,-0.36,2.18,-0.39,-0.98,-0.22,1.63,-2.18,0.22,1.13,0.24,
0.54,-0.85,-1.07,0.25)

N = length(dane)

mean(dane)

#petla bootstrapowa

N_iter = 1000

wektor.srednich = rep(NA, times=N_iter)
for(i in 1:N_iter) {
  podprobka = sample(x=dane, size = N, replace = TRUE)
  wektor.srednich[i] = mean(podprobka)
}
#sample losuje probke
#replace = ze zwracaniem

hist(wektor.srednich)
abline(v=mean(wektor.srednich), col="red")
abline(v=mean(dane), col="yellow")

#zad 3

dane = c(4.11,3.55,3.37,4.46,3.69,3.44,1.64,
3.29,3.57,3.35,3.30,1.58,1.75,3.30,2.68)
N =length(dane)

#przedzial ufnosci dla sredniej w rozkladzie z ktorego pochodzi probka

N_iter = 1000

wektor.srednich = rep(NA, times=N_iter)
for(i in 1:N_iter) {
  podprobka = sample(x=dane, size = N, replace = TRUE)
  wektor.srednich[i] = mean(podprobka)
}

hist(wektor.srednich)

#przedzial ufnosci 95%
#przedzial ufnosci w bootstrapie
#percentylowy przedzial ufnosci
alpha = 0.05
quantile(x=wektor.srednich, probs = c(alpha/2, 1-alpha/2))

#2.5%  97.5
#2.709 3.517

#CI = (2.71, 3.52)  -> przedzial ufnosci

#zadanie 4

library("resampledata")
dane = FishMercury

N_iter = 1000

N= length(dane$Mercury)
wektor.srednich=rep(NA, times=N_iter)

#petla bootstraopowa

for(i in 1:N_iter) {
  podprobka = sample(x=dane$Mercury, size = N, replace=TRUE)
  wektor.srednich[i] = mean(podprobka)
}
  
# percentylowy przedzial ufnosci

quantile(x=wektor.srednich, probs=c(alpha/2, 1-alpha/2))

# 2.5%     97.5% 
#0.112     0.314
#CI = (0.11, 0.31) -->  95% przedzial ufnosci

#zad 5

dane = c(431,450,431,453,481,449,441,476,460,482,472,
465,421,452,451,430,458,446,466,476)
N = length(dane)

#testujemy H0 : srednia czyli theta = 440 dla alpha = 0.05
#wyznaczam rozklad statystyki testowej
#za pomoca petli bootstrapowej

N_iter = 1000
wektor.U.test = rep(NA, times=N_iter)

for(i in 1:N_iter) {
  #losuje podprobke
  podprobka = sample(x=dane, size = N, replace=TRUE)
  #wyznaczam statystyke testowa dla podprobki
  U.test = (mean(podprobka) - mean(dane))/sd(podprobka)*sqrt(N)
  #zapisuje wynik
  wektor.U.test[i] = U.test
}


hist(wektor.U.test)

#przedzial ufnosci dla stat. testowej U
alpha =0.05
quantile(x=wektor.U.test, probs = c(alpha/2, 1-alpha/2))

#   2.5%     97.5% 
#-1.982696  2.202756
#CI = (-1.98, 2.2) --> przedzial ufnosci 95%

#wlasciwe wnioskowanie
#wartosci statystyki testowej U dla danych
#ktorymi dysponujemy

(U.test = (mean(dane)-440)/sd(dane)*sqrt(N))

#statystyka testowa = 3.62 nie wpada do przedziału ufnosci
#Wobec tego odrzucamy H0