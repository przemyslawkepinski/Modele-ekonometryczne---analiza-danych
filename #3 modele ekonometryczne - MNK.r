install.packages("tseries")
library("tseries")
#setwd("")


#zadanie 1
#sprawdzanie normalnosci rozkladu zmiennej
# a)
vec1=read.csv(file="vec1.csv", sep=",", header=TRUE)
wynik=jarque.bera.test(x=vec1$x)
print(wynik)

#b) vec2
vec2=read.csv(file="vec2.csv", sep=",", header=TRUE)
wynik=jarque.bera.test(x=vec2$x)
print(wynik)

#c) vec3
vec3=read.csv(file="vec3.csv", sep=",", header=TRUE)
wynik=jarque.bera.test(x=vec3$x)
print(wynik)

#d) vec4
vec4=read.csv(file="vec4.csv", sep=",", header=TRUE)
wynik=jarque.bera.test(x=vec4$x)
print(wynik)


#zadanie 2
#Przeprowadzic eksperyment skladajacy sie z 10 tysiecy powtorzen wygenerowania zmiennej o 100
#obserwacjach z rozkladu t-Studenta z 12 stopniami swobody i przeprowadzeniu testu JB. W kazdym
#powtorzeniu zapisujemy wynik (statystyka testowa lub wynik zerojedynkowy). Sprawdzic w jakim
#procencie przypadkow test wskazuje prawidlowy wynik.

#petla  for (i in 1:10000){
#}

Niter=10000
#to samo mozna inaczej zapisac
Niter=1e4
Nobs=100

wektor_wynikow=rep(NA, times=Niter)
for(i in 1:Niter) {
  vec=rt(n=Nobs, df=12)
  wynik=jarque.bera.test(vec)
  wektor_wynikow[i]=(wynik$p.value>0.05)
  #alternatywnie
  # p.value=wynik$p.value
  # wynik_testu=p.value>0.05
  # wektor_wynikow[i]=wynik_testu
}

#jaki procent to odrzucenia hipotezy o normalnosci

odrzucenia=which(wektor_wynikow==FALSE)
liczba.odrzucen=length(odrzucenia)
(procent.odrzucen=liczba.odrzucen/Niter)

#alternatywnie -SZYBSZY SPOSoB (liczy procentowy udzial
#spelnienia danego warunku)
mean(wektor_wynikow==FALSE)


#zadanie 3
#Wygenerowac zmienna o 30 obserwacjach z rozkladu normalnego ze srednia 4 i odchyleniem standardowym 2. Sprawdzic normalnosc rozkladu za pomoca testu JB. Zadanie powtorzyc 10000 razy.
#Sprawdzic w jakim procencie przypadkow test wskazuje prawidlowy wynik.
Niter=10000
Nobs=30

wektor_wynikow=rep(NA, times=Niter)
for(i in 1:Niter) {
  vec=rnorm(n=Nobs, mean=4, sd=2)
  wynik=jarque.bera.test(vec)
  wektor_wynikow[i]=(wynik$p.value>0.05)
}

#jaki procent nieodrzucamy

odrzucenia=which(wektor_wynikow==TRUE)
liczba.odrzucen=length(odrzucenia)
(procent.odrzucen=liczba.odrzucen/Niter)

mean(wektor_wynikow)

#################### METODA NAJMNIEJSZYCH KWADRAT�W ###########################

#zadanie 2
#Oszacowac model ekonometryczny dla danych o samochodach postaci cenai = �0 + �1lpgi + ??i
#1. cena=1, instalacja gazowa = nie
#2. cena=2, instalacja gazowa = tak
#3. cena=3, instalacja gazowa = nie

X=matrix(c(1,1,1,0,1,0), nrow=3, ncol=2, byrow=FALSE)
y = c(1,2,3)

(beta.hat=solve(t(X)%*%X)%*%t(X)%*%y)

#zadanie 4
#Dla zbioru obserwacji (x; y) ??? {(1; 1); (2; 2); (3; 1)} dla modelu empirycznego postaci
#yi = �0 + �1xi + ??i
#wyznaczy� Cov(�b0, �b1).

X=matrix(c(1,1,1,1,2,3), nrow=3, ncol=2, byrow=FALSE)
y = c(1,2,1)

(beta.hat=solve(t(X)%*%X)%*%t(X)%*%y)

#wyznaczamy estymator macierzy wariancji-kowariancji
#s^2=suma kwadratow reszt/N-K
#N- liczba obserwacji, K- liczba parametrow

e= y - X%*%beta.hat
s2e=sum(e^2)/(3-2)

#macierz w-k
(vcov=s2e*solve(t(X)%*%X))

#odpowiedz kowariancja= -2/3


#zadanie 5
#Wykorzystujac dane z pliku vacation.csv dla modelu liniowego postaci:
#  MILESi = �0 + �1INCOMEi + �2AGEi + �3KIDSi + ??i
#setwd("")
dane=read.csv(file="vacation.csv", sep=",", header=TRUE)

#wyznaczyc:
#1. estymatory parametrow 
N= nrow(dane)
X= cbind(rep(1, times=N), dane$income, dane$age, dane$kids)
X
y=dane$miles        

(beta.hat=solve(t(X)%*%X)%*%t(X)%*%y)

#2. wartosc wariancji reszt
e= y - X%*%beta.hat
K=ncol(X)
(s2e=sum(e^2)/(N-K))


#4. Cov(�b1, �b2) oraz Cov(�b2, �b2).
#macierz w-k
(vcov=s2e*solve(t(X)%*%X))

#3. wspoczynniki determinacji R2 oraz R^2 - takie wzory do modeli ze stala
e= y - X%*%beta.hat
RSS=sum(e^2)
TSS=sum((y-mean(y))^2)
(R2=1-RSS/TSS)

(R2adj=1-(N-1)/(N-K)*(1-R2))

#zadanie 6
#jezeli chcemy dac ograniczenie, ze beta2=10,
#wtedy wywalamy po prostu te zmienna,
#bo jej nie trzeba szacowac

#ZAD 7

Niter=10000
Nobs = 100

wektor_wynikow=rep(NA, times=Niter)
for(i in 1:Niter) {
  vec=rt(n=Nobs, df=5)
  wynik=ks.test(vec,"pt",df=5)
  wektor_wynikow[i]=(wynik$p.value>0.05)
}
wektor_wynikow
mean(wektor_wynikow==TRUE)
wynik

#ZAD 8 WYKRESY 
#A
curve(dgamma(x,2,3))
#B
curve(dexp(x,2))

#ZAD 9

Niter=15000
Nobs = 200

wektor_wynikow=rep(NA, times=Niter)
for(i in 1:Niter) {
  vec=rgamma(n=Nobs, 2,3)
  wynik=ks.test(vec,"pexp",2)
  wektor_wynikow[i]=(wynik$p.value>0.05)
}
wektor_wynikow
(mean(wektor_wynikow==TRUE))

#zad 10
Nobs=10000
wek = rchisq(Nobs,df=2)
#write.csv(wek, file="")

#zad 11


lb= (qchisq(0.95,df), Inf)

#zad 12

#setwd("")
dane=read.csv(file="beef.csv", sep=",", header=TRUE)
N= nrow(dane)
X= cbind(rep(1, times=N), log(dane$PB), log(dane$PL), log(dane$PP),
         log(dane$IN))
X
y=log(dane$QB)      
y
(beta.hat=solve(t(X)%*%X)%*%t(X)%*%y)



e= y - X%*%beta.hat
RSS=sum(e^2)
TSS=sum((y-mean(y))^2)
(R2=1-RSS/TSS)


#zad 13
X=matrix(c(1,2,3,4,5), nrow=5, ncol=1, byrow=FALSE)
y = c(5,4,3,2,1)

(beta.hat=solve(t(X)%*%X)%*%t(X)%*%y)

e= y - X%*%beta.hat
RSS=sum(e^2)
TSS=sum((y-mean(y))^2)
(R2=1-RSS/TSS)
#poniewaz jest model ekonometryczny bez stalej