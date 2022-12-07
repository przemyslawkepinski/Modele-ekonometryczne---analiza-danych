# WYKRESY W R

#zadanie 1
#a)
curve(2*x-4,from=0, to=1)
#b)
curve(x^3-2*x^2+x-6,from=0, to=1)
#c)
curve(sin(10*pi/3*x),from=0, to=2*pi, n=1000)

#zadanie 2 (wczytywanie danych)
setwd("C:\\Users\\bm412461\\Desktop\\Programowanie narzędzi analitycznych\\PNA_Z03")
dane=read.csv(file="EngScoResults.csv", header=TRUE, sep=";")

#wpisujemy odpowiedni rok dla obs 44 i 64, bo jest jakiś dziwny
dane$Rok[44]=1920
dane$Rok[64]=1947

# a) liczby zdobytych goli przez Anglików,
plot(x=dane$Rok, y=dane$Goals.England)

# b) liczby zdobytych goli przez Szkotów w meczach w Szkocj
szkocja=which(dane$Host=="Scotland")
plot(x=dane$Rok[szkocja], y=dane$Goals.Scotland[szkocja])


# QQPLOT - do sprawdzania czy funkcja ma rozklad normalny
#potrzebujemy kwantyle empiryczne i teoretyczne

#zadanie 3  
#Sporządzić samodzielnie wykres kwantylowy dla zmiennej zapisanej w pliku PNA_Z03.csv
#zakładając rozkład normalny zmiennej.
dane=read.csv(file="PNA_Z03.csv", header=TRUE, sep=",")
centyle=seq(from=0.01, to=0.99, by=0.01)
kwantyle_empiryczne=quantile(x=dane$x, probs=centyle)
kwantyle_teoretyczne=qnorm(p=centyle, mean=0, sd=1)

plot(kwantyle_empiryczne, kwantyle_teoretyczne)
abline(a=0, b=1, col="red") #dodawanie linii na wykresie

#zadanie 4
#Wygenerować zmienną tSt zawierającą 1000 obserwacji z rozkładu t-Studenta z 3 stopniami swobody.
#Sporządzić wykres kwantylowy zmiennej z kwantylami z rozkładu
#a) normalnego
tSt=rt(n=1000, df=3)
centyle=seq(from=0.01, to=0.99, by=0.01)
kw.emp=quantile(x=tSt, probs=centyle)

kw.teor=qnorm(p=centyle, mean=0, sd=1)
plot(kw.emp, kw.teor)
abline(a=0, b=1, col="blue")

#b) t-Studenta z 3 stopniami swobody
kw.teor=qt(p=centyle, df=3)
plot(kw.emp, kw.teor)
abline(a=0, b=1, col="blue")

#mozliwe, ze mamy za malo obserwacji,dlatego jest mala zbieznosc wykresow

#zadanie 5
#Wygenerować zmienną wyk zawierającą 10000 obserwacji z rozkładu wykładniczego z parametrem
#λ = 1. Sporządzić wykres kwantylowy zmiennej z kwantylami teoretycznymi z rozkładu Gamma(1,1).
wyk=rexp(n=10000, rate=1)
centyle=seq(from=0.01, to=0.99, by=0.01)
kw.emp=quantile(x=wyk, probs=centyle)
kw.teor=qgamma(p=centyle, shape=1, rate=1)

plot(kw.emp, kw.teor)
abline(a=0, b=1, col="blue")

# TWORZENIE FUNKCJI - "function()", na koniec dajemy "return()", musi też być {}

#zadanie 6
#Napisać funkcję silnia, która dla podanej liczby całkowitej zwróci wartość jej silni.

silnia<- function(x) {
  wartosc=prod(seq(from=1, to=x, by=1))
  return(wartosc)
}
silnia(3)

#zadanie 7
#Napisać funkcję ProcentZer, która dla podanej macierzy zawierającej 0 i 1 zwróci udział liczby zer
#w liczbie elementów macierzy.

procentZer= function(macierz) {
  l.elementow= nrow(macierz)*ncol(macierz)
  indeksy=which(macierz==0)
  l.zer=length(indeksy)
  return(l.zer/l.elementow)
}

mat=diag(2) #tworzymy macierz diagonalna 2x2
procentZer(mat)

mat=matrix(c(1,0,0,0),2,2)
procentZer(mat)

#zadanie 8
#Napisać funkcję PoleProstokata, która dla podanych długości boków wylicza pole prostokąta. Jeżeli
#długość drugiego boku nie będzie podana, to domyślnie ma być przyjmowana wartość
#a) 10,
PoleProstokata=function(a,b) {
  pole=a*b
  return(pole)
}

PoleProstokata(10,1)

#b) długości pierwszego boku.
PoleProstokata=function(a,b=a) {
  pole=a*b
  return(pole)
}

PoleProstokata(10)

# TEST JARQUE-BERA

#zadanie 9
#Napisać program/procedurę realizującą test Jarque-Bera.
#Program powinien zwracać wartość statystyki testowej, p-value oraz zmienną zerojedynkową z wynikiem testu (odrzucenie/nieodrzucenie H0)

testJB= function(x) {
  xbar=mean(x)
  N=length(x)
  mu3=1/N*sum((x-xbar)^3)
  sigma3=(1/N*sum((x-xbar)^2))^(3/2)
  mu4=1/N*sum((x-xbar)^4)
  sigma4=(1/N*sum((x-xbar)^2))^2
  S=mu3/sigma3
  C=mu4/sigma4
  JB=N/6*(S^2+1/4*(C-3)^2)
  return(JB)
}

v=rnorm(n=1000, mean=0, sd=1)
testJB(v)

### Kartkowka 4

#zad1
#a
polekola = function(x){
  pole = pi *x^2
  return(pole)
}
polekola(2)
#b
objkuli = function(x){
  obj = (4/3)*pi *x^3
  return(obj)
}
objkuli(2)

#zad2
objprostopad = function(a,b=a,H=a){
  obj=a*b*H
  return(obj)
}
objprostopad(2,2,2)
objprostopad(2,2)

#zad3
#a1
tSt120=rt(n=120, df=3)
centyle=seq(from=0.01, to=0.99, by=0.01)
kw.emp=quantile(x=tSt, probs=centyle)
kw.teor=qnorm(p=centyle, mean=0, sd=1)
plot(kw.emp, kw.teor)
abline(a=0, b=1, col="blue")

#a2
tSt2000=rt(n=2000, df=3)
centyle=seq(from=0.01, to=0.99, by=0.01)
kw.emp=quantile(x=tSt, probs=centyle)
kw.teor=qnorm(p=centyle, mean=0, sd=1)
plot(kw.emp, kw.teor)
abline(a=0, b=1, col="blue")

#b1
tSt120=rt(n=120, df=3)
centyle=seq(from=0.01, to=0.99, by=0.01)
kw.emp=quantile(x=tSt, probs=centyle)
kw.teor=qt(p=centyle, df=3)
plot(kw.emp, kw.teor)
abline(a=0, b=1, col="blue")

#b2
tSt2000=rt(n=2000, df=40)
centyle=seq(from=0.01, to=0.99, by=0.01)
kw.emp=quantile(x=tSt, probs=centyle)
kw.teor=qt(p=centyle, df=3)
plot(kw.emp, kw.teor)
abline(a=0, b=1, col="blue")

#b3
tSt2000=rt(n=2000, df=40)
centyle=seq(from=0.01, to=0.99, by=0.01)
kw.emp=quantile(x=tSt, probs=centyle)
kw.teor=qt(p=centyle, df=40)
plot(kw.emp, kw.teor)
abline(a=0, b=1, col="blue")
#b4
tSt120=rt(n=120, df=3)
centyle=seq(from=0.01, to=0.99, by=0.01)
kw.emp=quantile(x=tSt, probs=centyle)
kw.teor=qt(p=centyle, df=40)
plot(kw.emp, kw.teor)
abline(a=0, b=1, col="blue")

#zad4
setwd("C:\\Users\\Samorząd WNE\\Desktop\\R wozniak")
dane=read.csv(file="PNA_Z03.csv", header=TRUE, sep=",")
plot(x=dane$X, y=dane$x)

#zad5
#a
y = curve(x*sin(x),from=0, to=20*pi, n=100000)
print(y)

#b
y = curve(atan(x),from=-5*pi, to=5*pi, n=100000)
print(y)