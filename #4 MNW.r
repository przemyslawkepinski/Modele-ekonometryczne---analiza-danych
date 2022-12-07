# ---------- METODA NAJWIĘKSZEJ WIARYGODNOŚCI --------- #

# WAŻNE DO PROJEKTU ZALICZENIOWEGO !!!

# ZADANIE 1

#Załóżmy, że kupiliśmy monetę w sklepie z magicznymi rekwizytami. Moneta wygląda na symetryczną,
#ale nie koniecznie taka jest. Rzuciliśmy monetą 8 razy i otrzymaliśmy OOOROROR. Jaka wartość
#będzie dobrym oszacowaniem p, prawdopodobieństwa orła?
#  P(X1 = 1, X2 = 1, X3 = 1, X4 = 0, X5 = 1, X6 = 0, X7 = 1, X8 = 0)
#  = P(X1 = 1)P(X2 = 1)P(X3 = 1)· · · P(X8 = 0) = p^5(1 - p)^3
#Sporządzić wykres funkcji wiarygodności dla powyższego przykładu z asymetryczną monetą.


lnL = function (p) {
  ll = 5*log(p)+3*log(1-p)
  return(ll)
}
curve(lnL(x), from=0, to=1)
abline(v=5/8, col="red")

#a teraz za pomocą pakietu i funkcji

install.packages("maxLik")
library("maxLik")

# maksymalizacja z użyciem metody Newtona-Raphsona
wynik=maxNR(fn=lnL, start=0.5)
summary(wynik)

#gradient mówi nam o nachyleniu (dodatni to rosnąca),
#powinien być blisko 0, bo wtedy wiemy, że to maksimum

#Gradient to pochodna funkcji wiarygodności po parametrze
#Hessian to pochodna gradientu

### dodatkowo z użyciem gradientu i hessiana ###

gradient = function (p) {
  gr = 5/p-3/(1-p)
  return(gr)
}

hessjan = function(p) {
  h = -5/p^2-3/(1-p)^2
  return(h)
}

wynik= maxNR(fn=lnL, grad=gradient, hess=hessjan, start=0.05)
summary(wynik)

# ZADANIE 2

# Załóżmy, ze x1 = 3, x2 = 4, x3 = 3, x4 = 7 pochodzą z rozkładu Poissona z nieznanym parametrem
# ??. Funkcja gęstości rozkładu Poissona to f(x; ??) = ??^x*e^(-??x)/x!, dla x = 0, 1, 2, . . ..
# Sporządzić wykres funkcji wiarygodności.

x = c(3,4,3,7)

lnL= function(lambda) {
  ll=sum(x)*log(lambda)-lambda*sum(x)-sum(log(factorial(x)))
  return(ll)
}

curve(lnL(x), from=0, to=5)

#spodziewamy się, że lambda jest gdzieś w okolicy 1 (maksimum funkcji wiarygodności)
#czego szukamy? oszacownaia parametru lambda (gdzie: mianownik nie zależy od lambdy, więc usuwając <<go>> -->
# -sum(log(factorial(x)))) nic sę nie zmieni:
x=c(3,4,3,7)
lnl=function(lambda){
  ll=sum(x)*log(lambda)-lambda*sum(x)
  return(ll)
}
curve(lnl(x),from=0,to=5)

#liczymy gradient i hessian

gradient = function (lambda) {
  gr = sum(x)/lambda-sum(x)
  return(gr)
}

hessjan = function(lambda) {
  h = -sum(x)/lambda^2
  return(h)
}

wynik= maxNR(fn=lnL, grad=gradient, hess=hessjan, start=0.05)
summary(wynik)


# ZADANIE 3

#Załóżmy, ze x1 = 1.46, x2 = 0.81, x3 = 0.88, x4 = 0.53, x5 = 0.46 pochodzą z rozkładu wykładniczego
#z nieznanym parametrem ??. Funkcja gęstości rozkładu wykładniczego to f(x; ??) = ??e-??x, dla x ??? 0.
#Sporządzić wykres funkcji wiarygodności.



# ZADANIE 5

#Niech X1 = 1, X2 = X3 = 2, X4 = 3, X5 = 4 będą próbą losową z rozkładu Cauchy’ego o rozkładzie
#f(x; ??) = 1/ [??(1 + (x - ??)^2)] dla -??? < x < ???, -??? < ?? < ???.
#Znaleźć ^??MNW i narysować funkcję wiarygodności.

x=c(1,2,2,3,4)
N=length(x)

lnL= function(theta) {
  ll=-N*log(pi)-sum(log(1+(x-theta)^2))
  return(ll)
}

wynik=maxNR(fn=lnL, start=1)
summary(wynik)

curve(lnL(x), from=0, to=4)
# niestety nie wychodzi, bo próbujemy odjąć krótszy wektor x od wektora dłuższego theta!
# w związku z tym musimy to zrobić RĘCZNIE

#więc jeszcze raz - przy użuciu pętli dla x

theta_vector = seq(from=0, to=4, by=0.01)
lnL=0
for(i in 1:length(x)) {
  lnL=lnL+(-log(pi)-log(1+(x[i]-theta_vector)^2))
}

plot(theta_vector, lnL, type="l")


# ZADANIE 6

#W celu oszacowania wartości przeciętnej czasu bezawaryjnej pracy maszyny pewnego typu z partii
#tych maszyn wybrano w sposób losowy 7 maszyn i obserwowano czasy ich pracy do momentu awarii.
#Uszkodzenia wystąpiły w chwilach (w h): 51, 115, 150, 190, 217, 228, 350.
#Wiedząc, że czas bezawaryjnej pracy maszyny ma rozkład wykładniczy o gęstości danej wzorem f(x; ??) = ??e-??x
#znaleźć ocenę wartości przeciętnej czasu bezawaryjnej pracy maszyny.

x=c(51, 115, 150, 190, 217, 228, 350)
N=length(x)


lnl=function(lambda){
  ll=N*log(lambda)-lambda*sum(x)
  return(ll)
}

gradient = function (lambda) {
  gr = N/lambda-sum(x)
  return(gr)
}

hessjan = function(lambda) {
  h = -N/lambda^2
  return(h)
}

wynik= maxNR(fn=lnL, grad=gradient, hess=hessjan, start=0.05)
summary(wynik)

curve(lnl(x),from=0,to=0.01)
abline(v=wynik$estimate, col="blue")

#ZADANIE 7
install.packages("maxLik")
library("maxLik")

x=c(0.5677523, 0.8236657, 0.4568736, 0.5125185, 0.3192458)
N=length(miu)


lnl=function(miu){
  ll=5*log(1/sqrt(2*pi))-(1/2)*(sum(x^2)-2*miu*sum(x)+miu^2)
  return(ll)
}

gradient = function (miu) {
  gr = sum(x)-miu
  return(gr)
}

hessjan = -1
wynik= maxNR(fn=lnl(), grad=gradient, hess=hessjan, start=0.05)
summary(wynik)

curve(lnl(x),from=0,to=10)
abline(v=wynik$estimate, col="blue")

#wynik to miu =2.68

#ZAD 8

x=c(4.056, 3.823,3.456, 4.125,5.319)
N=length(x)


lnl=function(lambda){
  ll=N*log(1/2)+N*3*log(lambda)-lambda*sum(x)+2*(sum(log(x))
  return(ll)
}

gradient = function (lambda) {
  gr = (N*3)/lambda-sum(x)
  return(gr)
}

hessjan = function(lambda) {
  h = (-N*3)/lambda^2
  return(h)
}

wynik= maxNR(fn=lnl, grad=gradient, hess=hessjan, start=0.05)
summary(wynik)

curve(lnl(x),from=10,to=40)
abline(v=wynik$estimate, col="blue")

