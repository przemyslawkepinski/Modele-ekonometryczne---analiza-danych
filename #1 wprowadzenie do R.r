# 0
# Podstawowe funkcje

1+1
2+2*2
8*6
8*6-40
3*(2^3)-(100^0)
sqrt(49)
(5-3)/(2-1)
8^(1.5)
c(1, 4, 8, 2, 9)
c(0,0,0,0,0)
rep(c(17),4)
1:10
seq(8,0,by=-2)

# 1
# odwolanie do wektora

z <- c(0,-1,2,-3,4,-5,6,-7,8,-9)
z 
z2 = z
indeksy = which(z>0)
z2[indeksy] = 0
indeksy = which (z<=0)
z2[indeksy] = 1
z2
# alternatywnie
z2 = z
z2[which(z>0)]=1
# z3
indeksy = which(z<0)
(z3 = z[indeksy])
# wektor y
y = z 
indeksy = which(y<0)
(y[indeksy] = 999)
#tworzenie macierzy
(A = matrix(0, nrow = 2, ncol = 2))
(B = matrix(1:4, nrow=2, ncol = 2, byrow= TRUE))
(C = matrix(1, nrow = 2, ncol = 2))
(D = matrix(c(5,-5,-5,10),2,2))
#Laczenie macierzy 
(K= rbind(cbind(C, A,A), cbind(A,B,A), cbind(D,A,C)))

# 2
# Utworzyc wektory i macierze

(a = rep(c(log(10)),7))
(b= c(1/3, 1/2, 1/6,1/7))
(c=c(pi, pi/3, pi/5, pi/7))
(A= matrix(0, 3,3))
(B=matrix(1,4,4))
(d = c(7,10,9,-5,6,-14,8,-19,-9,-17,12))

# 3

(s= 1:5)
(s2 = s+100)

# 4 suma

sum(seq(1,300,1)^2)

# 5 warunkowanie
#a
(which(d==0))
#b
which(d>0)
#c
which (d<0)
#d
(d2 = which(d>0))
