#zadanie zaliczeniowe RPiS 2010/2011
#autor: Piotr Dobrowolski
#plik: dane.r

#cena zestawu obiadowego:
C = 10

#klienci bufetu
N = 240

#maksymalna liczba dni
MAXM = 1000

#maksymalna liczba grup
MAXk = 5

#inicjowanie innych zmiennych globalnych
init = function() {
   #zmienne globalne uzywane w programie
   koszt <<- 0
   jak_duzo <<- 0
   
   str_global_info <<- c(0)
}

#jakies przykladowe grupy
   g1 = c(5, 0.2, 1)
   g2 = c(5, 0.2, 1)
   g3 = c(5, 0.2, 1)
   g4 = c(4, 0.2, 0.8)
   g5 = c(2, 0.1, 0.8)
   Gr1 = data.frame(g1, g2, g3, g4, g5)
    
   g1 = c(8.001, 0, 1)
   g2 = c(8.001, 0, 1)
   g3 = c(8.0015, 0.1, 0.5)
   g4 = c(8.002, 0.1, 0.5)
   g5 = c(8.002, 0.1, 0.5)
   Gr2 = data.frame(g1, g2, g3, g4, g5)

   g1 = c(1, 0.01, 0.6)
   g2 = c(2, 0.01, 0.7)
   g3= c(1, 0.03, 0.7)
   g4 = c(1.5, 0.01, 0.8)
   g5 = c(1.5, 0.05, 0.8)
   Gr3 = data.frame(g1, g2, g3, g4, g5)

   g1 = c(9.99, 0, 1)
   g2 = c(9.99, 0, 1)
   g3 = c(9.99, 0, 1)
   g4 = c(9.99, 0, 1)
   g5 = c(9.99, 0, 1)
   Gr4 = data.frame(g1, g2, g3, g4, g5)

   g1 = c(5.5, 0.001, 0.99)
   g2 = c(5.5, 0.001, 0.991)
   g3 = c(5.59, 0.002, 0.93)
   g4 = c(5.555, 0.006, 0.899)
   g5 = c(5.535, 0.009, 0.999)
   Gr5 = data.frame(g1, g2, g3, g4, g5)
   
   g1 = c(3, 0.001, 0.8)
   g2 = c(4.5, 0.2, 0.7)
   g3 = c(5.5, 0.35, 0.77)
   g4 = c(6.98, 0.2, 0.5)
   g5 = c(8.33, 0.5, 0.6)
   Gr6 = data.frame(g1, g2, g3, g4, g5)