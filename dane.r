#zadanie zaliczeniowe RPiS 2010/2011
#autor: Piotr Dobrowolski
#plik: dane.r

#cena zestawu obiadowego:
C = 10

#klienci bufetu
N = 240

#maksymalna liczba dni
MAXN = 1000

#maksymalna liczba grup
MAXk = 5

#inicjowanie innych zmiennych globalnych
init = function() {
   #zmienne globalne uzywane w programie
   koszt <<- 0
   jak_duzo <<- 0
   
   str_global_info <<- c(0)
}