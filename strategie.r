#zadanie zaliczeniowe RPiS 2010/2011
#autor: Piotr Dobrowolski
#plik: strategie.r

#interfejs strategii
#każda strategia to krotka funkcji:
#numer -  nazwa                           (parametry)                         [co zwraca]
#  1   - inicjowanie strategii            (M - liczbą dni)                    [nic]
#  2   - podaje nowy koszt na dany dzień  (brak)                              [koszt - liczba]
#  3   - pobiera raport dnia              (wektor wartości logicznych,        [nic]
#                                           czy ity kupił obiad)
#  4   - wystawia wynik pracy             (brak)                              [wektor długości ilości dni c()]

# #####strategia losowa##### #
strategia_losowa_inicjuj  = function(M) {
   #nic nie robię#
}
strategia_losowa_koszt  = function() {
   return(sample(0:10, 1))
}
strategia_losowa_koniec_dnia  = function(kupil) {
   #nic nie robię#   
}
strategia_losowa_wynik = function() {
   return (1:5)
}
strategia_losowa = c(strategia_losowa_inicjuj, strategia_losowa_koszt,
                     strategia_losowa_koniec_dnia, strategia_losowa_wynik)
# #####koniec strategia losowa##### #

# #####strategia optymalna##### #
koszt = 0
jak_duzo = MAXN
strategia_optymalna_inicjuj_0 = function(Grupy, k) {
   EX = sum(C*Grupy[2,]) + sum((C-Grupy[1,])*Grupy[3,])
   koszt = C - (EX)/(2*k)
   return(koszt)
}
strategia_optymalna_inicjuj_1  = function(M) {
   #nic nie robię#
}
strategia_optymalna_koszt  = function() {
   return(sample(0:10, 1))
}
strategia_optymalna_koniec_dnia  = function(kupil) {
   #nic nie robię#   
}
strategia_optymalna_wynik = function() {
   return (1:5)
}
strategia_optymalna = c(strategia_optymalna_inicjuj_1, strategia_optymalna_koszt,
                        strategia_optymalna_koniec_dnia, strategia_optymalna_wynik)
# #####koniec strategia losowa##### #
