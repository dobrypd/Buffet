#zadanie zaliczeniowe RPiS 2010/2011
#autor: Piotr Dobrowolski
#plik: strategie.r

#interfejs strategii
#każda strategia to krotka funkcji:
#numer -  nazwa                           (parametry)                         [co zwraca]
#  1   - inicjowanie strategii            (M - liczbą dni)                    [nic]
#  2   - podaje nowy koszt na dany dzień  (brak)                              [koszt - liczba]
#  3   - pobiera raport dnia              (ile - ilość kupionych obiadów)     [nic]
#  4   - wystawia wynik pracy             (brak)                              [wektor długości ilości dni c()]

# #####strategia losowa##### #
strategia_losowa_inicjuj  = function(M) {
   #nic nie robię#
}
strategia_losowa_koszt  = function() {
   return(sample(0:10, 1))
}
strategia_losowa_koniec_dnia  = function(kupilo) {
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
jak_duzo = 0
strategia_optymalna_inicjuj_0 = function(Gr, k) {
   #koszt oplaca sie : 0, a1, a2, a3, a4, a5 - jeden z tych
   #ax to koszty graniczne dla każdej grupy
   zysk_max = 0
   koszt = 0   
   pr = Gr[2, ]
   zysk_max = sum(C*(Gr[2,])) #dla 0
   
   zysk = 0
   aktkoszt = 0
   for (i in 1:k) {
      for (j in 1:k) {
         pr[j] = if(Gr[1, i] < Gr[1,j])(Gr[2,j])else(Gr[3,j])
      }
      zysk = sum((C - Gr[1, i])*pr)
      if(zysk > zysk_max) {
         zysk_max = zysk
         koszt = Gr[1,i]
      }
   }
}
strategia_optymalna_inicjuj_1  = function(M) {
   jak_duzo = M
}
strategia_optymalna_koszt  = function() {
   return(koszt)
}
strategia_optymalna_koniec_dnia  = function(kupilo) {
   #nic nie robię#
}
strategia_optymalna_wynik = function() {
   return (1:jak_duzo)
}
strategia_optymalna = c(strategia_optymalna_inicjuj_1, strategia_optymalna_koszt,
                        strategia_optymalna_koniec_dnia, strategia_optymalna_wynik)
# #####koniec strategia losowa##### #
