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



# #####STRATEGIE##### #
# #####strategia hillclimbing##### #
strategia_szukam_inicjuj  = function(M) {
   jak_duzo = M
   max_kupilo = 0
   max_kupilo_dla = 0
   krok = M/10
   wyniki = c(M/10) #1/10 przeznaczam na wyszukiwanie
   nowy_koszt = M/10
   wykonano_krokow
}
strategia_szukam_koszt  = function() {
   nowy_koszt = nowy_koszt + krok
   wykonano_krokow = wykonano_krokow + 1
   return(nowy_koszt)
}
strategia_szukam_koniec_dnia  = function(kupilo) {
   wyniki[wykonano_krokow] = kupilo
}
strategia_szukam_wynik = function() {
   return (1:5)
}
strategia_szukam = c(strategia_szukam_inicjuj, strategia_szukam_koszt,
                     strategia_szukam_koniec_dnia, strategia_szukam_wynik)
# #####koniec strategii hillclinbing##### #