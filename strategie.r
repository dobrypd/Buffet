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
   init()
   jak_duzo <<- M
}
strategia_losowa_koszt  = function() {
   return(runif(1, 0, C))
}
strategia_losowa_koniec_dnia  = function(kupilo) {
   #nic nie robię#
}
strategia_losowa_wynik = function() {
   return (1:jak_duzo)
}
strategia_losowa = c(strategia_losowa_inicjuj, strategia_losowa_koszt,
                     strategia_losowa_koniec_dnia, strategia_losowa_wynik)
# #####koniec strategia losowa##### #
# #####strategia optymalna##### #
strategia_optymalna_inicjuj_0 = function(Gr, k) {
   #koszt oplaca sie : 0, a1, a2, a3, a4, a5 - jeden z tych
   #ax to koszty graniczne dla każdej grupy
   init()
   zysk_max = 0
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
         koszt <<- Gr[1,i]
      }
   }
}
strategia_optymalna_inicjuj_1  = function(M) {
   jak_duzo <<- M
   if (jak_duzo < 10) { #na razie dla malych olewczo
      strategia_optymalna <<- strategia_losowa
   }
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
# #####strategia szukajaca##### #
strategia_szukam_inicjuj  = function(M) {
   init()
   jak_duzo <<- M
   koszt <<- 0
   podzial_na = M/10
   str_global_info <<- c(0, 0, C / podzial_na, 0, C, podzial_na)
   names(str_global_info) = c("max_zysk", "max_zysk_dla", "krok", "lewy", "prawy", "podzial_na")
}
strategia_szukam_koszt  = function() {
   return(koszt)
}
strategia_szukam_koniec_dnia  = function(kupilo) {
  if (kupilo * (C-koszt) > str_global_info[1]){
     str_global_info[1] <<- kupilo * (C-koszt)
     str_global_info[2] <<- koszt
  }
  koszt <<- koszt + str_global_info[3]
  if (koszt >= str_global_info[5]) {
     #zmniejszam przedzial
     #tu nie w przedziale tylko troche wiecej
     str_global_info[4] <<- max(str_global_info[2] - str_global_info[3], 0)
     str_global_info[5] <<- min(str_global_info[2] + str_global_info[3], jak_duzo)
     koszt <<- str_global_info[4]
     str_global_info[3] <<- str_global_info[3]*2 / str_global_info[6]
     
  }
}
strategia_szukam_wynik = function() {
   return (1:jak_duzo)
}
strategia_szukam = c(strategia_szukam_inicjuj, strategia_szukam_koszt,
                     strategia_szukam_koniec_dnia, strategia_szukam_wynik)
# #####koniec strategii szukajacej##### #

# #####strategia szukam_wyzej##### #
strategia_szukam_wyzej_inicjuj  = function(M) {
   init()
   jak_duzo <<- M
   koszt <<- 0
   podzial_na = 15
   str_global_info <<- c(0, 0, C / podzial_na, 0, C, podzial_na)
   names(str_global_info) = c("max_zysk", "max_zysk_dla", "krok", "lewy", "prawy", "podzial_na")
}
strategia_szukam_wyzej_koszt  = function() {
   return(koszt)
}
strategia_szukam_wyzej_koniec_dnia  = function(kupilo) {
   if (kupilo * (C-koszt) > str_global_info[1]){
      str_global_info[1] <<- kupilo * (C-koszt)
      str_global_info[2] <<- koszt
   }
   koszt <<- koszt + str_global_info[3]
   if (koszt >= str_global_info[5]) {
      #zmniejszam przedzial
      #tu nie w przedziale tylko troche wiecej
      str_global_info[4] <<- max(str_global_info[2] - 2*str_global_info[3], 0)
      str_global_info[5] <<- min(str_global_info[2] + 3*str_global_info[3], jak_duzo)
      koszt <<- str_global_info[4]
      str_global_info[3] <<- str_global_info[3]*5 / str_global_info[6]
      
   }
}
strategia_szukam_wyzej_wynik = function() {
   return (1:jak_duzo)
}
strategia_szukam_wyzej = c(strategia_szukam_wyzej_inicjuj, strategia_szukam_wyzej_koszt,
                           strategia_szukam_wyzej_koniec_dnia, strategia_szukam_wyzej_wynik)
# #####koniec strategii szukam_wyzej##### #


# #####strategia ile\_gdzie\_lepiej##### #
strategia_ide_gdzie_lepiej_inicjuj  = function(M) {
   init()
   jak_duzo <<- M
   koszt <<- 0
   
   poczatek = M/10
   ktory_dzien = 0     #ktory dzien juz chodze
   delta = 1
   str_global_info <<- c(0, 0, delta, ktory_dzien, poczatek, 0, 0, 0, 0)
   names(str_global_info) = c("max_zysk", "max_zysk_dla", "delta", "ktory_dzien", "poczatek", "takt", "ponizej", "powyzej", "srodek")
}
strategia_ide_gdzie_lepiej_koszt  = function() {
   return(koszt)
}
strategia_ide_gdzie_lepiej_koniec_dnia  = function(kupilo) {
   str_global_info[4] <<- str_global_info[4] + 1
   
   if (str_global_info[5] >= str_global_info[4]) {
      if (kupilo * (C-koszt) > str_global_info[1]){
         str_global_info[1] <<- kupilo * (C-koszt)
         str_global_info[2] <<- koszt
      }
      koszt <<- min(koszt + (C/str_global_info[5]), C-0.001)
   } else {
      if(str_global_info[6] == 0){
         #sprawdzam bylego maxa #dostaje informacje o tym ponizej
         str_global_info[7] <<- kupilo * (C-koszt) #zysk poprzedniego (ponizej)
         #przesuwam sie w gore lub w dol o delte
         xxxx = max(c(str_global_info[7], str_global_info[8], str_global_info[9]))
         if (xxxx == str_global_info[7]) {
            str_global_info[2] <<- str_global_info[2] + str_global_info[3]
         } else if (xxxx == str_global_info[8]) {
            str_global_info[2] <<- str_global_info[2] - str_global_info[3]
         }
         koszt <<- str_global_info[2] #nowy koszt - srodkowy
         
         str_global_info[6] <<- 1
      } else if (str_global_info[6] == 1) {
         #sprawdzam wartosc wieksza #dostaje informacje o tym na srodku
         str_global_info[9] <<- kupilo * (C-koszt) #zysk poprzedniego (srodek)
         
         koszt <<- str_global_info[2] + str_global_info[3] #koszt = koszt + delta
         
         str_global_info[6] <<- 2
      } else if (str_global_info[6] == 2) {
         #sprawdzam wartosc mniejsza # dostaje informacje o tym powyzej
         str_global_info[8] <<- kupilo * (C-koszt) #zysk poprzedniego (powyzej)
         
         koszt <<- str_global_info[2] - str_global_info[3] #koszt = koszt - delta
         
         str_global_info[6] <<- 0
      }
   }
}
strategia_ide_gdzie_lepiej_wynik = function() {
   return (1:jak_duzo)
}
strategia_ide_gdzie_lepiej = c(strategia_ide_gdzie_lepiej_inicjuj, strategia_ide_gdzie_lepiej_koszt,
                               strategia_ide_gdzie_lepiej_koniec_dnia, strategia_ide_gdzie_lepiej_wynik)
# #####koniec strategii ile\_gdzie\_lepiej##### #




# #####strategia ile\_gdzie\_lepiej_2##### #
strategia_ide_gdzie_lepiej_2_inicjuj  = function(M) {
   init()
   jak_duzo <<- M
   koszt <<- 0
   
   poczatek = M/10
   ktory_dzien = 0     #ktory dzien juz chodze
   delta = 0.3
   str_global_info <<- c(0, 0, delta, ktory_dzien, poczatek, 0, 1)
   names(str_global_info) = c("max_zysk", "max_zysk_dla", "delta", "ktory_dzien", "poczatek", "poprzedni", "kierunek")
}
strategia_ide_gdzie_lepiej_2_koszt  = function() {
   return(koszt)
}
strategia_ide_gdzie_lepiej_2_koniec_dnia  = function(kupilo) {
   str_global_info[4] <<- str_global_info[4] + 1
   
   if ((str_global_info[5] + 1) >= str_global_info[4]) {
      if (kupilo * (C-koszt) > str_global_info[1]){
         str_global_info[1] <<- kupilo * (C-koszt)
         str_global_info[6] <<- kupilo * (C-koszt)
         str_global_info[2] <<- koszt
      }
      koszt <<- min(koszt + (C/str_global_info[5]), C-0.001)
   } else {
      if ((kupilo * (C-koszt)) < str_global_info[6]) {
         str_global_info[7] <<- str_global_info[7] * (-1)
      }
      str_global_info[6] <<- kupilo * (C-koszt)
      koszt <<- max(min(str_global_info[2] + (str_global_info[7]) * str_global_info[3], C-0.001), 0)
   }
}
strategia_ide_gdzie_lepiej_2_wynik = function() {
   return (1:jak_duzo)
}
strategia_ide_gdzie_lepiej_2 = c(strategia_ide_gdzie_lepiej_2_inicjuj, strategia_ide_gdzie_lepiej_2_koszt,
                                 strategia_ide_gdzie_lepiej_2_koniec_dnia, strategia_ide_gdzie_lepiej_2_wynik)
# #####koniec strategii ile\_gdzie\_lepiej_2##### #