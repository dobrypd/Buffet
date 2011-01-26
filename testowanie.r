#zadanie zaliczeniowe RPiS 2010/2011
#autor: Piotr Dobrowolski
#plik: testowanie.r

#TODO: generowanie grup
generuj_grupy =  function(k) {
   g1 = c(4, 0.1, 0.5)
   g2 = c(6, 0.5, 0.9)
   g3 = c(8, 0.4, 0.6)
   g4 = c(9, 0.1, 0.9)
   g5 = c(4, 0.4, 0.9)
   
   Gr = data.frame(g1, g2, g3, g4, g5)
   return(Gr)
}

# #####SYMULACJA##### #
#return c(ilu_kupilo, ilu_kupilo_grupa, koszt, strategia.wynik_koncowy()) 
   #gdzie każda z tych struktur daje mi informacje dla każdego dnia
symulacja = function(M, k, Grupy, strategia) {
   source("./dane.r")
   strategia[[1]](M)                         #inicjuj
   ilu_kupilo = rep(0, M)                    #ile sprzedanych obiadów każdego dnia
   ilu_kupilo_grupa = data.frame(
      rep(0, M), rep(0, M), rep(0, M),
      rep(0, M), rep(0, M))                  #jw dla kazdej grupy
   koszt = rep(0, M)                         #koszt danego dnia
   
   for(i in 1:M) {   #ity dzień     
      koszt[i] = strategia[[2]]()            #jaki koszt na dziś, może nowy
      
      klient_z_grupy = sample(rep(1:k, N/k))    #przychodzący klienci
      
      kupil = rep(FALSE, N)                     #informacja o tych którzy kupili (przekazywana pod koniec dnia)
      pr_kupi = rep(0, k)                       #licze sobie aktualne prawdopodobienstwo dla kazdej grupy
      for(g in 1:k) {
         if(koszt[i] < Grupy[1, g]) {
            pr_kupi[g] = Grupy[2, g]
         } else {
            pr_kupi[g] = Grupy[3, g]
         }
      }
      
      for(j in 1:N) {   #jty klient itego dnia
         if (sample(c(TRUE, FALSE), 1, prob = c(pr_kupi[klient_z_grupy[j]], 1-pr_kupi[klient_z_grupy[j]])) ) {
            kupil[j] = TRUE
            ilu_kupilo[i] = ilu_kupilo[i] + 1
            ilu_kupilo_grupa[i,klient_z_grupy[j]] = ilu_kupilo_grupa[i,klient_z_grupy[j]] + 1
         }
      }
      strategia[[3]](kupil)
   }

   return(c(ilu_kupilo, ilu_kupilo_grupa, koszt, strategia[[4]]()))
}


#wizualizacja powstałych grup
wizualizacja_grup = function(k, Gr) {
   old.par = par(no.readonly = TRUE)
   colors = rainbow(k)
   par(mfrow=c(3,2))
   for(i in 1:k){
      barplot(c(Gr[2, i], Gr[3,i]), 
              main = paste("Pr. grupa: ",i, ", cena graniczna: ", Gr[1, i]), 
              xlab = "przy niższej cenie / przy wyższej", 
              ylab = "Prawdopodobieństwo", col = c("red", "blue"));
   }
   hist(c(Gr[1,1],Gr[1,2],Gr[1,3],Gr[1,4],Gr[1,5]), 
        main="Częstość występowania ceny granicznej", 
        xlab="Cena", ylab="Częstość", 
        breaks=5, col=colors)
   par(old.par)
}


#test porównujący strategię do losowej i optymalnej
#wynik - wykresy
test_LO = function(M, k, Grupy, strategia.podajkoszt, strategia.pobierz_raport_dnia, strategia.wynik_koncowy) {
   source("./dane.r")            #stałe
   source("./strategie.r")       #strategie
   wizualizacja_grup(k, Grupy)   #grafy pokazujące dane grupy
   
   nazwy_str = c("Podana strategia", "Optymalna", "Losowa")       #nazwy
   akt_koszt = data.frame(0, 0, 0)                                #koszt obiadu tego dnia
   ile_kupilo_grupa = data.frame(rep(0, k), rep(0, k), rep(0, k)) #ile kupilo z danej grupy
   ile_kupilo = data.frame(0, 0, 0)                               #ilu kupilo w danej strategii
   names(akt_koszt) = nazwy_str
   
   
   zarobek = rep(0, M)           #zarobek w dniu i = zarobek[i]
   
   #symulacja
   for(i in 1:M) {   #ity dzień     
      akt_koszt = strategia.podajkoszt()        #jaki koszt na dziś, może nowy
      akt_koszt_losowy = sample(1:10, 1)         #a gdyby koszt był losowy?
      #akt_koszt_optymalny = wartosc_oczekiwana()#a tutaj koszt jest optymalny, dla porównania tej strategii
      klient_z_grupy = sample(rep(1:k, N/k))    #klienci z danej grupy przychodzą losowo
      
      ile_kupilo_grupa = date.frame(1:k, 1:k, 1:k)       #ile kupilo z danej grupy
      
      ile_kupilo_strategia = 0                  #ile kupilo w tej strategii
      ile_kupilo_optymalna = 0                  #ile kupilo w optymalnej str
      ile_kupilo_losowa = 0                     #ile kupilo w str - losuj koszt z równym pr
      kupil = rep(FALSE, N)                     #informacja o tych którzy kupili (przekazywana pod koniec dnia)
   kupil_optymalna = rep(FALSE, N)           #jw dla strategii optymalnej
   kupil_losowa = rep(FALSE, N)              #jw dla strategii - losuj koszt z równym pr
   
   pr_kupi = rep(0, k)                       #licze sobie aktualne prawdopodobienstwo dla kazdej grupy
   for(ik in 1:k) {
      if(akt_koszt < Grupy[1, ik]) {
         pr_kupi[ik] = Grupy[2, ik]
      } else {
         pr_kupi[ik] = Grupy[3, ik]
      }
   }
   for(j in 1:N) {   #jty klient itego dnia
      kupil[j] = sample(c(TRUE, FALSE), 1, prob = c(pr_kupi[klient_z_grupy[j]], 1-pr_kupi[klient_z_grupy[j]]) )
      kupil_optymalna[j] = sample(c(TRUE, FALSE), 1, prob = c(pr_kupi_[klient_z_grupy[j]], 1-pr_kupi[klient_z_grupy[j]]) )
   }
   strategia.pobiez_raport_dnia(kupil)
   }
   
   strategia.wynik_koncowy();
   
   #raport - wykresy
}