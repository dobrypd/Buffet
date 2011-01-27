#zadanie zaliczeniowe RPiS 2010/2011
#autor: Piotr Dobrowolski
#plik: testowanie.r

#TODO: generowanie grup
#moze generowanie grup z jakims parametrem jeszcze - np
# function(k, h) gdzie h to parametr grupy
generuj_grupy =  function(k) {
   #g1 = c(2, 0.5, 0.5)
   g1 = c(6, 0.5, 1)
   g2 = c(4, 0.2, 0.6)
   g3 = c(6, 0.3, 0.7)
   g4 = c(7, 0.2, 0.8)
   g5 = c(3, 0.1, 0.8)

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
      if (i %% (M/10) == 0) {
         print(paste("Symulacja: ", (i/M) * 100 , "%"))
      }
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

   return(data.frame(ilu_kupilo, ilu_kupilo_grupa, koszt, strategia[[4]]()))
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
              ylab = "Prawdopodobieństwo", col = 
              c("red", "blue"));
   }
   hist(c(Gr[1,1],Gr[1,2],Gr[1,3],Gr[1,4],Gr[1,5]), 
        main="Częstość występowania ceny granicznej", 
        xlab="Cena", ylab="Częstość", 
        breaks=5, col=colors)
   par(old.par)
}


#test porównujący strategię do losowej i optymalnej
#wynik - wykresy
test_LO = function(M, k, Grupy, strategia, metoda_symulacji) {
   source("./dane.r")            #stałe
   source("./strategie.r")       #strategie
   wizualizacja_grup(k, Grupy)   #grafy pokazujące dane grupy
   
   nazwy_str = c("Podana strategia", "Optymalna", "Losowa")       #nazwy
   akt_koszt = data.frame(0, 0, 0)                                #koszt obiadu tego dnia
   ilu_kupilo_grupa = data.frame(rep(0, k), rep(0, k),
                                 rep(0, k), rep(0, k),
                                 rep(0, k))                       #ile kupilo z danej grupy
   ilu_kupilo = data.frame(0, 0, 0)                               #ilu kupilo w danej strategii
   zarobek = data.frame(rep(0, M), rep(0, M), rep(0, M))          #zarobek w dniu i = zarobek[i]
   
   names(akt_koszt) = nazwy_str
   names(ilu_kupilo_grupa) = nazwy_str
   names(ilu_kupilo) = nazwy_str
   names(zarobek) = nazwy_str
   
   print("Rozpoczynam symulację strategii - losowa")
   losowo      = metoda_symulacji(M, k, Grupy, strategia_losowa)
   print("Rozpoczynam symulację strategii - optymalna")
   strategia_optymalna_inicjuj_0(Grupy, k)
   optymalnie  = metoda_symulacji(M, k, Grupy, strategia_optymalna)
   print("Rozpoczynam symulację strategii testowanej")
   png("opt.png")   
   plot((C - optymalnie[,k + 2]) * optymalnie[,1] ,
        ylab = "Zysk", xlab = "Numer dnia", 
        col="red", pch=16,
        main="Strategia optymalna")
   dev.off()
   png("rand.png")
   plot((C - losowo[,k + 2]) * losowo[,1] ,
        ylab = "Zysk", xlab = "Numer dnia", 
        col="red", pch=16,
        main="Strategia losowa")
   dev.off()
}