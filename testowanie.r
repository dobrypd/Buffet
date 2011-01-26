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

#testowanie danej kombinacji M, k, grupa, strategia
#dla danej strategii, od razu porównuję ją do optymalnej i do losowej
test = function(M, k, Grupy, strategia.podajkoszt, strategia.pobiez_raport_dnia, strategia.wynik_koncowy) {
   source("./dane.r")            #stałe
   wizualizacja_grup(k, Grupy)   #grafy pokazujące dane grupy
   
   zarobek = rep(0, M)           #zarobek w dniu i = zarobek[i]
   
   for(i in 1:M) {   #ity dzień
      akt_koszt = strategia.podajkoszt()        #jaki koszt na dziś, może nowy
      akt_koszt_losowy = sample(1:10, 1)         #a gdyby koszt był losowy?
      #akt_koszt_optymalny = wartosc_oczekiwana()#a tutaj koszt jest optymalny, dla porównania tej strategii
      
      klient_z_grupy = sample(rep(1:k, N/k))    #klienci z danej grupy przychodzą losowo
      
      ile_kupilo_grupa = 1:k                    #ile kupilo z danej grupy
      ile_kupilo_strategia = 1:3
      kupil = rep(FALSE, N)                     #informacja o tych którzy kupili (przekazywana pod koniec dnia)
      kupil_optymalna = rep(FALSE, N)           #ile by kupilo w strategii optymalnej
      kupil_losowa = rep(FALSE, N)              #ile by kupilo w strategii losowej
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
      }
      strategia.pobiez_raport_dnia(kupil)
   }
   
   strategia.wynik_koncowy();
   
   #raport - wykresy
}

#testowanie ogólniejsze
testy_0 = function() {
   #testowanie wielu różnych kombinacji M i k, dla różnych strategii
}