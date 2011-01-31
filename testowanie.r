#zadanie zaliczeniowe RPiS 2010/2011
#autor: Piotr Dobrowolski
#plik: testowanie.r

# #####generowanie grup (losowe)##### #
generuj_grupy =  function(k, rozbicie = 100) {
   Gr = data.frame(c(0,0,0), c(0,0,0), c(0,0,0), c(0,0,0), c(0,0,0))
   for(i in 1:k) {
      #Gr[123, 12345]
      Gr[1, i] = sample(seq(0, C, by=(C/rozbicie)), 1)
      Gr[2, i] = sample(seq(0, 0.5, by=(0.5/rozbicie)), 1)
      Gr[3, i] = sample(seq(0.5, 1, by=(0.5/rozbicie)), 1)
   }
   return(Gr)
}

# #####SYMULACJA##### #
symulacja = function(M, k, Grupy, strategia, wypisuj = 0) {
   source("./dane.r")
   strategia[[1]](M)                         #inicjuj
   
   ilu_kupilo = rep(0, M)                    #ile sprzedanych obiadów każdego dnia
   ilu_kupilo_grupa = data.frame(
      rep(0, M), rep(0, M), rep(0, M),
      rep(0, M), rep(0, M))                  #jw dla kazdej grupy
   koszt = rep(0, M)                         #koszt danego dnia
   pr_kupi = 0
   
   for(i in 1:M) {   #ity dzień     
      if(wypisuj > 0) {
         if (i %% (M/10) == 0) {
            print(paste("Symulacja: ", (i/M) * 100 , "%"))
         }
      }
      koszt[i] = strategia[[2]]()            #jaki koszt na dziś, może nowy
      
      for(g in 1:k) {
         pr_kupi = ifelse(koszt[i] < Grupy[1, g], Grupy[2, g], Grupy[3, g])
         kupil = sample(c(TRUE, FALSE), N/k, replace = TRUE, prob = c(pr_kupi, 1-pr_kupi))
         suma = sum(kupil)
         ilu_kupilo[i] = ilu_kupilo[i] + suma
         ilu_kupilo_grupa[i, g] = ilu_kupilo[i] + suma
      }

      strategia[[3]](ilu_kupilo[i])
   }

   return(data.frame(ilu_kupilo, ilu_kupilo_grupa, koszt, strategia[[4]]()))
}


# #####wizualizacja powstałych grup##### #
#wynik - wykresy
wizualizacja_grup = function(k, Gr) {
   old.par = par(no.readonly = TRUE)
   par(mfrow=c(3,2))
   colors = rainbow(k)
   for(i in 1:k){
      barplot(c(Gr[2, i], Gr[3, i]), 
              main = paste("Pr. grupa: ",i, ", cena graniczna: ", Gr[1, i]), 
              xlab = "przy niższej cenie / przy wyższej", 
              ylab = "Prawdopodobieństwo", col = 
              c("red", "blue"));
   }
   hist(c(Gr[1,1],Gr[1,2],Gr[1,3],Gr[1,4],Gr[1,5]), 
        main="Częstość występowania ceny granicznej", 
        xlab="Cena", ylab="Częstość", 
        breaks=10, col=colors)
   par(old.par)
}


# #####test porównujący strategię do losowej i optymalnej##### #
#wynik - wykresy
test_LO = function(M, k, Grupy, strategia, metoda_symulacji) {
   source("./dane.r")            #stałe
   source("./strategie.r")       #strategie
   wizualizacja_grup(k, Grupy)   #grafy pokazujące dane grupy
   
   print("Rozpoczynam symulację strategii - losowa")
   losowo      = metoda_symulacji(M, k, Grupy, strategia_losowa)
   print("Rozpoczynam symulację strategii - optymalna")
   strategia_optymalna_inicjuj_0(Grupy, k)
   optymalnie  = metoda_symulacji(M, k, Grupy, strategia_optymalna)
   print("Rozpoczynam symulację strategii testowanej")
   testowanie  = metoda_symulacji(M, k, Grupy, strategia)
   
   print("ZYSKI:")
   print("Strategia optymalna")
   opt_zysk = (C - optymalnie[,k + 2]) * optymalnie[,1]
   print(summary(opt_zysk))
   print(paste("Całkowity: ", sum((C - optymalnie[,k + 2]) * optymalnie[,1]), "zł"))
   print("Strategia losowa")
   los_zysk = (C - losowo[,k + 2]) * losowo[,1]
   print(summary(los_zysk))
   print(paste("Całkowity: ", sum((C - losowo[,k + 2]) * losowo[,1]), "zł"))
   print("Strategia testowana")
   test_zysk = (C - testowanie[,k + 2]) * testowanie[,1]
   print(summary(test_zysk))
   print(paste("Całkowity: ", sum((C - testowanie[,k + 2]) * testowanie[,1]), "zł"))
   
   wyniki = data.frame(rep(1:M, 3), c(opt_zysk, los_zysk, test_zysk), c(rep("Optymalna", M), rep("Losowa", M), rep("Testowana", M)))
   names(wyniki) = c("Dzień", "Zysk", "Strategia")
   #chcę wszystko na 1 wykresie
   colors = c( col=rgb(200,0,0,50,maxColorValue=255), 
               col=rgb(0,200,0,50,maxColorValue=255),
               col=rgb(0,0,200,50,maxColorValue=255))
   
   #old.par = par(no.readonly = TRUE)
   #par(mfrow=c(2,1))
   pdf("Zysk_strategii_porownanie.pdf")
   plot(Zysk ~ Dzień, data=wyniki, col = 
            ifelse(Strategia=="Optymalna", colors[1], ifelse(Strategia=="Losowa", colors[2], colors[3])), 
            xlab="Dzień", ylab="Zysk", pch=16,
            main="Zyski w danym dniu")
   wyniki.spline.Opt <- with(subset(wyniki,Strategia=="Optymalna"),
                            smooth.spline(Dzień, Zysk,df=12))
   wyniki.spline.Los <- with(subset(wyniki,Strategia=="Losowa"), 
                            smooth.spline(Dzień, Zysk, df=12))
   wyniki.spline.Test <- with(subset(wyniki,Strategia=="Testowana"), 
                            smooth.spline(Dzień, Zysk, df=12))
   
   lines(wyniki.spline.Opt, col="red")
   lines(wyniki.spline.Los, col="green")
   lines(wyniki.spline.Test, col="blue")
   legend("bottomright", legend=c("Optymalna", "Losowa", "Testowana"), col=c("red", "green", "blue"), lwd=2)
   dev.off()
   pdf("Ksztaltowanie_sie_kosztu.pdf")
   #koszt
   x <- 1:M
   y <- testowanie[x,k+2]
   z <- optymalnie[x,k+2]
   plot(x, y, type="n", main="Kształtowanie się kosztu", xlab="Dzień", ylab="Koszt")
   lines(x, y, col = "blue")
   lines(x, z, col = "red")
   legend("bottomright", legend=c("Str. optymalna", "Str. testowana"), col=c("red", "blue"), lwd=2)
   dev.off()
   
   #par(old.par)
}

# #####licze jaki zysk uzyskala strategia po symulacji##### #
test_zysk = function(M, k, Grupy, strategia, metoda_symulacji, wypisuj = 0) {
   source("./dane.r")            #stałe
   source("./strategie.r")       #strategie
   if (wypisuj > 0) {
      print("Rozpoczynam symulację strategii")
   }
   wynik = metoda_symulacji(M, k, Grupy, strategia, wypisuj - 1)
   zysk = (C - wynik[,k + 2]) * wynik[,1]
   
   return(sum(zysk))
}

# #####testy szukajace najgorszego parametru M##### #
testy_szukaj_najgorszych_M = function(Grupy, k, strategia, metoda_symulacji, wypisuj = 0) {
   if (wypisuj > 0) {
      print("SZUKAM NAJGORSZYCH WYNIKOW DLA ROZNYCH M, K i ROZNYCH GRUP")
   }
   source("./dane.r")            #stałe
   source("./strategie.r")       #strategie
   najgorsze_M = 0
   najgorszy_zysk = C * N        #DZIENNY!
   aktualny_zysk = 0
   #mam malo dni!
#    if (wypisuj > 0) {
#       print("Dla malych M")
#    }
#    for(i in 1:20) {
#       gr = generuj_grupy(k)
#       aktualny_zysk = test_zysk(i, k, gr, strategia, metoda_symulacji, wypisuj - 1) / i
#       if (aktualny_zysk < najgorszy_zysk) {
#          najgorszy_zysk = aktualny_zysk
#          najgorsze_M = i
#       }
#       if (wypisuj) {
#          print(paste("[M=", i, "]:: wynik= ", aktualny_zysk))
#       }
#    }
   #zobaczmy w wiekszym zakresie
   if (wypisuj > 0) {
      print("Dla wiekszych M")
   }
   for(i in seq(50, MAXM, by=50)){
      gr = generuj_grupy(k)
      aktualny_zysk = test_zysk(i, k, gr, strategia, metoda_symulacji, wypisuj - 1) / i
      if (aktualny_zysk < najgorszy_zysk) {
         najgorszy_zysk = aktualny_zysk
         najgorsze_M = i
      }
      if (wypisuj > 0) {
         print(paste("[M=", i, "]:: wynik= ", aktualny_zysk))
      }
   }
   
   return (c(najgorsze_M, najgorszy_zysk))
}

# #####testy szukajace najgorszej grupy##### #
testy_szukaj_najgorszych = function(strategia, metoda_symulacji, wypisuj = 0) {
   najgorszy_zysk_gr = C * N  #najgorszy osiagniety zysk
   najgorsza_grupa = 0        #grupa dla jakiej ten zysk osiagnieto
   najgorsze_M = 0            #najgorsze M dla jakiego ten zysk osiagnieto
   
   for(i in 1:10) {
      if (wypisuj > 0) {
         print(paste("SPRAWDZAM KOLEJNA GRUPE lp", i))
      }
      k = sample(1:5, 1)
      gr = generuj_grupy(k)
      if (wypisuj > 0) {
         print("GRUPY")
         print(gr)
      }
      wynik = testy_szukaj_najgorszych_M(gr, k, strategia, metoda_symulacji, wypisuj - 1)
      if (wynik[2] < najgorszy_zysk_gr) {
         najgorsza_grupa = gr
         najgorszy_zysk_gr = wynik[2]
         najgorsze_M = wynik[1]
      }
      #sprawdz optymalny zysk dzienny dla tej grupy
      strategia_optymalna_inicjuj_0(gr, k)
      optymalny = strategia_optymalna_koszt()
      pr = c(0, 0, 0, 0, 0)
      for (j in 1:k) {
         pr[j] = if(optymalny < gr[1,j])(gr[2,j])else(gr[3,j])
      }
      zysk_optymalny = sum((C - optymalny)*pr) / k
      if (wypisuj > 0) {
         print(paste("::{i=", i, "}:: aktualny najgorszy zysk= ", wynik[2], ", dla M= ", wynik[1], "najlepszy możliwy= ", zysk_optymalny * N))
      }
   }
   if (wypisuj > 0) {
      print(paste("Dla calosci najgorszy zysk= ", najgorszy_zysk_gr, ", dla M =", najgorsze_M, "dla grupy: ", najgorsza_grupa))
   }
   return(c(najgorszy_zysk_gr, najgorsza_grupa))
}