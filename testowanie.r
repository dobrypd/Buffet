#zadanie zaliczeniowe RPiS 2010/2011
#autor: Piotr Dobrowolski
#plik: testowanie.r

#TODO: generowanie grup
#moze generowanie grup z jakims parametrem jeszcze - np
# function(k, h) gdzie h to parametr grupy
generuj_grupy =  function(k) {
   g1 = c(5, 0.2, 1)
   g2 = c(5, 0.2, 1)
   g3 = c(5, 0.2, 1)
   g4 = c(4, 0.2, 0.8)
   g5 = c(2, 0.1, 0.8)

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
   pr_kupi = 0
   
   for(i in 1:M) {   #ity dzień     
      if (i %% (M/10) == 0) {
         print(paste("Symulacja: ", (i/M) * 100 , "%"))
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


#wizualizacja powstałych grup
wizualizacja_grup = function(k, Gr) {
   old.par = par(no.readonly = TRUE)
   colors = rainbow(k)
   par(mfrow=c(3,2))
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
        breaks=5, col=colors)
   par(old.par)
}


#test porównujący strategię do losowej i optymalnej
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
   
   plot(Zysk ~ Dzień, data=wyniki, col = 
            ifelse(Strategia=="Optymalna", colors[1], ifelse(Strategia=="Losowa", colors[2], colors[3])), 
            xlab="Dzień", ylab="Zysk", pch=16,
            main="Zyski w danym dniu" )
   #plot(xlab="Dzień", ylab="Zysk", pch=16,
   #     main="Zyski w danym dniu" )
   wyniki.spline.Opt <- with(subset(wyniki,Strategia=="Optymalna"),
                            smooth.spline(Dzień, Zysk,df=12))
   wyniki.spline.Los <- with(subset(wyniki,Strategia=="Losowa"), 
                              smooth.spline(Dzień, Zysk, df=12))
   wyniki.spline.Test <- with(subset(wyniki,Strategia=="Testowana"), 
                             smooth.spline(Dzień, Zysk, df=12))
   
   lines(wyniki.spline.Opt, col="red")
   lines(wyniki.spline.Los, col="green")
   lines(wyniki.spline.Test, col="blue")
   legend("bottomleft", legend=c("Optymalna", "Losowa", "Testowana"), col=c("red", "green", "blue"), lwd=2)
}