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
   for( i in 1:k){
      barplot(c(Gr[2, i], Gr[3,i]), 
              main = paste("Pr. grupa: ",i, ", cena graniczna: ", Gr[1, i]), 
              xlab = "przy niższej cenie / przy wyższej", 
              ylab = "Prawdopodobieństwo", col = c("red", "blue"));
   }
   hist(c(Gr[1,1],Gr[1,2],Gr[1,3],Gr[1,4],Gr[1,5]), 
        main="Częstość występowania ceny granicznej", 
        xlab="Cena", ylab="Częstość", 
        breaks=5, col=colors)
   #barplot(c(Gr[2,], Gr[3,]), beside=FALSE)
   par(old.par)
}

#testowanie danej kombinacji M, k, strategia
test = function(M, k, strategia) {
   source("./dane.r")
   Grupy = generuj_grupy(k)
   #wizualizacja powstałych grup
   wizualizacja_grup(k, Grupy)
   
   
}

#testowanie ogólniejsze
testy_0 = function() {
   #testowanie wielu różnych kombinacji M i k, dla różnych strategii
}