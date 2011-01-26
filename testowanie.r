#zadanie zaliczeniowe RPiS 2010/2011
#autor: Piotr Dobrowolski
#plik: testowanie.r

#testowanie wykonania danej strategii
#czyli policzenie zysku
#DEBUG - TO CHWILOWO
Gr = matrix(1:15, 5, 3);
Gr[1,]  = c(3, 0.5, 0.5);
Gr[2,]  = c(4, 0.4, 0.6);
Gr[3,]  = c(5, 0.3, 0.7);
Gr[4,]  = c(6, 0.2, 0.8);
Gr[5,]  = c(7, 0.1, 0.9);

test = function(M, k, Grupy) {
   plot(Gr)
}