Data.Without.miss <-
function(Ped){
j=0
Ped= as.matrix(Ped)
symptomes.Without.NA = Ped
for (i in 1:dim(Ped)[1]){
r=0
for (k in 1:dim(Ped)[2]) {
if (is.na(Ped[i, k]) == TRUE)   r=r+1
}
if ( r > 0 ) { ### ici on fait la condition r>1 car dans le fichier Ped il y a une colonne vide et R conside cette colonne comme des NA
#print(dim(symptomes.Without.NA)[1])
symptomes.Without.NA = symptomes.Without.NA[-(i-j), ]
j=j+1
}
}
return(symptomes.Without.NA)
}
