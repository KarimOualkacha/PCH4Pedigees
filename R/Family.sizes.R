Family.sizes <-
function(Ped){

fam.id <- NULL
fam.names <- as.vector(unique(Ped[ ,1]))
for(i in 1:length(fam.names))
{
    fam.id[Ped[ ,1]==fam.names[i]] <- i
}
Ped[,1] <- fam.id

nbre.family = length(unique(Ped[ ,1]))  #### ceci donne le nbre total de familles dans un jeu de donnees
ntype=vector(length = nbre.family)
rep.family = c(Ped[ ,1],0)
j=1
cont1=1
for (i in 1:nbre.family){
cont=0
while(rep.family[j] == Ped[cont1,1] ){
j=j+1
cont=cont+1
}
ntype[i]=cont
cont1 = cont1 + cont
}
return(ntype)
}
