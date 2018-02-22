ANOVA.data <-
function(ntype, c1.traits, Ped){
#c1.traits indique  la 1 ere colonne a partir de laquelle les traits commencent
Y=list("list", length = length(ntype))
k=0
for (i in 1:length(ntype)){
if (ntype[i] == 1) {
                   Y[[i]] = as.matrix(Ped[(1+k):(ntype[i]+k), c1.traits:dim(Ped)[2]])
                   Y[[i]] = t(Y[[i]])
                   } else {
                          Y[[i]] = as.matrix(Ped[(1+k):(ntype[i]+k), c1.traits:dim(Ped)[2]])
                          }
k = k + ntype[i]
}
return(Y)
}
